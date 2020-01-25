# ==============================================================================
# create edge list
# ==============================================================================

# import edge list (original)
edge_list <- read.dta13(file = here::here('input', 'arcsfall2015.dta')) %>% 
  as_tibble() # keeping i and j bc will change with fibit identifiers

# import id keys
id_key <- read.dta13(file = here::here('input', 'FitbitEgoId.dta')) %>% 
  as_tibble() %>% 
  mutate(fitbitid = case_when(fitbitid == 'NA' ~ NA_character_,
                              TRUE ~ fitbitid),
         fitbitid = stringr::str_trim(fitbitid, 'both'))

# join fitbit ids
edge_list_joined <- edge_list %>% 
  left_join(id_key, by = c('i' = 'egoid')) %>% 
  left_join(id_key, by = c('j' = 'egoid'), suffix = c('_1', '_2')) %>% 
  filter(!is.na(fitbitid_1) & !is.na(fitbitid_2)) %>% 
  mutate(fitbitid_1 = stringr::str_trim(fitbitid_1, 'both'),
         fitbitid_2 = stringr::str_trim(fitbitid_2, 'both')) %>% 
  unite(arc_1, fitbitid_1, fitbitid_2, sep = "-", remove = FALSE) %>% 
  unite(arc_2, fitbitid_2, fitbitid_1, sep = "-", remove = FALSE)

# missing data due to fitibit ids NA
edge_list %>% 
  left_join(id_key, by = c('i' = 'egoid')) %>% 
  left_join(id_key, by = c('j' = 'egoid'), suffix = c('_1', '_2')) %>% 
  summarise(i_missing = sum(is.na(fitbitid_1)),
            j_missing = sum(is.na(fitbitid_2)),
            iandj_missing = sum(is.na(fitbitid_1) & is.na(fitbitid_2)),
            iorj_missing = sum(is.na(fitbitid_1) | is.na(fitbitid_2)))

# how many fitbitid missing from beginning
id_key %>% 
  mutate(fitbitid = case_when(fitbitid == 'NA' ~ NA_character_,
                              TRUE ~ fitbitid)) %>% 
  summarise(n_missing = sum(is.na(fitbitid)))

# ==============================================================================
# collapsing arcs to edges via igraph

# get arc_list
arc_list <- edge_list_joined %>% 
  select(fitbitid_1, fitbitid_2)

library(igraph)

# create igraph object
graph_obj <- graph_from_data_frame(arc_list, directed = TRUE)

# collapse to undirected
graph_obj <- as.undirected(graph_obj, 
                           mode = "collapse") # edge if at least one directed

library(stringr)

# get new edgelist
edges_undir <- as_edgelist(graph_obj) %>% 
  as_tibble() %>% 
  mutate_at(vars(V1, V2), str_trim, side = 'both')

# add identifier
edges_undir <- edges_undir %>% 
  unite(identifier, V1, V2, sep = '-', remove = FALSE)

# test edge variable
data_final %>% 
  unite(id_dyad_rev, vertex_2, vertex_1, sep = '-', remove = FALSE) %>% 
  mutate(edge_test = case_when(id_dyad %in% edges_undir$identifier | id_dyad_rev %in% edges_undir$identifier ~ TRUE,
                               TRUE ~ FALSE)) %>% 
  summarise(sum(edge), sum(edge_test)) # still 687

# test on all dyads
data_dyad_steps %>% 
  distinct(vertex_1, vertex_2) %>% 
  unite(dyad1, vertex_1, vertex_2, sep = '-', remove = FALSE) %>% 
  unite(dyad2, vertex_2, vertex_1, sep = '-', remove = FALSE) %>% 
  mutate(edge_test = case_when(dyad1 %in% edges_undir$identifier | dyad2 %in% edges_undir$identifier ~ TRUE,
                               TRUE ~ FALSE)) %>% 
  summarise(sum(edge_test))


# ==============================================================================

# # combined fitbit ids into one
# edge_list_key <- edge_list %>%
#   unite(id_dyad, fitbitid_1, fitbitid_2, sep = '-', remove = FALSE)
# 
# # see how many in data_final
# data_final %>% 
#   select(id_dyad) %>% 
#   filter(id_dyad %in% edge_list_key$id_dyad) # 620

# create edge variable
data_final <- data_final %>% 
  mutate(edge = id_dyad %in% edge_list_key$id_dyad,
         edge_arc = case_when(id_dyad %in% edge_list_joined$arc_1 | id_dyad %in% edge_list_joined$arc_2 ~ TRUE,
                              TRUE ~ FALSE))

not_in_final <- data_final %>% 
  filter(!id_dyad %in% edge_list_joined$arc_1 & !id_dyad %in% edge_list_joined$arc_2) %>% 
  select(vertex_1, vertex_2)

vec_not_in_final <- tibble(
  not_found = c(not_in_final$vertex_1, not_in_final$vertex_2)
  ) %>% 
  distinct()

vec_not_in_final %>% 
  mutate(in_edgelist = case_when(not_found %in% edge_list_joined$fitbitid_1 | not_found %in% edge_list_joined$fitbitid_2 ~ TRUE,
                                 TRUE ~ FALSE))


# ==============================================================================
# firth logistic regression
model_edge_firth <- logistf::logistf(edge_arc ~ assigned_cluster, data = data_final)

# get summary
firth_summary <- summary(model_edge_firth)
aod::wald.test(b = coef(model_edge_firth), Sigma = vcov(model_edge_firth), Terms = 2:24)
l <- cbind(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
           0, 1, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0)
aod::wald.test(b = coef(model_edge_firth), Sigma = vcov(model_edge_firth), L = l)
logistic_test <- glm(edge ~ assigned_cluster, data = data_final, family = 'binomial')


# flic and flac are taken from 
# https://onlinelibrary-wiley-com.proxy.library.nd.edu/action/downloadSupplement?doi=10.1002%2Fsim.7273&file=sim7273-sup-0001-Supplementary.pdf
flic <- function(x,y) {
  temp.fit1 <- logistf::logistf(y~x)
  temp.lp <- temp.fit1$linear.predictors-temp.fit1$coef[1]
  temp.fit2 <- glm(y~1,family=binomial(link=logit),offset=temp.lp)
  W <- diag(temp.fit2$fitted.values*(1-temp.fit2$fitted.values))
  temp.var <- solve(t(cbind(1,x))%*%W%*%cbind(1,x))
  beta.0.se <- sqrt(temp.var[1,1])
  ic <- temp.fit2$coef
  res <- list()
  res$coefficients <- c(ic,temp.fit1$coef[-1])
  res$fitted <- temp.fit2$fitted
  res$linear.predictions <- temp.fit2$linear
  res$probabilities <- c(summary(temp.fit2)$coef[,"Pr(>|z|)"],temp.fit1$prob[-1])
  res$ci.lower <- c(ic-beta.0.se*1.96,temp.fit1$ci.lower[-1])
  res$ci.upper <- c(ic+beta.0.se*1.96,temp.fit1$ci.upper[-1])
  return(res)
}

flac <- function(x,y){
  temp.fit1 <- logistf::logistf(y~x, pl=FALSE)
  temp.pseudo <- c(rep(0,length(y)), rep(1, 2*length(y)))
  temp.neww <- c(rep(1,length(y)), temp.fit1$hat/2, temp.fit1$hat/2)
  temp.fit2 <- logistf::logistf(c(y,y,1-y)~rbind(x,x,x)+temp.pseudo, weights=temp.neww, family=binomial(logit),
                        firth=FALSE, pl=TRUE)
  res <- list()
  res$coefficients <- temp.fit2$coefficients[which("temp.pseudo"!= names(temp.fit2$coefficients) )]
  res$fitted <- temp.fit2$predict[1:length(y)]
  res$linear.predictors <- temp.fit2$linear.predictors[1:length(y)]
  res$probabilities <- temp.fit2$probabilities[which("temp.pseudo"!= names(temp.fit2$prob) )]
  res$ci.lower <- temp.fit2$ci.lower[which("temp.pseudo"!= names(temp.fit2$ci.lower)) ]
  res$ci.upper <- temp.fit2$ci.upper[which("temp.pseudo"!= names(temp.fit2$ci.upper)) ]
  return(res)
}

# run models
model_edge_flic <- flic(x = data_final$assigned_cluster, y = data_final$edge)
model_edge_flac <- flac(x = data_final$assigned_cluster, y = data_final$edge)


# output tables
output_firth <- tibble(
  betas = model_edge_firth$coefficients,
  prob = model_edge_firth$prob,
  ci_lower = model_edge_firth$ci.lower,
  ci_upper = model_edge_firth$ci.upper
)

output_flic <- tibble(
  beta = model_edge_flic$coefficients, 
  prob = model_edge_flic$probabilities, 
  ci_lower = model_edge_flic$ci.lower, 
  ci_upper = model_edge_flic$ci.upper
  ) 

# save to excel file
write.xlsx(list(output_firth, output_flic),
           sheetName = c('firth', 'flic'),
           file = here::here('output', 'tables_edge_reg.xlsx'))
