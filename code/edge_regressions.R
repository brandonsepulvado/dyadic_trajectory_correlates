# ==============================================================================
# collapsing arcs to edges via igraph

# load packages
library(igraph)
library(stringr)
library(dplyr)
library(glue)

# import arc list
edge_list_joined <- read.csv(here::here('input', 'arcs_fall_2015.csv')) %>% 
  as_tibble()

# get arc_list
arc_list <- edge_list_joined %>% 
  select(participid_1, participid_2)

# create igraph object
graph_obj <- graph_from_data_frame(arc_list, directed = TRUE)

# collapse to undirected
graph_obj <- as.undirected(graph_obj, 
                           mode = "collapse") # edge if at least one directed

# get new edgelist
edges_undir <- as_edgelist(graph_obj) %>% 
  as_tibble() %>% 
  mutate_at(vars(V1, V2), str_trim, side = 'both')

# add identifier
edges_undir <- edges_undir %>% 
  unite(identifier, V1, V2, sep = '-', remove = FALSE)

# data_final comes from analysis_clusters.R

# test edge variable
data_final %>% 
  unite(id_dyad_rev, vertex_2, vertex_1, sep = '-', remove = FALSE) %>% 
  mutate(edge_test = case_when(id_dyad %in% edges_undir$identifier | id_dyad_rev %in% edges_undir$identifier ~ TRUE,
                               TRUE ~ FALSE)) %>% 
  summarise(#sum(edge), 
            sum(edge_test)) # still 687

# test on all dyads
data_dyad_steps %>% 
  distinct(vertex_1, vertex_2) %>% 
  unite(dyad1, vertex_1, vertex_2, sep = '-', remove = FALSE) %>% 
  unite(dyad2, vertex_2, vertex_1, sep = '-', remove = FALSE) %>% 
  mutate(edge_test = case_when(dyad1 %in% edges_undir$identifier | dyad2 %in% edges_undir$identifier ~ TRUE,
                               TRUE ~ FALSE)) %>% 
  summarise(sum(edge_test)) # 4190


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
  mutate(#edge = id_dyad %in% edge_list_key$id_dyad,
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
  mutate(in_edgelist = case_when(not_found %in% edge_list_joined$participid_1 | not_found %in% edge_list_joined$participid_2 ~ TRUE,
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
logistic_test <- glm(edge_arc ~ assigned_cluster, data = data_final, family = 'binomial')


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

# flac <- function(x,y){
#   temp.fit1 <- logistf::logistf(y~x, pl=FALSE)
#   temp.pseudo <- c(rep(0,length(y)), rep(1, 2*length(y)))
#   temp.neww <- c(rep(1,length(y)), temp.fit1$hat/2, temp.fit1$hat/2)
#   temp.fit2 <- logistf::logistf(c(y,y,1-y)~rbind(x,x,x)+temp.pseudo, weights=temp.neww, family=binomial(logit),
#                         firth=FALSE, pl=TRUE)
#   res <- list()
#   res$coefficients <- temp.fit2$coefficients[which("temp.pseudo"!= names(temp.fit2$coefficients) )]
#   res$fitted <- temp.fit2$predict[1:length(y)]
#   res$linear.predictors <- temp.fit2$linear.predictors[1:length(y)]
#   res$probabilities <- temp.fit2$probabilities[which("temp.pseudo"!= names(temp.fit2$prob) )]
#   res$ci.lower <- temp.fit2$ci.lower[which("temp.pseudo"!= names(temp.fit2$ci.lower)) ]
#   res$ci.upper <- temp.fit2$ci.upper[which("temp.pseudo"!= names(temp.fit2$ci.upper)) ]
#   return(res)
# }

# run models
model_edge_flic <- flic(x = data_final$assigned_cluster, y = data_final$edge_arc) # was originally edge
# model_edge_flac <- flac(x = data_final$assigned_cluster, y = data_final$edge_arc) # was originally edge


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
           file = here::here('output', glue('tables_edge_reg_{Sys.Date()}.xlsx')))

# ROC (function in analysis_clusters.R) ========================================

# previous function won't work here

# get roc info
roc_curve <- roc(model_edge_firth$y ~ model_edge_firth$predict)

# plot figure
plot.roc(roc_curve,
         print.auc = TRUE,
         xlim=c(1,0))
