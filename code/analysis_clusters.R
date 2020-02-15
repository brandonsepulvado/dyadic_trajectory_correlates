# ==============================================================================
# relationship cluster membership and sociodemographic traits
# ==============================================================================

# load packages
library(openxlsx)
library(texreg)
library(margins)

# ==============================================================================
# import and visualize clustering results

# 21 clusters ==================================================================

# model with 21 clusters
kshape_21 <- readRDS(here::here('output', 'kshape_21_20200208.rds'))

# visualize 21 clusters 
plot(kshape_21, type = 'centroids') +
  facet_wrap(~cl, scales = 'free', ncol = 3) +
  labs(x = 'Day',
       y = 'Value (z-normalized)',
       title = NULL)

# get table with cluster name and size

# prepare info
cluster_sizes_21 <- kshape_21@clusinfo %>% 
  tibble::rowid_to_column(var = 'cluster_number') %>% 
  select(-av_dist)

# save table to excel
write.xlsx(cluster_sizes_21, file = here::here('output', 'cluster_sizes_21.xlsx'))

# plot the numbers
cluster_sizes_21 %>% 
  ggplot(aes(x = as.factor(cluster_number), y = size)) +
  geom_col(fill = 'orange2', alpha = 1) +
  geom_text(aes(label=size), vjust= 1.6, color="white", size=3) +
  theme_minimal() +
  labs(#title = 'Cluster sizes',
    x = 'Cluster number',
    y = 'Number of dyads')

# 24 clusters ==================================================================

# import model with 24 clusters
kshape_24 <- readRDS(here::here('output', 'kshape_24_20200208.rds'))

# visualize 24 clusters
plot(kshape_24, type = 'centroids') +
  facet_wrap(~cl, scales = 'free', ncol = 4) +
  labs(x = 'Day',
       y = 'Value (z-normalized)',
       title = NULL)

# get table with cluster name and size

# prepare info
cluster_sizes_24 <- kshape_24@clusinfo %>% 
  tibble::rowid_to_column(var = 'cluster_number') %>% 
  select(-av_dist)

# save table to excel
write.xlsx(cluster_sizes_24, file = here::here('output', 'cluster_sizes_24.xlsx'))

# plot the numbers
cluster_sizes_24 %>% 
  ggplot(aes(x = as.factor(cluster_number), y = size)) +
  geom_col(fill = 'orange2', alpha = 1) +
  geom_text(aes(label=size), vjust= 1.6, color="white", size=3) +
  theme_minimal() +
  labs(#title = 'Cluster sizes',
    x = 'Cluster number',
    y = 'Number of dyads')



# ==============================================================================
# prepare data

# create data to join
data_demog <- data_fitbit %>% 
  select(participid, gender, race, yourelig) %>% 
  distinct()

# get dyad_id from data_interpolated
dyads <- data_interpolated %>% 
  select(id_dyad) %>% 
  distinct()

# create vertex variables
dyads <- dyads %>% 
  separate(id_dyad, c('vertex_1', 'vertex_2'), sep = "-", remove = FALSE)

# join demographics
data_final <- dyads %>% 
  left_join(data_demog, by = c('vertex_1' = 'participid')) %>% 
  left_join(data_demog, by = c('vertex_2' = 'participid'),
            suffix = c('_v1', '_v2')) %>% 
  ungroup()

# verify no NAs
data_final %>% summarise_at(vars(gender_v1:yourelig_v2), ~sum(is.na(.)))

# generate homophily variables
data_final <- data_final %>% 
  mutate(gender_same = (gender_v1 == gender_v2),
         race_same = (race_v1 == race_v2),
         relig_same = (yourelig_v1 == yourelig_v2))

# add cluster assignments
data_final <- data_final %>% 
  mutate(assigned_cluster = as.factor(kshape_21@cluster),
         assigned_cluster = relevel(assigned_cluster, ref = '11')) # 11 is largest



# ==============================================================================
# descriptive stats

# function to get counts
get_counts <- function(variable){
  fitbit_data %>% 
    select(participid, {{variable}}) %>% 
    distinct() %>% 
    filter(!is.na({{variable}})) %>% 
    count({{variable}})
}

# number of each gender
get_counts(gender)

# number in each racial category
get_counts(race)

# number in each religious category
get_counts(yourelig)



# ==============================================================================
# estimate logistic models

# predicting same gender
model_gender <- glm(as.numeric(gender_same) ~ assigned_cluster,
                    family = "binomial",
                    data = data_final)
l <- cbind(0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
           0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0, 0)
aod::wald.test(b = coef(model_gender), Sigma = vcov(model_gender), L = l)

# model for race
model_race <- glm(as.numeric(race_same) ~ assigned_cluster,
                    family = "binomial",
                    data = data_final)


# model for religion
model_relig <- glm(as.numeric(relig_same) ~ assigned_cluster,
                  family = "binomial",
                  data = data_final)

l <- cbind(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
           0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
aod::wald.test(b = coef(model_relig), Sigma = vcov(model_relig), L = l)

# save output
htmlreg(list(model_gender, model_race, model_relig),
        file = here::here('output', 'reg_table_2020-02-15.doc'),
        single.row = TRUE)

# ROC for models ===============================================================

get_roc_plot <- function(log_reg){
  # get predictions
  predictions <- predict(log_reg, type = c('response'))
  # get roc info
  roc_curve <- roc(log_reg$y ~ predictions)
  # plot the results
  roc_plot <- plot.roc(roc_curve,
                       print.auc = TRUE,
                       xlim = c(1,0))
  # return the plot
  return(roc_plot)
}

# get curve for gender model
get_roc_plot(model_gender) # .52

# get curve for race model
get_roc_plot(model_race) # .56

# get curve for religion model
get_roc_plot(model_relig) # .56
