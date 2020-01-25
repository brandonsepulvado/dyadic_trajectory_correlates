# ==============================================================================
# relationship cluster membership and sociodemographic traits
# ==============================================================================

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
data_final %>% summarise_at(vars(genderv1:youreligv2), ~sum(is.na(.)))

# generate homophily variables
data_final <- data_final %>% 
  mutate(gender_same = gender_v1 == gender_v2,
         race_same = race_v1 == race_v2,
         relig_same = yourelig_v1 == yourelig_v2)

# add cluster assignments
data_final <- data_final %>% 
  mutate(assigned_cluster = as.factor(kshape_24@cluster),
         assigned_cluster = relevel(assigned_cluster, ref = '6'))



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

library(texreg)
library(margins)

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
        file = here::here('output', 'reg_table.doc'),
        single.row = TRUE)
