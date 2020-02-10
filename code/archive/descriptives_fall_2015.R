# ==============================================================================
# descriptives for first semester
# ==============================================================================

# load packages
library(tidyr)
library(tsibble)
library(furrr)
library(purrr)
library(imputeTS)

# must first run : data_import_clean, create_dyad_trends
# information on academic calendar in academic_cal_breaks

# steps

# remove mis
test <- data_dyad_steps %>% 
  filter(!is.na(abs_diff),
         datadate < '2015-12-19') %>% # start of xmas break
  distinct() # keep only distinct observations

# plot
test %>% 
  count(datadate) %>% 
  ggplot(aes(x = datadate, y = n)) +
  geom_col(fill = 'orange2', alpha = .6) +
  theme_minimal() +
  labs(x = 'Date',
       y = 'Number of dyad observations',
       title = 'Distribution of non-missing observations',
       subtitle = 'Semester: Fall 2015')

# add dyad identifier (single, rather than two cols)
test <- test %>% 
  unite(id_dyad, vertex_1, vertex_2, sep = '-', remove = FALSE)

# get number of days in period
date_start <- min(test$datadate)
date_end <- max(test$datadate)
period_in_days <- as.numeric(date_end - date_start)

# number of observations per dyad
dyad_days <- test %>% 
  group_by(id_dyad) %>% 
  summarise(n_days = n_distinct(datadate),
            prop_total = n_days / period_in_days)

# get distribution of non-missing data for semester 1
dyad_days %>% 
  ggplot(aes(prop_total)) +
  geom_density(fill = 'orange2', color = 'orange2', alpha = .6) +
  theme_minimal() +
  labs(title = 'Distribution of dyadic non-missing observations',
       x = 'Proportion of non-missing observations',
       y = 'Density')

# how many dyads are present at different thresholds
dyad_days %>% 
  summarise(p50 = nrow(filter(., prop_total >= .5)),
            p75 = nrow(filter(., prop_total >= .75)))

# keep only days starting in september
test <- test %>% 
  filter(datadate >= '2015-09-01') %>% # patterns established by Sept 1
  distinct() # keep only distinct observations

# keep only dyads with at least 75% of daily observations in given period

# set minimally acceptable proportion
prop_min <- .75

# get ids to retain
dyads_to_keep <- dyad_days %>% 
  filter(prop_total >= prop_min) %>% 
  pull(id_dyad)

# filter out irrelevant dyads
test <- test %>% 
  filter(id_dyad %in% dyads_to_keep)

# create nested 
data_steps_f2015 <- test %>% 
  select(id_dyad, datadate, abs_diff) %>% 
  group_by(id_dyad) %>% 
  nest(.key = 'dyad_data')
saveRDS(data_steps_f2015, file = here::here('output', 'data_steps_f2015.rds'))

data_sample <- data_steps_f2015 %>% slice(1:10)
test_sample <- test %>% 
  select(id_dyad, datadate, abs_diff) %>% 
  slice(1:200)

test_tsibble <- test %>% 
  select(id_dyad, datadate, abs_diff) %>% 
  as_tsibble(key = id_dyad, index = datadate) %>% 
  fill_gaps(.full = TRUE) %>% 
  group_by_key() %>% 
  nest(dyad_data = c(datadate, abs_diff))

test_tsibble %>% 
  slice(1:10) %>% 
  unnest()
  
  test_tsibble_sample <- test %>% 
  slice()
  select(id_dyad, datadate, abs_diff) %>% 
  as_tsibble(key = id_dyad, index = datadate) %>% 
  fill_gaps(.full = TRUE)
  
interpolate <- function(df){
  to_return <- df %>% 
    as_tibble() %>% 
    mutate(abs_diff = na_interpolation(abs_diff, option = 'linear'))
  return(to_return)
}

library(furrr)
library(tictoc)

future::plan(sequential)

tic()
data_interpolated <- test_tsibble %>% 
  mutate(dyad_data2 = map(dyad_data, interpolate)) %>% 
  unnest(dyad_data2)
toc()

# # save so don't have to re-run
# saveRDS(data_interpolated, file = here::here('output', 'data_interpolated.rds'))
# 
# plan(multiprocess)
# tic()
# test_tsibble %>% 
#               head(10000) %>% 
#               mutate(dyad_data2 = future_map(dyad_data, interpolate)) %>% 
#               unnest(dyad_data2)
# toc()
# 
# compare3 <- test_tsibble %>% 
#   head(10) %>% 
#   mutate(dyad_data2 = future_map(dyad_data, interpolate)) %>% 
#   unnest(dyad_data2)
# 
# test_tsibble_10 <- test_tsibble 
# 
# tic()
# test_tsibble_10$dyad_data2 <- test_tsibble_10$dyad_data %>% 
#   map(., interpolate)
# 
# testing <- test_tsibble_10 %>% 
#   group_split(id_dyad) %>% 
#   map_dfr(.,
#                  ~ .x %>% 
#                    unnest(dyad_data2))
# toc()
# 
# #plan(multiprocess)
# tic()
# test_tsibble_10$dyad_data2 <- test_tsibble_10$dyad_data %>% 
#   future_map(., interpolate)
# 
# testing <- test_tsibble_10 %>% 
#   group_split(id_dyad) %>% 
#   future_map_dfr(.,
#                   ~ .x %>% 
#                    unnest(dyad_data2))
# toc()


# testing sample of time series
tic()
test_prepped <- data_interpolated %>% 
  ungroup() %>% 
  slice(1:500000) %>% 
  select(id_dyad, abs_diff) %>% 
  group_split(id_dyad, keep = FALSE) %>% 
  map(., ~ .x %>% 
        pull(abs_diff))
toc()

# test_results <- tsclust(test_prepped, 
#         distance = 'sbd',
#         centroid = 'shape',
#         preproc = zscore
#         )

range_k <- 2:25
tic()
test_k <- tsclust(test_prepped,
                  k = range_k,
                  distance = 'sbd',
                  centroid = 'shape',
                  preproc = zscore)
names(test_k) <- paste0(range_k)
toc()
tic()
eval_k <- sapply(test_k, cvi, type = 'internal') %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column(var = 'index') %>% 
  # have minize or maximize variable
  mutate(goal = case_when(index %in% c('COP', 'DB', "DBstar") ~ 'minimize',
                          TRUE ~ 'maximize')) %>% 
  gather(key = 'number_k', value = 'value', -c(index, goal))
toc()

library(ggforce)
eval_k %>%
  group_by(index) %>% 
  mutate(value = (value - min(value)) / (max(value) - min(value))) %>% 
  ggplot(aes(x = as.numeric(number_k), y = value, group = index, color = index)) +
  geom_point() +
  geom_line() +
  facet_wrap(~goal) +
  theme_minimal() 


# estimating with k = 5
tic()
test_k5 <- tsclust(test_prepped,
                  k = 5,
                  distance = 'sbd',
                  centroid = 'shape',
                  preproc = zscore)
toc()
plot(test_k5)
