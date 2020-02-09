# goal : visualize the results of the cluster validity indices

# run after cvi_2_25.R
# note that it stopped running after sapply()

# import data
eval_k <- readRDS(here::here('output', 'cvi_2_25_20200203.rds'))

# get average across five runs
eval_k_mean5 <- (eval_k[,1:24] + eval_k[,25:48] + eval_k[,49:72] + eval_k[,73:96] + eval_k[,97:120])/5

# plot results
eval_k_mean5 %>%
  group_by(index) %>% 
  mutate(value = (value - min(value)) / (max(value) - min(value))) %>% 
  ggplot(aes(x = as.numeric(number_k), y = value, group = index, color = index)) +
  geom_point() +
  geom_line() +
  facet_wrap(~goal) +
  theme_minimal() +
  labs(x = 'Number of Clusters',
       y = 'Value',
       color = 'Metric')