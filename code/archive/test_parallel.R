# ==============================================================================
# testing parallelization 

library(dtwclust)
library(parallel)
library(dplyr)
library(here)
library(tictoc)
library(purrr)
library(tidyr)
library(ggplot2)
library(tibble)
library(openxlsx)

# load data
data_interpolated <- readRDS(file = here('output', 'data_interpolated.rds'))

# testing sample of time series
tic()
test_prepped <- data_interpolated %>% 
  ungroup() %>% 
  #slice(1:1500000) %>% 
  select(id_dyad, abs_diff) %>% 
  group_split(id_dyad, keep = FALSE) %>% 
  map(., ~ .x %>% 
        pull(abs_diff))
toc()


# create multi-process workers
workers <- makeCluster(4)
# load dtwcluster in each one, and make them use 1 thread per worker
invisible(clusterEvalQ(workers, {
  library(dtwclust)
  RcppParallel::setThreadOptions(1L)
}))
# register your workers, e.g., with doParallel
require(doParallel)
registerDoParallel(workers)

range_k <- 2:25
tic()
test_k <- tsclust(test_prepped,
                  k = range_k,
                  distance = 'sbd',
                  centroid = 'shape',
                  preproc = zscore,
                  seed = 1234,
                  control = partitional_control(iter.max = 200L))
names(test_k) <- paste0(range_k)
toc()

# save results
saveRDS(test_k, file = here::here('output', 'k_2-25.rds'))

# get tibble of cvi values for different k
eval_k <- sapply(test_k[1:2], cvi, type = 'internal') 

eval_k <- eval_k %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column(var = 'index') %>% 
  # have minize or maximize variable
  mutate(goal = case_when(index %in% c('COP', 'DB', "DBstar") ~ 'minimize',
                          TRUE ~ 'maximize')) 

# previous bit was run on crc server
eval_k <- readRDS(file = here('output', 'cvi_2_25_test.rds'))

eval_k <- eval_k %>% 
  gather(key = 'number_k', value = 'value', -c(index, goal))

eval_k %>%
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


# save results
saveRDS(eval_k, file = here::here('output', 'eval_k_2-25.rds'))

test_range <- readRDS(file = here('output', 'k_2-25_run5.rds'))

# # run with k = 24
# kshape_24 <- tsclust(test_prepped,
#                   k = 24,
#                   distance = 'sbd',
#                   centroid = 'shape',
#                   preproc = zscore,
#                   seed = 1234,
#                   control = partitional_control(iter.max = 500L))

# load from crc hpc
kshape_24 <- readRDS(file = here::here('output', 'kshape_24.rds'))

# get table with cluster name and size

cluster_sizes <- kshape_24@clusinfo %>% 
  rowid_to_column(var = 'cluster_number') %>% 
  select(-av_dist)

# save table to excel
write.xlsx(cluster_sizes, file = here::here('output', 'cluster_sizes.xlsx'))

# plot the numbers
cluster_sizes %>% 
  ggplot(aes(x = as.factor(cluster_number), y = size)) +
  geom_col(fill = 'orange2', alpha = 1) +
  geom_text(aes(label=size), vjust= 1.6, color="white", size=3) +
  theme_minimal() +
  labs(#title = 'Cluster sizes',
       x = 'Cluster number',
       y = 'Number of dyads')

# plot the clustering output
plot(kshape_24, type = 'centroids') +
  facet_wrap(~cl, scales = 'free', ncol = 4) +
  labs(x = 'Day',
       y = 'Value (z-normalized)',
       title = NULL)
