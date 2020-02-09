# ==============================================================================
# testing parallelization 

library(dtwclust)
library(parallel)
library(dplyr)
library(here)
library(purrr)

# load data
data_interpolated <- readRDS(file = here('Private', 'nethealth', 'data_interpolated_2020-02-02.rds'))

# testing sample of time series
test_prepped <- data_interpolated %>% 
  ungroup() %>% 
  select(id_dyad, abs_diff) %>% 
  group_split(id_dyad, keep = FALSE) %>% 
  map(., ~ .x %>% 
        pull(abs_diff))

# create multi-process workers
workers <- makeCluster(24)

# load dtwcluster in each one, and make them use 1 thread per worker
invisible(clusterEvalQ(workers, {
  library(dtwclust)
  RcppParallel::setThreadOptions(1L)
}))

# register your workers, e.g., with doParallel
require(doParallel)
registerDoParallel(workers)

# do with 24 clusters

# estimate across k values
kshape_24 <- tsclust(test_prepped,
                  k = 24,
                  distance = 'sbd',
                  centroid = 'shape',
                  preproc = zscore,
                  seed = 1234,
                  control = partitional_control(iter.max = 500L))

# save results
saveRDS(kshape_24, file = here('Private', 'nethealth', 'kshape_24_20200208.rds'))

# do with 21 clusters

# estimate across k values
kshape_21 <- tsclust(test_prepped,
                     k = 21,
                     distance = 'sbd',
                     centroid = 'shape',
                     preproc = zscore,
                     seed = 1234,
                     control = partitional_control(iter.max = 500L))

# save results
saveRDS(kshape_21, file = here('Private', 'nethealth', 'kshape_21_20200208.rds'))
