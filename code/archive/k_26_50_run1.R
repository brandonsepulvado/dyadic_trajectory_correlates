# ==============================================================================
# testing parallelization 

library(dtwclust)
library(parallel)
library(dplyr)
library(here)
#library(tictoc)
library(purrr)

# load data
data_interpolated <- readRDS(file = here('Private', 'nethealth', 'data_interpolated.rds'))

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

# set range for value of k
range_k <- 26:50

# estimate across k values
test_k <- tsclust(test_prepped,
                  k = range_k,
                  distance = 'sbd',
                  centroid = 'shape',
                  preproc = zscore,
                  seed = 1234,
                  control = partitional_control(iter.max = 500L))

# give names to elements based upon k value
names(test_k) <- paste0(range_k)

# save results
saveRDS(test_k, file = here('Private', 'nethealth', 'k_26_50_run1.rds'))
