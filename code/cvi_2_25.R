# ==============================================================================
# get metrics to evaluate k

library(dtwclust)
library(parallel)
library(dplyr)
library(here)
library(purrr)

# create multi-process workers
workers <- makeCluster(8)

# load dtwcluster in each one, and make them use 1 thread per worker
invisible(clusterEvalQ(workers, {
  library(dtwclust)
  RcppParallel::setThreadOptions(1L)
}))

# register your workers, e.g., with doParallel
require(doParallel)
registerDoParallel(workers)

# load data (output from k2_25)
test_k <- readRDS(file = here('Private', 'nethealth', 'k_2-25_20200203.rds'))

# get class
class(test_k)

# get tibble of cvi values for different k
eval_k <- sapply(test_k, cvi, type = 'internal') 

# # make into tibble with min/max variable
# eval_k <- eval_k %>%
#   as.data.frame() %>%
#   tibble::rownames_to_column(var = 'index') %>%
#   # have minize or maximize variable
#   mutate(goal = case_when(index %in% c('COP', 'DB', "DBstar") ~ 'minimize',
#                           TRUE ~ 'maximize'))
# # 
# # # gather for plotting
# # eval_k <- eval_k %>% 
# #   gather(key = 'number_k', value = 'value', -c(index, goal))
# 
# save results
saveRDS(eval_k, file = here::here('Private', 'nethealth', 'cvi_2_25_20200203.rds'))