#####
##### run time series models
#####

library(dplyr)
library(plyr)
library(lubridate)
library(forecast)

###
# load data if needed (uncomment)
#yad_sleep_diffs <- readRDS("dyad_sleep_diffs.rds")
#dyad_steps_diffs <- readRDS("dyad_steps_diffs.rds")
###



### prepare data

# data preparation conducted on ND servers due to space
# see the following files:
# sleep_steps_missing.sh
# sleep_steps_missing_input.R
# steps_filtered.rds
# sleep_filtered.rds

# import resulting objects (if more than 24 hour gap, df becomes NULL)
sleep_filtered <- readRDS("/afs/crc.nd.edu/user/b/bsepulva/Private/sleep_filtered.rds")
steps_filtered <- readRDS("/afs/crc.nd.edu/user/b/bsepulva/Private/steps_filtered.rds")

# import on laptop
sleep_filtered <- readRDS("sleep_filtered.rds")
steps_filtered <- readRDS("steps_filtered.rds")

# count non-null elements
sleep_null <- sapply(sleep_filtered, is.null)
sum(sleep_null) # 376388
steps_null <- sapply(steps_filtered, is.null)
sum(steps_null)
  # same number of missing elements (as expected)

# propotion of elements that are not null
1 - (sum(sleep_null) / length(sleep_filtered))

# remove NULL entries
sleep_filtered <- plyr::compact(sleep_filtered)
steps_filtered <- plyr::compact(steps_filtered)

# plot distribution of element nrow()s
sleep_nrow <- sapply(sleep_filtered, nrow)
steps_nrow <- sapply(steps_filtered, nrow)

# plot for sleep
sleep_nrow_dist <- sleep_nrow %>%
  as_tibble() %>% 
  ggplot(aes(x = value)) +
  geom_density(show.legend = FALSE) +
  #coord_cartesian(xlim=c(1, 100)) +
  labs(title = "Distribution of Number of Observations for Dyads",
       subtitle = "Final Data before Time Series Models",
       x = "Number of Observations",
       y = "Density") +
  theme_minimal()

ggsave(file = "sleep_nrow_dist.png", 
	plot = sleep_nrow_dist,
	path = "/afs/crc.nd.edu/user/b/bsepulva/Private/")

# plot for steps
steps_nrow_dist <- steps_nrow %>%
  as_tibble() %>% 
  ggplot(aes(x = value)) +
  geom_density(show.legend = FALSE) +
  #coord_cartesian(xlim=c(1, 100)) +
  labs(title = "Distribution of Number of Observations for Dyads",
       subtitle = "Final Data before Time Series Models",
       x = "Number of Observations",
       y = "Density") +
  theme_minimal()

ggsave(file = "steps_nrow_dist.png", 
	plot = steps_nrow_dist,
	path = "/afs/crc.nd.edu/user/b/bsepulva/Private/")


# remove elements with fewer than 10 observations
sleep_filtered <- mclapply(sleep_filtered, function(x){
  if (nrow(x) < 10) {
    return(NULL)
  } else {
    return(x)
  }
})

# remove NULL elements
sleep_filtered <- plyr::compact(sleep_filtered) # now 10820 elements


### run time series models using auto-arima

auto_arima_results <- parallel::mclapply(sleep_filtered, function(x){
  x$diff %>% 
    forecast::auto.arima()
}, mc.cores = getOption("mc.cores", 4L))

auto_arima_results <- lapply(dyad_sleep_diffs_filtered, function(x){
  x$diff %>% 
    forecast::auto.arima()})

# save output 
saveRDS(auto_arima_results, file = "auto_arima_results_1.rds")

# extract basic info for each model to use for clustering
arma_output_basic <- lapply(auto_arima_results, 
                      function(x){
                        arma <- x$arma %>% 
                          t() %>% 
                          as_tibble()
                      return(arma)})

# name variables with descriptive names

# collapse arma_output
arma_output <- bind_rows(arma_output)

# extract more ddetailed information
arma_output_detail <- lapply(auto_arima_results,
                             function(x){
                               output <- x$coef %>% 
                                 t() %>% 
                                 as.data.frame()
                               return(output)
                            
                             })










  # dichotomize variable types for clustering?

# extract basic info for each model to use for clustering
arma_output <- parallel::mclapply(auto_arima_results, 
                                  function(x){
                                    arma <- x$arma %>% 
                                      t() %>% 
                                      as_tibble()
                                    return(arma)
                                  }, mc.cores = getOption("mc.cores", 4L))

# name variables with descriptive names

# collapse arma_output
arma_output <- bind_rows(arma_output)

# check if all columns have non-zero values
max(arma_output$V3)
max(arma_output$V4)
max(arma_output$V7)

# keep only non zero cols and those that vary
arma_output <- arma_output %>% 
  select(V1, V2, V5, V6)



### clustering

# k-means

# optimal k
library(factoextra)
# via average silhouette width
fviz_nbclust(arma_output, 
             FUNcluster = kmeans, 
             method = "silhouette") # 7 

# via total within sum of squares
fviz_nbclust(arma_output, 
             FUNcluster = kmeans, 
             method = "wss") # 7 

# via gap stat
fviz_nbclust(arma_output, 
             FUNcluster = kmeans, 
             method = "gap_stat")# 7










# remove empty columns
arma_output_filtered <- arma_output %>% 
  select(V1, V2, V5, V6)

# remove V5 because variance == 0
arma_output_filtered <- arma_output_filtered %>% 
  select(-V5)

# clustering using k = 7
set.seed(1234)
kmeans_7 <- kmeans(arma_output_filtered, 
                   centers = 7,
                   nstart = 25)
# Visualize
fviz_cluster(kmeans_7, data = arma_output_filtered,
             ggtheme = theme_minimal())
