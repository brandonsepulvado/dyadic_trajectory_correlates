# ==============================================================================
# creating the time series data on which to run kshape
# ==============================================================================

# load packages
library(dplyr)
library(tidyr)
library(tsibble)
library(furrr)
library(purrr)
library(imputeTS)

# must first run : data_import_clean, create_dyad_trends
# information on academic calendar in academic_cal_breaks

# steps

# remove missing
df_dyad <- data_dyad_steps %>% 
  filter(!is.na(abs_diff),
         datadate < '2015-12-19') %>% # start of xmas break
  distinct() # keep only distinct observations

# add dyad identifier (single, rather than two cols)
df_dyad <- df_dyad %>% 
  unite(id_dyad, vertex_1, vertex_2, sep = '-', remove = FALSE)

# get number of days in period
date_start <- min(df_dyad$datadate)
date_end <- max(df_dyad$datadate)
period_in_days <- as.numeric(date_end - date_start)

# number of observations per dyad
dyad_days <- df_dyad %>% 
  group_by(id_dyad) %>% 
  summarise(n_days = n_distinct(datadate),
            prop_total = n_days / period_in_days)

# keep only days starting in september
df_dyad <- df_dyad %>% 
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
df_dyad <- df_dyad %>% 
  filter(id_dyad %in% dyads_to_keep)

# create nested 
data_steps_f2015 <- df_dyad %>% 
  select(id_dyad, datadate, abs_diff) %>% 
  group_by(id_dyad) %>% 
  nest(.key = 'dyad_data')
#saveRDS(data_steps_f2015, file = here::here('output', 'data_steps_f2015.rds'))

dyad_tsibble <- df_dyad %>% 
  select(id_dyad, datadate, abs_diff) %>% 
  as_tsibble(key = id_dyad, index = datadate) %>% 
  fill_gaps(.full = TRUE) %>% 
  group_by_key() %>% 
  nest(dyad_data = c(datadate, abs_diff))

# function to interpolate missing values
interpolate <- function(df){
  to_return <- df %>% 
    as_tibble() %>% 
    mutate(abs_diff = na_interpolation(abs_diff, option = 'linear'))
  return(to_return)
}

# ensure not parallel (for running on laptop)
future::plan(sequential)

# now interpolate missing observations
data_interpolated <- dyad_tsibble %>% 
  mutate(dyad_data2 = map(dyad_data, interpolate)) %>% 
  unnest(dyad_data2)

# save so don't have to re-run
saveRDS(data_interpolated, file = here::here('output',
                                             paste0('data_interpolated_', 
                                                    Sys.Date(), 
                                                    '.rds')))
