#####
##### load and clean daily fitbit data for nethealth project
#####

# call libraries
library(readstata13)
library(dplyr)
library(here)
library(lubridate)
library(janitor)
library(ggplot2)

# import data
fitbit_data <- read.dta13(file = here::here("dailyfitbit.dta")) %>% 
  clean_names()
  # checkout warnings to isolate specific variables (factor label warning)
  # most likely race and gender; ask about factor levels

# convert to tibble
fitbit_data <- as_tibble(fitbit_data)

# which variables in object
names(fitbit_data)

# class of each variable
glimpse(fitbit_data)

# note: idstudy is person-date participid is person identifier

# make datadate a proper date format
fitbit_data$datadate <- ymd(fitbit_data$datadate)

# number of unique participants
n_distinct(fitbit_data$NetIdEmail)

# number of unique daily participant-observation units
n_distinct(fitbit_data$idstudy) # 429071

# earliest and latest dates
fitbit_data %>% 
  summarise(earliest_date = min(datadate),
            latest_date = max(datadate))

# for a given person, are there duplicate observations for a date?
multiple_daily_obs <- fitbit_data %>% 
  group_by(participid) %>% 
  count(datadate) %>% 
  ungroup() %>% 
  filter(n > 1) %>% 
  left_join(select(fitbit_data, participid, datadate, steps, sleepmins), 
            by = c('participid' = 'participid',
                                'datadate' = 'datadate'))

# get differences between steps and sleep (date duplicates) for each obs
multiple_daily_obs %>% 
  group_by(participid, datadate) %>% 
  summarise(steps_diff = max(steps) - min(steps),
            sleep_diff = max(sleepmins) - min(sleepmins)) %>% 
  ungroup() %>% 
  summarise(max_steps_diff = max(steps_diff, na.rm = TRUE),
            max_sleep_diff = max(sleep_diff, na.rm = TRUE),
            n_nonzero_steps = sum(steps_diff > 0, na.rm = TRUE),
            n_nonserp_sleep = sum(sleep_diff > 0, na.rm = TRUE),
            n_na_steps = sum(is.na(steps_diff)),
            n_na_sleep  = sum(is.na(sleep_diff)))

# how many NA for a  person on a given day
multiple_daily_obs %>% 
  group_by(participid, datadate) %>% 
  summarise(sum_daily_na_steps = sum(is.na(steps)),
            sum_daily_na_sleep = sum(is.na(sleepmins)))

# when missing, is it both observations or sometimes only one?
multiple_daily_obs %>% 
  group_by(participid, datadate) %>% 
  summarise(sum_daily_na_steps = sum(is.na(steps)),
            sum_daily_na_sleep = sum(is.na(sleepmins))) %>% 
  ungroup() %>% 
  count(sum_daily_na_sleep)

# how many unique participants with multiple daily observations?
n_unique_dup <- multiple_daily_obs %>% 
  summarise(unique_ids = n_distinct(participid)) %>% 
  pull(unique_ids)

# do the days with repeated observations exhaust all observations for these ids?

# get number of dates per person in orig data
n_dates_orig <- fitbit_data %>% 
  filter(participid %in% multiple_daily_obs$participid) %>% 
  group_by(participid) %>% 
  summarise(n_dates_orig = n_distinct(datadate))

# repeat for duplicates data
n_dates_dup <- multiple_daily_obs %>% 
  group_by(participid) %>% 
  summarise(n_dates_dup = n_distinct(datadate)) 

# join together
n_dates_both <- n_dates_dup %>% 
  left_join(n_dates_orig, by = c('participid' = 'participid'))
