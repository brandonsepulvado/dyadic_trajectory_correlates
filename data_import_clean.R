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
  filter(n > 1)

# how many unique participants with multiple daily observations?
n_unique_dup <- multiple_daily_obs %>% 
  summarise(unique_ids = n_distinct(participid)) %>% 
  pull(unique_ids)

# filter daily steps and sleep for first case 
fitbit_data %>% 
  filter(participid == multiple_daily_obs$participid[5] &
           datadate == multiple_daily_obs$datadate[5]) %>% 
  distinct(steps, sleepmins)

# do the days with repeated observations exhaust all observations for these ids?
