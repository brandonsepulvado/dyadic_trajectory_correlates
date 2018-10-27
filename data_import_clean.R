#####
##### load and clean daily fitbit data for nethealth project
#####

# call libraries
library(readstata13)
library(dplyr)
library(here)
library(lubridate)

# import data
fitbit_data <- read.dta13(file = here::here("dailyfitbit.dta"))
  # checkout warnings to isolate specific variables

# convert to tibble
fitbit_data <- as_tibble(fitbit_data)

# which variables in object
names(fitbit_data)

# class of each variable
glimpse(fitbit_data)

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

# gender is an integer
class(fitbit_data$gender)

# race in a double
