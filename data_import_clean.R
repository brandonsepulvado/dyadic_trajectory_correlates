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

# make datadate a proper date format
fitbit_data$datadate <- ymd(fitbit_data$datadate)

# remove duplicate observations
data_fitbit <- fitbit_data %>% 
  select(-idstudy) %>% 
  distinct()

# create dyad trend objects
source(here('create_dyad_trends.R'))