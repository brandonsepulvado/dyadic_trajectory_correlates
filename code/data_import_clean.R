# ==============================================================================
# load and clean daily fitbit data for nethealth project
# ==============================================================================

# call libraries
library(dplyr)
library(here)
library(lubridate)
library(janitor)
library(ggplot2)

# import data
fitbit_data <- read.csv(file = here::here('input', 'data_public.csv')) %>% 
  clean_names()
  # checkout warnings to isolate specific variables (factor label warning)
  # most likely race and gender; ask about factor levels

# convert to tibble
fitbit_data <- as_tibble(fitbit_data)

# make datadate a proper date format
fitbit_data$datadate <- ymd(fitbit_data$datadate)

# convert participant id to string
fitbit_data <- fitbit_data %>% 
  mutate(participid = as.character(participid))

# remove duplicate observations
data_fitbit <- fitbit_data %>% 
  distinct()

# create dyad trend objects
source(here::here('code', 'create_dyad_trends.R'))
