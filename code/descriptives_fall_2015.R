# ==============================================================================
# descriptives for first semester
# ==============================================================================

# load packages
library(tidyr)

# must first run : data_import_clean, create_dyad_trends
# information on academic calendar in academic_cal_breaks

# steps

# remove mis
test <- data_dyad_steps %>% 
  filter(!is.na(abs_diff),
         datadate < '2015-12-19') %>% # start of xmas break
  distinct() # keep only distinct observations

# plot
test %>% 
  count(datadate) %>% 
  ggplot(aes(x = datadate, y = n)) +
  geom_col(fill = 'orange2', alpha = .6) +
  theme_minimal() +
  labs(x = 'Date',
       y = 'Number of dyad observations',
       title = 'Distribution of non-missing observations',
       subtitle = 'Semester: Fall 2015')

# add dyad identifier (single, rather than two cols)
test <- test %>% 
  unite(id_dyad, vertex_1, vertex_2, sep = '-', remove = FALSE)

# get number of days in period
date_start <- min(test$datadate)
date_end <- max(test$datadate)
period_in_days <- as.numeric(date_end - date_start)

# number of observations per dyad
dyad_days <- test %>% 
  group_by(id_dyad) %>% 
  summarise(n_days = n_distinct(datadate),
            prop_total = n_days / period_in_days)

# get distribution of non-missing data for semester 1
dyad_days %>% 
  ggplot(aes(prop_total)) +
  geom_density(fill = 'orange2', color = 'orange2', alpha = .6) +
  theme_minimal() +
  labs(title = 'Distribution of dyadic non-missing observations',
       x = 'Proportion of non-missing observations',
       y = 'Density')

# how many dyads are present at different thresholds
dyad_days %>% 
  summarise(p50 = nrow(filter(., prop_total >= .5)),
            p75 = nrow(filter(., prop_total >= .75)))

# keep only days starting in september
test <- test %>% 
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
test <- test %>% 
  filter(id_dyad %in% dyads_to_keep)

