#####
##### exploring data (not inferential analyses)
#####

### using the dyad differences created in the create_dyad_trends.R file

### steps

# rename
dyad_steps_diffs <- all_dyad_diffs_mc

# remove old object now unnecessary
# rm(all_dyad_diffs_mc)

### create dyad id
# create function
create_dyad_id <- function(data){
  data <- data %>% 
    unite(dyad_id, node1, node2, sep = "", remove = FALSE)
  return(data)
}

# apply
dyad_steps_diffs <- lapply(dyad_steps_diffs, create_dyad_id)
dyad_sleep_diffs <- lapply(dyad_sleep_diffs, create_dyad_id)

# save these 
saveRDS(dyad_steps_diffs, "dyad_steps_diffs.rds")
saveRDS(dyad_sleep_diffs, "dyad_sleep_diffs.rds")


# count how many were in study at same time
same_time_ <- sum(sapply(dyad_steps_diffs, function(x) nrow(x) > 0))
  # 354116

# how many dyads have missing data, meaning both in study at same time
# but one person (at least) has NA for day on given measure
missing_steps_days <- sapply(dyad_steps_diffs, function(x) sum(is.na(x['diff'])))
missing_sleep_days <- sapply(dyad_sleep_diffs, function(x) sum(is.na(x['diff'])))

# get proportion of dyad's common dates with missing data
missing_steps_prop <- sapply(dyad_steps_diffs, function(x) sum(is.na(x['diff'])) / nrow(x))
missing_sleep_prop <- sapply(dyad_sleep_diffs, function(x) sum(is.na(x['diff'])) / nrow(x))

# visualizing missing proportions
hist(missing_steps_prop)
hist(missing_sleep_prop)

# make tibble to be able to facet histogram
steps_miss_prop_t <- as_tibble(missing_steps_prop) %>% 
  rename(proportion = value) %>% 
  mutate(activity = "steps")
sleep_miss_prop_t <- as_tibble(missing_steps_prop) %>% 
  rename(proportion = value) %>% 
  mutate(activity = "sleep")
missing_prop_combined <- bind_rows(steps_miss_prop_t,
                                   sleep_miss_prop_t)

# get mean for missing proportion types for histograms below
mean_steps <- mean(steps_miss_prop_t$proportion, na.rm = TRUE)
mean_sleep <- mean(steps_miss_prop_t$proportion, na.rm = TRUE)

# plot histogram faceted
missing_prop_combined %>% 
  ggplot(aes(x = proportion, fill = as.factor(activity))) +
  geom_histogram(alpha = 0.5) +
  facet_wrap(~as.factor(activity)) +
  labs(title = "Proportion of day data missing for each dyad",
       x = "Proportion",
       y = "Count") +
  scale_fill_discrete(name = "Type of activity") +
  geom_vline(data=filter(missing_prop_combined, as.factor(activity)=="steps"),
             aes(xintercept=mean_steps), linetype = "dashed", colour="black") +
  geom_vline(data=filter(missing_prop_combined, as.factor(activity)=="sleep"),
             aes(xintercept=mean_sleep), linetype = "dashed", colour="black")
  
  