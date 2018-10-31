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

# count how many were in study at same time
same_time <- sum(sapply(dyad_steps_diffs, function(x) nrow(x) > 0))
  # 354116

# how many dyads have missing data, meaning both in study at same time
# but one person (at least) has NA for day on given measure
missing_data_days <- sapply(all_dyad_diffs_mc, function(x) sum(is.na(x['diff'])))
