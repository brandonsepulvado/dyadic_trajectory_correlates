#####
##### create vector of differences for each dyad combination
#####

# data comes from "data_import_clean.R"

# load packages that were not in data creation file
library(tidyr)
library(gtools)

# get all combinations
get_dyads <- function(user_id_col, directed = FALSE){
  if (directed) {
    # get unique user ids in two columns
    dyads <- permutations(n=length(unique(user_id_col)),
                          r=2,
                          v=unique(user_id_col),
                          repeats.allowed=FALSE) %>% 
      as_tibble(.name_repair = ~c('vertex_1', 'vertex_2'))
  } else {
    # get unique user ids in two columns
    dyads <- combinations(n=length(unique(user_id_col)),
                          r=2,
                          v=unique(user_id_col),
                          repeats.allowed=FALSE)  %>% 
      as_tibble(.name_repair = ~c('vertex_1', 'vertex_2'))
  }

  # return dyads
  return(dyads)
}

# create dyad data; undirected 
data_dyads <- get_dyads(fitbit_data$participid)


# produce function to get dyadic differences for a given activity 
# variable names require quotation marks
get_dyad_diffs <- function(dyad_df, activity_name, activity_data = fitbit_data){
  # get activity data for each vertex as new variable
  to_return <- dyad_df %>% 
    left_join(select(activity_data, participid, activity_name, datadate), 
              by = c('vertex_1' = 'participid')) %>% 
    left_join(select(activity_data, participid, activity_name, datadate),
              by = c('vertex_2' = 'participid', 'datadate' = 'datadate'),
              suffix = c('_v1', '_v2')) %>%  
    select(vertex_1, vertex_2, datadate, glue::glue('{activity_name}_v1'), 
           glue::glue('{activity_name}_v2')) %>% 
    mutate(abs_diff = case_when(is.na(.[[4]]) | is.na(.[[5]]) ~ NA_integer_,
                                TRUE ~ abs(.[[4]] - .[[5]])))
  # return data
  return(to_return)
} 

# create data objects
data_dyad_steps <- get_dyad_diffs(data_dyads, 'steps')
data_dyad_sleep <- get_dyad_diffs(data_dyads, 'sleepmins')


