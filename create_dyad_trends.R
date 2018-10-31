#####
##### create vector of differences for each dyad combination
#####

# data comes from "data_import_clean.R"

# get all combinations
get_unique_dyads <- function(user_id_col){
  # get unique user ids in two columns
  unique_ids_1 <- unique(user_id_col)
  unique_ids_2 <- unique(user_id_col)
  
  # get all possible combinations
  dyad_list <- expand.grid(unique_ids_1, unique_ids_2) 
    # more informative names
    
  dyad_list <- dyad_list %>% 
    dplyr::rename(id1 = Var1,
                 id2 = Var2) %>% 
    # remove cases where id1 and id2 are same person
    dplyr::filter(id1 != id2)
  
  dyad_list$id1 <- as.character(dyad_list$id1)
  dyad_list$id2 <- as.character(dyad_list$id2)
  
  # return dyad list
  return(dyad_list)
}




# create difference vectors for all combinations
diff_trends <- function(dyad_list){
  filtered <- fitbit_data %>% 
    filter(participid ==  dyad_list[,1] | participid == dyad_list[,2]) %>% 
    group_by(datadate) %>% 
    filter(n_distinct(participid) == 2) %>% 
    summarise(diff = max(steps) - min(steps)) %>% 
    mutate(node1 =  dyad_list[,1],
           node2 = dyad_list[,2]) %>% 
    ungroup()
  
  #filtered <- data %>% 
    #filter(data_id == single_dyad_df[1,1] | data_id == single_dyad_df[1,2]) 
  
  #filtered <- filtered %>% 
    #group_by_(date) %>% 
    #filter(n_distinct(data_id) == 2)
  #%>% 
    #group_by(date) %>% 
    #filter(n_distinct(data_id) == 2) %>% 
    #summarise(diff = max(focal_var) - min(focal_var)) %>% 
    #ungroup()
  return(filtered)
}

# make list
dyad_as_list <- split(dyad_list, seq(nrow(dyad_list)))


# apply to all dyads
#all_dyad_diffs <- lapply(dyad_as_list[1:10], diff_trends)

# apply using parallelization 
dyad_steps_diffs <- parallel::mclapply(dyad_as_list, diff_trends, 
                                        mc.cores = getOption("mc.cores", 10L))
  # empty if dates don't overlap; NA if one had no values for day


# save object
saveRDS(dyad_steps_diffs, "dyad_steps_diffs.rds")




### rerun above function for sleep

# create difference vectors for all combinations
diff_trends_sleep <- function(dyad_list){
  filtered <- fitbit_data %>% 
    filter(participid ==  dyad_list[,1] | participid == dyad_list[,2]) %>% 
    group_by(datadate) %>% 
    filter(n_distinct(participid) == 2) %>% 
    summarise(diff = max(sleepmins) - min(sleepmins)) %>% 
    mutate(node1 =  dyad_list[,1],
           node2 = dyad_list[,2]) %>% 
    ungroup()
  return(filtered)
}

# make list
dyad_as_list <- split(dyad_list, seq(nrow(dyad_list)))

# apply using parallelization 
dyad_sleep_diffs <- parallel::mclapply(dyad_as_list, diff_trends_sleep, 
                                        mc.cores = getOption("mc.cores", 10L))
# empty if dates don't overlap; NA if one had no values for day

# save object
saveRDS(dyad_sleep_diffs, "dyad_sleep_diffs.rds")











# get_trend_diffs <- function(ids, focal_var, date_var) {
#   if(length(ids) < 2){
#     stop("You have entered fewer than two user ids!")
#   } else if(length(ids) > 2) {
#     stop("You have entered more than two user ids!")
#   } else if (!is.numeric(focal_var)) {
#     stop(paste(focal_var, "is not numeric!", sep = " "))
#   } else {
#    
#     # get date vectors for each user id
#     node1 <- dplyr::filter(data, participid == ids[1]) %>% 
#       filter(!is.na(focal_var))
#     node2 <- dplyr::filter(data, participid == ids[2])  %>% 
#       filter(!is.na(focal_var))
#     
#     # get intersection of dates
#     common_dates <- intersect(node1$ddate_var, node2$date_var)
#     
#     # keep only those dates in node1 and node2 that appear in both
#     node1 <- node1 %>% 
#       filter(datadate %in% common_dates)
#     node2 <- node2 %>% 
#       filter(datadate %in% common_dates)
#     
#     # give warning if lengths are not equal; would indicate duplicate dates
#     if(nrow(node1) != nrow(node2)){
#       warning(paste(ids[1], "and", ids[2], 
#                     "have an unequal number of date observations, 
#                     which likely indicates duplicate observations
#                     for one or more dates for either or both
#                     individuals. Using unique() to remove duplicates.",
#                     sep = " "))
#       node1 <- unique(node1$date_var, node1$focal_var)
#       node2 <- unique(node2$date_var, node2$focal_var)
#       
#       # check if still unequal 
#       if(nrow(node1) == nrow(node2)){
#         warning(paste(ids[1], "and", ids[2], "still have an unequal number of date
#                       observations!"))
#       }
#     }
#     
#     # arrange in ascending dates
#     node1 <- node1 %>% 
#       arrange(datadate)
#     node2 <- node2 %>% 
#       arrange(datadate)
#     
#     # get difference vector
#     diff_vector <- node1$focal_var - node2$focal_var
#     
#    # add dates
#     diff_dates_vector <- bind_cols(node1$datadate, diff_vector)
#     
#   }
# } 
# 
# 
# 
# 
# 
# 
# # attempt 2
# get_trend_diffs2 <- function(data, dyad, id_var, focal_var, date_var){
#   # dyad must be a tibble/df
#   
#   # get date vectors for each user id
#   node1 <- data %>% 
#     dplyr::filter(id_var == sa.character(dyad[1,1]) %>% 
#     dplyr::filter(!is.na(focal_var))
#   node2 <- data %>% 
#     dplyr::filter(id_var == as.character(dyad[1,2]) %>% 
#     dplyr::filter(!is.na(focal_var))
#   
# }
#     
#   # get intersection of dates
#   common_dates <- intersect(node1$date_var, node2$date_var)
#   
#   # keep only those dates in node1 and node2 that appear in both
#   node1 <- node1 %>% 
#     filter(date_var %in% common_dates)
#   node2 <- node2 %>% 
#     filter(date_var %in% common_dates)
#   
#   # arrange in ascending dates
#   node1 <- node1 %>% 
#     arrange(date_var)
#   node2 <- node2 %>% 
#     arrange(date_var)
#   
#   # get difference vector
#   diff_vector <- node1$focal_var - node2$focal_var
#   
#   # return differences
#   return(diff_vector)
# }
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # problems in getting daily pairwise differences
#   # what to do when start and end dates do not align?
#   # what to do when day for person i has missing data but not for person j?
# 
# 
# 
# test_fitbit_data <- fitbit_data
# 
# test_fitbit_data <- test_fitbit_data %>% 
#   filter(NetIdEmail == "bquigley@nd.edu" | NetIdEmail == "hbahadur@nd.edu")
# 
# test_fitbit_data1 <- test_fitbit_data %>% 
#   filter(!is.na(datadate)) %>% 
#   
#   
#   
#   
#   
# ##### check for examples of duplicates
# # get date vectors for each user id
# node1 <- dplyr::filter(fitbit_data, participid == "3LZJFK") %>% 
#   filter(!is.na(steps))
# node2 <- dplyr::filter(fitbit_data, participid == "3M5ZZL")  %>% 
#   filter(!is.na(steps))
# # get intersection of dates
# common_dates <- intersect(node1$datadate, node2$datadate)
# 
# # keep only those dates in node1 and node2 that appear in both
# node1 <- node1 %>% 
#   filter(datadate %in% common_dates) 
# node2 <- node2 %>% 
#   filter(datadate %in% common_dates) 
# 
# # node2 %>% arrange(datadate) %>% select(steps, datadate) %>% View()
# 
# # this isgnores other DIFFERENT cell contents!!!
# node1_unique <- unique(node1$participid, node1$steps) %>% 
#   arrange(datadate)
# node2_unique <- unique(node2$participid, node2$steps) %>% 
#   arrange(datadate)
# 
# setequal(nrow(node1_unique), nrow(node2_unique))
# 
# test_vector <- node1_unique
# 
# 
