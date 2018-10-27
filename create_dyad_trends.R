#####
##### create vector of differences for each dyad combination
#####

# data comes from "data_import_clean.R"

# get all combinations
get_unique_dyads <- function(user_id_col, col_for_diffs){
  # get unique user ids in two columns
  unique_ids_1 <- unqiue(user_id_col)
  unique_ids_2 <- unique(user_id_col)
  
  # get all possible combinations
  user_combinations <- expand.grid(unique_ids_1, unique_ids_2) %>% 
    # more informative names
    rename(id1 = Var1,
           id2 = Var2) %>% 
    # remove cases where id1 and id2 are same person
    filter(id1 != id2)
  
  # 
  
}

# create difference vectors for all combinations
get_trend_diffs <- function(data, ids, focal_var, date_var) {
  if(length(ids) < 2){
    stop("You have entered fewer than two user ids!")
  } else if(length(ids) > 2) {
    stop("You have entered more than two user ids!")
  } else if (!is.numeric(focal_var)) {
    stop(paste(focal_var, "is not numeric!", sep = " "))
  } else {
   
    # get date vectors for each user id
    node1 <- dplyr::filter(data, participid == ids[1]) #%>% 
      #dplyr::select(participid, datadate)
    node2 <- dplyr::filter(data, participid == ids[2]) #%>% 
      #dplyr::select(participid, datadate)
    
    # get intersection of dates
    common_dates <- intersect(node1$ddate_var, node2$date_var)
    
    # keep only those dates in node1 and node2 that appear in both
    node1 <- node1 %>% 
      filter(datadate %in% common_dates)
    node2 <- node2 %>% 
      filter(datadate %in% common_dates)
    
    # give warning if lengths are not equal; would indicate duplicate dates
    if(nrow(node1) != nrow(node2)){
      warning(paste(ids[1], "and", ids[2], 
                    "have an unequal number of date observations, 
                    which likely indicates duplicate observations
                    for one or more dates for either or both
                    individuals. Using unique() to remove duplicates.",
                    sep = " "))
      node1 <- unique(node1)
      node2 <- unique(node2)
    }
    
    # arrange in ascending dates
    node1 <- node1 %>% 
      arrange(datadate)
    node2 <- node2 %>% 
      arrange(datadate)
    
    # get difference vector
    diff_vector <- node1$focal_var - node2$focal_var
    
  }
} 


# problems in getting daily pairwise differences
  # what to do when start and end dates do not align?
  # what to do when day for person i has missing data but not for person j?



test_fitbit_data <- fitbit_data

test_fitbit_data <- test_fitbit_data %>% 
  filter(NetIdEmail == "bquigley@nd.edu" | NetIdEmail == "hbahadur@nd.edu")

test_fitbit_data1 <- test_fitbit_data %>% 
  filter(!is.na(datadate)) %>% 
  
  
  
  
  
##### check for examples of duplicates
# get date vectors for each user id
node1 <- dplyr::filter(fitbit_data, participid == "3LZJFK") #%>% 
#dplyr::select(participid, datadate)
node2 <- dplyr::filter(fitbit_data, participid == "3M5ZZL") #%>% 
#dplyr::select(participid, datadate)

# get intersection of dates
common_dates <- intersect(node1$datadate, node2$datadate)

# keep only those dates in node1 and node2 that appear in both
node1 <- node1 %>% 
  filter(datadate %in% common_dates)
node2 <- node2 %>% 
  filter(datadate %in% common_dates)

# node2 %>% arrange(datadate) %>% select(steps, datadate) %>% View()

# this isgnores other DIFFERENT cell contents!!!
node1_unique <- unique(node1$participid, node1$steps) 
node2_unique <- unique(node2$participid, node2$steps)

setequal(nrow(node1_unique), nrow(node2_unique))
