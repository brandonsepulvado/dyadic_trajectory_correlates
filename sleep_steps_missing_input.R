#####
##### run time series models
#####

library(dplyr)
library(lubridate)
library(parallel)

###
# load data if needed (uncomment)
dyad_sleep_diffs <- readRDS("/afs/crc.nd.edu/user/b/bsepulva/Private/dyad_sleep_diffs.rds")
dyad_steps_diffs <- readRDS("/afs/crc.nd.edu/user/b/bsepulva/Private/dyad_steps_diffs.rds")
###



### prepare data

# remove list element if it has discontinuous dates

# function to get interval vector of date differences
get_date_differences <- function(data, format = "day"){
  #date_var <- enquo(date_var)
  
  temp <- data %>% 
    filter(!is.na(diff)) %>% 
    select(datadate)
  
  temp <- ymd(temp$datadate)
  
  diffs <- temp %>% 
    int_diff() %>% 
    as.duration() %>%
    as.numeric()
  
  if (format == "day") {
    return(diffs / 86400)
  } else if (format == "seconds") {
    return(diffs)
  } else {
    stop("You have chosen an unsupported date format!")
  }
}


# apply to sleep
sleep_filtered <- mclapply(dyad_sleep_diffs, function(x){
  
    # get difference vector
    diff_vector <- get_date_differences(x)
    
    # return NULL if there is at least one dicontinuity or only one date
    if (length(diff_vector) == 0) {
      return(NULL)
    } else if (max(diff_vector, na.rm = TRUE) > 1) {
      return(NULL)
    } else {
      # return original data object if no discontinuity
      return(x)
    }
  }, mc.cores = getOption("mc.cores", 4L)
)

# save 
saveRDS(sleep_filtered, file = "/afs/crc.nd.edu/user/b/bsepulva/Private/sleep_filtered.rds")

# apply to steps
steps_filtered <- mclapply(dyad_steps_diffs, function(x){
  
    # get difference vector
    diff_vector <- get_date_differences(x)
    
    # return NULL if there is at least one dicontinuity or only one date
    if (length(diff_vector) == 0) {
      return(NULL)
    } else if (max(diff_vector, na.rm = TRUE) > 1) {
      return(NULL)
    } else {
      # return original data object if no discontinuity
      return(x)
    }
  }, mc.cores = getOption("mc.cores", 4L)
)

# save 
saveRDS(steps_filtered, file = "/afs/crc.nd.edu/user/b/bsepulva/Private/steps_filtered.rds")


