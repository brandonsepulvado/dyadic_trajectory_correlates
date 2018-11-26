#####
##### run time series models
#####

###
# load data if needed (uncomment)
#yad_sleep_diffs <- readRDS("dyad_sleep_diffs.rds")
#dyad_steps_diffs <- readRDS("dyad_steps_diffs.rds")
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
  
  if(format == "day") {
    return(diffs / 86400)
  } else if (format == "seconds") {
    return(diffs)
  } else {
    stop("You have chosen an unsupported date format!")
  }
}


# apply to sleep
sleep_filtered <- lapply(dyad_sleep_diffs, function(x){
  
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
})





### run time series models using auto-arima

auto_arima_results <- lapply(dyad_sleep_diffs[1:10], function(x){
  x$diff %>% 
    forecast::auto.arima()
})

ggplot() +
  geom_line(data = dyad_sleep_diffs[[1]], aes(x = datadate, y = diff)) +
  geom_line(data = dyad_sleep_diffs[[2]], aes(x = datadate, y = diff)) 