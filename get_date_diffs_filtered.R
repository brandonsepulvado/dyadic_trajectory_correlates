# load packages
library(lubridate)
library(dplyr)
library(tibble)
library(parallel)

### get filtered objects

# create dates to remove
midterm_f2015 <- seq(from = ymd("2015-10-17"), to = ymd("2015-10-25"),
                    by = "days") %>% as_tibble()

thx_2015 <- seq(from = ymd("2015-11-25"), to = ymd("2015-11-29"),
                    by = "days") %>% as_tibble()

finals_f2015 <- seq(from = ymd("2015-12-14"), to = ymd("2015-12-18"),
                    by = "days") %>% as_tibble()

xmas_2015 <- seq(from = ymd("2015-12-19"), to = ymd("2016-01-11"),
                    by = "days") %>% as_tibble()

midterm_s2016 <- seq(from = ymd("2016-03-05"), to = ymd("2016-03-13"),
                    by = "days") %>% as_tibble()

easter_2016 <- seq(from = ymd("2016-03-25"), to = ymd("2016-03-28"),
                     by = "days") %>% as_tibble()

finals_s2016 <- seq(from = ymd("2016-05-02"), to = ymd("2016-05-06"),
                     by = "days") %>% as_tibble()

summer_2016 <- seq(from = ymd("2016-05-07"), to = ymd("2016-08-21"),
                     by = "days") %>% as_tibble()

midterm_f2016 <- seq(from = ymd("2016-10-15"), to = ymd("2016-10-23"),
                     by = "days") %>% as_tibble()

thx_2016 <- seq(from = ymd("2016-11-23"), to = ymd("2016-11-27"),
                     by = "days") %>% as_tibble()

finals_f2016 <- seq(from = ymd("2016-12-12"), to = ymd("2016-12-16"),
                     by = "days") %>% as_tibble()

xmas_2016 <- seq(from = ymd("2016-12-17"), to = ymd("2017-01-16"),
                     by = "days") %>% as_tibble()

midterm_s2017 <- seq(from = ymd("2017-03-11"), to = ymd("2017-03-19"),
                     by = "days") %>% as_tibble()

easter_2017 <- seq(from = ymd("2017-04-14"), to = ymd("2017-04-17"),
                     by = "days") %>% as_tibble()

finals_s2017 <- seq(from = ymd("2017-05-08"), to = ymd("2017-05-12"),
                     by = "days") %>% as_tibble()

summer_2017 <- seq(from = ymd("2017-05-13"), to = ymd("2017-08-21"),
                     by = "days") %>% as_tibble()

midterm_f2017 <- seq(from = ymd("2017-10-14"), to = ymd("2017-10-22"),
                     by = "days") %>% as_tibble()

thx_2017 <- seq(from = ymd("2017-11-22"), to = ymd("2017-11-26"),
                     by = "days") %>% as_tibble()

finals_f2017 <- seq(from = ymd("2017-12-11"), to = ymd("2017-12-15"),
                     by = "days") %>% as_tibble()

xmas_2017 <- seq(from = ymd("2017-12-16"), to = ymd("2018-01-16"),
                     by = "days") %>% as_tibble()

midterm_s2018 <- seq(from = ymd("2018-03-10"), to = ymd("2018-03-18"),
                     by = "days") %>% as_tibble()

easter_2018 <- seq(from = ymd("2018-03-30"), to = ymd("2018-04-02"),
                     by = "days") %>% as_tibble()

finals_s2018 <- seq(from = ymd("2018-05-07"), to = ymd("2018-05-11"),
                     by = "days") %>% as_tibble()

summer_2018 <- seq(from = ymd("2018-05-12"), to = ymd("2018-08-20"),
                     by = "days") %>% as_tibble()

# bind relevant objects together 
dates_to_remove <- bind_rows(midterm_f2015,
                                    thx_2015,
                                    xmas_2015,
                                    midterm_s2016,
                                    easter_2016,
                                    summer_2016,
                                    midterm_f2016,
                                    thx_2016,
                                    xmas_2016,
                                    midterm_s2017,
                                    easter_2017,
                                    summer_2017,
                                    midterm_f2017,
                                    thx_2017,
                                    xmas_2017,
                                    midterm_s2018,
                                    easter_2018,
                                    summer_2018) %>% 
  rename(date = value)

# remove constituent objects no longer necessary
rm(list=setdiff(ls(), "dates_to_remove"))
###

# load data objects
dyad_sleep_diffs <- readRDS("/afs/crc.nd.edu/user/b/bsepulva/Private/dyad_sleep_diffs.rds")
dyad_steps_diffs <- readRDS("/afs/crc.nd.edu/user/b/bsepulva/Private/dyad_steps_diffs.rds")

# remove for sleep data
dyad_sleep_diffs_filtered <- lapply(dyad_sleep_diffs, function(x){
  x <- x %>% 
    filter(!datadate %in% dates_to_remove$date)
  
  return(x)
})

# remove for steps
dyad_steps_diffs_filtered <- lapply(dyad_steps_diffs, function(x){
  x <- x %>% 
    filter(!datadate %in% dates_to_remove$date)
  
  return(x)
})

# function to get interval vector of date differences
get_date_differences <- function(data, format = "day"){
  
  temp <- data %>% 
    filter(!is.na(diff)) %>% 
    select(datadate, dyad_id)
  
  temp <- ymd(temp$datadate)
  
  diffs <- temp %>% 
    int_diff() %>% 
    as.duration() %>%
    as.numeric() %>% 
    as_tibble() %>% 
    mutate(dyad_id = unique(data$dyad_id)) %>% 
    rename(diff = value)
  
  if (format == "day") {
    return(mutate(diffs, diff = diff / 86400))
    #return(diffs / 86400)
  } else if (format == "seconds") {
    return(diffs)
  } else {
    stop("You have chosen an unsupported date format!")
  }
}

# apply to sleep
sleep_date_diffs <- mclapply(dyad_sleep_diffs_filtered, function(x){
  # get difference vector
  diff_tibble <- get_date_differences(x)

  # return outptu
  return(diff_tibble)
  }, mc.cores = getOption("mc.cores", 4L)
)

# save
saveRDS(sleep_date_diffs, file = "/afs/crc.nd.edu/user/b/bsepulva/Private/sleep_date_diffs.rds")

# apply to steps
steps_date_diffs <- mclapply(dyad_steps_diffs_filtered, function(x){
  # get difference vector
  diff_tibble <- get_date_differences(x)

  # return outptu
  return(diff_tibble)
  }, mc.cores = getOption("mc.cores", 4L)
)

# save
saveRDS(steps_date_diffs, file = "/afs/crc.nd.edu/user/b/bsepulva/Private/steps_date_diffs.rds")

# put in one tibble
sleep_steps_date_diffs_t <- bind_rows(sleep_date_diffs, steps_date_diffs)

# save tibble
saveRDS(sleep_steps_date_diffs_t, file = "/afs/crc.nd.edu/user/b/bsepulva/Private/sleep_steps_date_diffs_t.rds")







