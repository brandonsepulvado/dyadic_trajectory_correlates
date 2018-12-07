#####
##### dealing with missing data
#####

##### missing data due to breaks

### create column of break dates

# load lubridate
library(lubridate)

# create sequences
midterm_f2015 <- seq(from = ymd("2015-10-17"), to = ymd("2015-10-25"),
                    by = "days")

thx_2015 <- seq(from = ymd("2015-11-25"), to = ymd("2015-11-29"),
                    by = "days")

finals_f2015 <- seq(from = ymd("2015-12-14"), to = ymd("2015-12-18"),
                    by = "days")

xmas_2015 <- seq(from = ymd("2015-12-19"), to = ymd("2016-01-11"),
                    by = "days")

midterm_s2016 <- seq(from = ymd("2016-03-05"), to = ymd("2016-03-13"),
                    by = "days")

easter_2016 <- seq(from = ymd("2016-03-25"), to = ymd("2016-03-28"),
                     by = "days")

finals_s2016 <- seq(from = ymd("2016-05-02"), to = ymd("2016-05-06"),
                     by = "days")

summer_2016 <- seq(from = ymd("2016-05-07"), to = ymd("2016-08-21"),
                     by = "days")

midterm_f2016 <- seq(from = ymd("2016-10-15"), to = ymd("2016-10-23"),
                     by = "days")

thx_2016 <- seq(from = ymd("2016-11-23"), to = ymd("2016-11-27"),
                     by = "days")

finals_f2016 <- seq(from = ymd("2016-12-12"), to = ymd("2016-12-16"),
                     by = "days")

xmas_2016 <- seq(from = ymd("2016-12-17"), to = ymd("2017-01-16"),
                     by = "days")

midterm_s2017 <- seq(from = ymd("2017-03-11"), to = ymd("2017-03-19"),
                     by = "days")

easter_2017 <- seq(from = ymd("2017-04-14"), to = ymd("2017-04-17"),
                     by = "days")

finals_s2017 <- seq(from = ymd("2017-05-08"), to = ymd("2017-05-12"),
                     by = "days")

summer_2017 <- seq(from = ymd("2017-05-13"), to = ymd("2017-08-21"),
                     by = "days")

midterm_f2017 <- seq(from = ymd("2017-10-14"), to = ymd("2017-10-22"),
                     by = "days")

thx_2017 <- seq(from = ymd("2017-11-22"), to = ymd("2017-11-26"),
                     by = "days")

finals_f2017 <- seq(from = ymd("2017-12-11"), to = ymd("2017-12-15"),
                     by = "days")

xmas_2017 <- seq(from = ymd("2017-12-16"), to = ymd("2018-01-16"),
                     by = "days")

midterm_s2018 <- seq(from = ymd("2018-03-10"), to = ymd("2018-03-18"),
                     by = "days")

easter_2018 <- seq(from = ymd("2018-03-30"), to = ymd("2018-04-02"),
                     by = "days")

finals_s2018 <- seq(from = ymd("2018-05-07"), to = ymd("2018-05-11"),
                     by = "days")

summer_2018 <- seq(from = ymd("2018-05-12"), to = ymd("2018-08-20"),
                     by = "days")


