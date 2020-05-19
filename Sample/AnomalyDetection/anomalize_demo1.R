# File: anomalize_demo1.R
# Author: Nick.
##
# Just an exercise to test one package for anomaly detection I haven't used before (today)

## I didn't have these in my packages. Added to my libraries.R:
# utils::install.packages("anomalize")
# utils::install.packages("tidyverse")

library(tidyverse)
library(tibbletime)
## The objective here:
library(anomalize)

## 
# tibbletime was needed for anomalize...
# So let's check how to work with tibbletime objects...
##

## From tibbletime package doc:
# data(FANG)
# FANG <- as_tbl_time(FANG, date)
# FANG <- dplyr::group_by(FANG, symbol)
# # Respects groups
# as_period(FANG, "yearly")
# # Every 6 months, respecting groups
# as_period(FANG, "6 months")
# 
## Clearer for me as I usually work with dataframes... 
#
# ex1 <- data.frame(date = Sys.Date(), value = 1)
# ex1_tbl_time <- as_tbl_time(ex1, date)
# class(ex1_tbl_time)
# attributes(ex1_tbl_time)
# 
# # Converting a tibble to a `tbl_time`
# # Using POSIXct index
# ex2 <- tibble::tibble(
# time = as.POSIXct(c("2017-01-01 10:12:01", "2017-01-02 12:12:01")),
# value = c(1, 2)
# )
# as_tbl_time(ex2, time)


## Seems fair enough. OK so next, let's check out the anomalize thingy...

## I created bogus data for sparkline Data Tables... Let's use that.
dat <- read.csv("temperatures_invented_sample.csv")

# Trick to check simply the functionality: We need time data, let's make it:
# We had years, no matter:
dat$year <- as.POSIXct(strptime(dat$year, "%Y"),tz = "UTC")
#head(dat)

# So let's make it a tibble time object then:
dat_tibble <- as_tbl_time(dat, year)

# We want to work with a coherent subset, so let's say, January:
anomalize_test1 <- filter(dat_tibble, month == "january") %>%
    time_decompose(temp, merge = TRUE) %>%
    anomalize(remainder) %>%
    time_recompose()

anomalize_test1 %>% glimpse()

anomalize_test1 %>% plot_anomalies()
anomalize_test1 %>% plot_anomaly_decomposition()

# Not bad for a first test :)
