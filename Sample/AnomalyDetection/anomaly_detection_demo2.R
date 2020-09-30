# File: anomaly_detection_demo2.R
# An exercise to play with anomaly detection concepts
# Looking for massive files changes in a folder, "ransomware-style"

library(tidyverse)
library(tibbletime) # Needed for Anomalize
library(ggplot2)
library(dplyr)
library(lubridate)

## The objective here:
library(anomalize)
#library(tsoutliers)

# Important if not set, you need your timezone in lubridate-compatible format:
# Fixes error:
# Error: Problem with `mutate()` input `my_minutes`.
# x CCTZ: Unrecognized tzone: "/UTC"
# i Input `my_minutes` is `collapse_index(...)`.
Sys.setenv(TZ='UTC')

##
# Part 1: How to monitor a folder to gather the needed data:
##

# For reference: A function to gather in the format we want the number of files changed in the last minute:
update_last_changed_values <- function() {
  changed_last_minute <- difftime(Sys.time(), 
                                  file.info(list.files(path = ".", recursive = TRUE))$mtime,
                                  units = "weeks") < 60
  data.frame(date_time = Sys.time(), 
             num_changes = length(changed_last_minute[changed_last_minute == "TRUE"]))
}

# # The above could be used like so:
# # Initialization:
# changes_df <- update_last_changed_values()
# 
# # Simplest version: Get the data for the next 5 minutes, totalling 6 rows
# for(i in 1:5) {
#   Sys.sleep(60)
#   changes_df <- rbind(changes_df, update_last_changed_values())
# }

# Now on to the anomalies detection per se.

##
# Part 2: Theoretical dataset
##

# First, let's create a "dummy" dataset to play with:
# 1 week in minutes: 7 * 24 * 60
number_of_rows <- 10080

# Simulating file accesses by minutes during work day 8-18h:
workday_volumes <- function() {
  day_set <- c()
  for(i in 1:5) {
    day_set <- c(day_set, c(round(rnorm(7*60, mean = 0, sd = 2)),
                            round(rnorm(6*60, mean = 10, sd = 5)),
                            round(rnorm(2*60, mean = 2, sd = 5)),
                            round(rnorm(3*60, mean = 10, sd = 5)),
                            round(rnorm(6*60, mean = 0, sd = 2))))
  }
  sapply(day_set, function(x) { ifelse(x<0, 0, round(x)) })
}

# Assuming very low volumes on weekends:
weekend_volumes <- function() {
  sapply(rnorm(2*24*60, mean = 0, sd = 2), function(x) { ifelse(x<0, 0, round(x)) })
  
}
# Let's create our dummy dataset:

# we consider today at 00h "as a Monday":
base_time <- Sys.time()
start_date <- base_time - 3600 * hour(base_time) - 60 * minute(base_time)
start_date <- as.POSIXct(start_date, tz = "UTC")
changes_df <- data.frame(my_minutes = start_date + 60 * seq(1:number_of_rows),
                              num_files_changed = 
                                c(workday_volumes(), weekend_volumes()))
# quick check
str(changes_df)
# Just for us to see it better:
weekend_rows <- (24*60*5):(24*60*7-1)
# In a real-world setup, weekends would happen on weekends... We just assumed we started
# on a Monday at 00h00.

# Let's "add features" (quotes needed, yes):
changes_df$wday <- as.POSIXlt(changes_df[,1])$wday
changes_df$hour <- as.POSIXlt(changes_df[,1])$hour
# Now what we really want is to use normal data for workdays&hours, and weekends&hours
changes_df$weekend <- "No"
changes_df[weekend_rows, "weekend"] <- "Yes"

# For later use, let's add a second week worth of simulated data:
start_date2 <- base_time + 7 * 24* 60 * 60 - 3600 * hour(base_time) - 60 * minute(base_time)
start_date2 <- as.POSIXct(start_date2, tz = "UTC")
changes_df2 <- data.frame(my_minutes = start_date2 + 60 * seq(1:number_of_rows),
                               num_files_changed = 
                                 c(workday_volumes(), weekend_volumes()))
changes_df2$wday <- as.POSIXlt(changes_df2[,1])$wday
changes_df2$hour <- as.POSIXlt(changes_df2[,1])$hour
# Now what we really want is to use normal data for workdays&hours, and weekends&hours
changes_df2$weekend <- "No"
changes_df2[weekend_rows, "weekend"] <- "Yes"

two_weeks_data <- rbind(changes_df, changes_df2)

# Let's have a look:
ggplot(two_weeks_data, aes(my_minutes, num_files_changed, colour = factor(weekend))) + geom_point()
ggplot(two_weeks_data, aes(num_files_changed, colour = factor(weekend))) + geom_histogram(binwidth = 1)

##
# Part 3: Anomaly Detection
##

# Anomaly detection V0: Simplest, dumbest version.

# For this part, we consider that the first week is our "training" dataset.
# We thereby focus on detecting, with this method, attacks beginning on the SECOND week of data.

# Suppose you have a dataset, for which you know there was no attack of ransomware involved (1st week).
# So you could assume that anything above the max number of files changes per minute observed could be weird.
# You could just create an "anomaly" for anything above that value.
# This is simplistic, but if you have enough data, it might be a first approach (better than nothing).
anomaly_v0 <- max(changes_df[1:nrow(changes_df),2]) + 1
# Of course, we don't have anomalies yet. We'll come back to this later on.


# Anomaly detection V1: Nothing too hard here, mathematically-speaking.

# Once again, we consider that the first week is our "training" dataset.

# We want to create an alert when something is "above normal values".
# Normal values however depend on the hour and the day, as could be observed above.
# So we need to set the normal value per day-hour pairs, and then compare to that.

# For each hour, let's compute mean and standard deviation. Then we can "play the game" with the assumption:
# "Let's suppose our data is normally distributed along any given 1 full hour".
# Incidentally, we know it is.
# Then we could use the traditional approach, for very few alerts, of mean + (3 * SD)
# as the threshold for alerting. That should give us alerts only in (1-0,9972)/2 cases.
# For a week with 10080 minutes, that should add up to ABOUT 14 alerts.
# Visually speaking, these should be easy to discard as false-positive, so impact on a SOC could 
# be acceptable...
# Only this is only for one folder... If you monitor hundreds, well, that's another story.
# But we'll use a "trick" later on.

changes_df_v1 <- changes_df # 1st week of data.

changes_df_v1_summaries <- changes_df_v1 %>% group_by(weekend, hour) %>%
  summarize(hourly_mean = mean(num_files_changed),
            hourly_sd = sd(num_files_changed))
changes_df_v1_summaries$threshold <- ceiling(changes_df_v1_summaries$hourly_mean + 
                                                  3 * changes_df_v1_summaries$hourly_sd)


changes_df_v1 <- merge(changes_df_v1, changes_df_v1_summaries[,c("weekend", "hour", "threshold")], 
                            by.x = c("weekend", "hour"),
                            by.y = c("weekend", "hour"),
                            all.x = TRUE)
changes_df_v1$alert <- "No" # default
changes_df_v1[changes_df_v1$num_files_changed > changes_df_v1$threshold, "alert"] <- "Yes"
nrow(changes_df_v1[changes_df_v1$alert == "Yes",])
# In my tests... 34.

ggplot(changes_df_v1, aes(x = my_minutes, y = num_files_changed, colour = alert)) + 
  geom_point() +
  ggtitle("Anomaly based on thresholds as mean+3*SD")


# Approach 2: Use "anomaly detection" packages.
# This is more interesting in theory, although a bit overkill for this exercise.
# But as this is, after all, an exercise... Why not?

# The package "anomalize" requires tibble datasets to work.
# So let's make it a tibble time object then:
dat_tibble <- as_tbl_time(changes_df, my_minutes) # 1st week of data.

# Anomalize uses time decomposition, frequency and trend. Let's use the defaults on the 1st week:
anomalize_test1 <- dat_tibble %>%
    time_decompose(num_files_changed, frequency = "auto", trend = "auto") %>%
    anomalize(remainder) %>%
    time_recompose()

anomalize_test1 %>% plot_anomalies()
#anomalize_test1 %>% plot_anomaly_decomposition()
nrow(anomalize_test1[anomalize_test1$anomaly == "Yes",])
# Too many false positives, as we know there was no "attack" in the dataset.

# The following gives us the "template" for default values of Frequency & Trend.
# We have data down to the minute:
get_time_scale_template()
# > get_time_scale_template()
# # A tibble: 8 x 3
# time_scale frequency trend   
# <chr>      <chr>     <chr>   
#   1 second     1 hour    12 hours
# 2 minute     1 day     14 days 
# 3 hour       1 day     1 month 
# 4 day        1 week    3 months
# 5 week       1 quarter 1 year  
# 6 month      1 year    5 years 
# 7 quarter    1 year    10 years
# 8 year       5 years   30 years
# And so it uses two weeks for trend, which we haven't fed the algorithms.

# Now we know our frequency is about < 1 hour, and trend repeats every week.
# This is relevant data to apply LOESS
# Let's try different values, more adjusted to what we'd expect:
anomalize_test1 <- dat_tibble %>%
  time_decompose(num_files_changed, frequency = "1 hour", trend = "1 week") %>%
  anomalize(remainder, alpha = 0.025) %>%
  time_recompose()

anomalize_test1 %>% plot_anomalies()
nrow(anomalize_test1[anomalize_test1$anomaly == "Yes",])
# That's much better! 26 "anomalies" detected over 1 week, or 10080 data points!
# This is becoming more manageable.

# Let's see if what kind of regressions we get for two weeks worth of data:
dat_tibble2 <- as_tbl_time(two_weeks_data, my_minutes)

anomalize_test2 <- dat_tibble2 %>%
  time_decompose(num_files_changed, frequency = "1 hour", trend = "1 week") %>%
  anomalize(remainder) %>%
  time_recompose()
anomalize_test2 %>% plot_anomalies()
nrow(anomalize_test2[anomalize_test2$anomaly == "Yes",])
# We have our base-dataset, and 25 "false positives".

# Let's review what happened:
anomalize_test2 %>% glimpse()
anomalize_test2 %>% plot_anomaly_decomposition()


# NOW:
# What would happen if we added the activity of a ransomware, the way we defined it?
# So we suppose a machine is capable of ciphering an average of 100 files per minute
# Stupid assumption though, it could be a thousand: Depends on the machine capacity,
# files sizes, etc.
# Anyhow, so 100 it is.
# Let's assume we keep a rolling dataset of up to two weeks.
# Now, let's add the noise, in a set with 10.000 files (who knows...), 2/3 of the way.
# Average 100 changes/minute, up to 10.000 files, will use up to 100 rows:
attack_start <- floor(nrow(dat_tibble2) * 2/3)
attack_rows <- attack_start:(attack_start + 99) 
dat_tibble2[attack_rows,2] <- round(rnorm(100, mean=100, sd=5))

# Do we detect the attack?
anomalize_test_w_attack <- dat_tibble2 %>%
  time_decompose(num_files_changed, frequency = "1 hour", trend = "1 week") %>%
  anomalize(remainder, method = "iqr") %>%
  time_recompose()

anomalize_test_w_attack %>% plot_anomalies()

nrow(anomalize_test_w_attack[anomalize_test_w_attack$anomaly == "Yes" & anomalize_test_w_attack$observed > 60,])
nrow(anomalize_test_w_attack[anomalize_test_w_attack$anomaly == "Yes",])
# Well, we have quite a few false positives.

# Tuning further our algorithm:
anomalize_test_w_attack <- dat_tibble2 %>%
  time_decompose(num_files_changed, frequency = "1 hour", trend = "1 week") %>%
  anomalize(remainder, method = "iqr", alpha = 0.025) %>%
  time_recompose()

anomalize_test_w_attack %>% plot_anomalies()
anomalize_test_w_attack %>% plot_anomaly_decomposition()

nrow(anomalize_test_w_attack[anomalize_test_w_attack$anomaly == "Yes" & anomalize_test_w_attack$observed > 60,])
nrow(anomalize_test_w_attack[anomalize_test_w_attack$anomaly == "Yes",])
