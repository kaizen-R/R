## File: dnsmasq_log_reader_v001.R
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)

library('visNetwork')
library('shiny')

# Lubridate cheat sheet: https://rawgit.com/rstudio/cheatsheets/master/lubridate.pdf
#options(encoding = "UTF-8")

# For dnsmasq to log queries, I added the log-queries option to the dnsmasq config

# Version 0.1:
# For the exercise, I gathered some /var/log/syslog files locally (we'll see later on how to evolve that)
dns_logs_dir <- "/mnt/R/Demos/server_data/syslog_data/"
dns_logs <- lapply(list.files(dns_logs_dir), function(x) { 
  tempfile <- paste0(dns_logs_dir, x)
  # First of, I have an issue with the logs: In the format I have them, the year is not informed.
  # As we are already in December, this will soon be a problem...
  # So I'll add the year of the file to each log entry like so: (requires lubridate package)
  paste(year(file.info(tempfile)$mtime),
        readLines(tempfile, warn = FALSE))
  })

# We have a list of entries per file read:
dns_logs <- unlist(dns_logs)

# Now from all the logs, we keep only those that include a query:
dns_logs <- dns_logs[grep("dnsmasq.*query\\[", dns_logs)]

# Finally we prepare a data.frame to work with later on:
dns_logs <- data.frame(log = dns_logs, req.ip = "", domain = "", 
                       stringsAsFactors = FALSE)

# The requesting IP address appears at the very end of the string.
# One way to go about it is a to look for "everything not a space until the end":
dns_logs$req.ip <- with(dns_logs, str_extract(log, "[^ ]+$"))

# Check https://evoldyn.gitlab.io/evomics-2018/ref-sheets/R_strings.pdf for reference
# Another way to go about extracting the domain name from the string:
# So what we do is first look for everything after the last "]" character:
dns_logs$domain <- with(dns_logs, str_extract(log, "[^\\]]+$"))
# and then everything not a space character after the first " ":
dns_logs$domain <- str_extract(dns_logs$domain, "[^ ]+")
# Now domain contains the FQDN

# Now let's add the date & time
dns_logs$datetime <- with(dns_logs, str_sub(log, 1, 20))
# Date might have up to two spaces (because of 1 or 2 digits day number)
dns_logs$datetime <- sub("  ", " ", dns_logs$datetime)
dns_logs$datetime <- ymd_hms(dns_logs$datetime)

# Now we're ready to get rid of the log itself
dns_logs <- dns_logs[,-1]

# Let's add some useful variables for visualization:
dns_logs$date <- date(dns_logs$datetime)
dns_logs$hour <- hour(dns_logs$datetime)

# Let's check volume of queries per day first:
dns_logs_days <- dns_logs %>% group_by(date) %>% tally()
days_dns_plot <- ggplot(dns_logs_days, aes(date, n, group = 1)) + geom_line()
days_dns_plot
# As I don't have much spare time, I don't test the server as consistently as I'd like...

dns_logs_hours <- dns_logs %>% group_by(hour) %>% tally()
hours_dns_plot <- ggplot(dns_logs_hours, aes(hour, n)) + geom_line()
hours_dns_plot
# It seems like I do most of my testing with the server early in the mornings...

# Next, let's look at the domains:
# First level (TLD) are not too interesting, so let's focus on second and third level:
# Let's first get all the levels, separated by "."
dns_logs$domain_levels <- strsplit(dns_logs$domain, "\\.")

# Now let's get "functional" - Part of the exercise:
# For more on this, see: http://adv-r.had.co.nz/Functional-programming.html
sublevel_n <- function(n) {
  function(x) {
    x_l <- length(x)
    ifelse(x_l >= n,
           paste(x[(x_l-n+1):x_l], collapse = "."),
           NA
    )
  }
}
# So I've just created a function that creates functions using a parameter.
# Why, you ask?
first_level <- sublevel_n(1)
sapply(dns_logs$domain_levels, first_level)
second_level <- sublevel_n(2)
sapply(dns_logs$domain_levels, second_level)

# I could go on. And for a future evolution, connecting one level to the next, I would simply create and connect
# sublevel_minus1 to sublevel_current, in a loop of some sorts... We'll see if I do it later on.
