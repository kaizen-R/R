# File: Comparing_Reading_Speeds_v001.R

library(plyr)
library(data.table)

# In a recent exercise, we splitted a big file into smaller files.
# We'll test the rbind.fill and fread functions, comparing to read.csv and for loops.
# In this case, we won't use microbenchmark, as we won't need that kind of precision.

# Spoiler alert: I have ran this test a few times, the second option was 10x faster.

# Point to your directory of CSV files:
files_dir <- "/mnt/R/Demos/conn_extracts_log/Conns_extracts/"

files <- paste0(files_dir, dir(files_dir))
files
# > files
# [1] "/mnt/R/Demos/conn_extracts_log/Conns_extracts/conn_extract_log.aa"
# [2] "/mnt/R/Demos/conn_extracts_log/Conns_extracts/conn_extract_log.ab"
# [3] "/mnt/R/Demos/conn_extracts_log/Conns_extracts/conn_extract_log.ac"
# [4] "/mnt/R/Demos/conn_extracts_log/Conns_extracts/conn_extract_log.ad"
# [5] "/mnt/R/Demos/conn_extracts_log/Conns_extracts/conn_extract_log.ae"
# ...

# In my case, "files" contains a list of 26 file names, with their path.
# All files contain rows of the exact same format.
# The "game" here is to load them all in one data.frame.

# I know the size of each of the files, 100K lines, 20 variables.
# Now for the exercise, let's assume we only have num_files files to read.
num_files <- 10

# Let's read the files into a data.frame more or less the way I would have 
# done it a few months ago:
start_t <- Sys.time()
tempdf <- read.csv(file = files[1], header = FALSE, sep = "\t")
for(i in files[2:num_files]) {
  print(Sys.time())
  tempdf <- rbind(tempdf,
                read.csv(i, header = FALSE, sep = "\t"))
  print(Sys.time())
  cat("\n") # This is added here to explain that each iteration is slower...
}
end_t <- Sys.time()

end_t - start_t

# So that was OK with a few small files.
# But at one point I needed to read MANY such files.
# More precisely, 24*365 files of a few thousand lines each.
# And the above was NOT the way to go, as only reading the data ended up
# taking, literally, hours.

# It actually ended taking me less time to investigate, find a better way, and fix
# the script, than waiting for the reading to finish.
# Tough call tough at the time:
#  It was late already and I had a delivery of a dashboard that depended 
#  on working with that data due "first thing" the next day... But worth it!

# How I go about it now:
#
# Many thanks to StackOverflow, as it often happens:
# https://stackoverflow.com/questions/31316343/reading-multiple-csv-files-faster-into-data-table-r
#
# rbind.fill from plyr
# fread from data.table.

start_t2 <- Sys.time()
tempdf2 <- rbind.fill(lapply(files[1:num_files], fread, header = FALSE, sep = "\t"))
end_t2 <- Sys.time()

end_t2 - start_t2

# CAREFUL with data types set by fread if you're used to read.csv
str(tempdf)
str(tempdf2)
