library(iptools)
library(dplyr)
library(reshape2)
library(stringr)
library(lubridate)
library(maps)
library(ggmap)
library(ggthemes)

# first let's refresh IP subnetting & al.
# This time around, instead of programming my own version, I'll use the package
# iptools, which will help make things a bit faster. Let's test it:

# This one will be useful almost anytime:
private_ip_ranges <- c("10.0.0.0/8", "172.16.0.0/12",
                       "192.168.0.0/16", "169.254.1.0/24")

# # Quick tests:
# test_ips <- c("10.0.0.1", "41.242.140.10")
# test_subnet_masks <- c("10.0.0.0/8", "41.242.140.0/31")
# 
# is_ipv4("10.0.0.1000")
# ip_in_range(test_ips[1], test_subnet_masks[1])
# ip_in_any(test_ips[1], private_ip_ranges)
# ip_in_any(test_ips[2], private_ip_ranges)
# ip_in_any(test_ips, private_ip_ranges)
# 
# # Nice. I haven't checked speed, but let's assume
# ip_in_range(test_ips, test_subnet_masks)
# # Very nice indeed, we'll be able to use that


# from ggplot2
map_data_world <- map_data("world")

# maps::iso3166 contains the relevant data for countries:
# Not used, but could be useful to bypass mismatching countries names by using
# iso country names instead.
#context_countries <- iso3166

# Now map_data_world has many point marking the limits of each country...
# For later use, let's calculate the Countries "centers", i.e. the means of
# latitude and longitude (albeit a simplified approach):
countries_centers <- map_data_world %>%
  group_by(region) %>%
  summarise(mean_long = mean(long), mean_lat = mean(lat)) %>%
  select(region, mean_long, mean_lat) %>%
  distinct() %>%
  mutate(mean_long = ifelse(region == "USA", -98.5, mean_long),
         mean_lat = ifelse(region == "USA", 39.5, mean_lat)
  ) # Because of Alaska mainly, USA shows mis-aligned, hence manual fix

# Next, we will want geo data for IP ranges, as exported from MaxMind GeoLite2:
geo_ip_data <- read.csv("/mnt/R/Demos/maxmind_extract/GeoLite2-Country-Blocks-IPv4.csv")
geo_ip_context <- read.csv("/mnt/R/Demos/maxmind_extract/GeoLite2-Country-Locations-en.csv")

geo_ip_data <- merge(geo_ip_data[, c("network", "geoname_id")], 
                     geo_ip_context[, c("geoname_id", "country_iso_code", "country_name")], 
                     all.x = TRUE)
# We'll come back to this later

# Time to play a bit, with our NetFlow data -- we'll get to DNS later on...
# This being just an exercise, a test, if you will:
# From the Home Lab Server, I capture fprobe data on my Wifi interface.
# Then exported with nfdump in csv format:
demo_nf <- read.csv("/mnt/R/Demos/server_data/nfdump_data/nfdump_example_20201205_morning.csv")
demo_nf <- demo_nf[,c(1,3,4,5,6,7,8)]
all_ips_seen <- c(demo_nf$sa, demo_nf$da)
# Keep only valid IP v4: (package iptools::is_ipv4)
all_ips_seen <- all_ips_seen[!is.na(is_ipv4(all_ips_seen))]
# For geo_location, we will focus on public IPs...:
all_ips_seen <- all_ips_seen[!ip_in_any(all_ips_seen, 
                                        private_ip_ranges)]
# For later "merge()":
short_ip_df <- as.data.frame(table(all_ips_seen))
short_ip_df[] <- lapply(short_ip_df, as.character)

# We now have the base data we need. Let's add the countries in there now.
# Let's try a reasonably simple solution here.
# Because we want to do this faster, we'll use numeric values.
# range_boundaries is from iptools package:
head(geo_ip_data)
geo_ip_data <- cbind(geo_ip_data,
                     range_boundaries(geo_ip_data$network)[,c("min_numeric", 
                                                              "max_numeric")])
short_ip_df$numeric_ip <- ip_to_numeric(short_ip_df$all_ips_seen)

country_code_for_IP <- function(t_ip_num) {
  country <- geo_ip_data[(t_ip_num >= geo_ip_data$min_numeric) & 
                           (t_ip_num <= geo_ip_data$max_numeric),
              "country_name"]
  data.frame(numeric_ip = t_ip_num, 
    country = ifelse(!is.null(country), country, ""))
}

ip_country <- melt(lapply(short_ip_df$numeric_ip, country_code_for_IP)) %>%
  # lapply returns a list of two variables, melt makes it into a data.frame
  filter(!is.na(country)) %>%
  select(value, country) # to remove unnecessary columns added by melt.

# Let's prepare for the graph now:
for_graph <- merge(short_ip_df, ip_country,
                   by.x = "numeric_ip", by.y = "value",
                   all.x = TRUE)
for_graph <- for_graph[!is.na(for_graph$country),]
for_graph$Freq <- as.numeric(for_graph$Freq)

for_graph_short <- for_graph %>% 
  group_by(country) %>%
  summarise(n = sum(Freq)) %>%
  mutate(
    country = ifelse(country == "United States", "USA", country),
    country = ifelse(country == "Antigua and Barbuda", "Antigua", country)
  ) # Those two at least wouldn't match with the map_data_world otherwise

for_graph_short <- merge(for_graph_short,
                  countries_centers, # Created earlier
                  by.x = "country",
                  by.y = "region",
                  all.x = TRUE)

# Let's plot some maps now:

# That's the basis for our "painting".
world <- ggplot() +
  borders("world", colour = "gray85", fill = "gray80") +
  theme_map() 

# Let's now add some "dots" showing the number of connections to/from IPs seen
# in our Netflow data:
map <- world +
  geom_point(aes(x = mean_long, y = mean_lat, size = n),
             data = for_graph_short, 
             colour = 'red', alpha = .5) +
  ggtitle("Netflow's observed Geo IPs by number of connections")

map
