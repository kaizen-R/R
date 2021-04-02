#shodan_api_tests_v001.R
# Requires a Shodan.io API key to work...

library(curl)
library(jsonlite)

hostname <- "www.kaizen-R.com" # Replace with your own website of interest

# As usual, do not put confidential data in your code...
# Keys management is a world in itself, just... Be careful:
s_apikey <- readLines("/mnt/R/.shodanAPI.conf")

# Now on to ourselves as an example: Working on an IP
conn <- curl(paste0("https://api.shodan.io/shodan/host/", 
                    nslookup(hostname)[1], # Or you can lookup any IP you wish...
                    "?key=", s_apikey))
s_q_result <- readLines(conn, warn = FALSE)
close(conn)
s_q_result <- fromJSON(s_q_result, flatten = TRUE)

# Look at what you get:
str(s_q_result)

s_q_result$domains
s_q_result$ip
s_q_result$ip_str
s_q_result$country_code
s_q_result$asn
s_q_result$last_update
s_q_result$ports

# And at the "service" level, for that IP:
df <- s_q_result$data 
df$location.country_code



# Example for search query with some more results...
object_q <- "kaizen"
conn <- curl(paste0("https://api.shodan.io/shodan/host/search?key=", s_apikey,
                    "&query=", object_q))
s_q_result <- readLines(conn, warn = FALSE)
close(conn)
s_q_result <- fromJSON(s_q_result, flatten = TRUE)

str(s_q_result) # A hell of a lot more verbose than looking up one IP, of course...
