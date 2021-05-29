#shodan_api_tests_w_db_v002.R
# Requires a Shodan.io API key AND a pre-set Postgres DB up and running...

# Focus for today: Postgres SQL Database interaction, with JSON data. Examples only.

library(curl)
library(jsonlite)
library(RPostgres)

##
# Variables
##
hostname <- "www.kaizen-R.com" # Replace with your own website of interest
# As usual, do not put confidential data in your code...
# Keys management is a world in itself, just... Be careful:
s_apikey <- readLines("/mnt/R/.shodanAPI.conf")
db_t_pass <- readLines("/mnt/R/.test_postgres_pass")

##
# Functions:
##
establish_con <- function() {
  dbConnect(RPostgres::Postgres(), dbname = "default_db",
            host = "postgres_test",
            port = 5432,
            user = "default_user",
            pass = db_t_pass)
}

##
# Main Script
##

# Now on to ourselves as an example: Working on an IP
conn <- curl(paste0("https://api.shodan.io/shodan/host/", 
                    nslookup(hostname)[1], # Or you can lookup any IP you wish...
                    "?key=", s_apikey))
s_q_result <- readLines(conn, warn = FALSE)
close(conn)
#s_q_result <- fromJSON(s_q_result, flatten = TRUE) ## Not needed anymore...

con <- establish_con()
dbListTables(con)
dbDisconnect(con)

# Writing into Table directly from a data.frame...
con <- establish_con()
## DUMMY EXAMPLE, the IP should be real...
dbWriteTable(con, "shodan_data", data.frame(ip = "my IP checked", s_data = s_q_result),
             append = TRUE, row.names = FALSE)
dbDisconnect(con)

# Adding data to a table, using INSERT:
con <- establish_con()
t_query <- "INSERT INTO shodan_data(ip, s_data) VALUES ('anotherIP', '{\"object_test\": \"test\"}');"
res <- dbSendQuery(con, t_query)
print(res)
dbClearResult(res)
dbDisconnect(con)

con <- establish_con()
t_query <- "SELECT * FROM shodan_data"
res <- dbSendQuery(con, t_query)
res2 <- dbFetch(res) # For potentially later use...
dbClearResult(res)
dbDisconnect(con)

# Let's check what we have there now:
res2

# What if you want to change some value(s)?
con <- establish_con()
t_query <- "UPDATE shodan_data SET s_data = '{\"object_test\": \"change test\"}' WHERE ip = 'anotherIP';"
res <- dbSendQuery(con, t_query)
dbClearResult(res)
dbDisconnect(con)

# Which you can then check like so:
con <- establish_con()
t_query <- "SELECT * FROM shodan_data WHERE ip = 'anotherIP';"
res <- dbSendQuery(con, t_query)
res2 <- dbFetch(res) # For potentially later use...
dbClearResult(res)
dbDisconnect(con)

# Finally, how to get data piece from a JSON saved entry?
## IN SOME CASES, one JSON variable contains multiple objects... use --><number>
## In more complex cases, it can become (s_data->'variable'->><number>)::json->'object variable'
## And so on...
con <- establish_con()
t_query <- "SELECT ip, 
    s_data->'country_code' AS country_ISO,
    s_data->'domains'->>0 AS first_domain
    FROM shodan_data"
res <- dbSendQuery(con, t_query)
res2 <- dbFetch(res) # For potentially later use...
dbClearResult(res)
dbDisconnect(con)

# Let's check what we have there now:
res2
