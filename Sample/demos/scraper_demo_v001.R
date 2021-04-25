# scraper_demo_v001.R
# Gathers all unique websites pointed to by Certificates associated to a domain.
# Uses CRT.sh as data source.
# Scrapes the CRT.sh website with CURL, then parses output with XML2.
# Then navigates to relevant table contents using rvest and xpath for that page.
# We do some necessary cleaning...
# We then filter the results for certificates still active by indicated date.
# Finally puts together a list of website names, and that's that.

library(curl)
library(xml2)
library(htmltools)
library(rvest) # to extract table: html_table
library(lubridate)
library(dplyr)
library(stringr)

# get the data:
conn <- curl("https://crt.sh/?q=kaizen-r.com")
to_parse <- xml2::read_html(conn)
close(conn)

# Locating our objective table using xpath (use F12 in your browser to find that):
table_object <- html_node(to_parse, xpath="/html/body/table[2]/tr/td")

# html_table removes the <br> tags, which strings together cells contents, so:
xml_find_all(table_object, ".//br") %>% 
  xml_add_sibling("p", "|")
xml_find_all(table_object, ".//br") %>% xml_remove()

parsed_df <- as.data.frame(html_table(table_object))
names(parsed_df) <- make.names(names(parsed_df))
# Let's keep only certificates which are currently active
dates_columns <- 1:2
parsed_df[, dates_columns] <- lapply(parsed_df[, dates_columns], lubridate::as_date)
parsed_df <- parsed_df %>% dplyr::filter(Not.After > Sys.Date())

# Finally, I want only the URIs out of it:
table_domain_rows <- paste(parsed_df$Common.Name, parsed_df$Matching.Identities, sep = "|")
# then put that in a string:
table_domains <- paste(table_domain_rows, collapse = "|")
# separate in substrings with | separator:
domains_vector <- unique(unlist(str_split(table_domains, "\\|")))
# And we have our own vector of strings of domain names.
