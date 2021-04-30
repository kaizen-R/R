# Testing future package and future.apply

# So we know that rbind.fill(lapply()) is faster than rbind+for loop.
# But for "slow" functions, like Whois resolution, it might not be relevant.
# In those cases, we could make things faster however, by parallelizing!
# And I didn't know about "futures" until today...

library(future.apply)
library(Rwhois) # A reasonable slow function
library(microbenchmark)

availableCores() # 4 on the Home Lab Server
plan(multisession) # Let's compare a couple of speed tests



domains_vector <- c("google.com", "kaizen-r.com", "nytimes.com", 
                    "economist.com", "had.co.nz", "urlscan.io",
                    "towardsdatascience.com", "aquasec.com", "ipvoid.com",
                    "rstudio.com", "mitre.org", "what2log.com")

microbenchmark(
  lapply(domains_vector, whois_query),
  future_lapply(domains_vector, whois_query),
  times = 5L
)
