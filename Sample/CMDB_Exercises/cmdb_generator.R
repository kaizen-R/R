# File: cmdb_generator.R
# Version: 0.1
# Created by: Nico (a.k.a. Nick)
# Date last edited: 2020/07/11

# Demo dataset: Trying to be true to real life, but without showing real data.
# This demo dataset is messy ON PURPOSE, to demo some R code to work on it and make it "tidy".

# NOTICE: The messiness of the data generated here is NOT representative of any PARTICULAR real-world dataset.
# Rather it intends to be representative of MANY datasets out there.
# As per why call it a "CMDB", it's just to show a use-case for using R I came across a few times in the past.
# And yes, you could do it with Excel, but think big: Many files, thousands of machines, etc.
# Then throw in tens of thousands of users for example, and daily updates of software versions and bugs,
# and Excel becomes less practical rather quickly.
# Although, that's probably because I like using R. I consider Excel to be a great tool, one I definitely use!

# So what am I doing here?

# A few machines names and their IP addresses, as you might find them in a CMDB.
# Here I'll assume the machines are simply added to the CMDB as they are created.
# They can have one or more IP addresses, and be a Windows or a Linux server.
# IP addresses are added randomly from those still available in a standard private IP range.
# I throw in some upper case and mixed up FQDN, for good measure...

## Let's make this a double exercise: We'll check on memory usage as we go...
#utils::install.packages("pryr")
library(pryr)


num_machines <- 100

# I have never found such a simple scenario, but this should be good enough for demo purposes:
machine_names <- NULL

machine_names <- paste0(sample(c("Win", "Lin"), num_machines, replace = TRUE), "Demo0", 1:num_machines)

object_size(machine_names)

# Let's keep a copy for later: And let's see what happens to memory after this...
mem_used()
mem_change(machine_names_bck <- machine_names)
object_size(machine_names_bck)
object_size(machine_names)
object_size(machine_names) + object_size(machine_names_bck)
object_size(machine_names, machine_names_bck)
# Interesting how this is not double the size at all...

length(machine_names)
machine_names[100] <- "test memory and variables in R - 'copy on modify' trick"
object_size(machine_names_bck)
object_size(machine_names)
object_size(machine_names) + object_size(machine_names_bck)
object_size(machine_names, machine_names_bck)

mem_used()
machine_names <- NULL
machine_names <- machine_names_bck

# Let's make it more interesting/realistic:
complex_machine_names <- sample(1:num_machines, 50)
machine_names[complex_machine_names] <- toupper(paste0(machine_names[complex_machine_names], ".kaizen-R.com"))

# We will make sure some machines have two IP addresses, just for the fun of it:
machine_names <- c(machine_names, machine_names[sample(1:num_machines, 30)])

# So we have 100 unique machine names, + 50 entries for those with 2 IP addresses. Let's simply sample out of a private range:
cmdb <- data.frame(machine_name = machine_names,
           ip_address = paste0("192.168.1.", sample(1:255, 130)))
# head(cmdb)
# str(cmdb)
# hist(table(cmdb$machine_name))
# View(cmdb)


# Our CMDB also inventories maps of servers to applications
# You see here, when we want to merge these, there will be some issues, because of naming things...
cmdb_apps <- data.frame(machine_name = machine_names_bck, app = I(""), type = I("Production"))

# The I() trick above is relevant, as I'll want to add values, and that'd be levels for factors.

# We'll keep these for nodes lists:
server_apps <- c("Corporate Mail", "Web Servers", "Data Platform")


cmdb_apps[sample(1:num_machines, 25), "app"] <- server_apps[1]
cmdb_apps[sample(1:num_machines, 25), "app"] <- server_apps[2]
cmdb_apps[sample(1:num_machines, num_machines - 25), "app"] <- server_apps[3] 

cmdb_apps[sample(1:num_machines, 20), "type"] <- "Devel"
cmdb_apps[sample(1:num_machines, 30), "type"] <- "Pre-Production"

# str(cmdb_apps)
# View(cmdb_apps)

# Finally, we keep these for future use. And we throw in some noise once again, on purpose:
write.csv(cmdb, file = "machine_inventory.csv", row.names = FALSE)
# This set will serve to demo comparison of grep and alternatives.

# this next bit we'll use to demo rbind.fill and fread instead of for loops:
write.csv2(cmdb_apps[cmdb_apps$type == "Production", ], file = "machine_apps_pro.csv", row.names = TRUE)
write.csv2(cmdb_apps[cmdb_apps$type == "Pre-Production", ], file = "machine_apps_PRE.csv", row.names = TRUE)
write.csv2(cmdb_apps[cmdb_apps$type == "Devel", ], file = "machine_apps_des.csv", row.names = TRUE)
