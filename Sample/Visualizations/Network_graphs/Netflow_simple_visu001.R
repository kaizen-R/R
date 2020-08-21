# File: Netflow_simple_visu001.R
# Using Netflows data and Network Graphs Visualizations

# Data used is a sample of 10000 random lines extracted of the first 500000 netflow entries found here:
#  https://www.secrepo.com/Security-Data-Analysis/Lab_1/conn.log.zip 

library('data.table')
library('visNetwork')
library('dplyr')

conns <- read.csv("/mnt/R/Demos/conn_extracts_log/second_sample.log", header = FALSE, sep = "\t")
# or, probably much faster (at least from my experience), which we could test:
conns2 <- fread(file = "/mnt/R/Demos/conn_extracts_log/second_sample.log", header = FALSE, sep = "\t")

#First and foremost, an initial "Data Exploration"
class(conns)
dim(conns)
head(conns)
str(conns)

# Clearly defaults differ between one and the other:
class(conns2)
dim(conns2)
head(conns2)
str(conns2)

# OK so this one is not an exercise about optimization. Let's work with data.frames,
# which is more common/traditional.
rm(conns2)

# So to the visualization: A Network graph, using the visNetwork package:
# In the simplest version, we want to have pairs of origins to destinations.
# And then a list of numbered "nodes" that conform the pairs.
conns_from_to <- conns[, c("V3", "V5")]
names(conns_from_to) <- c("from", "to")
head(conns_from_to)
# Before we go any further, this simplified version is only origins to destinations.
# But, we could aggregate a "weight" in the form of number of ocurrences of each
# unique pair. That's for a future, more elaborate version.
# Hence, let's keep only one entry per pair:
conns_from_to <- conns_from_to %>% distinct()
# This has reduced a LOT the data.frame size here (this will depend on the sample).
# careful though:
str(conns_from_to)
# We actually want values:
conns_from_to[] <- sapply(conns_from_to, as.character)

# If we look at it, we might see IPv6 in there. Let's simplify a bit the dataset and focus on IPv4:
is_valid_ip_string_format <- function(ip = NA)  {
  is.character(ip) && grepl("^((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)(\\.|$)){4}", ip)
}
# Now we keep only the IPv4:
conns_from_to <- conns_from_to[sapply(conns_from_to$from, is_valid_ip_string_format) &
                                 sapply(conns_from_to$to, is_valid_ip_string_format), ]
# Next, we need to create a list of nodes with identifiers ("id"):
nodes <- unique(c(conns_from_to$from, conns_from_to$to))
conns_nodes <- data.frame(id = seq(1:length(nodes)), node = nodes)

# OK so finally, let's merge things (one way to go, NOT optimal):
conns_from_to <- merge(conns_from_to, conns_nodes, by.x = "from", by.y = "node", all.x = TRUE)
conns_from_to <- conns_from_to[, -1]
conns_from_to$from <- conns_from_to$id
conns_from_to$id <- NULL

# Or another option:
conns_from_to$to <- sapply(conns_from_to$to, function(x) { conns_nodes[conns_nodes$node == x, "id"]})

# Note: The choice of Data.Frame over Data.Table has made the code above less nice to the eye...
# Nevertheless, the above can be put in a function for future use.

# Now we have a nodes list and an edges list.
# Final step for this simplified exercise:

conns_nodes$shape  <- "dot"  
conns_nodes$shadow <- TRUE # Nodes will drop shadow
conns_nodes$label  <- conns_nodes$node # Node label
vis.nodes$title <- conns_nodes$node # On click
conns_nodes$borderWidth <- 2 # Node border width
conns_nodes$color.background <- "lightblue"

visNetwork(conns_nodes, conns_from_to) #, height="800px", width="1600px")
