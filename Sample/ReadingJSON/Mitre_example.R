## Quick & Dirty:
# Reading in a JSON document.
# Then extract some part of the data and visualize it a bit.
# As always, just a DEMO, so don't look for perfection here...

library(jsonlite)
library(visNetwork)
library(dplyr)

#Getting the data "anew", from JSON to Dataframe in one go:
df <- fromJSON("https://raw.githubusercontent.com/mitre/cti/master/enterprise-attack/enterprise-attack.json")
# To not require internet access everytime...
#save(df, file = "/mnt/R/Demos/mitre_data.RData")
#load(file = "/mnt/R/Demos/mitre_data.RData")

df <- df$objects
# Because of JSON extraction to one data-frame we now have MANY empty values. We might want to pass that to different DFs:
unique(df$type)

# Re-creating level 1 of the Matrix:
head(df[df$type == "x-mitre-tactic",])

# Doing some digging into the dataset to try and understand it.
# We could try stuff like:
unique(unlist(df$tactic_refs))
# This tactic_refs being a list for each entry is a bother, indeed. Let's see if we can create a 1-on-1 relationship dataframe here:
extract_l1_rels <- function(df){
  for(i in 1:nrow(df)) {
    if(!is.null(unlist(df[i,"tactic_refs"]))) {
      print(paste(i, df[i,"tactic_refs"]))
    }
  }
}
extract_l1_rels(df)
# Not VERY useful, no...

# So now we want tactics to techniques relationships.
# Easier said than done, let's see what we can do here:
head(df[df$type == "attack-pattern",])
# It would seem that, of all the possibilities, attack-patterns (i.e. techniques) map through
# kill_chain_phases to the tactics (makes sense):
head(str(df[df$type == "attack-pattern", "kill_chain_phases"]))
# Hell, we get data.frames for the techniques mappings...
# Let's create a function for that:
techs_df <- df[df$type == "attack-pattern",]
mappings <- data.frame(tech_id = character(), kill_chain_phase = character())
for(i in 1:nrow(techs_df)) {
  tech_i <- techs_df[i,c("id", "kill_chain_phases")]
  
  if(!is.null(tech_i$kill_chain_phases[[1]])) {
    for(j in tech_i$kill_chain_phases[[1]]$phase_name) {
      mappings <- rbind(mappings, 
                        data.frame(tech_id = tech_i$id, 
                                   kill_chain_phase = j))
    }
  }
}

par(mar=c(4,11,4,4))
barplot(table(mappings$kill_chain_phase), main = "Techniques per tactic Dist.", xlab = "Count", horiz = TRUE, las = 1)

# So mappings have stuff we can visualize in network graph maybe?
# Let's reuse some old code that does just that:

# So to the visualization: A Network graph, using the visNetwork package:
# In the simplest version, we want to have pairs of origins to destinations.
# And then a list of numbered "nodes" that conform the pairs.
conns_from_to <- mappings[, c(2, 1)]
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
conns_nodes$title <- conns_nodes$node # On click
conns_nodes$borderWidth <- 2 # Node border width
conns_nodes$color.background <- "lightblue"
conns_nodes[grep("^attack-pattern.*$", conns_nodes$label), "color.background"] <- "red"

visNetwork(conns_nodes, conns_from_to) %>%
visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  #visHierarchicalLayout() %>%
  #visIgraphLayout() %>%
  visPhysics(solver = "forceAtlas2Based")
