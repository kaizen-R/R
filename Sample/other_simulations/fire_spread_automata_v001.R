## Playing with Cellular Automata
## Forest Fire Simulator!
## Author: Nico
## Date: November 2023

library(MBCbook) ## For imshow()

p0 <- 0.6 ## Initial prevalence of trees on the map
map_size_l <- 100 ## Map size, a matrix to be, so squared

view_speed <- 0.1 ## animation speed (smaller == faster)

## This is the key! Identify whether any direct neighbour is on fire:
## Von Neumann Neighbourhood, not circular:
neighbour_on_fire <- function(pos, temp_map) {
    ## Works better with C-kind indexes, but can be tricked:
    t_row <- pos %% map_size_l
    i <- ifelse(t_row == 0, map_size_l, t_row) ## i will be row
    j <- ceiling(pos/map_size_l) ## j will be column
    
    if(((j > 1) && (temp_map[i, j-1] == on_fire)) || ## left
       ((j < map_size_l) && (temp_map[i, j+1] == on_fire)) || ## right
       ((i > 1) && (temp_map[(i-1), j] == on_fire)) || ## bottom
       ((i < map_size_l) && (temp_map[i+1, j] == on_fire))) ## up
        return(TRUE)
    
    FALSE ## no neighbour is burning...
}

## For more readable code
pos_free <- 0; pos_tree <- 1; on_fire <- 2; burnt <- 3
## Initialize the map
the_map <- matrix(rep(pos_free, map_size_l^2), 
                  byrow = TRUE, 
                  nrow=map_size_l)

## Initial setting is p0 proportion of map is covered randomly by trees
## Trick with R matrices, one-position-only gives you cols*rows:
the_map[sample(map_size_l^2, round(p0*map_size_l^2))] <- pos_tree

## Start the fire :
the_map[sample(which(the_map == pos_tree), 1)] <- on_fire

next_map <- the_map
for(t_step in 1:1000) {
    
    ## Lit by neighbour(s) on fire:
    t_trees <- which(the_map == pos_tree)
    next_map[t_trees] <- sapply(t_trees, function(t_pos) {
        ifelse(neighbour_on_fire(t_pos, the_map), 
               on_fire,
               pos_tree)
    })

    ## Trees burn in one turn
    next_map[which(the_map == on_fire)] <- burnt
    
    ## Visualize current time-step's map:
    Sys.sleep(view_speed) ## Toggle animation speed
    imshow(the_map,
           col=c("white", "green", "red", "black"))
   
    ## Break if no change...
    if(all(next_map == the_map)) break;
    ## Else, well, continue...
    the_map <- next_map
}

