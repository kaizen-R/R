# Example implementation of Havel-Hakimi algorithm
# Based on Pseudo-code from "Graph Theory and its applications", 3rd ed., J.L. Gross, J. Yellen, M. Anderson, Ed. CRC
# The code is my own, and any error is my mistake.

library(igraph) # here only for sample_degseq()

graphic_sequence <- function(my_seq, verbose = FALSE) {
    my_seq <- my_seq[order(my_seq, decreasing = TRUE)]
    if(verbose) print(paste("<", paste(my_seq, collapse=","), ">, 
                            |V|=", length(my_seq)))
    d1 <- my_seq[1]
    if ((d1 >= length(my_seq)) || any(my_seq == -1)) {
        if(verbose) {
            message("False")
            message(my_seq)
        }
        return(FALSE)
    }
        
    if(my_seq[1] == 0) {
        if(verbose) message(my_seq)
        return(TRUE)
    }
    
    new_seq <- my_seq[2:length(my_seq)]
    new_seq[1:my_seq[1]] <- new_seq[1:my_seq[1]]-1
    if(verbose) message(new_seq)
    graphic_sequence(new_seq, verbose)
}

# Testing it:
test_degrees_seq <- c(6,3,3,3,3,2,2,2,2,1,1) # Example from Wikipedia
graphic_sequence(test_degrees_seq, verbose = TRUE) # should return TRUE, as the sequence is "graphic"

# Plotting
# Supposing the sequence is OK, this function from the igraph library is handy:
plot(sample_degseq(test_degrees_seq, method="vl")) 

test_degrees_seq <- c(6,6,5,3,4,4)
graphic_sequence(test_degrees_seq, verbose = TRUE) # should return FALSE, not enough vertices for the sequence
test_degrees_seq <- c(6,6,6,6,4,4,3)
graphic_sequence(test_degrees_seq) # should return FALSE, odd total for degrees
