library(igraph)

my_sort_matrix <- function(t_m) {
    # For readability...
    t_m[order(rownames(t_m)), order(colnames(t_m))]
}

clean_adj_mat_from_g <- function(g) {
    m_g <- igraph::as_adjacency_matrix(g)
    m_g <- as.matrix(m_g) # required for transposing.
    my_sort_matrix(m_g)
}

# Traditional look of an adjacency matrix, as defined by "change" here (edges of a graph, visually):
# A -> B, B -> C, C -> C
g <- igraph::graph(c("A", "A", "B", "C", "C", "C"), directed = TRUE)
plot(g)

# Now to the directed adjacency matrix, igraph format:
m_g <- clean_adj_mat_from_g(g)


# W. Ross Ashby's "An introduction to Cybernetics" expects edges to be 
# represented in the OPPOSITE direction - so read columns downwards,
# e.g. Column A -> Row B..., with a "+" sign where there is supposed to be a 
# directed edge.

# For now, we're transposing the matrices... Let's try that:
# A "transformation" here will put a 1 where Ashby expects a "+".
# Written for clarity, not optimization:
transformation_from_adjacency <- function(t_m) {
    # an adjacency matrix, with 1 for directed edges in the reverse direction
    # 0 otherwise.
    my_sort_matrix(
        t(t_m) # Quite straightforward. But expects valid numerical matrix as input
    )
}

# The advantage of the so-called "transformation", there is a composition
# V(x) = U(T(x)) can be expressed then as V(x) = (U %*% T)x
# taking the '+' signs as '1'.

# We'll see in a moment, it's no "advantage", we can simply operate the other
# way around to do a composition directly on the adjacency matrices...

# Take a "normal" igraph directed adjacency matrix,
# make it follow the "transformation" format
t_m_g <- transformation_from_adjacency(m_g)
# The normal behaviour from igraph is then to show edges in the wrong direction
# ...
plot(graph_from_adjacency_matrix(t_m_g))
# Not what we want...

# Print a "transformation" in Ashby's "matrix" format:
# Note: We assume each operand and transform is named with one letter only.
print_1char_transformation <- function(transformation_adjacency_matrix) {
  t_a_m <- transformation_adjacency_matrix # shorter
  cat(paste(' V', 
            paste0(colnames(t_a_m), collapse= ' '),
            "\n",
            paste0(lapply(rownames(t_a_m),
                          function(x) { 
                              paste(x, paste0(ifelse(t_a_m[x,] == 1, '+', '0'), 
                                              collapse=' '), '\n ') 
                              }), collapse='')))
}

# We've essentially "inverted" the edges, no more.
# Now print it in Ashby's proposed format:
print_1char_transformation(t_m_g)
# The corresponding igraph plot requires though the original adjacency matrix...
plot(graph_from_adjacency_matrix(m_g))

# With that, we can work with "traditional" adjacency matrices,
# put them in Ashby's format, 
# compose them (mmul), and then transform back:
composition <- function(m_t, m_u) {
    transformation_from_adjacency(
        transformation_from_adjacency(m_u) %*% 
            transformation_from_adjacency(m_t)
    )
}

# Exercise 3 of section 2/16 of the book becomes:
gt <- igraph::graph(c("a", "b", "b", "d", "c", "a", "d", "b"), directed = TRUE)
gu <- igraph::graph(c("a", "d", "b", "c", "c", "d", "d", "b"), directed = TRUE)
gv <- igraph::graph(c("a", "c", "b", "b", "c", "d", "d", "c"), directed = TRUE)

m_t <- clean_adj_mat_from_g(gt)
m_u <- clean_adj_mat_from_g(gu)
m_v <- clean_adj_mat_from_g(gv)

# Let's look at some examples here of what things look like:
m_t # adjacency matrix
print_1char_transformation(transformation_from_adjacency(m_t)) # Ashby's transformation in matrix notation
m_u
print_1char_transformation(transformation_from_adjacency(m_u))
m_v # Expected result of composition

m_t_then_u <- composition(m_t, m_u) # Apply T, then U
# This should give the same result as for m_v
print_1char_transformation(transformation_from_adjacency(m_t_then_u))
print_1char_transformation(transformation_from_adjacency(m_v))
# CORRECT

# Compare results visually:
plot(graph_from_adjacency_matrix(m_v))
plot(graph_from_adjacency_matrix(m_t_then_u))

# But why work so much??

# It turns out Ashby's composition of transformations, is a MMul in the other
# order of the adjacency matrices...
simple_compose <- m_t %*% m_u
plot(graph_from_adjacency_matrix(simple_compose))
# We can still show it in Ashby's format, sure...
print_1char_transformation(transformation_from_adjacency(simple_compose))

# So now we can test the composed transformations:
x <- c(1, 0, 0, 0) # What would the input "a" become?
names(x) <- c("a", "b", "c", "d")
x
x %*% simple_compose # a should become c, and indeed it does...
# However I like this format better, more similar to the equation
# usual mathematical notation of Matrix times vector (column) "Ax":
transformation_from_adjacency(simple_compose) %*% x # here x is made vertical implicitely

x <- c(0, 1, 0, 0) # What would the input "a" become?
x %*% simple_compose # b stays at b after composed transformation
transformation_from_adjacency(simple_compose) %*% x

# The end... Ashby's format for presenting a transformation has no real
# advantage over the directed adjacency matrix in igraph...
# As long as "compositions" and "transformations" are run in the right order.
