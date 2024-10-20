## Lagrange Polynomials
## Inspired by the math describing it

lagrange_p_v1 <- function(x, vec_x, vec_y) {
    ## Vec_x are x positions, vec_y the y positions of (x, y) 2-dimensional points
    n <- length(vec_x)
    
    if(n != length(vec_y)) { print("Error, coordinates are not right"); return(NULL) }
    
    x_multipliers <- sapply(1:n, function(item_pos) {
        prod(x - vec_x[-item_pos]) / prod(vec_x[item_pos] - vec_x[-item_pos])
    })
    ## Pass vector of x for known points each time to prepare for l_N(x) 
    ## is done *implicitely*
    ## through Scoping of function here.
    ## same for searched interpolated position x

    sum(vec_y * x_multipliers) # Implicit return
}

## Example N reference points for interpolation (inc. range)
ex1_x <- c(-2, -1, 1, 2)
ex1_y <- c(-6, 0, 0, 6)

y_hat <- sapply(x, lagrange_p_v1, ex1_x, ex1_y)

plot(x, y_hat, type="l")
points(ex1_x, ex1_y, col="red")
