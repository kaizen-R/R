## Function - Search space: Decision to optimize, here looking for MIN(f(x,y))
sample_2d_decision_fun <- function(x, y) {
    a <- 20; b <- 0.2; c = 2*pi
    d <- 2 # number of variables of decision
    a + exp(1) -
        a * exp(-b*sqrt((x^2+y^2)/d)) -
        exp((cos(c*x)+cos(c*y))/d)
} ## plenty of local minima, simulated annealing hard.

## Bin2Real
## As this requires a few repeating parameters, we use a "closure":
bin2real_1d_params <- function(l, u, t_nb) {
    function(t_bin) {
        ## Simplified: 2^reverse of position, summed:
        t_val <- sum(2^(which(rev(t_bin)==1)-1))
        ## Values in a range:
        t_nb <- t_nb - 1
        t_max_range <- sum(2^which(rep(1, t_nb)==1))+1
        l + t_val*((u-l)/t_max_range)
    }
}
#bin2real_1d(rep(0,11))
# bin2real_1d(rep(1,11))
# bin2real_1d(c(0,0,0,0,0,0,0,0,0,0,1))

## Let's generate a few "individuals", each bivariate in binary:
generate_pop <- function(n_individuals, bits_per_individual) {
    lapply(1:n_individuals, function(x) {
        list(x = sample(0:1, bits_per_individual, replace = TRUE),
             y = sample(0:1, bits_per_individual, replace = TRUE))
    })
}

## Generate visual Space: x, y, z GLOBAL Variables
draw_space <- function() {
    persp(x, y, z,
          theta = 20, phi = 20, ## Show some different perspective
          expand = 0.5,
          col = "lightblue",
          ticktype = "detailed")
}

## Draw a population overlay:
draw_pop <- function(my_individuals) {
    x1_store <- sapply(my_individuals, function(t_ind) bin2real_1d(t_ind$x))
    y1_store <- sapply(my_individuals, function(t_ind) bin2real_1d(t_ind$y))
    z_store <- sample_2d_decision_fun(x1_store, y1_store)
    
    pmat <- draw_space()
    my_points <- trans3d(x1_store, y1_store, z_store,
                         pmat=pmat)
    points(my_points, pch = 16, col = "red")
}
