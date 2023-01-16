library(MASS) # fitdistr, great function, estimates parameters...
library(ggplot2)

## FOR FUTURE VERSIONS, not for basic demo:
#library(fitdistrplus) # descdist, fitdist, more advanced options
#library(logspline) # Never used, pending - Found in StackOverflow somewhere

vis_compare_hist <- function(orig_dist, selected_dist) {
    x <- orig_dist
    t_df <- as.data.frame(x=x)
    tryCatch({
        t_fit <- fitdistr(x, selected_dist) # tryCatch because...
        # careful w/ negatives for some distribution, but don't test one by one...
        
        t_den <- switch(selected_dist,
                        normal = dnorm(x, mean=t_fit$estimate[1], sd=t_fit$estimate[2]),
                        exponential = dexp(x, rate=t_fit$estimate[1], log=FALSE),
                        lognormal = dlnorm(x, t_fit$estimate[1], t_fit$estimate[2]))
        
        g <- ggplot(data = t_df) +
            geom_histogram(aes(x=x, y=..density..)) +
            geom_line(aes(x=x, y=t_den), col="blue") +
            theme_bw()
    },
    error=function(e) { message(e); return() })
    g # implicit return
}

noise <- runif(1000,0,1) # to make it a bit harder for the fitting estimations...
my_normvec <- rnorm(1000, mean=3, sd=2) + noise # self describing
my_expvec <- rexp(1000, rate=2) + noise/10 # Generating 1000 numbers of exponential with lambda 2
my_lognormvec <- rlnorm(100, meanlog=1, sdlog=2) + noise*5

vis_compare_hist(my_normvec, "normal") # Good apparent fit
vis_compare_hist(my_normvec, "exponential") # must have positive values only! That's why we use tryCatch.
vis_compare_hist(my_expvec, "exponential") # Good apparent fit
vis_compare_hist(my_expvec, "normal") # clearly not matching...
vis_compare_hist(my_lognormvec, "lognormal") # ...

# Future use: One can generate random samples from a fitted distribution, to feed a simulation for instance...:
t_fit <- fitdistr(my_normvec, "normal") # fitting a distribution to some data
t_fit$estimate # looking at the estimated parameters for the fitted function
sample_fit <- rnorm(t_fit$n, t_fit$estimate) # generating n values with the "fitted" distribution parameters
