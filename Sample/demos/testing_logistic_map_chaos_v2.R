library(microbenchmark)
library(tidyverse)
library(testthat)

my_logistic_map2 <- function(my_r, x) {
  my_r * x * (1-x)
}

my_step <- 0.001 # for changing growth rates
my_growth_rates <- seq(0, 4, by=my_step)
n_gens <- 1000

v1 <- function() {
  
  
  # Let's initialize a matrix with that
  res_mat <- matrix(0, nrow=length(my_growth_rates), ncol = n_gens, byrow = TRUE)
  res_mat[,1] <- 0.5 # Initial population in all cases
  rownames(res_mat) <- my_growth_rates
  
  for(my_r in 1:length(my_growth_rates)) { # I'm not a fan of for loops, but here...
    for(my_gen in 2:n_gens) {
      res_mat[my_r, my_gen] <- my_logistic_map2(my_growth_rates[my_r], res_mat[my_r, my_gen-1])
    }
  }
  res_mat
}

v2 <- function() {
  # my_step <- 0.001 # for changing growth rates
  # my_growth_rates <- seq(0, 4, by=my_step)
  # n_gens <- 1000
  
  # Let's initialize a matrix with that
  res_mat <- matrix(0, nrow=length(my_growth_rates), ncol = n_gens, byrow = TRUE)
  #res_mat[,1] <- 0.5 # Initial population in all cases
  rownames(res_mat) <- my_growth_rates
  
  for(my_r in 1:(length(my_growth_rates)-1)) { # I'm not a fan of for loops, but here...
    res_mat[my_r, ] <- Reduce(function(x, y) my_growth_rates[my_r] * x * (1-x), 
                              x = 1:(n_gens-1), 
                              init = 0.5, 
                              accumulate = TRUE)
  }
  res_mat
}
microbenchmark(v1(),
               v2(),
               times = 5L)

temp1 <- v1()
temp2 <- v2()

test_that(expect_equal(temp1[1,], temp2[1,]))
