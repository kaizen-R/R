## 3D - Genetic_Algorithms - Binary - Simple - V001

## Most functions moved, for clarity: <CAREFUL, relative paths... Easier in an RStudio Project>
source("3d_optim_genetic_algo_bin_funs_v001.R")

## For evaluation of Function, we work with Real, but values are binary encoded
bits_per_individual <- 11

## Search space, e.g. min & max values for params, here (x,y), square:
max_search_space <- 30
min_search_space <- -15

## Easier to call later on, using "Closure":
bin2real_1d <- bin2real_1d_params(min_search_space, max_search_space, bits_per_individual)

pop_init_individuals <- 100

## Generate Space: x, y, z GLOBAL Variables
x <- y <- seq(min_search_space, max_search_space, length=100)
z <- outer(x, y, sample_2d_decision_fun) ## Apply function to all pair-wise values!
contour(x, y, z,
        nlevels = 30)
draw_space()

## Now we can visualize populations (1 generation at a time):
my_individuals <- generate_pop(pop_init_individuals, bits_per_individual)
draw_pop(my_individuals)

## A different population will look different...
my_individuals <- generate_pop(pop_init_individuals, bits_per_individual)
draw_pop(my_individuals)

