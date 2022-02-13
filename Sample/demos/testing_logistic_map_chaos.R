# After reading on Chaos and Logistic Maps...
#library(mosaicCore)
library(plyr)
# library(ggplot2)
# library(ggpubr)
# library(shiny)
library(tidyverse)
# Logistic map definition is: x_1 = rx_0 * (1-x_0)

# Let's start with a closure:
my_logistic_map <- function(my_r) {
  f1 <- function(x) {
    my_r * x * (1-x)
  }
}

# And some tests
my_lmap05 <- my_logistic_map(0.5)
my_xplus1 <- my_lmap05(0.5) # we expect: 0.5*0.5*0.5
my_xplus1 <- my_lmap05(my_xplus1) # and then feed each step...

# Prepare a few example with different growth ratios
my_lmap1<- my_logistic_map(1.0)
my_lmap15 <- my_logistic_map(1.5)
my_lmap2 <- my_logistic_map(2)
my_lmap25 <- my_logistic_map(2.5)
my_lmap3 <- my_logistic_map(3)
my_lmap35 <- my_logistic_map(3.5)

# How to go about it now?
res_df <- data.frame(loop_n = 0, 
                     xn05 = 0.5,
                     xn1 = 0.5,
                     xn15 = 0.5,
                     xn2 = 0.5,
                     xn25 = 0.5,
                     xn3 = 0.5,
                     xn35 = 0.5)

for(i in 2:20) {
  res_df[i,] <- c(i-1, 
                  my_lmap05(res_df[i-1, "xn05"]),
                  my_lmap1(res_df[i-1, "xn1"]),
                  my_lmap15(res_df[i-1, "xn15"]),
                  my_lmap2(res_df[i-1, "xn2"]),
                  my_lmap25(res_df[i-1, "xn25"]),
                  my_lmap3(res_df[i-1, "xn3"]),
                  my_lmap35(res_df[i-1, "xn35"])) 
}

# Prepare the data for visualization:
res_df %>% pivot_longer(cols = contains("xn"),
                        names_to = "growth_rates",
                        values_to = "vals") -> for_plot

ggplot(for_plot, aes(x = loop_n, y = vals, colour = growth_rates)) +
  geom_line() +
  ggtitle("Testing different growth rates for the logistic map")+
  xlab("generation") +
  ylab("Population") +
  labs(colour = "Growth Rate")

# OK but now that's not at all practical an approach to test for more values, so:
my_logistic_map2 <- function(my_r, x) {
  my_r * x * (1-x)
}

my_step <- 0.001 # for changing growth rates
my_growth_rates <- seq(0, 4, by=my_step)
n_gens <- 1000

# Let's initialize a matrix with that
res_mat <- matrix(0, nrow=length(my_growth_rates), ncol = n_gens, byrow = TRUE)
res_mat[,1] <- 0.5 # Initial population in all cases
rownames(res_mat) <- my_growth_rates

for(my_r in 1:length(my_growth_rates)) { # I'm not a fan of for loops, but here...
  for(my_gen in 2:n_gens) {
    res_mat[my_r, my_gen] <- my_logistic_map2(my_growth_rates[my_r], res_mat[my_r, my_gen-1])
  }
}

res_mat <- res_mat[, 100:ncol(res_mat)] # Let's see what happens after 100 generations
res_df <- as.data.frame(res_mat)
res_df$x <- rownames(res_df)
res_df %>% pivot_longer(cols = contains("V"),
                 names_to = "gen",
                 values_to = "vals") -> for_plot

ggplot(for_plot, aes(x = as.numeric(x), y=vals)) +
  geom_point(size = 0.01, colour = "blue", alpha = 0.5) +
  ggtitle("Bifurcation Diagram")+
  xlab("Growth Rate") +
  ylab("Population") +
  scale_x_continuous(breaks = seq(0, 4, 0.1))

# Focus by regions of data: For certain specific growth ratios:
ggplot(for_plot[for_plot$x > 0.8 & for_plot$x < 3.1, ],
       aes(x = as.numeric(x), y=vals)) +
  geom_point(size = 0.01, colour = "blue", alpha = 0.5) +
  ggtitle("Bifurcation Diagram")+
  xlab("Growth Rate") +
  ylab("Population") +
  scale_x_continuous(breaks = seq(0.8, 3.1, 0.1))

ggplot(for_plot[for_plot$x > 2.8, ], aes(x = as.numeric(x), y=vals)) +
  geom_point(size = 0.01, colour = "blue", alpha = 0.5) +
  ggtitle("Bifurcation Diagram")+
  xlab("Growth Rate") +
  ylab("Population")+
  scale_x_continuous(breaks = seq(2.8, 4, 0.1))

# ***
# comparing seemingly chaotic to random generators
# Linear congruential generator
random_numbers_vector <- c()
X0 <- 12 # seed
a <- 36
c <- 2
m <- 3583 # also, this one is prime
max_iterations <- 200
Xi_plus_1 <- function(x, current_iter = 0, max_iter = max_iterations) {
  random_numbers_vector[current_iter+1] <<- x
  if(current_iter == max_iter) return(x)
  Xi_plus_1( (a * x + c) %% m, current_iter+1, max_iter)
}
random_numbers_vector <- rep(0, max_iterations)
Xi_plus_1(X0)
# scale to have values between 0 and 1
random_numbers_vector <- random_numbers_vector/max(random_numbers_vector)

# Recursive version of our Logistic Map function
logistic_numbers_vector <- rep(0.5, max_iterations)
my_logistic_map3 <- function(my_r, x, current_iter = 0, max_iter = max_iterations) {
  logistic_numbers_vector[current_iter+1] <<- x
  if(current_iter == max_iter) return(x)
  my_logistic_map3(my_r, my_r * x * (1-x), current_iter+1, max_iter)
}
my_logistic_map3(3.999, 0.5)

comparator_df <- data.frame(generation = 0:200, 
                            logistic_map_num = logistic_numbers_vector, 
                            random_num = random_numbers_vector)
comparator_df %>% pivot_longer(cols = contains("num"),
                        names_to = "generator",
                        values_to = "vals") -> for_plot
ggplot(for_plot[for_plot$generation %in% 30:50,], aes(x = generation, y = vals, colour = generator)) +
  geom_line()

comparator_df$logistic_map_num1 <- c(comparator_df$logistic_map_num[2:nrow(comparator_df)],0)
comparator_df$random_num1 <- c(comparator_df$random_num[2:nrow(comparator_df)],0)
# Finally, the beauty!
ggplot(comparator_df) +
  geom_point(aes(x = logistic_map_num, y = logistic_map_num1, colour = "logistic_map")) +
  geom_point(aes(x = random_num, y = random_num1, colour = "pseudo_random")) + 
  scale_color_manual(values = c("logistic_map" = "blue", "pseudo_random" = "red"))+
  ggtitle("Comparing Logistic Map and Pseudo-Random Generator")+
  xlab("'Generation N'") +
  ylab("'Generation N+1'")
