## This takes pseudo-code from Book:
## "Optimización - Algoritmos programados con Matlab", Ed. Marcombo/AlfaOmega
## ISBN 978-84-267-2411-3
## Authors: E. V. Cuevas Jiménez, J. V. Osuna Enciso, D. A. Oliva Navarro, M. A. Díaz Cortés

## Visualization of 3D functions was implemented & tested in R in past entries.
## Gradient Descent tested in past entries, too.

## ----
## Simulated Annealing
## ----

## Reference book section 1.7 (in Matlab), Pseudo-code Algorithm 1.3

multi_modal_harder_gradient <- function(x, y) {
    3*(1-x)^2*exp(-x^2-(y+1)^2)+10*(x/5-x^3-y^5)*exp(-x^2-y^2)-1/3*exp(-(x+1)^2-y^2)
}
x <- y <- seq(-3, 3, length=100)
z <- outer(x, y, multi_modal_harder_gradient) ## Apply function to all pair-wise values!

t_ini <- 1 ## Initial temperature high -> more exploration
t_fin <- 10^-10 ## End temperature low -> local optimization
my_Beta <- 0.98 ## Can be changed, and referenced as Eta in some cases
my_sigma <- 2.5 ## Normal center sd, so for optimization step, smaller with small T
n_iter <- 1500 ## Just a loop limit
k <- 0

## Initial point
x1 <- runif(1, min = min(x), max = max(x)) ## Random number in range
y1 <- runif(1, min = min(y), max = max(y)) ## Random number in range
x1_store <- y1_store <- c()

my_t <- t_ini

while(my_t > t_fin && k < n_iter) {
    x1_store <- c(x1_store, x1)
    y1_store <- c(y1_store, y1)
    z_old <- multi_modal_harder_gradient(x1, y1) ## Try to optimize same function
    
    valid <- 0
    while(valid == 0) {
        ## Random Lambda, but "tempered" with Annealing
        delta_x <- rnorm(1, 0, my_sigma * my_t)
        delta_y <- rnorm(1, 0, my_sigma * my_t)
        
        vxn <- x1 + delta_x
        vyn <- y1 + delta_y
        
        ## Check for validity:
        if(vxn >= min(x) & vxn <= max(x) & vyn >= min(y) & vyn <= max(y))
            valid <- 1
        else {
            print(paste(vxn, vyn, "not valid"))
            next
        }
        
        ## Not we check for validity
        delta_f <- multi_modal_harder_gradient(vxn, vyn) - z_old
        if(delta_f < 0) { ## New value is lower than before, direction local minima
            x1 <- vxn
            y1 <- vyn
            break
        }
        
        ## Otherwise, continue randomly if Pa = e^(delta_f/t) is chosen
        if(exp(delta_f/my_t) <= runif(1, min = 0, max = 1)) {
            ## Now this keeps OFTEN for HIGH T (excited system, high temperature)
            ## BUT rarely with LOW temperatures (so bad cases are rarely considered)
            x1 <- vxn
            y1 <- vyn
            break
        }
    }
    k <- k+1
    my_t <- my_Beta * my_t
}

## Plot steps
contour(x, y, z,
        nlevels = 30)
points(x1_store, y1_store, col="red")

pmat <- persp(x, y, z,
              theta = 130, phi = 30, ## Show some different perspective
              expand = 0.5,
              col = "lightblue",
              ticktype = "detailed")

my_points <- trans3d(x1_store, y1_store,
                     multi_modal_harder_gradient(x1_store, y1_store),
                     pmat=pmat)
points(my_points, pch = 16, col = "red")
