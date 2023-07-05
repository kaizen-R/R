## This takes pseudo-code from Book:
## "Optimización - Algoritmos programados con Matlab", Ed. Marcombo/AlfaOmega
## ISBN 978-84-267-2411-3
## Authors: E. V. Cuevas Jiménez, J. V. Osuna Enciso, D. A. Oliva Navarro, M. A. Díaz Cortés

## Visualization of 3D functions was implemented & tested in R in past entries.

## With book example 1.1:
mono_modal_easy_gradient <- function(x, y) {
    10-exp(-(x^2+3*y^2))
} ## The above function is twice "derivable"
x <- y <- seq(-1, 1, length=50)
z <- outer(x, y, mono_modal_easy_gradient) ## Apply function to all pair-wise values!

filled.contour(x, y, z,
               plot.axes = {
                   axis(1)
                   axis(2)
                   contour(x, y, z, add = TRUE,
                           lwd=2,
                           nlevels = 30)
               })

## ----
## Gradient Descent
## ----

## gradient step for perturbation
hstep <- 0.001

## Optimization step
my_alpha <- 0.05

## Prep iterations
k <- 0
n_iter <- 100

x1 <- runif(1, min = min(x), max = max(x)) ## Random number in range
y1 <- runif(1, min = min(y), max = max(y)) ## Random number in range
x1_store <- y1_store <- c()

while(k < n_iter) {
    x1_store <- c(x1_store, x1)
    y1_store <- c(y1_store, y1)
    
    zn <- mono_modal_easy_gradient(x1, y1)
    
    ## Calculate gradients for all variables
    vxn <- x1 + hstep
    vyn <- y1 + hstep
    gx1 <-  (mono_modal_easy_gradient(vxn, y1) - zn) / hstep
    gy1 <-  (mono_modal_easy_gradient(x1, vyn) - zn) / hstep
    
    ## Calculate new point
    x1 <- x1 - my_alpha*gx1
    y1 <- y1 - my_alpha*gy1
    
    k <- k+1
}

## Plot steps
contour(x, y, z,
        nlevels = 30)
points(x1_store, y1_store, col="red")

#dev.off()
pmat <- persp(x, y, z,
              theta = 30, phi = 30, ## Show some different perspective
              expand = 0.5,
              col = "lightblue",
              ticktype = "detailed")

my_points <- trans3d(x1_store, y1_store,
                     mono_modal_easy_gradient(x1_store, y1_store),
                     pmat=pmat)
points(my_points, pch = 16, col = "red")
