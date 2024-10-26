## Some Clustering with DBScan
## "Density-based Spatial Clustering of Applications with Noise"

library(dbscan) # Clustering (distances, i.e. "sub-symbolic" stuff)
library(rgl) # For 3D Visus & Fun

## Inspired (HEAVILY & A LOT) by dbscan package documentation!
n <- 100
x <- cbind(
        x = runif(10, 0, 10) + rnorm(n, sd = 0.4),
        y = runif(10, 0, 10) + rnorm(n, sd = 0.4)
)

## Step 0:
plot(x)
kNNdistplot(x, minPts = 5)
abline(h=.85, col= "red", lty = 2) ## Noise at .7 for 4-NN distance
## Step 1: Clustering using "KNN"
res <- dbscan(x, eps = .85, minPts = 5)
plot(x, col = res$cluster)
hullplot(x, res)

## Step 2: What if new data?
newdata <- x[1:12,] + rnorm(10, 0, .3)
hullplot(x, res)
points(newdata, pch = 3, col = "red", lwd = 3)
text(newdata, pos = 1)
pred_label <- predict(res, newdata, data = x)
pred_label
points(newdata, col = pred_label +1L, cex = 2, lwd = 2)

## Step 3: Multi-dimensional data (More than 3 dimensions)
## Let's see how "ML" fares here...
data(iris) ## Nothing less typical in the World...
#iris <- as.matrix(iris[, 1:4])
pairs(iris[, 1:4])

## Visualize "Noise threshold":
kNNdistplot(iris[, 1:4], minPts = 5)
abline(h=.675, col= "red", lty = 2) ## Noise at .675 for 4-NN distance
## Although a bit of a judgement call...

pairs(iris[, 1:4], col = iris$Species) ## There are 3 classes indeed.

## Cluster using DBScan, using "noise threshold" above, and N+1 Dims:
res <- dbscan(iris[, 1:4], eps = .675, minPts = 5)
res
pairs(iris[, 1:4], col = res$cluster + 1L) ## ML yes, but not all right there...


## Supplementary: RGL
## With 3D - but just for the fun of it
## Inspired by: http://www.sthda.com/english/wiki/a-complete-guide-to-3d-visualization-device-system-in-r-r-software-and-data-visualization

n <- 80
xyz <- cbind(
    x = runif(8, 0, 10) + rnorm(n, sd = 0.4),
    y = runif(8, 0, 10) + rnorm(n, sd = 0.4),
    z = runif(8, 0, 10) + rnorm(n, sd = .2)
)
kNNdistplot(xyz, minPts = 4)
abline(h=.79, col= "red", lty = 2) ## Noise at .7 for 4-NN distance
res <- dbscan(xyz, eps = .79, minPts = 4)
res
plot3d(xyz, type = 's', size = 2, col=res$cluster+1L)
movie3d( spin3d(), duration = 10, fps = 40 )

