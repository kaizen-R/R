## 3D Visualization of a simple function

## A sample function for visualizing
cone <- function(x, y)  {
    sqrt(x^2 + y^2)
}

x <- y <- seq(-1, 1, length=50) ## X & Y
## Apply function to all pair-wise values!
z <- outer(x, y, cone) ## Evaluate "cone" for all pairs of X & Y

## Learning to visualize Contours
contour(x, y, z,
        nlevels = 30) ## Simplest version

## See: https://www.rdocumentation.org/packages/graphics/versions/3.6.2/topics/filled.contour
filled.contour(x, y, z,
               plot.axes = {
                   axis(1)
                   axis(2)
                   contour(x, y, z, add = TRUE,
                           lwd=2,
                           nlevels = 30)
               }) ## Can be made more elaborate
filled.contour(x, y, z,                
               nlevels = 30,
               color.palette = terrain.colors) ## Or colourful.

par(mfrow=c(1, 1))
## Working on 3D functions visualizations:)
## I needed to lookup these:
## https://en.wikipedia.org/wiki/Azimuth <- theta
## https://en.wikipedia.org/wiki/Colatitude <- phi
persp(x, y, z,
      theta = 0, phi = 90, ## Bad Perspective
      expand = 0.5)
persp(x, y, z,
      theta = 30, phi = 30, ## Show some different perspective
      expand = 0.5,
      col = "lightgreen",
      ticktype = "detailed") ## A bit better
