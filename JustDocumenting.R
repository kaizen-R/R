# install package does NOT make it available directly
install.packages(c("slidify", "ggplot2", "devtools", "KernSmooth"))

# To make it available, you need to load it, using library, NO quotes needed...
library(slidify)
# somehow slidify was not available for R v3.1.2...

# check if a package is available
find.package("slidify")
# Will return Error if does not exist...
# So you need to get it from somewhere else, i.e. not CRAN:
devtools::install_github(c('ramnathv/slidifyLibraries', 'ramnathv/slidify'))
# Thanks to https://github.com/ramnathv
