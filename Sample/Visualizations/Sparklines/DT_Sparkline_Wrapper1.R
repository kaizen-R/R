# Good stuff inspired originally by: https://leonawicz.github.io/HtmlWidgetExamples/ex_dt_sparkline.html

library(dplyr)
library(tidyr)

library(DT)
library(sparkline)

# DT Sparkline wrapper function:
DT_sparklines <- function(mydf, myvar, mygroup = NULL, myfilter = NULL) {
  ## Sparklines for DT
  js <- "function(data, type, full){ return '<span class=spark>' + data + '</span>' }"

  # targets for colDefs1 is the target to which we will apply the sparkline. In this case, last column.
  colDefs1 <- list(list(targets = if(!is.null(mygroup)) length(mygroup) else 0, 
                        render = JS(js)))

  colnames(mydf)[colnames(mydf) == myvar] <- "dtspark_n"
  r <- range(mydf[,"dtspark_n"])
  x <- "function (oSettings, json) { $('.spark:not(:has(canvas))').sparkline('html', { "
  
  line_string <- "type: 'line', lineColor: 'black', fillColor: '#ccc', highlightLineColor: 'orange', highlightSpotColor: 'orange'"
  cb_line <- DT::JS(paste0(x, line_string, ", chartRangeMin: ", r[1], 
                       ", chartRangeMax: ", r[2], " }); }"),
                collapse = "")
  
  # Optional Grouping (though better with...):
  if(!is.null(mygroup)) {
    dat_t <- mydf %>% group_by_at(mygroup) %>% summarise(dtspark_n = paste(dtspark_n, collapse = ","))
  } else {
    dat_t <- mydf %>% summarise(dtspark_n = paste(dtspark_n, collapse = ","))
  }
  
  # Restore correct variable name:
  colnames(dat_t)[colnames(dat_t) == "dtspark_n"] <- myvar

  d1 <- datatable(dat_t, rownames = FALSE, 
                  options = list(columnDefs = colDefs1, 
                                 fnDrawCallback = cb_line))
  d1$dependencies <- append(d1$dependencies, htmlwidgets:::getDependency("sparkline"))
  d1
}

# ----
# DT Sparkline demo:

dat <- read.csv("temperatures_invented_sample.csv")
datatable(dat, rownames = FALSE)

## Example usage:
# You need one dataframe with at least one variable that is actually numerical:
DT_sparklines(dat, "temp")
# Should you need to filter you data, do it before calling the function...:
DT_sparklines(filter(dat, decade == "2010s"), "temp")
# Now without groups, your table will show up with only one entry...
# Add a grouping variable:
DT_sparklines(filter(dat, decade == "2010s"), "temp", "month")
# You could use multiple columns of your dataframe as grouping variables, like so:
DT_sparklines(dat, "temp", c("decade","month"))
# ----
