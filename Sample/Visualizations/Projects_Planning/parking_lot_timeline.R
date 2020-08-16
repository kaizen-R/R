# File: parking_lot_timeline.R
# Created: 2020/08/16
# This is an exercise towards visualization of advancement of several projects on one 
# page.
# In a way, it mixes the parking lot diagrams and Gantt charts (for milestones only).
# This was probably invented already, but I didn't find it, so here goes...

library(data.table)
library(lubridate)
library(ggplot2)
#install.packages("ggExtra") # Was missing that one...
library(ggExtra)

# Demo Data ----
# Let's create sample data to demo the objectives:
# A list of projects to report progress upon: We create five projects
projects <- sort(rep(paste0("project-", seq(1:5)), 4))
# All project must have an entry for each phase, although it needs not start.
phases <- rep(c("Acquisition", "Deployment", "Migration", "Testing"), 5)
# Status of task/phase: 
# 0: Not started, 
# 0 < x < 1: ongoing, 
# 1: finished,
# -1: Does not apply
status <- c(c(1.0, 1.0, 1.0, 0.5),
            c(1.0, 0.5, 0, 0),
            c(1, -1.0, 0, 0),
            c(0.5, 0, 0, 0),
            c(0, 0, 0, 0))
duedates <- c(c("20200501", "20200601", "20200615", "20200630"), # Past date, that will show red if not completed by specific date.
              c("20200715", "20200830", "20200930", "20201031"),
              rep("20201115", 4),
              rep("20201130", 4),
              rep("2020-12-30", 4)) # Lubridate will be managing different compatible formats on its own.


# Playing with data table instead of dataframes ----

# Let's play with Data.table instead of Dataframes, as an exercise:
demo_dt <- data.table(project = projects, phase = phases, status = status, duedate = duedates)

# This is quite different from data.frame: Replace types on one column:
demo_dt[, phases := as.factor(demo_dt[, phases])]
demo_dt[, duedate := ymd(duedate)]

# We will color-code the status
set_color <- function(status) {
  if(status == 0) return("grey")
  if(status > 0 && status < 1) return("lightgreen")
  if(status == 1) return("green")
  return("white")
}
demo_dt[, status_color := sapply(status, set_color)]
# Anything not finished and past due date is red to us:
demo_dt[(status < 1) & (duedate < Sys.Date()), status_color := "red"]

# Check resulting data table:
demo_dt

# Let's get to the visualization: ----
demo_plot <- ggplot(demo_dt, aes(duedate, project, fill = status_color))
demo_plot <- demo_plot + geom_tile()
demo_plot <- demo_plot + scale_fill_manual(breaks = demo_dt$status_color, values = demo_dt$status_color)
demo_plot <- demo_plot + facet_grid(phase~.)
demo_plot <- demo_plot + theme_minimal(base_size = 8) + removeGrid() + theme(legend.position = "none")
demo_plot <- demo_plot + geom_vline(xintercept = Sys.Date(), colour = "red")
demo_plot
