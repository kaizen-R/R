# Playing with new visualizations...
library(dplyr)
library(ggplot2)
library(waffle) # New arrow for my quiver
library(emojifont) # New arrow for my quiver

# Testing waffle package from Bob Rudis
# Much inspired from his demos at: https://github.com/hrbrmstr/waffle

parts <- c(80, 30, 18, 10)
waffle(parts, rows = 10)
waffle(parts, rows = 5, flip = TRUE)

parts <- data.frame(
  names = factor(LETTERS[1:4]), # Must be factor! Otherwise doesn't seem to work
  vals = c(80L, 30L, 20L, 10L)
)
waffle(parts, rows = 8)

waffle(c(working = 10, relaxing = 14), rows = 3)
iron(
  waffle(c(waking_up = 2, working = 10, relaxing = 5, sleeping = 7), rows = 3,
         title = "Some Day Grouped in Hours"),
  waffle(c(email = 3, meetings = 4, actual_productivity = 3), rows = 2,
         title="Typical Workday")
)

emoji(search_emoji("sun"))
x <- seq(0, 2*pi, length=30)
y <- sin(x)
ggplot() +
  theme_void() +
  geom_emoji(c(rep("sunny", 15), rep("crescent_moon", 15)), x=x, y=y, size=10) +
  geom_hline(yintercept = 0, colour="lightgrey")
