ex_ls = list(1, 'some text', TRUE,)
library(pacman)
p_load(ISLR, ggplot2)
library(ISLR)
summary(Auto)
plot(Auto$mpg, Auto$acceleration)

plot(
  Auto$mpg, 
  Auto$acceleration, 
  xlab = "Miles per Gallon", # x label
  ylab = "Acceleration", # y label
  main = "A Scatterplot", # title
  col = "red", # sets color
  pch = 20 # dot shape 
)