#this is the main file

#packages
library(MASS)
#setting working directory
goalwd <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(goalwd)
#load data
data <- read.csv2("MonthlyPanel.csv", header = T)
View(data)
