#########this is the main file
rm(list=ls())

#install.packages("MASS")
#install.packages("tidyverse")
#install.packages("dplyr")
#install.packages("ggplot2")



###1. A Ossi B Miia



#packages (install before!)
library(MASS)
library(tidyverse)
library(dplyr)
library(ggplot2)
#setting working directory to the right folder wrt github
goalwd <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(goalwd)


#load data (in same folder)
data <- read.csv2("MonthlyPanel.csv", header = T)
View(data)
########################
#2. Open data and provide summary statistics similar to those in table 1.

#institu1 = dummy for jewish institution
#institu3 = dummy for jewish institution 1 block away

#new dummy that has 0 if no jewish institution in same or neighbouring block
jewin <- as.numeric(data$institu1|data$institu3)
jewin
