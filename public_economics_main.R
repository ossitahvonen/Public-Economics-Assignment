#########this is the main file

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

########################
#2. Open data and provide summary statistics similar to those in table 1.

