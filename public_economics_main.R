#########this is the main file
rm(list=ls())

#install.packages("MASS")
#install.packages("tidyverse")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("stargazer")



###1. A Ossi B Miia



#packages (install before!)
library(MASS)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(stargazer)
#setting working directory to the right folder wrt github
goalwd <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(goalwd)


#load data (in same folder)
data <- read.csv2("MonthlyPanel.csv", header = T)
#View(data)
########################
#2. Open data and provide summary statistics similar to those in table 1.

#institu1 = dummy for jewish institution
#institu3 = dummy for jewish institution 1 block away

#new dummy that has 0 if no jewish institution in same or neighbouring block
jewin <- as.numeric(data$institu1|data$institu3)
data <- cbind(data,jewin)
#table
#(hieman hankala tapa tehdä)
data2 <- as.tibble(data)
data2 <- data %>% dplyr::select(distanci, edpub, estserv, banco,totrob, jewin)
table <- cbind(apply(data2[data2$jewin==0,],2,mean),apply(data2[data2$jewin==1,],2,mean))[1:5,]
table <- cbind(table,table[,1]-table[,2])
colnames(table) <- c("Census tracts without Jewish institutions", "Census tracts with Jewish institutions", "Difference")

#saako standard deviationin jotenkin helposti mukaan taulukkoon?
#xtable(table)
#get standard deviations
SD <- cbind(apply(data2[data2$jewin==0,],2,sd),apply(data2[data2$jewin==1,],2,sd))[1:5,]

######
#


#3d uses data after events
table(data$distanci)
#add dummy for 2 blocks to data 
#ie. month fixed effect
names <- colnames(data)
names <- append(c(names), c("twoblock"))
names
data <- cbind(data, as.numeric(data$distanci==2))
colnames(data) <- names

###block fixed effect should be done similarly

#months before the dramatic events when everyone was happy 
data_pre <- data[data$mes<=12,]
#and after
data_post <- data[data$mes>12,]
#3d
#this currently produces wrong results
#not even close
lm(data = data_post, formula = totrob ~ institu1 + institu3 + twoblock + as.factor(mes))
#we need Hubert-White SE:s




