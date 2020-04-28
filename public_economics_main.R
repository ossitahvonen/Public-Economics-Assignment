#########this is the main file
rm(list=ls())

#install.packages("MASS")
#install.packages("tidyverse")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("stargazer")
#install.packages("areg")
install.packages("felm")

###1. A Ossi B Miia



#packages (install before!)
library(MASS)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(stargazer)
library(foreign)
#setting working directory to the right folder wrt github
goalwd <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(goalwd)


#load data (in same folder)
#data <- read.csv2("MonthlyPanel.csv", header = T)
data <- read.dta("MonthlyPanel.dta")
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
table
#saako standard deviationin jotenkin helposti mukaan taulukkoon?
#xtable(table)
#get standard deviations
SD <- cbind(apply(data2[data2$jewin==0,],2,sd),apply(data2[data2$jewin==1,],2,sd))[1:5,]
SD
######
#


#3d uses data after events
table(data$distanci)
#new institu3
institu3_new <- data$institu3-data$institu1
names <- colnames(data)
names <- append(c(names), c("institu3_neww"))
data <- cbind(data,institu3_new)
colnames(data) <- names
table(data$institu1,data$institu3)
table(data$institu1,data$institu3_new)
#add dummy for 2 blocks to data 
#ie. month fixed effect
names <- colnames(data)
names <- append(c(names), c("twoblock"))
data <- cbind(data, as.numeric(data$distanci==2))
colnames(data) <- names

#create the post variable
post <- as.numeric(data$mes>7 & data$mes<50)
data <- cbind(data,post)
colnames(data) <- c(colnames(data)[1:15],"postt")

#######################################
###block fixed effect should be done similarly
#######################################
#all variables created







#months before the dramatic events when everyone was happy 
data_pre <- data[data$mes<=7,]
#and after
data_post <- data[data$mes>7 & data$mes<50,]


#3d
#this currently produces wrong results
#not even close
apply(data,2,length)
as.factor(data$mes)

model1 <- lm(data = data_post, formula = totrob ~ institu1 + institu3_neww + twoblock + I(mes) - 1)
summary(model1)
#we need Hubert-White SE:s also


#3e
#new data with only dist<=2 blocks
data_close <- data_post[data_post$distanci<=2,]
#create new totrob with month days..
#[months 5,8,10,12] * 30/31
#[7] * 30/17

#new dataset
totrob2 <- cbind(data$totrob,data$mes)
totrob2 <- as.data.frame(totrob2)
colnames(totrob2) <- c("a","b")
#choose and replace
totrob2[totrob2$b==5|totrob2$b==8|totrob2$b==10|totrob2$b==12,]
totrob2[totrob2$b==5|totrob2$b==8|totrob2$b==10|totrob2$b==12,][,1] <- totrob2[totrob2$b==5|totrob2$b==8|totrob2$b==10|totrob2$b==12,][,1]*30/31
totrob2[totrob2$b==5,]
totrob2[totrob2$b==7,][,1] <- totrob2[totrob2$b==7,][,1]*30/17
#add the nw totrob to data
data$totrob_2 <- totrob2$a
data[data$mes==7,]

data_close <- data[data$distanci<3&data$mes < 50,]
dim(data_close)


model2 <- lm(data = data_close, totrob_2 ~ institu1:postt + institu3_neww:postt + twoblock:postt + I(observ) )
summary(model2)

#3A
model3a <- lm(data = data[data$mes < 50,], totrob ~ institu1 + postt + postt:institu1 + I(mes))
summary(model3a)
