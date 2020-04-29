#########this is the main file
rm(list=ls())

#install.packages("MASS")
#install.packages("tidyverse")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("stargazer")
#install.packages("areg")

###1. A Ossi B Miia



#packages (install before!)
library(MASS)
#tibble etc
library(tidyverse)
library(dplyr)
#plots
library(ggplot2)
library(stargazer)
#statadata
library(foreign)
#robust SE:s
library(lmtest)
library(sandwich)

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
#(hieman hankala tapa tehdÃ¤)
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
data$institu3_neww <- data$institu3-data$institu1

table(data$institu1,data$institu3)
table(data$institu1,data$institu3_neww)
#add dummy for 2 blocks to data 
#ie. month fixed effect
data$twoblock <- ifelse(data$distanci==2,1,0)

#create the post variable
data$postt <- ifelse(data$mes>7 & data$mes<70,1,0)

#######################################
###block fixed effect should be done similarly
#######################################
#all variables created







#months before the dramatic events when everyone was happy 
data_pre <- data[data$mes<=7,]
#and after
data_post <- data[data$mes>7 & data$mes<50,]


#3d

#Näillä samat tulokset kuin esimerkissä ja robust errors 
## oneblock pyöristyy 0.011 ja paperissa 0.010, mutta en saanut millään eri tulosta 
### ja Aliisallakin noin
model1 <- lm(data = data_post, formula = totrob ~ institu1 + institu3_neww + twoblock + as.factor(mes) )
summary(model1)
coeftest(model1, vcov= vcovHC(model1, "HC1"))

#3e
#new data with only dist<=2 blocks
data_close <- data_post[data_post$distanci<=2,]

#create new totrob that takes month lengths into account
#[months 5,8,10,12] * 30/31 & [7] * 30/17

#Tällä pitäisi tulla samat kuin sun pitkässä versiossa
## jätin kuitenkin pitkän talteen just in case 

data$totrobc <- ifelse(data$mes==7, data$totrob*(30/17), data$totrob)
data$totrobc2 <- ifelse(data$mes==5|data$mes==8|data$mes==10|data$mes==12,
                        data$totrob*(30/31), data$totrobc)

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

#3e regression
model2 <- lm(data = data_close, totrob_2 ~ institu1:postt + institu3_neww:postt + twoblock:postt + I(observ))
summary(model2)

#tÃ¤mÃ¤ ei toimi
#muita kokeiluja:
lm(data = data_close, totrob_2 ~ institu1:postt + institu3_neww:postt + twoblock:postt)
###
#tÃ¤mÃ¤ nÃ¤yttÃ¤isi olevan oikein!!
model3e <- lm(data = data_close, totrob_2 ~ institu1 + institu1:postt + institu3_neww + institu3_neww:postt + twoblock:postt)
summary(model3e)

#3A
model3a <- lm(data = data[data$mes < 50,], totrob ~ institu1 + postt + postt:institu1 + I(mes))
summary(model3a)

#3B
model3b <- lm(data = data[data$mes < 50,], totrob ~ institu1 + institu3_neww + postt + postt:institu1 + postt:institu3_neww + I(mes))
summary(model3b)
#oikein

#3C
model3c <- lm(data = data[data$mes < 50,], totrob ~ institu1 + institu3_neww + twoblock + postt + postt:institu1 + postt:institu3_neww + postt:twoblock + I(mes))
summary(model3c)
#oikein!

#SE:t vielÃ¤ vÃ¤Ã¤rin




