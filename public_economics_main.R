#########this is the main file
rm(list=ls())

#install.packages("MASS")
#install.packages("tidyverse")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("stargazer")
#install.packages("areg")
#install.packages("lfe")

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
#fixed effects
library(lfe)

#setting working directory to the right folder wrt github
goalwd <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(goalwd)


#load data (in same folder)
data <- read.csv2("MonthlyPanel.csv", header = T)
#data <- read.dta("MonthlyPanel.dta")
data==data2
View(data2)
data2[data2$totrob<1&data$totrob>0,]

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

t.test(data2[data2$jewin==0,]$edpub,data2[data2$jewin==1,]$edpub)
t.test(data2[data2$jewin==0,]$banco,data2[data2$jewin==1,]$banco)


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

#N?ill? samat tulokset kuin esimerkiss? ja robust errors 
## oneblock py?ristyy 0.011 ja paperissa 0.010, mutta en saanut mill??n eri tulosta 
### ja Aliisallakin noin
model1 <- lm(data = data_post, formula = totrob ~ institu1 + institu3_neww + twoblock + as.factor(mes) )
summary(model1)
coeftest(model1, vcov= vcovHC(model1, "HC1"))

#3e
#new data with only dist<=2 blocks
data_close <- data_post[data_post$distanci<=2,]

#create new totrob that takes month lengths into account
#[months 5,8,10,12] * 30/31 & [7] * 30/17

#T?ll? pit?isi tulla samat kuin sun pitk?ss? versiossa
## j?tin kuitenkin pitk?n talteen just in case 

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

#tämä näyttäisi olevan oikein!!
model3e <- lm(data = data_close, totrob_2 ~ institu1 + institu1:postt + institu3_neww + institu3_neww:postt + twoblock:postt)
summary(model3e)

#N?ill? pit?isi saada clusterit lm modeliin, mutta ei ainakaan mulla toimi
cluster.vcov(model3e,data$observ)


vcov1 <- vcovCL(model3e,cluster=data_close$observ)

#t?ss? mun best effort SE:n kanssa
m3e <- felm(data = data_close, totrob_2 ~ institu1:postt + 
institu3_neww:postt +  twoblock:postt | observ | 0 | observ, cmethod ="cgm2")
summary(m3e)
m3e$cse

coeftest(model3e, vcov. = vcov1)


# institu1:postt postt:institu3_neww      postt:twoblock 
# 0.02341930          0.01463890          0.01073673 

#pit?isi olla same block: (0.024293) 
## one-block: (0.014186), two-block:(0.0108578)
#eli l?hell?, mutta niin kaukana

#3A
model3a <- lm(data = data[data$mes < 50,], totrob ~ institu1 + postt + postt:institu1 + I(mes))
summary(model3a)

data5 <- data[data$mes < 50,]
m3a <- felm(data=data5, totrob ~ postt:institu1 | mes + observ)
summary(m3a, robust = T)
#oikein ja SE sama kuin paperissa
#3B
model3b <- lm(data = data[data$mes < 50,], totrob ~ institu1 + institu3_neww + postt + postt:institu1 + postt:institu3_neww + I(mes))
summary(model3b)
#oikein

m3b <- felm(data=data5, totrob ~ postt:institu1 + postt:institu3_neww | mes + observ)
summary(m3b, robust = T)
#py?ristyksiss? v?h?n ongelmaa, oneblock ja SE vikat desimaali ei t?sm?? paperiin, mutta l?hell?

#3C
model3c <- lm(data = data[data$mes < 50,], totrob ~ institu1 + institu3_neww + twoblock + postt + postt:institu1 + postt:institu3_neww + postt:twoblock + I(mes))
summary(model3c)
#oikein!

m3c <- felm(data=data5, totrob ~ postt:institu1 + postt:institu3_neww + postt:twoblock | mes + observ)
summary(m3c, robust = T)
#oneblock ja twoblock coeff. ja sameblock ja oneblock SE vikat desimaalit py?ristyy v??rin

vcovCL(model3c,cluster=data$observ[data$mes <50])

datamean_treat <- data[data$institu1==1,] %>% as.tibble() %>%
  group_by(mes) %>%
  summarise_at(vars(totrob),
               list(name2 = mean, sdev2 = sd))

datamean_contr <- data[data$institu1==0,] %>% as.tibble() %>%
  group_by(mes) %>%
  summarise_at(vars(totrob),
               list(name2 = mean, sdev2 = sd))
datamean_treat

datamean <- cbind(as.data.frame(datamean_contr),as.data.frame(datamean_treat[,2:3]))[1:9,]
datamean <-  cbind(datamean, datamean_contr[1:9,2] - datamean_treat[1:9,2])
#the correct values for the standard errors are sd/sqrt(n)

n_contr <- nrow(data[data$institu1==0,])
n_treat <- nrow(data[data$institu1==1,])

colnames(datamean) <- c("mes","name2", "sdev2", "name", "sdev", "dif")


colors1 <- c("Treatment Group" = "red", "Control Group" = "blue")

p1 <- ggplot(data = datamean, aes(x=mes)) +
  geom_point(aes(x=mes, y = name, color = "Treatment Group")) + geom_line(aes(x=mes, y = name, color = "Treatment Group")) +
  geom_point(aes(x = mes, y = name2, colour = "Control Group")) + 
  geom_line(data = datamean, aes(x = mes, y = name2, color = "Control Group")) +
  geom_errorbar(data = datamean, aes(ymin = name - 1.96*sdev/sqrt(n_treat), ymax = name + 1.96*sdev/sqrt(n_treat)), width=.2, position=position_dodge(.9), colour = "red") +
  geom_errorbar(data = datamean, aes(ymin = name2 - 1.96*sdev2/sqrt(n_contr), ymax = name2 + 1.96*sdev2/sqrt(n_contr)), width=.2, position=position_dodge(.9), colour = "blue") +
  geom_vline(xintercept = 7.5, linetype = 2) + 
  xlab("Months") + ylab("Mean crime") +
  theme(legend.position="bottom") +
  labs(x = "Months",
       y = "Mean Crime, 95% CI",
       color = "Legend") +
  scale_color_manual(values = colors1)
p1



#95% confidence intervals
p12 <- ggplot(data = datamean, aes(x=mes, y = name, color = mes)) +
  geom_point(colour = "red") + geom_line(colour = "red") +
  geom_point(data = datamean, aes(x = mes, y = name2), colour = "blue") + 
  geom_line(data = datamean, aes(x = mes, y = name2), colour = "blue") +
  geom_errorbar(data = datamean, aes(ymin = name - 1.96*sdev/sqrt(n_treat), ymax = name + 1.96*sdev/sqrt(n_treat)), width=.2, position=position_dodge(.9), colour = "red") +
  geom_errorbar(data = datamean, aes(ymin = name2 - 1.96*sdev2/sqrt(n_contr), ymax = name2 + 1.96*sdev2/sqrt(n_contr)), width=.2, position=position_dodge(.9), colour = "blue") +
  geom_vline(xintercept = 7.5, linetype = 2) + 
  xlab("Months") + ylab("Mean crime") +
  theme(legend.position="bottom")
p12
#SE*6 (random number but close the the goal picture)
p13 <- ggplot(data = datamean, aes(x=mes, y = name, color = mes)) +
  geom_point(colour = "red") + geom_line(colour = "red") +
  geom_point(data = datamean, aes(x = mes, y = name2), colour = "blue") + 
  geom_line(data = datamean, aes(x = mes, y = name2), colour = "blue") +
  geom_errorbar(data = datamean, aes(ymin = name - 6*sdev/sqrt(n_treat), ymax = name + 6*sdev/sqrt(n_treat)), width=.2, position=position_dodge(.9), colour = "red") +
  geom_errorbar(data = datamean, aes(ymin = name2 - 6*sdev2/sqrt(n_contr), ymax = name2 + 6*sdev2/sqrt(n_contr)), width=.2, position=position_dodge(.9), colour = "blue") +
  geom_vline(xintercept = 7.5, linetype = 2) + 
  xlab("Months") + ylab("Mean crime") +
  theme(legend.position="bottom")
p12
p13

p3 <- ggplot(data = datamean, aes(x=mes, y = dif)) +
  geom_point(colour = "red") + geom_line(colour = "red") +
  geom_errorbar(data = datamean, aes(ymin = dif - sdev2, ymax = dif + sdev2), width=.2, position=position_dodge(.9), colour = "blue") +
  geom_vline(xintercept = 7.5, linetype = 2) + 
  xlab("Months") + ylab("Mean crime") +
  theme(legend.position="bottom")

p3

