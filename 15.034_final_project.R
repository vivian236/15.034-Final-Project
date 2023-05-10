#################### 15.034 Metrics For Managers   ##################
##################### Group Project: Does Management Save Lives? ######################

# directory
#setwd("")
setwd("/Users/vivianlo/Downloads")
rm(list = ls())

mydata <- read.csv("management_and_health_034.csv")
attach(mydata)

str(mydata)
################ Some Descriptive Statistics ################ 
#install.packages('psych')
library(psych)
library(plm)
library(car)
library(ggplot2)
library(plm)
library(systemfit)
describe(mydata,fast=TRUE)
summary(mydata)

#y variable should be mortality rate
#x variable is z management
#fixed effects model?
#
plot(hospital_id,zmanagement)
plot(zmanagement,zami_rate)

model<-lm(zami_rate~zmanagement)
summary(model)

table(survey_reliability<5, country)
table(yy,country)
table(zmanagement, country)
table(ave(zmanagement, country, FUN = function(x) mean(x, trim = 0.1)), country)


cor(zmanagement, cc_uk)
cor(zmanagement, cc_br)
cor(zmanagement, cc_ca)
cor(zmanagement, cc_sw)
cor(zmanagement, cc_us)

cor(mydata[23:27])
cor(zmanagement, zami_rate)

model<- plm(zami_rate~zmanagement+hos_lbed+hos_fprofit+hos_nfprofit+hos_numcompetitors+yy06+yy09, model = "within",  data = mydata, index = "country")
summary(model)

model<-lm(zami_rate~zmanagement+cc_br+cc_ca+cc_sw+cc_uk+cc_us+yy06+yy09+hos_lbed+hos_fprofit+hos_nfprofit+hos_numcompetitors)
summary(model)
