#################### 15.034 Metrics For Managers   ##################
##################### Group Project: Does Management Save Lives? ######################

# directory
#setwd("")
setwd("/Users/vivianlo/Downloads")
rm(list = ls())

mydata <- read.csv("management_and_health_034.csv")
attach(mydata)

str(mydata)

library(psych)
library(plm)
library(car)
library(ggplot2)
library(plm)
library(systemfit)
library(tidyverse)
library(AER)
library(rmarkdown)

################ Exporatory Data Analysis ################ 

describe(mydata,fast=TRUE)
summary(mydata)

#scatter plot between outcome variable and dependent variable
plot(zmanagement,zami_rate)

#box plot of distribution of z management score
plot(as.factor(country), zmanagement, xlab = "Country", ylab = "Management Score")

#table of average z management score per country
aggregate(zmanagement, list(country), FUN=mean)

#plot of distribution of z management scores per country
ggplot(data = mydata, aes(zmanagement))+geom_histogram(binwidth = 0.5, aes(y=..density..),color="black", fill="white")+geom_density(alpha=0.2)+facet_grid(.~country)

#plot of distribution of mortality rate per country
ggplot(data = mydata, aes(zami_rate))+geom_histogram(binwidth = 0.5, aes(y=..density..),color="black", fill="white")+geom_density(alpha=0.2)+facet_grid(.~country)

#scatter plot of z management scores and mortality rate per country
ggplot(data = mydata, aes(zmanagement, zami_rate))+geom_point(shape=1)+facet_grid(.~country)

#correlation matrix between z management and mortality rate and various hospital controls
cor(mydata[, c("zami_rate", "hos_lbed", "hos_fprofit", "hos_nfprofit")], zmanagement)

#correlation matrix between z management and different countries
cor(mydata[, c("cc_uk", "cc_sw", "cc_ca", "cc_br", "cc_us")], zmanagement)

################ Models ################ 

#basic model with outcome variable (zami_rate) and dependent variable (zmanagement) 
#and controls for year survey was taken, survey reliability, type of hospital, and country hospital is located in
basic_model <- lm(zami_rate~zmanagement++yy06+yy09+survey_reliability+survey_reliability_miss+hos_lbed+hos_fprofit+hos_nfprofit+as.factor(country))
summary(basic_model)
summary(basic_model)$coefficients

#We are using logcom_ttime as our instrument
#check if instrumental variable is correlated with zmanagement
m<-lm(zmanagement~logcom_ttime)
summary(m)

#check the strength of the instrument
cor(logcom_ttime, zmanagement)

#IV model with logcom_ttime as the instrument
#and controls for year survey was taken, survey reliability, type of hospital, and country hospital is located in
model <- ivreg(zami_rate~zmanagement+yy06+yy09+survey_reliability+survey_reliability_miss+hos_lbed+hos_fprofit+hos_nfprofit+as.factor(country) |
                 logcom_ttime+yy06+yy09+survey_reliability+survey_reliability_miss+hos_lbed+hos_fprofit+hos_nfprofit+as.factor(country))
summary(model)

#IV model with same controls but using lqs_com as the instrument
model <- ivreg(zami_rate~zmanagement+yy06+yy09+survey_reliability+survey_reliability_miss+hos_lbed+hos_fprofit+hos_nfprofit+as.factor(country) |
                 lqs_com+yy06+yy09+survey_reliability+survey_reliability_miss+hos_lbed+hos_fprofit+hos_nfprofit+as.factor(country))
summary(model)
#note this model is weaker than the first IV model
#therefore logcom_ttime is an appropriate instrument to use in this situation
