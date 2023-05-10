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

table(survey_reliability<5)
table(country)
cc_ca

