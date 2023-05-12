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
#install.packages("AER")
library(psych)
library(plm)
library(car)
library(ggplot2)
library(plm)
library(systemfit)
library(tidyverse)
library(AER)
describe(mydata,fast=TRUE)
summary(mydata)

#y variable should be mortality rate
#x variable is z management
#fixed effects model?
#
plot(hospital_id,zmanagement)
plot(zmanagement,zami_rate)

plot(zmanagement, as.factor(country))
as.factor(country)
model<-lm(zami_rate~zmanagement)
summary(model)

table(survey_reliability<5, country)
table(yy,country)
table(zmanagement, country)
table(ave(zmanagement, country, FUN = function(x) mean(x, trim = 0.1)), country)

newdata = filter(mydata, country=="United Kingdom")
h<-hist(newdata[, "zmanagement"])
xfit<-seq(min(newdata[, "zmanagement"]),max(newdata[, "zmanagement"]),length=40)
yfit<-dnorm(xfit,mean=mean(newdata[, "zmanagement"]),sd=sd(newdata[, "zmanagement"]))
yfit <- yfit*diff(h$mids[1:2])*length(newdata[, "zmanagement"])
lines(xfit, yfit, col="blue", lwd=2)

cor(zmanagement, cc_uk)
cor(zmanagement, cc_br)
cor(zmanagement, cc_ca)
cor(zmanagement, cc_sw)
cor(zmanagement, cc_us)

cor(mydata[23:27])
cor(zmanagement, zami_rate)

basic_model <- lm(zami_rate~zmanagement)
summary(basic_model)
summary(basic_model)$coefficients


model<- plm(zami_rate~zmanagement+hos_lbed+hos_fprofit+hos_nfprofit+hos_numcompetitors+yy06+yy09, model = "within",  data = mydata, index = "country")
summary(model)

model<-lm(zami_rate~zmanagement+cc_br+cc_ca+cc_sw+cc_uk+cc_us+yy06+yy09+hos_lbed+hos_fprofit+hos_nfprofit+hos_numcompetitors)
summary(model)

model<-systemfit(method = "2SLS", zami_rate~zmanagement+hos_lbed+hos_fprofit+hos_nfprofit+yy06+yy09, inst = ~hos_lbed+hos_fprofit+hos_nfprofit+yy06+yy09+logcom_ttime)
summary(model)

#hospital controls seems to be bad control variables - high p values
model<- ivreg(zami_rate~zmanagement+yy06+yy09+hos_lbed+hos_fprofit+hos_nfprofit+hos_numcompetitors | logcom_ttime+yy06+yy09+hos_lbed+hos_fprofit+hos_nfprofit+hos_numcompetitors)
summary(model)

#model with geographic controls - control for GDP seems to be bad - high p value (0.96)
model<- ivreg(zami_rate~zmanagement+yy06+yy09+grid_elev_srtm_pred+grid_ldis_ocean+grid_ldis_ocean_miss+grid_ldis_river+grid_ldis_river_miss+grid_lmerpop2005+grid_lmerpop2005m+grid_prec_new+grid_temp_new+grid_temp_new_miss
              | logcom_ttime+yy06+yy09+grid_elev_srtm_pred+grid_ldis_ocean+grid_ldis_ocean_miss+grid_ldis_river+grid_ldis_river_miss+grid_lmerpop2005+grid_lmerpop2005m+grid_prec_new+grid_temp_new+grid_temp_new_miss)
summary(model)

model<- ivreg(zami_rate~zmanagement+yy06+yy09+grid_elev_srtm_pred+grid_ldis_ocean+grid_ldis_ocean_miss+grid_ldis_river+grid_ldis_river_miss+grid_prec_new+grid_temp_new+grid_temp_new_miss
              +as.factor(country)+survey_reliability+survey_reliability_miss
              | logcom_ttime+yy06+yy09+grid_elev_srtm_pred+grid_ldis_ocean+grid_ldis_ocean_miss+grid_ldis_river+grid_ldis_river_miss+grid_lmerpop2005+grid_lmerpop2005m+grid_prec_new+grid_temp_new+grid_temp_new_miss +as.factor(country)+survey_reliability+survey_reliability_miss)
summary(model)

#model with geographic controls and survey reliability
model<- ivreg(zami_rate~zmanagement+yy06+yy09+grid_elev_srtm_pred+grid_ldis_ocean+grid_ldis_ocean_miss+grid_ldis_river+grid_ldis_river_miss+grid_prec_new+grid_temp_new+grid_temp_new_miss
              +cc_br+cc_ca+cc_sw+cc_uk+cc_us+survey_reliability+survey_reliability_miss
              | logcom_ttime+yy06+yy09+grid_elev_srtm_pred+grid_ldis_ocean+grid_ldis_ocean_miss+grid_ldis_river+grid_ldis_river_miss+grid_lmerpop2005+grid_lmerpop2005m+grid_prec_new+grid_temp_new+grid_temp_new_miss
              +cc_br+cc_ca+cc_sw+cc_uk+cc_us+survey_reliability+survey_reliability_miss)
summary(model)

model<- lm(zmanagement~logcom_ttime)
summary(model)

model<-lm(zami_rate~logcom_ttime)
summary(model)

#instrument can't be correlated with other variables
#no direct effect of the treatment on the outcome
model<-lm(zami_rate~zmanagement)
summary(model)


