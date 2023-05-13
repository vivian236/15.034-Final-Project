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

#correlation matrix between z managementa and different countries
cor(mydata[, c("cc_uk", "cc_sw", "cc_ca", "cc_br", "cc_us")], zmanagement)

################ Models ################ 

#basic model with only outcome variable and dependent variable
basic_model <- lm(zami_rate~zmanagement)
summary(basic_model)
summary(basic_model)$coefficients

#IV model with year control and survey reliability
model<- ivreg(zami_rate~zmanagement+yy06+yy09+survey_reliability+survey_reliability_miss | logcom_ttime+yy06+yy09+survey_reliability+survey_reliability_miss)
summary(model)

#IV model with all controls
all_controls <- ivreg(zami_rate~zmanagement+yy06+yy09+survey_reliability+survey_reliability_miss+yy06+yy09+survey_reliability+survey_reliability_miss
                      +hos_lbed+hos_fprofit+hos_nfprofit+hos_numcompetitors
                      +grid_elev_srtm_pred+grid_ldis_ocean+grid_ldis_ocean_miss+grid_ldis_river+grid_ldis_river_miss+grid_prec_new+grid_temp_new+grid_temp_new_miss
                      +as.factor(country)+grid_lmerpop2005+grid_lmerpop2005m|logcom_ttime+yy06+yy09+survey_reliability+survey_reliability_miss+yy06+yy09+survey_reliability+survey_reliability_miss
                      +hos_lbed+hos_fprofit+hos_nfprofit+hos_numcompetitors
                      +grid_elev_srtm_pred+grid_ldis_ocean+grid_ldis_ocean_miss+grid_ldis_river+grid_ldis_river_miss+grid_prec_new+grid_temp_new+grid_temp_new_miss
                      +as.factor(country)+grid_lmerpop2005+grid_lmerpop2005m)
summary(all_controls)

#IV model with hospital controls and year controls
#hospital controls seems to be bad control variables - high p values
model<- ivreg(zami_rate~zmanagement+yy06+yy09+hos_lbed+hos_fprofit+hos_nfprofit+hos_numcompetitors | logcom_ttime+yy06+yy09+hos_lbed+hos_fprofit+hos_nfprofit+hos_numcompetitors)
summary(model)

#IV model with all geographic controls  and year controls
#control for GDP seems to be bad - high p value (0.96)
model<- ivreg(zami_rate~zmanagement+yy06+yy09+grid_elev_srtm_pred+grid_ldis_ocean+grid_ldis_ocean_miss+grid_ldis_river+grid_ldis_river_miss+grid_lmerpop2005+grid_lmerpop2005m+grid_prec_new+grid_temp_new+grid_temp_new_miss
              | logcom_ttime+yy06+yy09+grid_elev_srtm_pred+grid_ldis_ocean+grid_ldis_ocean_miss+grid_ldis_river+grid_ldis_river_miss+grid_lmerpop2005+grid_lmerpop2005m+grid_prec_new+grid_temp_new+grid_temp_new_miss)
summary(model)

#IV model with geographic controls and year control and survey reliability
model<- ivreg(zami_rate~zmanagement+yy06+yy09+grid_elev_srtm_pred+grid_ldis_ocean+grid_ldis_ocean_miss+grid_ldis_river+grid_ldis_river_miss+grid_prec_new+grid_temp_new+grid_temp_new_miss
              +as.factor(country)+survey_reliability+survey_reliability_miss
              | logcom_ttime+yy06+yy09+grid_elev_srtm_pred+grid_ldis_ocean+grid_ldis_ocean_miss+grid_ldis_river+grid_ldis_river_miss+grid_lmerpop2005+grid_lmerpop2005m+grid_prec_new+grid_temp_new+grid_temp_new_miss +as.factor(country)+survey_reliability+survey_reliability_miss)
summary(model)

#IV model with geographic controls and and year controls and country and survey reliability
model<- ivreg(zami_rate~zmanagement+yy06+yy09+grid_elev_srtm_pred+grid_ldis_ocean+grid_ldis_ocean_miss+grid_ldis_river+grid_ldis_river_miss+grid_prec_new+grid_temp_new+grid_temp_new_miss
              +as.factor(country)+survey_reliability+survey_reliability_miss
              | logcom_ttime+yy06+yy09+grid_elev_srtm_pred+grid_ldis_ocean+grid_ldis_ocean_miss+grid_ldis_river+grid_ldis_river_miss+grid_lmerpop2005+grid_lmerpop2005m+grid_prec_new+grid_temp_new+grid_temp_new_miss
              +as.factor(country)+survey_reliability+survey_reliability_miss)
summary(model)

#check instrument variable with dependent variable and outcome variable
model<- lm(zmanagement~logcom_ttime)
summary(model)

model<-lm(zami_rate~logcom_ttime)
summary(model)



