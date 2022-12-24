rm(list=ls())

#setwd("C:/Users/Five Star Computer/Desktop/TUM MIM/Master Thesis - Asset Pricing/DataSet")
load("2018_DS_monthly.RData")
load("2018_DS_static.RData")
load("2018_WS_yearly.RData")
ls()
#install.packages("tidyverse")
library(data.table)
library(dplyr)
library(lubridate)
library(zoo)
library(DT)
library(quantmod)
library(tidyverse)
library(ISLR)
# create a help column to merge the accounting data
# from july in year Y on, use accounting information from year Y-1
DS.monthly[,month := month(Date)]
DS.monthly[,year := year(Date)]
DS.monthly[,hcdec := ifelse(month>=7,year-1,year-2)]

smp_siz = floor(0.25*nrow(DS.monthly))
set.seed(123)   # set seed to ensure you always have same random numbers generated
monthly= sample(seq_len(nrow(DS.monthly)),size = smp_siz) 
monthly_db =DS.monthly[monthly,] 

smp_siz = floor(0.25*nrow(WS.yearly))
set.seed(123)   # set seed to ensure you always have same random numbers generated
#yearly= sample(seq_len(nrow(WS.yearly)),size = smp_siz) 
#yearly_db =DS.monthly[yearly,] 
panel_country <- merge(WS.yearly,monthly_db, by.x=c("Id","YEAR"),by.y=c("Id","hcdec"),all.x=T) # no impact here: yearly panel covers all obs. of monthly panel 
panel_country <- data.frame(panel_country)
#head(panel_country)

dbrow=as.integer(nrow(panel_country))
dbcol=as.integer(ncol(panel_country))
panel_country$YEAR<-as.character(panel_country$YEAR)
panel_country$year<-as.character(panel_country$year)
meandata <- mean(na.omit(panel_country[1:dbrow,j]) )
panel_country[1:dbrow,j] <- panel_country[1:dbrow,j] %>% replace_na(meandata)
for(j in 1:dbcol){
  te=sapply(panel_country[1,j], class)
  if (te=="numeric"){
 # a<- as.character(panel_country[1:dbrow,j])
    meandata <- mean(na.omit(panel_country[1:dbrow,j]) )
  panel_country[1:dbrow,j] <- panel_country[1:dbrow,j] %>% replace_na(meandata)
  #panel_country[1:dbrow,j] <- as.numeric(panel_country[1:dbrow,j])
  }
  else
  {panel_country[1:dbrow,j]<-panel_country[1:dbrow,j]}
}

panel_country$YEAR<-as.integer(panel_country$YEAR)
panel_country$year<-as.integer(panel_country$year)