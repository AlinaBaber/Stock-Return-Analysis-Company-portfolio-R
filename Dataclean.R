library(xts)
library(forecast)
library(ggplot2)
library(tidyr)
library(dplyr)

library(magrittr)
rm(list=ls())

#setwd("C:/Users/Five Star Computer/Desktop/TUM MIM/Master Thesis - Asset Pricing/DataSet")
#setwd("C:/Users/Five Star Computer/Desktop/TUM MIM/Master Thesis - Asset Pricing/DataSet")
load("2018_DS_monthly.RData")
load("2018_DS_static.RData")

ls()
library(data.table)
library(dplyr)
library(lubridate)
library(zoo)
library(quantmod)
library(tidyverse)

# create a help column to merge the accounting data
# from july in year Y on, use accounting information from year Y-1
DS.monthly[,month := month(Date)]
DS.monthly[,year := year(Date)]
DS.monthly[,hcdec := ifelse(month>=7,year-1,year-2)]

df<-DS.monthly
#summary(df) # Provides basic descriptive statistics and frequencies.
#edit(df) # Open data editor
str(df) # Provides the structure of the dataset
names(df) # Lists variables in the dataset
head(df) # First 6 rows of dataset
head(df, n=10)# First 10 rows of dataset
head(df, n= -10) # All rows but the last 10
tail(df) # Last 6 rows
tail(df, n=10) # Last 10 rows
tail(df, n= -10) # All rows but the first 10
#df[1:10, ] # First 10 rows
#df[1:10,1:3] # First 10 rows of data of the first 3 variables

rowSums(is.na(df)) # Number of missing per row
colSums(is.na(df)) # Number of missing per column/variable
rowMeans(is.na(df))*length(df) # No. of missing per row (another way)
# length = num. of variables/elements in an ob
#library(fBasics)
#basicStats(df)

df[sapply(df, is.infinite)] <- NA
datacolmnan<- df$MV[!is.na(df$MV)]
datacolm<-datacolmnan[!is.na(datacolmnan)]
meandata <- mean(datacolm)
df$MV[is.nan(df$MV)]<-meandata
df$MV[is.na(df$MV9)]<-meandata

datacolmnan<- df$MV.USD[!is.na(df$MV.USD)]
datacolm<-datacolmnan[!is.na(datacolmnan)]
meandata <- mean(datacolm)
df$MV.USD[is.nan(df$MV.USD)]<-meandata

datacolmnan<- df$RET[!is.na(df$RET)]
datacolm<-datacolmnan[!is.na(datacolmnan)]
meandata <- mean(datacolm)
df$RET[is.nan(df$RET)]<-meandata
df$RET[is.na(df$RET)]<-meandata

datacolmnan<- df$RET.USD[!is.na(df$RET.USD)]
datacolm<-datacolmnan[!is.na(datacolmnan)]
meandata <- mean(datacolm)
df$RET.USD[is.nan(df$RET.USD)]<-meandata
df$RET.USD[is.na(df$RET.USD)]<-meandata

datacolmnan<- df$PCH[!is.na(df$PCH)]
datacolm<-datacolmnan[!is.na(datacolmnan)]
meandata <- mean(datacolm)
df$PCH[is.nan(df$PCH)]<-meandata
df$PCH[is.na(df$PCH)]<-meandata

datacolmnan<- df$PCH.USD[!is.na(df$PCH.USD)]
datacolm<-datacolmnan[!is.na(datacolmnan)]
meandata <- mean(datacolm)
df$PCH.USD[is.nan(df$PCH.USD)]<-meandata
df$PCH.USD[is.na(df$PCH.USD)]<-meandata

datacolmnan<-df$UP[!is.na(df$UP)]
datacolm<-datacolmnan[!is.na(datacolmnan)]
meandata <- mean(datacolm)
df$UP[is.nan(df$UP)]<-meandata
df$UP[is.na(df$UP)]<-meandata

datacolmnan<-df$NOSH[!is.na(df$NOSH)]
datacolm<-datacolmnan[!is.na(datacolmnan)]
meandata <- mean(datacolm)
df$NOSH[is.nan(df$NOSH)]<-meandata
df$NOSH[is.na(df$NOSH)]<-meandata

datacolmnan<-df$AF[!is.na(df$AF)]
datacolm<-datacolmnan[!is.na(datacolmnan)]
meandata <- mean(datacolm)
df$AF[is.nan(df$AF)]<-meandata
df$AF[is.na(df$AF)]<-meandata

datacolmnan<-df$hcdec[!is.na(df$hcdec)]
datacolm<-datacolmnan[!is.na(datacolmnan)]
meandata <- mean(datacolm)
df$hcdec[is.nan(df$hcdec)]<-meandata
df$hcdec[is.na(df$hcdec)]<-meandata

df$Date<-as.Date(df$Date)
MV<- xts(df$MV,df$Date)
MV<- tsclean(MV)
df$MV<-as.numeric(MV)

MV.USD<- xts(df$MV.USD,df$Date)
MV.USD<- tsclean(MV.USD)
df$MV.USD<-as.numeric(MV.USD)

RET<- xts(df$RET,df$Date)
RET<- tsclean(RET)
df$RET<-as.numeric(RET)

RET.USD<- xts(df$RET.USD,df$Date)
RET.USD<- tsclean(RET.USD)
df$RET.USD<-as.numeric(RET.USD)

PCH<- xts(df$PCH,df$Date)
PCH<- tsclean(PCH)
df$PCH<-as.numeric(PCH)

PCH.USD<- xts(df$PCH.USD,df$Date)
PCH.USD<- tsclean(PCH.USD)
df$PCH.USD<-as.numeric(PCH.USD)

UP<- xts(df$UP,df$Date)
UP<- tsclean(UP)
df$UP<-as.numeric(UP)

NOSH<- xts(df$NOSH,df$Date)
NOSH<- tsclean(NOSH)
df$NOSH<-as.numeric(NOSH)

AF<- xts(df$AF,df$Date)
AF<- tsclean(AF)
df$AF<-as.numeric(AF)

DS.monthly<-df

save.image(DS.monthly)
save(DS.monthly,file="DS.monthly.RData")

load("2018_WS_yearly.RData")
df<-WS.yearly
#summary(df) # Provides basic descriptive statistics and frequencies.
#edit(df) # Open data editor
str(df) # Provides the structure of the dataset
names(df) # Lists variables in the dataset
head(df) # First 6 rows of dataset
head(df, n=10)# First 10 rows of dataset
head(df, n= -10) # All rows but the last 10
tail(df) # Last 6 rows
tail(df, n=10) # Last 10 rows
tail(df, n= -10) # All rows but the first 10
#df[1:10, ] # First 10 rows
#df[1:10,1:3] # First 10 rows of data of the first 3 variables

rowSums(is.na(df)) # Number of missing per row
colSums(is.na(df)) # Number of missing per column/variable
rowMeans(is.na(df))*length(df) # No. of missing per row (another way)
# length = num. of variables/elements in an ob
#library(fBasics)
#basicStats(df)

df[sapply(df, is.infinite)] <- NA
df<-as.data.frame(df)
for(i in 1:length(df)){
  a<-df[,i]
datacolmnan<- a[!is.na(df[,i])]
datacolm<-datacolmnan[!is.na(datacolmnan)]
meandata <- mean(datacolm)
a[is.nan(df[,i])]<-meandata
a[is.na(df[,i])]<-meandata
df[,i]<-a

}
#dff<-na.omit(df)
df$YEAR<-as.character(df$YEAR)
a<-sprintf("%s-01-01",df$YEAR)
df$Date<-a
df$Date<-as.Date(df$Date)
for(i in 3:length(df)){
  a<-as.numeric(df[,i])
  MV<- xts(a,df$Date)
  MV<- tsclean(MV)
  df[,i]<-as.numeric(MV)
}


MV.USD<- xts(df$MV.USD,df$Date)
MV.USD<- tsclean(MV.USD)
df$MV.USD<-as.numeric(MV.USD)

RET<- xts(df$RET,df$Date)
RET<- tsclean(RET)
df$RET<-as.numeric(RET)

RET.USD<- xts(df$RET.USD,df$Date)
RET.USD<- tsclean(RET.USD)
df$RET.USD<-as.numeric(RET.USD)

PCH<- xts(df$PCH,df$Date)
PCH<- tsclean(PCH)
df$PCH<-as.numeric(PCH)

PCH.USD<- xts(df$PCH.USD,df$Date)
PCH.USD<- tsclean(PCH.USD)
df$PCH.USD<-as.numeric(PCH.USD)

UP<- xts(df$UP,df$Date)
UP<- tsclean(UP)
df$UP<-as.numeric(UP)

NOSH<- xts(df$NOSH,df$Date)
NOSH<- tsclean(NOSH)
df$NOSH<-as.numeric(NOSH)

AF<- xts(df$AF,df$Date)
AF<- tsclean(AF)
df$AF<-as.numeric(AF)

DS.monthly<-df

save.image(DS.monthly)
save(DS.monthly,file="DS.monthly.RData")
