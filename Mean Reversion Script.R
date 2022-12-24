rm(list=ls())

setwd("C:/Users/Five Star Computer/Desktop/TUM MIM/Master Thesis - Asset Pricing/DataSet")
load("2018_DS_monthly.RData")
load("2018_DS_static.RData")
load("2018_WS_yearly.RData")
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

smp_siz = floor(0.25*nrow(DS.monthly))
set.seed(123)   # set seed to ensure you always have same random numbers generated
monthly= sample(seq_len(nrow(DS.monthly)),size = smp_siz) 
monthly_db =data.frame(DS.monthly[monthly,])
panel_country <- merge(WS.yearly,monthly_db, by.x=c("Id","YEAR"),by.y=c("Id","hcdec"),all.x=T) # no impact here: yearly panel covers all obs. of monthly panel 

#head(panel_country)

# validate the merge
panel_country[YEAR==2015 & month %in% c(108,109) & Id=="13016J"]
WS.yearly[YEAR==2013 & Id=="13016J"]
WS.yearly[YEAR==2014 & Id=="13016J"]
panel_country[YEAR==2014 & Id=="13016J"] #Just to check with the full data

panel_country[YEAR==2015 & Id=="13016J"] #Just to check with the full data


## Determine portfolio breakpoints for Size
# Fama-French take the MV from end-of-June and rebalance yearly
panel_country[,month := month(Date)]
panel_country[,year := year(Date)]
panel_country1 <- panel_country[,hcjun := ifelse(month>=7,year,year-1)]


panel_country1[YEAR==2015 & Id=="13016J"] #Just to check with the full data


#Lagging MV.USD by one month grouped by each company as recommended to have weight w.r.t start of the period

panel_country1[, MV.USD_lag:=c(NA, MV.USD[-.N]), by = panel_country1$Id]

panel_country1[YEAR==2015 & Id=="13016J"] #Just to check with the full data after lagging

#Calculating book to market ratio to use in HML from merged data
#wc03501 - equity & wc03263 - deferred taxes
#MV should be multiplied with 1000,000 because value is in Millions in actual & Numerator values in thousands
#(Book to market ratio is calculated monthly without taking the value end of the year i.e., June)
panel_country1$book_market <- (panel_country1$WC03501 + panel_country1$WC03263) / ((panel_country1$MV.USD_lag)*1000 )  #MV should be multiplied with 100,000 because value is in Millions in actual & Numerator values in thousands


panel_country1[YEAR==2016 & Id=="13016J"]
summary(panel_country1$book_market)
print(panel_country1$book_market)

#To check the data dispersion and analyze value & growth stocks - At which percentile do we have which value
quantile(panel_country1$book_market, probs = c(0.3,0.5,0.7,0.715, 0.75, 0.8 ,0.9), na.rm = TRUE)

x--------------------------------------------------------------------------------------------------x
#FOR FAMA FRENCH SIZE PORTFOLIO

hlpvariable <- panel_country1[month==7,(size_bb_80 =quantile(MV.USD_lag, probs = 0.8, na.rm=T)),by=year]
hlpvariable
hist(panel_country1[month==7 & year==2011,MV],breaks=500)
# better use a log scale
hist(panel_country1[month==7 & year==2011,log(MV)],breaks=500)


# Merge the breakpoints back from July Y to June Y+1
panel_c <- merge(panel_country1,hlpvariable, by.x=c("hcjun"),by.y=c("year"),all.x=T)
panel_c

# in June, we still have the value from end-of-June of last year
panel_c[month==6 & Id=="13016J"]
panel_c[month==6 & Id=="13080F"]

hlpvariable[year==2016]


size_bb_80 <- quantile(panel_c$V1, probs = 0.8, na.rm=T) #Schmidt Principle for Size Breakpoint and using MV.USD for the end of July for each year (Yearly)
size_bb_80

# assign stocks to portfolios: Small/Big = pf.size

panel_c[!is.na(MV.USD_lag), pf.size :=ifelse(MV.USD_lag>size_bb_80,"Big",ifelse(MV.USD_lag<size_bb_80,"Small",NA))]

table(panel_c$pf.size)

x-----------------------------------------------------------------------------------x
#For Value Portfolio

qq <- quantile(panel_c$book_market, c(0.3, 0.7), na.rm= TRUE)

panel_c[!is.na(book_market), pf.value :=
          ifelse(book_market<qq[1],"Low",
                 ifelse(book_market>qq[1] & book_market<qq[2],"Neutral",
                        ifelse(book_market>qq[2],"High" ,
                               NA)))]

table(panel_c$pf.value)


x------------------------------------------------------------------------------------x
#For mean Reversion

meanreversiondata <- panel_c %>% select(Id ,Date, YEAR, RET.USD, UP, book_market, pf.size, pf.value) %>% na.omit()

meanreversiondata$mix <- paste0(meanreversiondata$pf.size,".",meanreversiondata$pf.value)               #Concatenate vectors after converting to character 

meanreversiondata <- meanreversiondata %>% rename( "SIZE_VALUE" = "mix")

names(meanreversiondata)
summary(meanreversiondata)

is.na(meanreversiondata) <- sapply(meanreversiondata, is.infinite)
na.omit(meanreversiondata)
summary(meanreversiondata)

meanreversion <- na.omit(meanreversiondata)
summary(meanreversion)
df<-meanreversion

#df<-df[,c("Id","Year","RET.USD","UP","book_market")]

df$averagebook_market

df <- df[order(df$book_market),]

df$rank<-ave(-df$book_market, df$YEAR,FUN =rank)
df
unique(df$YEAR)

dfper1990<-df[which(as.integer(df$YEAR)==as.integer(1990)),]
dfper1990<- dfper1990[order(dfper1990$rank),]
dfper1990
dfper1986<-df[which(as.integer(df$YEAR)==as.integer(1986)),]
dfper1986<- dfper1986[order(dfper1986$rank),]
dfper1986
dfper2000<-df[which(as.integer(df$YEAR)==as.integer(2000)),]
dfper2000<- dfper2000[order(dfper2000$rank),]
dfper2000
dfper2016<-df[which(as.integer(df$YEAR)==as.integer(2016)),]
dfper2016<- dfper2016[order(dfper2016$rank),]
dfper2016

#Correlation

#library(mlbench)
#install.packages("caret")
library(caret)
#install.packages("Hmisc")
#library("Hmisc")
#install.packages("corrplot")
library(corrplot)


stockrankcor<-df
stockrankcor$Id<-as.numeric(stockrankcor$Id)
stockrankcor$rank<-as.numeric(stockrankcor$rank)
correlations1 <- cor(stockrankcor[,c("Id","YEAR","book_market","rank")])
correlations1
corrplot(correlations1, method="square")

library(plyr)
TotalStockPerYearInfo<-count(df, c("YEAR"))


TotalStockPerYearInfopersize<-count(df, c("YEAR","SIZE_VALUE"))

total <- merge(TotalStockPerYearInfo,TotalStockPerYearInfopersize,by="YEAR")
total <-total  %>% group_by(YEAR) %>%
  mutate(ratio =freq.y/freq.x,
         percentage =ratio*100)
total 


df1  <- df %>%
  group_by(YEAR)   %>%
  mutate( averagebook_market=mean(book_market,by.x=c("Id","YEAR","pf.value")),
          averageRET.USD=mean(RET.USD,by.x=c("Id","YEAR","pf.value")),
          highvaluebookmarket=max(averagebook_market),
          lowvaluebookmarket=min(averagebook_market),
          differenceaveragebookmarket=highvaluebookmarket-lowvaluebookmarket,
          logdifferenceaveragebookmarket=log(differenceaveragebookmarket))

dfhigh<-data.frame(matrix(ncol = length(df1)))
dflow<-data.frame(matrix(ncol = length(df1)))
dfneutral<-data.frame(matrix(ncol = length(df1)))
colnames(dfhigh)<-colnames(df1)
colnames(dflow)<-colnames(df1)
colnames(dfneutral)<-colnames(df1)
dfhigh<-df1[which(df1[,"pf.value"]=="High"),]
dflow<-df1[which(df1[,"pf.value"]=="Low"),]
dfneutral<- df1[which(df1[,"pf.value"]=="Neutral"),]
df1$YEAR<-as.integer(df1$YEAR)
dfgroup1<- df1[which(df1[,"YEAR"]<=2007),]
dfgroup1high<-dfgroup1[which(dfgroup1[,"pf.value"]=="High"),]
dfgroup1low<-dfgroup1[which(dfgroup1[,"pf.value"]=="Low"),]
dfgroup1neutral<- dfgroup1[which(dfgroup1[,"pf.value"]=="Neutral"),]
dfgroup2<- df1[which(df1[,"YEAR"]>=2008),]
dfgroup2high<-dfgroup2[which(dfgroup2[,"pf.value"]=="High"),]
dfgroup2low<-dfgroup2[which(dfgroup2[,"pf.value"]=="Low"),]
dfgroup2neutral<- dfgroup2[which(dfgroup2[,"pf.value"]=="Neutral"),]

stockrankcorgroup1<-dfgroup1
stockrankcorgroup1$Id<-as.numeric(stockrankcorgroup1$Id)
stockrankcorgroup1$rank<-as.numeric(stockrankcorgroup1$rank)
correlationsgroup1 <- cor(stockrankcorgroup1[,c("Id","YEAR","book_market","rank")])
correlationsgroup1
corrplot(correlationsgroup1, method="square")

stockrankcorgroup2<-dfgroup2
stockrankcorgroup2$Id<-as.numeric(stockrankcorgroup2$Id)
stockrankcorgroup2$rank<-as.numeric(stockrankcorgroup2$rank)
correlationsgroup2 <- cor(stockrankcorgroup2[,c("Id","YEAR","book_market","rank")])
correlationsgroup2
corrplot(correlationsgroup2, method="square")

single_timeseries_plotpvalue = function(data,value,ylabel) {
  
  ggplot(data, aes(x=as.character(YEAR))) + 
    geom_line(aes(y = value), color = "darkred")+
    xlab("YEAR")+
    ylab(ylabel)
  
}
single_timeseries_barplotpvalue = function(data,value,ylabel) {
  
  ggplot(data, aes(x=as.character(YEAR), y=value,fill = pf.value)) +
    geom_bar(stat="identity")+
    xlab("Year")+
    ylab(ylabel)
  
}

summary(df1$averagebook_market)
single_timeseries_plotpvalue(df1,df1$averagebook_market,"Average Book to Market Ratio")
single_timeseries_barplotpvalue(df1,df1$averagebook_market,"Average Book to Market Ratio")
single_timeseries_barplotpvalue(dfhigh,dfhigh$averagebook_market,"Average Book to Market Ratio For High Pf.Value")
single_timeseries_barplotpvalue(dflow,dflow$averagebook_market,"Average Book to Market Ratio For Low Pf.Value")
single_timeseries_barplotpvalue(dfneutral,dfneutral$averagebook_market,"Average Book to Market Ratio For Neutral Pf.Value")
single_timeseries_barplotpvalue(dfgroup1,dfgroup1$averagebook_market,"Average Book to Market Ratio For Years Below-2007 Pf.Value")
single_timeseries_barplotpvalue(dfgroup2,dfgroup2$averagebook_market,"Average Book to Market Ratio For Years 2008-above Pf.Value")
single_timeseries_barplotpvalue(dfgroup1high,dfgroup1high$averagebook_market,"Average Book to Market Ratio For Years Below-2007 High Pf.Value")
single_timeseries_barplotpvalue(dfgroup1low,dfgroup1low$averagebook_market,"Average Book to Market Ratio For Years Below-2007 Low Pf.Value")
single_timeseries_barplotpvalue(dfgroup1neutral,dfgroup1neutral$averagebook_market,"Average Book to Market Ratio For Years Below-2007 Neutral Pf.Value")
single_timeseries_barplotpvalue(dfgroup2high,dfgroup2high$averagebook_market,"Average Book to Market Ratio For Years 2008-above High Pf.Value")
single_timeseries_barplotpvalue(dfgroup2low,dfgroup2low$averagebook_market,"Average Book to Market Ratio For Years 2008-above Low Pf.Value")
single_timeseries_barplotpvalue(dfgroup2neutral,dfgroup2neutral$averagebook_market,"Average Book to Market Ratio For Years 2008-above Neutral Pf.Value")

summary(df1$averageRET.USD)
single_timeseries_plotpvalue(df1,df1$averageRET.USD,"Average RET.USD")
single_timeseries_barplotpvalue(df1,df1$RET.USD,"Average RET.USD")
single_timeseries_barplotpvalue(dfhigh,dfhigh$averageRET.USD,"Average RET.USD For High Pf.Value")
single_timeseries_barplotpvalue(dflow,dflow$averageRET.USD,"Average RET.USD For Low Pf.Value")
single_timeseries_barplotpvalue(dfneutral,dfneutral$averageRET.USD,"Average RET.USD For Neutral Pf.Value")
single_timeseries_barplotpvalue(dfgroup1,dfgroup1$averageRET.USD,"Average RET.USD For Years Below-2007 Pf.Value")
single_timeseries_barplotpvalue(dfgroup2,dfgroup2$averageRET.USD,"Average RET.USD For Years 2008-above Pf.Value")
single_timeseries_barplotpvalue(dfgroup1high,dfgroup1high$averageRET.USD,"Average RET.USD For Below-2007 High Pf.Value")
single_timeseries_barplotpvalue(dfgroup1low,dfgroup1low$averageRET.USD,"Average RET.USD For Below-2007 Low Pf.Value")
single_timeseries_barplotpvalue(dfgroup1neutral,dfgroup1neutral$averageRET.USD,"Average RET.USD For Below-2007 Neutral Pf.Value")
single_timeseries_barplotpvalue(dfgroup2high,dfgroup2high$averageRET.USD,"Average RET.USD For 2008-above High Pf.Value")
single_timeseries_barplotpvalue(dfgroup2low,dfgroup2low$averageRET.USD,"Average RET.USD For Low 2008-above Pf.Value")
single_timeseries_barplotpvalue(dfgroup2neutral,dfgroup2neutral$averageRET.USD,"Average RET.USD For 2008-above Neutral Pf.Value")

summary(df1$differenceaveragebookmarket)
summary(df1$logdifferenceaveragebookmarket)
single_timeseries_plotpvalue(df1,df1$logdifferenceaveragebookmarket,"Log difference of Average Book to Market Ratio")
single_timeseries_barplotpvalue(df1,df1$logdifferenceaveragebookmarket,"Log difference of Average Book to Market Ratio")
single_timeseries_barplotpvalue(dfhigh,dfhigh$logdifferenceaveragebookmarket,"Log difference of Average Book to Market Ratio For High Pf.Value")
single_timeseries_barplotpvalue(dflow,dflow$logdifferenceaveragebookmarket,"Log difference of Average Book to Market Ratio For Low Pf.Value")
single_timeseries_barplotpvalue(dfneutral,dfneutral$logdifferenceaveragebookmarket,"Log difference of Average Book to Market Ratio For Neutral Pf.Value")
single_timeseries_barplotpvalue(dfgroup1,dfgroup1$logdifferenceaveragebookmarket,"Log difference of Average Book to Market Ratio For Years Below-2007 Pf.Value")
single_timeseries_barplotpvalue(dfgroup2,dfgroup2$logdifferenceaveragebookmarket,"Log difference of Average Book to Market Ratio For 2008-above Pf.Value")
single_timeseries_barplotpvalue(dfgroup1high,dfgroup1high$logdifferenceaveragebookmarket,"Log difference of Average Book to Market Ratio For Below-2007 High Pf.Value")
single_timeseries_barplotpvalue(dfgroup1low,dfgroup1low$logdifferenceaveragebookmarket,"Log difference of Average Book to Market Ratio For Below-2007 Low Pf.Value")
single_timeseries_barplotpvalue(dfgroup1neutral,dfgroup1neutral$logdifferenceaveragebookmarket,"Log difference of Average Book to Market Ratio For Below-2007 Neutral Pf.Value")
single_timeseries_barplotpvalue(dfgroup2high,dfgroup2high$logdifferenceaveragebookmarket,"Log difference of Average Book to Market Ratio For 2008-above High Pf.Value")
single_timeseries_barplotpvalue(dfgroup2low,dfgroup2low$logdifferenceaveragebookmarket,"Log difference of Average Book to Market Ratio For 2008-above Low Pf.Value")
single_timeseries_barplotpvalue(dfgroup2neutral,dfgroup2neutral$logdifferenceaveragebookmarket,"Log difference of Average Book to Market Ratio For 2008-above Neutral Pf.Value")