rm(list=ls())

#setwd("C:/Users/Five Star Computer/Desktop/TUM MIM/Master Thesis - Asset Pricing/DataSet")
load("2018_DS_monthly.RData")
load("2018_DS_static.RData")
load("2018_WS_yearly.RData")
ls()
#install.packages("tidyverse")
#install.packages("Hmisc")
#install.packages("data.table")
#install.packages("dplyr")
#install.packages("lubridate")
#install.packages("zoo")
#install.packages("DT")
#install.packages("quantmod")
#install.packages("tidyverse")
#install.packages("ISLR")
#install.packages("ggplot2")
#install.packages("plyr")
#install.packages("ggplot2")
install.packages("tidyr")
library(Hmisc)
library(tidyr)
library(data.table)
library(dplyr)
library(lubridate)
library(zoo)
library(DT)
library(tidyr)
library(quantmod)
library(tidyverse)
library(ISLR)
library(ggplot2)
library(plyr)
library(lubridate)
# create a help column to merge the accounting data
# from july in year Y on, use accounting information from year Y-1
DS.monthly[,month := month(Date)]
DS.monthly[,year := year(Date)]
DS.monthly[,hcdec := ifelse(month>=7,year-1,year-2)]


monthly_db =data.frame(DS.monthly)
dbrow=as.integer(nrow(monthly_db))
dbcol=as.integer(ncol(monthly_db))
for(j in 1:dbcol){
  te=sapply(monthly_db[1,j], class)
  if (te=="numeric"){
    # a<- as.character(panel_country[1:dbrow,j])
    meandata <- mean(na.omit(monthly_db[1:dbrow,j]) )
    monthly_db[1:dbrow,j] <- monthly_db[1:dbrow,j] %>% replace_na(meandata)
    #panel_country[1:dbrow,j] <- as.numeric(panel_country[1:dbrow,j])
  } 
  if(te=="integer"){ meandata <- mean(na.omit(monthly_db[1:dbrow,j]) )
  monthly_db[1:dbrow,j] <- monthly_db[1:dbrow,j] %>% replace_na(meandata) }
  else
  {monthly_db[1:dbrow,j]<-monthly_db[1:dbrow,j]}
}
WS.yearly =data.frame(WS.yearly )
dbrow=as.integer(nrow(WS.yearly))
dbcol=as.integer(ncol(WS.yearly))
for(j in 1:dbcol){
  te=sapply(WS.yearly[1,j], class)
  if (te=="numeric"){
    # a<- as.character(panel_country[1:dbrow,j])
    meandata <- mean(na.omit(WS.yearly[1:dbrow,j]) )
    WS.yearly[1:dbrow,j] <- WS.yearly[1:dbrow,j] %>% replace_na(meandata)
    #panel_country[1:dbrow,j] <- as.numeric(panel_country[1:dbrow,j])
  } 
  if(te=="integer"){ meandata <- mean(na.omit(WS.yearly[1:dbrow,j]) )
  WS.yearly[1:dbrow,j] <- WS.yearly[1:dbrow,j] %>% replace_na(meandata) }
  else
  {WS.yearly[1:dbrow,j]<-WS.yearly[1:dbrow,j]}
}
smp_siz = floor(0.25*nrow(monthly_db))
set.seed(123)   # set seed to ensure you always have same random numbers generated
monthly= sample(seq_len(nrow(monthly_db)),size = smp_siz) 
monthly_db =data.frame(monthly_db[monthly,])

panel_country <- merge(WS.yearly,monthly_db, by.x=c("Id","YEAR"),by.y=c("Id","hcdec"),all.x=T) # no impact here: yearly panel covers all obs. of monthly panel 
panel_country <- data.table(panel_country)
WS.yearly <- data.table(WS.yearly)
#panel_country <- data.table(panel_country)
#head(panel_country)


panel_country$YEAR<-as.integer(panel_country$YEAR)
panel_country$year<-as.integer(panel_country$year)
#}

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
#MV should be multiplied with 100,000 because value is in Millions in actual & Numerator values in thousands
#(Book to market ratio is calculated monthly without taking the value end of the year i.e., June)
panel_country1$book_market <- (panel_country1$WC03501 + panel_country1$WC03263) / ((panel_country1$MV.USD_lag)*1000 )  #MV should be multiplied with 100,000 because value is in Millions in actual & Numerator values in thousands


panel_country1[YEAR==2016 & Id=="13016J"]
summary(panel_country1$book_market)
print(panel_country1$book_market)

B_M <- panel_country1 %>% select( YEAR ,book_market) %>% na.omit()
BM <-   aggregate(x = B_M$book_market,                # Specify data column
                  by = list(B_M$YEAR),              # Specify group indicator
                  FUN = sum)

#To check the data dispersion and analyze value & growth stocks - At which percentile do we have which value
quantile(panel_country1$book_market, probs = c(0.3,0.5,0.7,0.715, 0.75, 0.8 ,0.9), na.rm = TRUE)

#x--------------------------------------------------------------------------------------------------x
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

#x-----------------------------------------------------------------------------------x
#For Value Portfolio

qq <- quantile(panel_c$book_market, c(0.3, 0.7), na.rm= TRUE)

panel_c[!is.na(book_market), pf.value :=
          ifelse(book_market<qq[1],"Low",
                 ifelse(book_market>qq[1] & book_market<qq[2],"Neutral",
                        ifelse(book_market>qq[2],"High" ,
                               NA)))]

table(panel_c$pf.value)



#x------------------------------------------------------------------------------------x

#panel_c$mix <- paste0(panel_c$pf.size,".",panel_c$pf.value)
test <- panel_c %>% select(Date,YEAR , month, ym ,pf.size,pf.value,RET.USD,MV.USD_lag, UP, book_market) %>% na.omit()
test$mix <- paste0(test$pf.size,".",test$pf.value)               #Concatenate vectors after converting to character 
#test_reselected <- test %>% select(!pf.size,!pf.value)
#names(test)
test<- data.frame(test)
test<-test %>% dplyr::rename(SIZE_VALUE = mix, LMV.USD= MV.USD_lag)
#test$SIZE_VALUE=test$mix
#test$LMV.USD=test$MV.USD_lag
names(test)

portfolio_returns <- test %>% # this operator nests functions
  group_by(YEAR,SIZE_VALUE) %>% # do "everything" for the groups specified here
  summarize(ret.port = weighted.mean(RET.USD,
                                     LMV.USD)) %>% # vw returns using lagged mcap
  tidyr::spread(SIZE_VALUE,ret.port) %>% # create one column for each group
  mutate(
    Small = (Small.High + Small.Neutral + Small.Low)/3, # just exemplary
    Big = (Big.High + Big.Neutral + Big.Low)/3,
    SMB = Small-Big,
    High = (Small.High + Big.High)/2,
    Low = (Small.Low + Big.Low)/2,
    HML = High-Low
  )


portfolio_returns <- as.data.table(portfolio_returns)



#x------------------------------------------------------------------------------------x

#checking the overall average return of size and value
mean(portfolio_returns$HML)
mean(portfolio_returns$Small.High)
mean(portfolio_returns$Small.Low)
mean(portfolio_returns$Big.High)
mean(portfolio_returns$Big.Low)
mean(portfolio_returns$SMB)


plot(portfolio_returns$YEAR,portfolio_returns$HML, type = "S")  #Potraying the overall HML return trend over all these years



#Calculating Standard Deviation
sqrt(var(portfolio_returns$HML))
sqrt(var(portfolio_returns$SMB))

# Check the factors
portfolio_returns[,t.test(SMB)]
portfolio_returns[,t.test(HML)]

#HML <- table(portfolio_returns$HML)


summary(test$book_market)
ggplot(test, aes(test$Date , test$book_market )) +
  geom_line()
ggplot(portfolio_returns, aes(portfolio_returns$YEAR , portfolio_returns$HML )) +
  geom_line()
ggplot(portfolio_returns, aes(portfolio_returns$YEAR , portfolio_returns$HML )) +
  geom_col()
ggplot(portfolio_returns, aes(portfolio_returns$YEAR , portfolio_returns$SMB )) +
  geom_line()
ggplot(portfolio_returns, aes(portfolio_returns$YEAR , portfolio_returns$SMB )) +
  geom_col()


#Result for above SMB & HML t.test shows small cap companies couldn't outperform big cap companies but high b/m companies did outperform low b/m companies

#Calculate monthly returns just change the argument "period"


#X---------------------------------------------------------------------------------------------------------X
#VALUE WEIGHTED MONTHLY MARKET RETURN

#Summation of Lagged market cap. w.r.t each month - Needed for the calculation of value weighted return
LMV.SUM <-   aggregate(x = test$LMV.USD,                # Specify data column
                       by = list(test$Date),              # Specify group indicator
                       FUN = sum)

# Here x = Total monthly sum of market cap

#merging the Summed LMV.USD with data to calculate value we
test.new<- merge(test,LMV.SUM, by.x=c("Date"),by.y=c("Group.1"),all.x=T)

test.new$RM <-  (test.new$LMV.USD/test.new$x)*(test.new$RET.USD)  #Monthly value weighted Market Return


RM <- aggregate(test.new[, 12:13], list(test.new$YEAR), mean) #Taking yearly average for market return


library(readxl)
KF_Yearly_Data <- read_excel("KF Yearly Data.xlsx") #Importing KF Yearly data for comparison

dbrow=as.integer(nrow(KF_Yearly_Data))
dbcol=as.integer(ncol(KF_Yearly_Data))
for(j in 1:dbcol){
  te=sapply(KF_Yearly_Data[1,j], class)
  if (te=="numeric"){
    # a<- as.character(panel_country[1:dbrow,j])
    meandata <- mean(na.omit(KF_Yearly_Data[1:dbrow,j]) )
    KF_Yearly_Data[1:dbrow,j] <- KF_Yearly_Data[1:dbrow,j] %>% replace_na(meandata)
    #panel_country[1:dbrow,j] <- as.numeric(panel_country[1:dbrow,j])
  } 
  if(te=="integer"){ meandata <- mean(na.omit(KF_Yearly_Data[1:dbrow,j]) )
  KF_Yearly_Data[1:dbrow,j] <- KF_Yearly_Data[1:dbrow,j] %>% replace_na(meandata) }
  else
  {KF_Yearly_Data[1:dbrow,j]<-KF_Yearly_Data[1:dbrow,j]}
}
KF_Final <- KF_Yearly_Data %>% select(Year, `Mkt-RF (KF)`, `RF (KF)`, `SMB (KF)`, `HML (KF)`) %>% na.omit()
Test_Final <- merge(RM,KF_Final , by.x=c("Group.1"),by.y=c("Year"),all.x=T) #Merging the KF Data with my data 

Value_Size_Return <- merge(Test_Final, portfolio_returns , by.x=c("Group.1"),by.y=c("YEAR"),all.x=T)

Model <- Value_Size_Return %>% select(Group.1, RM,  `RF (KF)`, `Mkt-RF (KF)`, `SMB (KF)`, SMB, `HML (KF)`, HML)

Model <- Model %>% rename( "Year" = "Group.1")

Model <- na.omit(Model) #All FF3 factors from the data and KF from 1991 till 2017

Model$RMRF <- (Model$RM - Model$`RF (KF)`)

mean(Model$`HML (KF)`)
mean(Model$HML)
mean(Model$`SMB (KF)`)
mean(Model$SMB)
mean(Model$`Mkt-RF (KF)`)
mean(Model$RMRF)

sqrt(var(Model$`HML (KF)`))
sqrt(var(Model$HML))
sqrt(var(Model$`SMB (KF)`))
sqrt(var(Model$SMB))
sqrt(var(Model$`Mkt-RF (KF)`))
sqrt(var(Model$RMRF))

ggplot(Model, aes(Model$Year , Model$HML )) +
  geom_line()
ggplot(Model, aes(Model$Year , Model$`HML (KF)` )) +
  geom_line()
ggplot(Model, aes(Model$Year , Model$SMB )) +
  geom_line()
ggplot(Model, aes(Model$Year , Model$`SMB (KF)` )) +
  geom_line()
ggplot(Model, aes(Model$Year , Model$RMRF )) +
  geom_line()
ggplot(Model, aes(Model$Year , Model$`Mkt-RF (KF)` )) +
  geom_line()

summary(Model$HML)
summary(test.new$book_market)


ggplot(panel_country1, aes(panel_country1$YEAR , panel_country1$book_market )) +
  geom_line()

cor(Model$SMB, Model$`SMB (KF)`)
cor(Model$HML, Model$`HML (KF)`)
cor(Model$RMRF, Model$`Mkt-RF (KF)`)

Model <- Model %>% select(Year, `Mkt-RF (KF)`, `HML (KF)`, `SMB (KF)`, RMRF, SMB, HML)


single_timeseries_plot = function(data,value, date) {
  # Base plot with date axis
  p <- ggplot(data = economics, aes(x = date, y = value)) + 
    geom_line(color = "#00AFBB", size = 1)
  p
  # Set axis limits c(min, max)
  min <- as.Date("2020")
  max <- NA
  p + scale_x_date(limits = c(min, max))
}
single_timeseries_plot(Model,`Mkt-RF (KF)`, Year)
single_timeseries_plot(Model,`HML (KF)`, Year)
single_timeseries_plot(Model,`SMB (KF)`, Year)
single_timeseries_plot(Model,RMRF, Year)
single_timeseries_plot(Model,SMB, Year)
single_timeseries_plot(Model,HML, Year)



Multiple_timeseries_plot = function(dataset) {
  # Multiple line plot
  ggplot(dataset, aes(x = Year, y = c(`Mkt-RF (KF)`, `HML (KF)`, `SMB (KF)`, RMRF, SMB, HML))) + 
    geom_line(aes(color = variable), size = 1) +
    theme_minimal()
}

Multiple_timeseries_plot(Model)


#Calculating the corelation between my data and KF data. It should be higher than 0.8 
#library("Hmisc")
#Factor_Corelations <- rcorr(as.matrix(Model))
#Factor_Corelations

#x----------------------------------------------------------------------------------------------------------x
#Cumulative Graph for Portfolio Return0.

#FF3 <- ts( Model , start=c(1991), end=c(2017), frequency=1) #Converting the data into time series



#library(PerformanceAnalytics)


#chart.CumReturns(FF3[, 2:7, drop = FALSE], FF3[, 1, drop = FALSE], colorset=rich8equal, ylog = TRUE , legend.loc="bottomright", main="Relative Performance to S&P")

#plot(ecdf(FF3[,"HML"]))

#chart.CumReturns(FF3[,"HML"],wealth.index=TRUE, main="Growth of $1")
#chart.CumReturns(FF3[,"SMB"],wealth.index=TRUE, main="Growth of $1")

#chart.CumReturns(FF3[,"HML"],main="Cumulative Returns")


#charts.PerformanceSummary(managers[,c(manager.col,indexes.cols)], colorset=ric
#charts.PerformanceSummary(FF3 [,("Mkt-RF (KF)", "SMB (KF)", "HML (KF)", "RMRF", "SMB", "HML")], colorset = rich12equal)



#chart.CumReturns(portfolio_returns$Date, portfolio_returns$HML, portfolio_returns$SMB, main="Cumulative Returns",begin="first")
#chart.CumReturns(portfolio_returns$Date, portfolio_returns$HML,main="Cumulative Returns",begin="axis")
#chart.CumReturns(portfolio_returns[,"Date", "HML"],wealth.index=TRUE, main="Growth of $1")

#x-----------------------------------------------------------------------------------------------------------------x



#plot(ecdf(portfolio_returns$HML))

#CunulativeChart <- Test_Final %>% select(YEAR , RM, `Mkt-RF (KF)`, `HML (KF)` , `SMB (KF)`, RF) %>% na.omit()

#CunulativeChart$HML <- portfolio_returns$HML

#chart.CumReturns(portfolio_returns[,"HML"],wealth.index=TRUE, main="Growth of $1")
#chart.CumReturns(ecdf(portfolio_returns$HML))


#X----------------------------------------------------------------------------------------------------X

library(PerformanceAnalytics)


#chart.CumReturns(portfolio_returns$Date, portfolio_returns$HML, portfolio_returns$SMB, main="Cumulative Returns",begin="first")
#chart.CumReturns(portfolio_returns$Date, portfolio_returns$HML,main="Cumulative Returns",begin="axis")
#chart.CumReturns(portfolio_returns[,"Date", "HML"],wealth.index=TRUE, main="Growth of $1")




