rm(list=ls())

#setwd("C:/Users/Five Star Computer/Desktop/TUM MIM/Master Thesis - Asset Pricing/DataSet")
#setwd("C:/Users/Five Star Computer/Desktop/TUM MIM/Master Thesis - Asset Pricing/DataSet")
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

B_M <- panel_country1 %>% select( YEAR ,book_market) %>% na.omit()
#Just to check yearly average mean trend for my understanding
BM <-   aggregate(x = B_M$book_market,                # Specify data column
                       by = list(B_M$YEAR),              # Specify group indicator
                       FUN = mean)

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
#For Fama French Three & Five Factor Model
#percentage change in total assets (WC02999)
#profits-to-book equity as operating income (WC01250) divided by book equity
#return on equity as earnings before extraordinary items (WC01551) divided by book equity (wc03501 + wc03263)
# Book equity is defined as common equity (Worldscope item WC03501) plus deferred taxes (WC03263, zero if missing).


test <- panel_c %>% select(Id ,Date, YEAR, month, ym, WC02999, WC01250, WC03501, UP, RET.USD, MV.USD_lag, WC01551, WC03263, book_market, pf.size, pf.value) %>% na.omit()

test$mix <- paste0(test$pf.size,".",test$pf.value)               #Concatenate vectors after converting to character 
#test_reselected <- test %>% select(!pf.size,!pf.value)
#names(test)

test <- test %>% rename( "SIZE_VALUE" = "mix",
                         "LMV.USD"  = "MV.USD_lag")

names(test)

portfolio_returns <- test %>% # this operator nests functions
  group_by(YEAR, SIZE_VALUE) %>% # do "everything" for the groups specified here
  summarize(ret.port = weighted.mean(RET.USD,
                                     LMV.USD)) %>% # vw returns using lagged mcap
  spread(SIZE_VALUE,ret.port) %>% # create one column for each group
  mutate(
    Small = (Small.High + Small.Neutral + Small.Low)/3, # just exemplary
    Big = (Big.High + Big.Neutral + Big.Low)/3,
    SMB = Small-Big,
    High = (Small.High + Big.High)/2,
    Low = (Small.Low + Big.Low)/2,
    HML = High-Low
  )


portfolio_returns <- as.data.table(portfolio_returns)



x------------------------------------------------------------------------------------x

#checking the overall average return of size and value
mean(portfolio_returns$HML)
mean(portfolio_returns$Small.High)
mean(portfolio_returns$Small.Low)
mean(portfolio_returns$Big.High)
mean(portfolio_returns$Big.Low)
mean(portfolio_returns$SMB)

#Calculating Standard Deviation
sqrt(var(portfolio_returns$HML))
sqrt(var(portfolio_returns$SMB))

# Check the factors
portfolio_returns[,t.test(SMB)]
portfolio_returns[,t.test(HML)]

library(ggplot2)

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


X---------------------------------------------------------------------------------------------------------X
#VALUE WEIGHTED MONTHLY MARKET RETURN

#Summation of Lagged market cap. w.r.t each month - Needed for the calculation of value weighted return
LMV.SUM <-   aggregate(x = test$LMV.USD,                # Specify data column
         by = list(test$Date),              # Specify group indicator
        FUN = sum)

# Here x = Total monthly sum of market cap

#merging the Summed LMV.USD with data to calculate value we
test.new<- merge(test,LMV.SUM, by.x=c("Date"),by.y=c("Group.1"),all.x=T)

test.new$RM <-  (test.new$LMV.USD/test.new$x)*(test.new$RET.USD)  #Monthly value weighted Market Return


RM <- aggregate(test.new[,18:19], list(test.new$YEAR), mean) #Taking yearly average for market return


library(readxl)
KF_Yearly_Data <- read_excel("KF Yearly Data.xlsx") #Importing KF Yearly data for comparison
KF_Final <- KF_Yearly_Data %>% select(Year, `HML (KF)`, `Mkt-RF (KF)`, `SMB (KF)`, `RF (KF)`) %>% na.omit()

Test_Final <- merge(RM, KF_Final , by.x=c("Group.1"),by.y=c("Year"),all.x=T) #Merging the KF Data with my data 

Value_Size_Return <- merge(Test_Final, portfolio_returns , by.x=c("Group.1"),by.y=c("YEAR"),all.x=T)

Model <- Value_Size_Return %>% select(Group.1, `Mkt-RF (KF)`, `SMB (KF)`, `HML (KF)`, `RF (KF)`, HML, SMB, RM)

Model <- Model %>% rename( "Year" = "Group.1")

Model <- na.omit(Model) #All FF3 factors from the data and KF from 1991 till 2017

Model$RMRF <- (Model$RM - Model$`RF (KF)`)

x--------------------------------------------------------------------------------------------------x
#Mean, Standard Deviation & Corelation between data and KF for Fama French Three factor model

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
ggplot(Model, aes(Model$Year , Model$HML )) +
  geom_col()
ggplot(Model, aes(Model$Year , Model$`HML (KF)` )) +
  geom_line()
ggplot(Model, aes(Model$Year , Model$`HML (KF)` )) +
  geom_col()
ggplot(Model, aes(Model$Year , Model$SMB )) +
  geom_line()
ggplot(Model, aes(Model$Year , Model$`SMB (KF)` )) +
  geom_line()
ggplot(Model, aes(Model$Year , Model$RMRF )) +
  geom_line()
ggplot(Model, aes(Model$Year , Model$`Mkt-RF (KF)` )) +
  geom_line()

summary(Model$HML)

Model <- Model %>% select(Year, `Mkt-RF (KF)`, `HML (KF)`, `SMB (KF)`, RMRF, SMB, HML)

#Calculating the corelation between my data and KF data. It should be higher than 0.8 
library("Hmisc")
Factor_Corelations <- rcorr(as.matrix(Model))
Factor_Corelations

x--------------------------------------------------------------------------------------------x

single_timeseries_plot = function(dataset,value, date) {
  # Base plot with date axis
  p <- ggplot(x = date, y = value) + 
    geom_line(color = "#00AFBB", size = 1)
  p
  
}
single_timeseries_plot(Model,Model$`Mkt-RF (KF)`,Model$Year)
single_timeseries_plot(Model,Model$`HML (KF)`, Model$Year)
single_timeseries_plot(Model,Model$`SMB (KF)`, Model$Year)
single_timeseries_plot(Model,Model$RMRF, Model$Year)
single_timeseries_plot(Model,Model$SMB, Model$Year)
single_timeseries_plot(Model,Model$HML, Model$Year)

Multiple_timeseries_plot = function(dataset,Year) {
  # Multiple line plot
  # name<- colnames(dataset)[2:7]
  ggplot(dataset, aes(x=Year)) + 
    geom_line(aes(y = MktRfKF), color = "darkred") +
    geom_line(aes(y = HMLKF), color = "red") +
    geom_line(aes(y = SMBKF), color = "blue") +
    geom_line(aes(y = RMRF), color = "green") + 
    geom_line(aes(y = SMB), color="steelblue", linetype="twodash") +
    geom_line(aes(y = HML), color = "orange") +
    scale_colour_hue(l=25)+ 
    scale_linetype_manual(values = c('Mkt-RF (KF)' = 1,'HML (KF)' = 2,'SMB (KF)'=3,'RMRF'=4,'SMB'=5,'HML'=6))
  # geom_point(aes(group=variable, color=variable, size=6))
}

colnames(Model)[2] <- "MktRfKF"
colnames(Model)[3] <- "HMLKF"
colnames(Model)[4]<- "SMBKF"
Model
#variable<- c("`Mkt-RF (KF)`", "`HML (KF)`", "`SMB (KF)`","RMRF","SMB","HML")
Multiple_timeseries_plot(Model,Model$Year)

library(PerformanceAnalytics)

Return.cumulative(Model, geometric = TRUE)

x--------------------------------------------------------------------------------------------x
#For Fama French Five Factor Model
x--------------------------------------------------------------------------------------------x
#Investment (Conservative Minus Agressive)

Asset_Growth <- quantile(test$WC02999, c(0.3, 0.7), na.rm = TRUE)
Asset_Growth
summary(test$WC02999)
test[!is.na(WC02999), pf.investment :=
        ifelse(WC02999<Asset_Growth[1],"Conservative",
               ifelse(WC02999>Asset_Growth[1] & WC02999<Asset_Growth[2],"Neutral",
                      ifelse(WC02999>Asset_Growth[2],"Agressive" ,
                             NA)))]

table(test$pf.investment)

x------------------------------------------------------------------------------------------------x
#Operating Profitability (Robust Minus Weak) (OP_BE is the column in test Data for operating profitabilty-to-book equity)
#(the WC01250 in test data shows the operating profits)
#(from the pf.value it could be determined the stocks are either value stocks (High) or growth stocks (Low))

test$OP_BE <- (test$WC01250)/(test$WC03501 + test$WC03263)
Profitability <- quantile(test$OP_BE, c(0.3, 0.7), na.rm = TRUE)
Profitability
test[!is.na(OP_BE), pf.profit :=
        ifelse(OP_BE<Profitability[1],"Weak",
               ifelse(OP_BE>Profitability[1] & OP_BE<Profitability[2],"Neutral",
                      ifelse(OP_BE>Profitability[2],"Robust" ,
                             NA)))]
table(test$pf.profit)

x----------------------------------------------------------------------------------------------x

#train$size_value <- paste0(train$pf.size, ".", train$pf.value)
#train$size_investment <- paste0(train$pf.size, ".", train$pf.investment)
#train$size_profit <- paste0(train$pf.size, ".", train$pf.profit)


x-----------------------------------------------------------------------------------x



#Removing NAs and +/- Inf values by replacing them with zero or with the mean value
summary(B_M)
#B_M[which(!is.finite(B_M))] <- 0
is.na(B_M)<-sapply(B_M, is.infinite)
B_M[is.na(B_M)]<- mean(B_M$book_market)
summary(B_M)



X------------------------------------------------------------------------------------------------------------X


