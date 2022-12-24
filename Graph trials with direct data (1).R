df <- ProfitabilityFactorCalculations

names(df)

library(data.table)
library(dplyr)
library(lubridate)
library(zoo)
library(quantmod)
library(tidyverse)
library(ggplot2)
library(xts)
library(forecast)
library(ggplot2)
library(tidyr)
library(dplyr)
library(tidyverse)
library(tidyquant)
library(recipes)
library(timetk)
library(magrittr)

df$Date <- as.Date(as.POSIXct(df$Date,origin="1987-04-30"))
pf.value<- xts(df$pf.value,df$Date)
pf.value<- tsclean( pf.value)
returnonequity <- xts(df$returnonequity,df$Date)
returnonequity<- tsclean(returnonequity)
bookequity <- xts(df$bookequity, df$Date)
bookequity<- tsclean(bookequity)
grossprofits <- xts(df$grossprofits,df$Date)
grossprofits<- tsclean(grossprofits)
OP_BE<- xts(df$OP_BE,df$Date)
OP_BE<- tsclean(OP_BE)
WC02999 <- xts(df$WC02999, df$Date)
WC02999<- tsclean(WC02999)


df$pf.value<-as.numeric(pf.value)
df$returnonequity<-as.numeric(returnonequity)
df$bookequity<-as.numeric(bookequity )
df$grossprofits<-as.numeric(grossprofits)
df$OP_BE<-as.numeric(OP_BE)

#timeseriesdata <- merge(WC02999 , WC01250, WC03501,UP,RET.USD,LMV.USD,WC01551,WC03263,book_market,OP_BE)
#colnames(timeseriesdata) <- c("WC02999" , "WC01250", "WC03501","UP","RET.USD","LMV.USD","WC01551","WC03263","book_market","OP_BE")
#test_plot<-df$UP

#require(PerformanceAnalytics)
#require(PerformanceAnalytics)
portfolio_returns_data <-as.data.frame(df)


df1 <- df %>%
  group_by(pf.value)   %>%
  mutate( averageassetchange=mean(WC02999,by.x=c("Id","YEAR","month")),
          averageOP_BE=mean(OP_BE,by.x=c("Id","YEAR","month")),
          averagebookequity = mean(bookequity,by.x=c("Id","YEAR","month")),
          averagereturnonequity = mean(returnonequity,by.x=c("Id","YEAR","month")),
          averagegrossprofits = mean(grossprofits, by.x = c("Id","YEAR", "month")))

summary(df)
# Create two new columns: group difference and group Upspread
#df1 <- df %>%
#   group_by(YEAR)   %>%
#   mutate( median= median(UP),
#           UPSpread = sqrt( abs(YEAR - median)))
# df2 <- df %>%
#   group_by(YEAR)   %>%
#   mutate( maximum=max(UP),
#           minimum=min(UP),
#           Dif = maximum-minimum)
# Generate the s-l plot
#ggplot(df1, aes(x=YEAR, y = Dif)) + 
#  geom_jitter(alpha = 0.4, width = 0.2) #+
#stat_summary(fun.y = Dif, geom = "line", col = "red") +
#ylab(expression(sqrt(abs(" UP Spread "))))



#portfolio_returns_data <-as.data.frame(df)

single_timeseries_plot = function(data,value,ylabel) {
  
  ggplot(data, aes(x=YEAR)) + 
    geom_line(aes(y = value), color = "darkred")+
    xlab("Year")+
    ylab(ylabel)
  
}
single_timeseries_plot(df1,df1$returnonequity,"return on equity")
single_timeseries_plot(df1,df1$bookequity,"book equity")
single_timeseries_plot(df1,df1$grossprofits,"gross profits")
single_timeseries_plot(df1,df1$OP_BE,"OP_BE")
single_timeseries_plot(df1,df1$WC02999,"WC02999")

single_timeseries_plot(df1,df1$averageassetchange,"average asset change")
single_timeseries_plot(df1,df1$averageOP_BE,"average OP_BE")
single_timeseries_plot(df1,df1$averagebookequity,"average book equity")
single_timeseries_plot(df1,df1$averagereturnonequity,"average book equity")
single_timeseries_plot(df1,df1$averagegrossprofits,"average gross profits")

single_timeseries_barplot = function(data,value,ylabel) {
  
  ggplot(data, aes(x=as.character(YEAR), y=value)) +
    geom_bar(stat="identity")+
    xlab("Year")+
    ylab(ylabel)
  
}
single_timeseries_barplot(df1,df1$returnonequity,"return on equity")
single_timeseries_barplot(df1,df1$bookequity,"book equity")
single_timeseries_barplot(df1,df1$grossprofits,"gross profits")
single_timeseries_barplot(df1,df1$OP_BE,"OP_BE")
single_timeseries_barplot(df1,df1$WC02999,"WC02999")

single_timeseries_barplot(df1,df1$averageassetchange,"average asset change")
single_timeseries_barplot(df1,df1$averageOP_BE,"average OP_BE")
single_timeseries_barplot(df1,df1$averagebookequity,"average book equity")
single_timeseries_barplot(df1,df1$averagereturnonequity,"average book equity")
single_timeseries_barplot(df1,df1$averagegrossprofits,"average gross profits")

x---------------------------------------------------------------------------------------------------x
#Changes should be made from afterwards. But please check it from before as well



dfhigh<-data.frame(matrix(ncol = length(df1)))
dflow<-data.frame(matrix(ncol = length(df1)))
dfneutral<-data.frame(matrix(ncol = length(df1)))
colnames(dfhigh)<-colnames(df1)
colnames(dflow)<-colnames(df1)
colnames(dfneutral)<-colnames(df1)
dfhigh<-df1[which(df1$pf.value=="High"),]
dflow<-df1[which(df1$pf.value=="Low"),]
dfneutral<- df1[df1$pf.value=="Neutral"),]

single_timeseries_plotpvalue = function(data,value,ylabel) {
  
  ggplot(data, aes(x=pf.value)) + 
    geom_line(aes(y = value), color = "darkred")+
    xlab("PF Value")+
    ylab(ylabel)
  
}
single_timeseries_barplotpvalue = function(data,value,ylabel) {
  
  ggplot(data, aes(x=as.character(YEAR), y=value,fill = pf.value)) +
    geom_bar(stat="identity")+
    xlab("Year")+
    ylab(ylabel)
  
}
summary(df1$averageassetchange)
single_timeseries_plotpvalue(df1,df1$averageassetchange,"Average Asset Change")
single_timeseries_barplotpvalue(df1,df1$averageassetchange,"Average Asset Change")

single_timeseries_barplotpvalue(df1,df1$averageassetchange,"Average Asset Change")
single_timeseries_barplotpvalue(dfhigh,dfhigh$averageassetchange,"Average Asset Change For High Pf.Value")
single_timeseries_barplotpvalue(dflow,dflow$averageassetchange,"Average Asset Change For Low Pf.Value")
single_timeseries_barplotpvalue(dfneutral,dfneutral$averageassetchange,"Average Asset Change For Neutral Pf.Value")
summary(df1$averagebookequity)
single_timeseries_plotpvalue(df1,df1$averagebookequity ,"Average Book Equity ")
single_timeseries_barplotpvalue(df1,df1$averagebookequity ,"Average Book Equity ")
single_timeseries_barplotpvalue(dfhigh,dfhigh$averagebookequity,"Average Asset Change For High Pf.Value")
single_timeseries_barplotpvalue(dflow,dflow$averagebookequity,"Average Asset Change For Low Pf.Value")
single_timeseries_barplotpvalue(dfneutral,dfneutral$averagebookequity,"Average Asset Change For Neutral Pf.Value")

summary(df1$averageOP_BE)
single_timeseries_plotpvalue(df1,df1$averageOP_BE,"Average OP_BE")
single_timeseries_barplotpvalue(df1,df1$averageOP_BE,"Average OP_BE")
summary(df1$averagereturnonequity)
single_timeseries_plotpvalue(df1,df1$averagereturnonequity,"Average Return on Equity")-
single_timeseries_barplotpvalue(df1,df1$averagereturnonequity,"Average Return on Equity")


