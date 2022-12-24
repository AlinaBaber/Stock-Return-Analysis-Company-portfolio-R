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
df<-test
summary(df) # Provides basic descriptive statistics and frequencies.
edit(df) # Open data editor
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




datacolmnan<- df$WC02999[!is.na(df$WC02999)]
datacolm<-datacolmnan[!is.na(datacolmnan)]
meandata <- mean(datacolm)
df$WC02999[is.nan(df$WC02999)]<-meandata
df$WC02999[is.na(df$WC02999)]<-meandata

datacolmnan<- df$WC01250[!is.na(df$WC01250)]
datacolm<-datacolmnan[!is.na(datacolmnan)]
meandata <- mean(datacolm)
df$WC01250[is.nan(df$WC01250)]<-meandata
df$WC01250[is.na(df$WC01250)]<-meandata

datacolmnan<- df$WC03501[!is.na(df$WC03501)]
datacolm<-datacolmnan[!is.na(datacolmnan)]
meandata <- mean(datacolm)
df$WC03501[is.nan(df$WC03501)]<-meandata
df$WC03501[is.na(df$WC03501)]<-meandata

datacolmnan<- df$UP[!is.na(df$UP)]
datacolm<-datacolmnan[!is.na(datacolmnan)]
meandata <- mean(datacolm)
df$UP[is.nan(df$UP)]<-meandata
df$UP[is.na(df$UP)]<-meandata

datacolmnan<- df$RET.USD[!is.na(df$RET.USD)]
datacolm<-datacolmnan[!is.na(datacolmnan)]
meandata <- mean(datacolm)
df$RET.USD[is.nan(df$RET.USD)]<-meandata
df$RET.USD[is.na(df$RET.USD)]<-meandata

datacolmnan<- df$LMV.USD[!is.na(df$LMV.USD)]
datacolm<-datacolmnan[!is.na(datacolmnan)]
meandata <- mean(datacolm)
df$LMV.USD[is.nan(df$LMV.USD)]<-meandata
df$LMV.USD[is.na(df$LMV.USD)]<-meandata

datacolmnan<- df$WC01551[!is.na(df$WC01551)]
datacolm<-datacolmnan[!is.na(datacolmnan)]
meandata <- mean(datacolm)
df$WC01551[is.nan(df$WC01551)]<-meandata
df$WC01551[is.na(df$WC01551)]<-meandata

datacolmnan<- df$WC03263[!is.na(df$WC03263)]
datacolm<-datacolmnan[!is.na(datacolmnan)]
meandata <- mean(datacolm)
df$WC03263[is.nan(df$WC03263)]<-meandata
df$WC03263[is.na(df$WC03263)]<-meandata

datacolmnan<- df$book_market[!is.na(df$book_market)]
datacolm<-datacolmnan[!is.na(datacolmnan)]
meandata <- mean(datacolm)
df$book_market[is.nan(df$book_market)]<-meandata
df$book_market[is.na(df$book_market)]<-meandata

datacolmnan<- df$OP_BE[!is.na(df$OP_BE)]
datacolm<-datacolmnan[!is.na(datacolmnan)]
meandata <- mean(datacolm)
df$OP_BE[is.nan(df$OP_BE)]<-meandata
df$OP_BE[is.na(df$OP_BE)]<-meandata
df<-na.omit(df)
data<-df[,c("WC02999" , "WC01250", "WC03501","UP","RET.USD","LMV.USD","WC01551","WC03263","book_market")]
library(fBasics)
basicStats(data)

df$Date <- as.Date(as.POSIXct(df$Date,origin="1987-04-30"))
WC02999 <- xts(df$WC02999,df$Date)
WC02999<- tsclean(WC02999)
WC01250 <- xts(df$WC01250,df$Date)
WC01250<- tsclean(WC01250)
WC03501 <- xts(df$WC03501, df$Date)
WC03501<- tsclean(WC03501)
UP <- xts(df$UP,df$Date)
UP<- tsclean(UP)
RET.USD<- xts(df$RET.USD,df$Date)
RET.USD<- tsclean(RET.USD)
LMV.USD <- xts(df$LMV.USD, df$Date)
LMV.USD<- tsclean(LMV.USD)
WC01551<- xts(df$WC01551, df$Date)
WC01551<- tsclean(WC01551)  
WC03263<- xts(df$WC03263, df$Date)
WC03263<- tsclean(WC03263)
book_market<- xts(df$book_market, df$Date) 
book_market<- tsclean(book_market)
OP_BE<- xts(df$OP_BE, df$Date) 
OP_BE<- tsclean(OP_BE)

df$WC02999<-as.numeric(WC02999)
df$WC01250<-as.numeric(WC01250)
df$WC03501<-as.numeric(WC03501)
df$UP<-as.numeric(UP)
df$RET.USD<-as.numeric(RET.USD)
df$LMV.USD<-as.numeric(LMV.USD)
df$WC01551<-as.numeric(WC01551)
df$WC03263<-as.numeric(WC03263)
df$book_market<-as.numeric(book_market)
df$OP_BE<-as.numeric(OP_BE)
#timeseriesdata <- merge(WC02999 , WC01250, WC03501,UP,RET.USD,LMV.USD,WC01551,WC03263,book_market,OP_BE)
#colnames(timeseriesdata) <- c("WC02999" , "WC01250", "WC03501","UP","RET.USD","LMV.USD","WC01551","WC03263","book_market","OP_BE")
#test_plot<-df$UP

#require(PerformanceAnalytics)


a<- summary(df$UP)
# Create two new columns: group difference and group Upspread
df1 <- df %>%
  group_by(YEAR)   %>%
  mutate( median= median(UP),
          UPSpread = sqrt( abs(YEAR - median)))
df2 <- df %>%
  group_by(YEAR)   %>%
  mutate( maximum=max(UP),
          minimum=min(UP),
          Dif = maximum-minimum)
# Generate the s-l plot
#ggplot(df1, aes(x=YEAR, y = Dif)) + 
#  geom_jitter(alpha = 0.4, width = 0.2) #+
  #stat_summary(fun.y = Dif, geom = "line", col = "red") +
  #ylab(expression(sqrt(abs(" UP Spread "))))



portfolio_returns_data <-as.data.frame(df)

single_timeseries_plot = function(data,value,ylabel) {
  
  ggplot(data, aes(x=YEAR)) + 
    geom_line(aes(y = value), color = "darkred")+
    xlab("Year")+
    ylab(ylabel)
  
}
single_timeseries_plot(df1,df1$median,"UP median")
single_timeseries_plot(df2,df2$Dif,"UP Dif between MAX UP and Min UP")
single_timeseries_barplot = function(data,value,ylabel) {
  
  ggplot(data=df, aes(x=as.character(YEAR), y=value)) +
    geom_bar(stat="identity")+
    xlab("Year")+
    ylab(ylabel)
  
}
single_timeseries_barplot(df2,df2$Dif,"UP Dif between MAX UP and Min UP")

df$bookequity<- df$WC03501 + df$WC03263
df$returnonequity<- df$WC01551/df$bookequity
OP_BE = df$WC01250/(df$WC03501+df$WC03263)
dff3<-df
dff3$OP_BE<-OP_BE
df<-dff3
df4 <- df %>%
  group_by(pf.value)   %>%
  mutate( averageassetchange=mean(WC02999,by.x=c("Id","YEAR","month")),
          pct_change = ((WC02999/lag(WC02999) - 1) * 100),
          averageOP_BE=mean(OP_BE,by.x=c("Id","YEAR","month")),
          averagebookequity = mean(bookequity,by.x=c("Id","YEAR","month")),
          averagereturnonequity = mean(returnonequity,by.x=c("Id","YEAR","month")))
summary(df4$averageassetchange)
single_timeseries_plotpvalue = function(data,value,ylabel) {
  
  ggplot(data, aes(x=pf.value)) + 
    geom_line(aes(y = value), color = "darkred")+
    xlab("Year")+
    ylab(ylabel)
  
}
single_timeseries_barplotpvalue = function(data,value,ylabel) {
  
  ggplot(data=df, aes(x=as.character(pf.value), y=value)) +
    geom_bar(stat="identity")+
    xlab("Year")+
    ylab(ylabel)
  
}
summary(df4$averageassetchange)
single_timeseries_plotpvalue(df4,df4$averageassetchange,"Average Asset Change")
single_timeseries_barplotpvalue(df4,df4$averageassetchange,"Average Asset Change")
summary(df4$averagebookequity)
single_timeseries_plotpvalue(df4,df4$averagebookequity ,"Average Book Equity ")
single_timeseries_barplotpvalue(df4,df4$averagebookequity ,"Average Book Equity ")
summary(df4$averageOP_BE)
single_timeseries_plotpvalue(df4,df4$averageOP_BE,"Average OP_BE")
single_timeseries_barplotpvalue(df4,df4$averageOP_BE,"Average OP_BE")
summary(df4$averagereturnonequity)
single_timeseries_plotpvalue(df4,df4$averagereturnonequity,"Average Return on Equity")
single_timeseries_barplotpvalue(df4,df4$averagereturnonequity,"Average Return on Equity")


