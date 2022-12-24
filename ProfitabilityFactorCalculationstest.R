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
 df<-ProfitabilityFactorCalculations
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

 #df$WC02999<-as.numeric(WC02999)
 #timeseriesdata <- merge(WC02999 , WC01250, WC03501,UP,RET.USD,LMV.USD,WC01551,WC03263,book_market,OP_BE)
 #colnames(timeseriesdata) <- c("WC02999" , "WC01250", "WC03501","UP","RET.USD","LMV.USD","WC01551","WC03263","book_market","OP_BE")
 #test_plot<-df$UP
 
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
 
 df$bookequity<- df$WC03501 + df$WC03263
 df$returnonequity<- df$WC01551/df$bookequity
 df$OP_BE = df$WC01250/(df$WC03501+df$WC03263)
 dff3<-df
 dff3$OP_BE<-OP_BE
 df<-dff3
 df4 <- df %>%
   group_by(pf.value)   %>%
   mutate( averageassetchange=mean(WC02999,by.x=c("Id","YEAR","month")),
           averageOP_BE=mean(OP_BE,by.x=c("Id","YEAR","month")),
           averagebookequity = mean(bookequity,by.x=c("Id","YEAR","month")),
           averagereturnonequity = mean(returnonequity,by.x=c("Id","YEAR","month")))
 summary(df4)
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
 dfhigh<-data.frame(matrix(ncol = length(df4)))
 dflow<-data.frame(matrix(ncol = length(df4)))
 dfneutral<-data.frame(matrix(ncol = length(df4)))
 colnames(dfhigh)<-colnames(df4)
 colnames(dflow)<-colnames(df4)
 colnames(dfneutral)<-colnames(df4)
 dfhigh<-df4[which(df4[,"pf.value"]=="High"),]
 dflow<-df4[which(df4[,"pf.value"]=="Low"),]
 dfneutral<- df4[which(df4[,"pf.value"]=="Neutral"),]
 
 summary(df4$averageassetchange)
 single_timeseries_plotpvalue(df4,df4$averageassetchange,"Average Asset Change")
 single_timeseries_barplotpvalue(df4,df4$averageassetchange,"Average Asset Change")
 single_timeseries_barplotpvalue(df4,df4$averageassetchange,"Average Asset Change")
 single_timeseries_barplotpvalue(dfhigh,dfhigh$averageassetchange,"Average Asset Change For High Pf.Value")
 single_timeseries_barplotpvalue(dflow,dflow$averageassetchange,"Average Asset Change For Low Pf.Value")
 single_timeseries_barplotpvalue(dfneutral,dfneutral$averageassetchange,"Average Asset Change For Neutral Pf.Value")
 summary(df4$averagebookequity)
 single_timeseries_plotpvalue(df4,df4$averagebookequity ,"Average Book Equity ")
 single_timeseries_barplotpvalue(df4,df4$averagebookequity ,"Average Book Equity ")
 single_timeseries_barplotpvalue(dfhigh,dfhigh$averagebookequity,"Average Asset Change For High Pf.Value")
 single_timeseries_barplotpvalue(dflow,dflow$averagebookequity,"Average Asset Change For Low Pf.Value")
 single_timeseries_barplotpvalue(dfneutral,dfneutral$averagebookequity,"Average Asset Change For Neutral Pf.Value")
 
 summary(df4$averageOP_BE)
 single_timeseries_plotpvalue(df4,df4$averageOP_BE,"Average OP_BE")
 single_timeseries_barplotpvalue(df4,df4$averageOP_BE,"Average OP_BE")
 summary(df4$averagereturnonequity)
 single_timeseries_plotpvalue(df4,df4$averagereturnonequity,"Average Return on Equity")]-
  single_timeseries_barplotpvalue(df4,df4$averagereturnonequity,"Average Return on Equity")
