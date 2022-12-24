library(readxl)
library(ggplot2)
library(PerformanceAnalytics)
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
portfolio_returns$YEAR<-as.numeric(portfolio_returns$YEAR)
portfolio_returns_data <-as.data.frame(portfolio_returns)

single_timeseries_plot = function(data,value,ylabel) {
  
  ggplot(data, aes(x=YEAR)) + 
    geom_line(aes(y = value), color = "darkred")+
    xlab("Year")+
  ylab(ylabel)
  
}
single_timeseries_plot(portfolio_returns_data ,portfolio_returns_data$Big.High,"Big.High")
single_timeseries_plot(portfolio_returns_data ,portfolio_returns_data$Big.Low,"Big.Low")
single_timeseries_plot(portfolio_returns_data ,portfolio_returns_data$Big.Neutral,"Big.Neutral")
single_timeseries_plot(portfolio_returns_data ,portfolio_returns_data$Small.High,"Small.High")
single_timeseries_plot(portfolio_returns_data ,portfolio_returns_data$Small.Low,"Small.Low")
single_timeseries_plot(portfolio_returns_data ,portfolio_returns_data$Small.Neutral,"Small.Neutral")
single_timeseries_plot(portfolio_returns_data ,portfolio_returns_data$Small,"Small")
single_timeseries_plot(portfolio_returns_data ,portfolio_returns_data$Big,"Big")
single_timeseries_plot(portfolio_returns_data ,portfolio_returns_data$SMB,"SMB")
single_timeseries_plot(portfolio_returns_data ,portfolio_returns_data$High,"High")
single_timeseries_plot(portfolio_returns_data ,portfolio_returns_data$Low,"Low")
single_timeseries_plot(portfolio_returns_data ,portfolio_returns_data$HML,"HML")
Multiple_timeseries_plot = function(dataset) {
  # Multiple line plot
  # name<- colnames(dataset)[2:7]
  ggplot(dataset, aes(x=YEAR)) + 
    geom_line(aes(y = Big.High), color = "darkred") +
    geom_line(aes(y = Big.Low), color = "red") +
    geom_line(aes(y = Big.Neutral), color = "blue") +
    geom_line(aes(y = Small.Low), color = "green") + 
    geom_line(aes(y = Small.Neutral), color="steelblue") +
    geom_line(aes(y = Small), color = "orange") +
    geom_line(aes(y = Big), color = "blue") +
    geom_line(aes(y = SMB), color = "purple") +
    geom_line(aes(y = High), color = "pink") +
    geom_line(aes(y = Low), color = "grey") +
    geom_line(aes(y = HML), color = "brown") +
    scale_colour_hue(l=25)+ 
    scale_linetype_manual(values = c('Big.High' = 1,'Big.Low' = 2,'Big.Neutral'=3,'Small.Low'=4,'Small.Neutral'=5,'Small'=6,'Big'=7,'SMB'=8,'High'=9,'Low'=10,'HML'=11)) #+
  #geom_point(aes(group=variable, color=variable, size=6))
}

Multiple_timeseries_plot(portfolio_returns_data)
portfolio_returns$YEAR<-as.character(portfolio_returns$YEAR)
a<-sprintf("%i-01-01",portfolio_returns$YEAR)

portfolio_returns$Date<-as.Date(a)
portfolio_returns_data <-as.data.frame(portfolio_returns)

Big.High <- xts(portfolio_returns_data$Big.High,portfolio_returns_data$Date)
Big.Low <- xts(portfolio_returns_data$Big.Low,portfolio_returns_data$Date)
Big.Neutral <- xts(portfolio_returns_data$Big.Neutral,portfolio_returns_data$Date)
Small.High<- xts(portfolio_returns_data$Small.High,portfolio_returns_data$Date)
Small.Low <- xts(portfolio_returns_data$Small.Low,portfolio_returns_data$Date)
Small.Neutral <- xts(portfolio_returns_data$Small.Neutral,portfolio_returns_data$Date)
Small <- xts(portfolio_returns_data$Small,portfolio_returns_data$Date)
Big <- xts(portfolio_returns_data$Big,portfolio_returns_data$Date)
SMB <- xts(portfolio_returns_data$SMB,portfolio_returns_data$Date)
High <- xts(portfolio_returns_data$High,portfolio_returns_data$Date)
Low <- xts(portfolio_returns_data$Low,portfolio_returns_data$Date)
HML<- xts(portfolio_returns_data$HML,portfolio_returns_data$Date)

Portfolio_Return <- merge(Big.High , Big.Low, Big.Neutral,Small.High,Small.Low,Small.Neutral,Small,Big,SMB,High,Low,HML)
colnames(Portfolio_Return ) <- c("Big.High" , "Big.Low", "Big.Neutral","Small.High","Small.Low","Small.Neutral","Small","Big","SMB","High","Low","HML")
charts.PerformanceSummary(Portfolio_Return, main = 'Portfolio Performance Summary')
