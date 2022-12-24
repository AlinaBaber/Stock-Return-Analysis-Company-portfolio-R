library(readxl)
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
Model <- read_excel("Model Data.xlsx")

single_timeseries_plot = function(data,value, Date,ylabel) {
  ggplot(data, aes(x=Date)) + 
    geom_line(aes(y = value), color = "darkred")+
    xlab("Year")+
    ylab(ylabel)
  
}
single_timeseries_plot(Model,Model$`Mkt-RF (KF)`,Model$Year,"`Mkt-RF (KF)`")
single_timeseries_plot(Model,Model$`HML (KF)`, Model$Year,"`HML (KF)`")
single_timeseries_plot(Model,Model$`SMB (KF)`, Model$Year,"SMB (KF)`")
single_timeseries_plot(Model,Model$RMRF, Model$Year,"RMRF")
single_timeseries_plot(Model,Model$SMB, Model$Year,"SMB")
single_timeseries_plot(Model,Model$HML, Model$Year,"HML")

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
    scale_linetype_manual(values = c('Mkt-RF (KF)' = 1,'HML (KF)' = 2,'SMB (KF)'=3,'RMRF'=4,'SMB'=5,'HML'=6)) #+
    #geom_point(aes(group=variable, color=variable, size=6))
}

colnames(Model)[2] <- "MktRfKF"
colnames(Model)[3] <- "HMLKF"
colnames(Model)[4]<- "SMBKF"
Model
#variable<- c("`Mkt-RF (KF)`", "`HML (KF)`", "`SMB (KF)`","RMRF","SMB","HML")
Multiple_timeseries_plot(Model,Model$Year)
#Model$Year<-as.character(Model$Year)
a<-sprintf("%s-01-01",Model$Year)

Model$Date<-as.Date(a)
Model_data <-as.data.frame(Model)

MktRfKF <- xts(Model_data$MktRfKF,Model_data$Date)
HMLKF <- xts(Model_data$HMLKF,Model_data$Date)
SMBKF <- xts(Model_data$SMBKF,Model_data$Date)
RMRF<- xts(Model_data$RMRF,Model_data$Date)
SMB <- xts(Model_data$SMB,Model_data$Date)
HML <- xts(Model_data$HML,Model_data$Date)


Model_ts <- merge(MktRfKF,HMLKF,SMBKF,RMRF,SMB,HML)
colnames(Model_ts ) <- c("MktRfKF","HMLKF","SMBKF","RMRF","SMB","HML")
charts.PerformanceSummary(Model_ts, main = 'Portfolio Performance Summary')

