library(PerformanceAnalytics)
library(readxl)
library(ggplot2)
require(xts)
#load("data.RData")

WC02999 <- xts(test$WC02999,test$Date)
WC01250 <- xts(test$WC01250,test$Date)
WC03501 <- xts(test$WC03501, test$Date)
UP <- xts(test$UP,test$Date)
RET.USD<- xts(test$RET.USD,test$Date)
LMV.USD <- xts(test$LMV.USD, test$Date)
WC01551<- xts(test$WC01551, test$Date) 
WC03263<- xts(test$WC03263, test$Date)
book_market<- xts(test$book_market, test$Date) 
#pf.size<- xts(test$pf.size, test$Date) 
#pf.value<- xts(test$pf.value, test$Date) 
#SIZE_VALUE<- xts(test$SIZE_VALUE, test$Date) 
rtn.obj <- merge(WC02999 , WC01250, WC03501,UP,RET.USD,LMV.USD,WC01551,WC03263,book_market)
colnames(rtn.obj) <- c("WC02999" , "WC01250", "WC03501","UP","RET.USD","LMV.USD","WC01551","WC03263","book_market")
require(PerformanceAnalytics)
charts.PerformanceSummary(rtn.obj, geometric=TRUE)

gg.charts.PerformanceSummary <- function(rtn.obj, geometric=TRUE, main="",plot=TRUE){
  
  # load libraries
  suppressPackageStartupMessages(require(ggplot2))
  suppressPackageStartupMessages(require(scales))
  suppressPackageStartupMessages(require(reshape))
  suppressPackageStartupMessages(require(PerformanceAnalytics))
  # create function to clean returns if having NAs in data
  clean.rtn.xts <- function(univ.rtn.xts.obj,na.replace=0){
    univ.rtn.xts.obj[is.na(univ.rtn.xts.obj)]<- na.replace
    univ.rtn.xts.obj
  }
  # Create cumulative return function
  cum.rtn <- function(clean.xts.obj, g=TRUE){
    x <- clean.xts.obj
    if(g==TRUE){y <- cumprod(x+1)-1} else {y <- cumsum(x)}
    y
  }
  # Create function to calculate drawdowns
  dd.xts <- function(clean.xts.obj, g=TRUE){
    x <- clean.xts.obj
    if(g==TRUE){y <- Drawdowns(x)} else {y <- Drawdowns(x,geometric=FALSE)}
    y
  }
  # create a function to create a dataframe to be usable in ggplot to replicate charts.PerformanceSummary
  cps.df <- function(xts.obj,geometric){
    x <- clean.rtn.xts(xts.obj)
    series.name <- colnames(xts.obj)[1]
    tmp <- cum.rtn(x,geometric)
    tmp$rtn <- x
    tmp$dd <- dd.xts(x,geometric)
    colnames(tmp) <- c("Cumulative_Return","Daily_Return","Drawdown")
    tmp.df <- as.data.frame(coredata(tmp))
    tmp.df$Date <- as.POSIXct(index(tmp))
    tmp.df.long <- melt(tmp.df,id.var="Date")
    tmp.df.long$asset <- rep(series.name,nrow(tmp.df.long))
    tmp.df.long
  }
  # A conditional statement altering the plot according to the number of assets
  if(ncol(rtn.obj)==1){
    # using the cps.df function
    df <- cps.df(rtn.obj,geometric)
    # adding in a title string if need be
    if(main==""){
      title.string <- paste0(df$asset[1]," Performance")
    } else {
      title.string <- main
    }
    # generating the ggplot output with all the added extras....
    gg.xts <- ggplot(df, aes_string(x="Date",y="value",group="variable"))+
      facet_grid(variable ~ ., scales="free", space="free")+
      geom_line(data=subset(df,variable=="Cumulative_Return"))+
      geom_bar(data=subset(df,variable=="Daily_Return"),stat="identity")+
      geom_line(data=subset(df,variable=="Drawdown"))+
      ylab("")+
      geom_abline(intercept=0,slope=0,alpha=0.3)+
      ggtitle(title.string)+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      scale_x_datetime(breaks = date_breaks("6 months"), labels = date_format("%d/%m/%Y"))
    
  } else {
    # a few extra bits to deal with the added rtn columns
    no.of.assets <- ncol(rtn.obj)
    asset.names <- colnames(rtn.obj)
    df <- do.call(rbind,lapply(1:no.of.assets, function(x){cps.df(rtn.obj[,x],geometric)}))
    df$asset <- ordered(df$asset, levels=asset.names)
    if(main==""){
      title.string <- paste0(df$asset[1]," Performance")
    } else {
      title.string <- main
    }
    if(no.of.assets>5){legend.rows <- 5} else {legend.rows <- no.of.assets}
    gg.xts <- ggplot(df, aes_string(x="Date", y="value",group="asset"))+
      facet_grid(variable~.,scales="free",space="free")+
      geom_line(data=subset(df,variable=="Cumulative_Return"),aes(colour=factor(asset)))+
      geom_bar(data=subset(df,variable=="Daily_Return"),stat="identity",aes(fill=factor(asset),colour=factor(asset)),position="dodge")+
      geom_line(data=subset(df,variable=="Drawdown"),aes(colour=factor(asset)))+
      ylab("")+
      geom_abline(intercept=0,slope=0,alpha=0.3)+
      ggtitle(title.string)+
      theme(legend.title=element_blank(), legend.position=c(0,1), legend.justification=c(0,1),
            axis.text.x = element_text(angle = 45, hjust = 1))+
      guides(col=guide_legend(nrow=legend.rows))+
      scale_x_datetime(breaks = date_breaks("6 months"), labels = date_format("%d/%m/%Y"))
    
  }
  
  assign("gg.xts", gg.xts,envir=.GlobalEnv)
  if(plot==TRUE){
    plot(gg.xts)
  } else {}
  
}
# seeing the ggplot equivalent....
gg.charts.PerformanceSummary(rtn.obj, geometric=TRUE)