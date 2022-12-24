#install packages (collections of functions which allow more
#statistical techniques, and graphical devices)
#install.packages("quantmode")
#install.packages("fBasics")
#install.packages("moments")
#install.packages("PerformanceAnalytics")
#install.packages("normtest")
#install.packages("tseries")
#install.packages("roll")
#install.packages("xts")
#install.packages("quantmode")
library(quantmode)
library(fBasics)
library(moments)
library(PerformanceAnalytics)
library(normtest)
library(tseries)
library(roll)
library(xts)
load("data.RData")
#read data
data<-test
#Compute daily returns

data$WC02999<-Delt(x1 = data$WC02999,x2 = NULL,k = 1,type = "log")
data$WC01250<-Delt(x1 = data$WC01250,x2 = NULL,k = 1,type = "log")
data$WC01250<-Delt(x1 = data$WC03501,x2 = NULL,k = 1,type = "log")
data$WC01250<-Delt(x1 = data$UP,x2 = NULL,k = 1,type = "log")
data$WC01250<-Delt(x1 = data$RET.USD,x2 = NULL,k = 1,type = "log")
data$WC01250<-Delt(x1 = data$LMV.USD,x2 = NULL,k = 1,type = "log")
data$WC01250<-Delt(x1 = data$WC01551,x2 = NULL,k = 1,type = "log")
data$WC01250<-Delt(x1 = data$WC03263,x2 = NULL,k = 1,type = "log")
data$WC01250<-Delt(x1 = data$book_market,x2 = NULL,k = 1,type = "log")
#data$WC01250<-Delt(x1 = data$pf.size,x2 = NULL,k = 1,type = "log")
#data$WC01250<-Delt(x1 = data$pf.value,x2 = NULL,k = 1,type = "log")
#data$WC01250<-Delt(x1 = data$SIZE_VALUE,x2 = NULL,k = 1,type = "log")
datacolm<- data$WC02999[!is.na(data$WC02999)]
datacolm<-datacolm[!is.na(datacolm)]
summary(datacolm)
meandata <- mean(datacolm) 
#risk_factor[j] <- risk_factor[j] %>% replace_na(meandata)
data$WC02999[is.nan(data$WC02999)]<-meandata
data$WC02999[is.na(data$WC02999)]<-meandata
#dbcol=length(data)
#for(j in 1:dbcol){
#  meandata <- mean(na.omit(data[j]) )
  #risk_factor[j] <- risk_factor[j] %>% replace_na(meandata)
#  data[is.nan(data)]<-meandata
#  data[is.na(data)]<-meandata
#}
data
#data$Rf<-log(1+data$RFR/250)
#Clean up data object
#suppress 1st row
#data<-data[-1,]
#suppress colums 2,3,4 (we only keep colums 1, 5 to 7)
data<-data[,c(6:14)]

library(fBasics)
basicStats(data)
require(xts)
data.WC02999.rtns <- xts(rnorm(data$WC02999), data[,2])
Y.stock.rtns <- xts(rnorm(1000,0.00003,0.0004), as.Date(data[,2]))
Z.stock.rtns <- xts(rnorm(1000,0.00005,0.0005), Sas.Date(data[,2]))
rtn.obj <- merge(X.stock.rtns , Y.stock.rtns, Z.stock.rtns)
colnames(rtn.obj) <- c("x.stock.rtns","y.stock.rtns","z.stock.rtns")

  
  require(PerformanceAnalytics)
charts.PerformanceSummary(rtn.obj, geometric=TRUE)

yMean.Ri<-mean(data$WC02999)*252
#yMean.Rm<-mean(data$Rm)*252
ySD.Ri<-sd(data$WC02999)*sqrt(252)
#ySD.Rm<-sd(data$Rm)*sqrt(252)

library(moments)
kurtosis(data$WC02999)
mean(((data$WC02999-mean(data$WC02999))/sd(data$WC02999))^4)
library(PerformanceAnalytics)
kurtosis(data$Ri, na.rm = FALSE, method = "excess")
kurtosis(data$Ri, na.rm = FALSE, method = "moment")

library(moments)
skewness(data$Ri)
mean(((data$Ri-mean(data$Ri))/sd(data$Ri))^3)
library(PerformanceAnalytics)
skewness(data$Ri, na.rm = FALSE)

library(moments)
skewness(data$Ri)
mean(((data$Ri-mean(data$Ri))/sd(data$Ri))^3)
library(PerformanceAnalytics)
skewness(data$Ri, na.rm = FALSE)

hist(data$Ri,main = "Histograms of Ri and Rm", breaks = 100, freq =
       FALSE, xlab = "Ri", xlim = c(min(data$Ri,data$Rm),
                                    max(data$Ri,data$Rm)), col = "green",axes = F)
axis(1, pos = 0, cex.axis = 0.8)
axis(2,pos = 0,cex.axis = 0.8,las = 2)
hist(data$Rm,breaks = 50, freq = FALSE, xlim =c(min(data$Ri,data$Rm), max(data$Ri,data$Rm)), col = "red",,axes =F,add = TRUE)
curve(dnorm(x, mean(data$Ri), sd(data$Ri)), xlim = c(min(data$Ri),
                                                     max(data$Ri)), lwd = 2,col = "grey", add = TRUE)
curve(dnorm(x, mean(data$Rm), sd(data$Rm)), xlim = c(min(data$Rm),
                                                     max(data$Rm)), lwd = 2,col = "red", add = TRUE)
legend("topleft",c("Histogram of Ri","Histogram of Rm"),lty =
         c(1,1),col = c("green","red"),bty = "n")

(min(data$Ri)-mean(data$Ri))/sd(data$Ri)
pnorm(q = (min(data$Ri)-mean(data$Ri))/sd(data$Ri),mean = 0,sd =
        1, lower.tail = TRUE, log.p = FALSE)
format(x = pnorm(q = -4.376432,mean = 0,sd = 1, lower.tail =
                   TRUE,log.p = FALSE),scientific = FALSE, digits = 10)
format(x = 1/(pnorm(q = -4.376432, mean = 0, sd = 1,TRUE,
                    FALSE)*nrow(data$Ri)), scientific = FALSE, digits = 10)
