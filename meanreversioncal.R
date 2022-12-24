library(repmis)
library(zoo)
library(xts)
library(tseries)
library(dplyr)
library(plotrix)
library(ggplot2)


# UrlAddress <- paste0("https://github.com/cablegui/MeanReversion/blob/master/data/",
# "d440c333100d5a9592c0e1485107a0eb56a405a4",
# "/nym_ngatimeseries.csv")

#UrlAddress <- paste0("https://raw.githubusercontent.com/cablegui/MeanReversion/master/data/",
#                     "nym_ngatimeseries.csv")


#Download data using repmis package
#nymexTS <- repmis::source_data(UrlAddress, sep = ",", header = TRUE, fill =  TRUE) 

nymexTS<-test
#Display column names
colnames(nymexTS)

#Search for duplicate date entries
freq <- as.data.frame(table(nymexTS$Date))

#Catch duplicate time series where number of data is > 2
duplTS <- freq[which(freq$Freq >= 2),]
TS_clean <- nymexTS[!nymexTS$Date %in% as.vector(duplTS$Var1),]

#Replace NA's in the data with the data as of t-1
TS_clean <- na.locf(TS_clean)

TS_clean <- zoo(data.frame(apply(TS_clean[,2:ncol(TS_clean)],2,as.numeric)),as.Date(TS_clean[,1],"%d/%m/%Y"))

#Check if there are NA's present
which(is.na(TS_clean))


#Calculate logReturns
logReturns31day <- diff(log(TS_clean$NYM_NGA31D))

#Remove innovations which are 3 std away 
EventStd31day <- 3 * sd(coredata(logReturns31day))

# Time series where data cleaned of jump events. Removing outlier time series
TS_clean_jumps31day <- as.xts(TS_clean$NYM_NGA31D[which(abs(logReturns31day) < EventStd31day) + 1])

#Run adf test. Null hypothesis is non stationary
adfTest31day <- adf.test(na.omit(coredata(TS_clean_jumps31day)))
p_val31day <- adfTest$p.value
p_val31day

#Run adf test on log time series. Null hypothesis is non stationary
adfTest31dayLog <- adf.test(na.omit(coredata(log(TS_clean_jumps31day))))
p_val31dayLog <- adfTest$p.value
p_val31dayLog


Years<-unique(df$YEAR)
ncoll = length(Years)+1
nrows = length(Ids)
stockcategory<-data.frame(matrix(nrow=nrows,ncol=ncoll))
colnames(stockcategory)<-c("Id",as.character(Years))
df6<-arrange(df, desc(na.omit(df$id)), by.x=c("YEAR"))
Ids<- unique(df6$Id)
temp<-data.frame()
stockcategory$Id<-as.factor(Ids)
for(i in 1:nrows){
  temp<-df6[which(as.character(df6$Id)==as.character(stockcategory[i,1])),]
  for(j in 1:length(Years)){
    stockcategory[i,j+1]<- temp[which(as.integer(temp$YEAR)==Years[j]),17]
  }
}
dflow<-df4[which(df4[,16]=="Low"),]
dfneutral<- df4[which(df4[,16]=="Neutral"),]
df5 <- df %>%
  group_by(YEAR)   %>%
  mutate( averagebook_market=mean(df$book_market,by.x=c("Id","SIZE_VALUE")))
arrange(df, desc(na.omit(df$book_market)), by.x=c("YEAR"))
stock <- df %>% # this operator nests functions
  group_by(ID, YEAR) %>% # vw returns using lagged mcap
  summarize(ret.port = weighted.mean(RET.USD,
                                     LMV.USD)) %>% # vw returns using lagged mcap
  spread(YEAR,SIZE_VALUE) %>% # create one column for each group
  mutate(
    Small = (Small.High + Small.Neutral + Small.Low)/3, # just exemplary
    Big = (Big.High + Big.Neutral + Big.Low)/3,
    SMB = Small-Big,
    High = (Small.High + Big.High)/2,
    Low = (Small.Low + Big.Low)/2,
    HML = High-Low
  )
portfolio_returns <- test %>% # this operator nests functions
  group_by(Id, YEAR) %>% # do "everything" for the groups specified here
  summarize(SIZE_VALUE <-SIZE_VALUE) %>% # vw returns using lagged mcap
  spread(YEAR,SIZE_VALUE) %>% # create one column for each group
  mutate(
    Small = (Small.High + Small.Neutral + Small.Low)/3, # just exemplary
    Big = (Big.High + Big.Neutral + Big.Low)/3,
    SMB = Small-Big,
    High = (Small.High + Big.High)/2,
    Low = (Small.Low + Big.Low)/2,
    HML = High-Low
  )


portfolio_returns <- as.data.table(portfolio_returns)


datacolmnan<- df$returnonequity[!is.na(df$returnonequity)]
datacolm<-datacolmnan[!is.na(datacolmnan)]
meandata <- mean(datacolm)
df$returnonequity[is.nan(df$returnonequity)]<-meandata
df$returnonequity[is.na(df$returnonequity)]<-meandata

datacolmnan<- df$bookequity[!is.na(df$bookequity)]
datacolm<-datacolmnan[!is.na(datacolmnan)]
meandata <- mean(datacolm)
df$bookequity[is.nan(df$bookequity)]<-meandata
df$bookequity[is.na(df$bookequity)]<-meandata

datacolmnan<- df$grossprofits[!is.na(df$grossprofits)]
datacolm<-datacolmnan[!is.na(datacolmnan)]
meandata <- mean(datacolm)
df$grossprofits[is.nan(df$grossprofits)]<-meandata
df$grossprofits[is.na(df$grossprofits)]<-meandata

datacolmnan<- df$OP_BE[!is.na(df$OP_BE)]
datacolm<-datacolmnan[!is.na(datacolmnan)]
meandata <- mean(datacolm)
df$OP_BE[is.nan(df$OP_BE)]<-meandata
df$OP_BE[is.na(df$OP_BE)]<-meandata

datacolmnan<- df$WC02999[!is.na(df$WC02999)]
datacolm<-datacolmnan[!is.na(datacolmnan)]
meandata <- mean(datacolm)
df$WC02999[is.nan(df$WC02999)]<-meandata
df$WC02999[is.na(df$WC02999)]<-meandata

df<-na.omit(df)
data<-df[,c("returnonequity" , "bookequity", "grossprofits","OP_BE","WC02999")]
library(fBasics)
basicStats(data)

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