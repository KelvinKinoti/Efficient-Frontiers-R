library(psych)
library(PerformanceAnalytics)
library(kableExtra)
#Data loading and processing
stock<-read.csv("C:/Users/hp/Downloads/StockReturns.csv")
stock$Australian.All.Ordinary.Index<-gsub("%","",stock$Australian.All.Ordinary.Index)
stock$Hang.Seng.Index<-gsub("%","",stock$Hang.Seng.Index)
stock$Nikkei.225.Index<-gsub("%","",stock$Nikkei.225.Index)
stock$Amsterdam.AEX...Index<-gsub("%","",stock$Amsterdam.AEX...Index)
stock$FTSE.100.Index<-gsub("%","",stock$FTSE.100.Index)
stock$S.P.500...Index<-gsub("%","",stock$S.P.500...Index)
stock$BSE.500.Index<-gsub("%","",stock$BSE.500.Index)
stock[2:8]<-apply(stock[2:8],2,as.numeric)
stock<-na.omit(stock[,-1])
Date1<-seq(as.Date("2007/03/28"), by = "3 month", length.out =56 )
rownames(stock)<-Date1

REIT<-read.csv("C:/Users/hp/Downloads/REIT_Returns.csv")
REIT$Australia.GPR.250.AUS...GPAULOC<-gsub("%","",REIT$Australia.GPR.250.AUS...GPAULOC)
REIT$Hong.Kong.GPR.250.HK...GPHKLOC<-gsub("%","",REIT$Hong.Kong.GPR.250.HK...GPHKLOC)
REIT$Japan.GPR.250.JP...GPJPLOC<-gsub("%","",REIT$Japan.GPR.250.JP...GPJPLOC)
REIT$Netherlands.GPR.250.NL...GPNLLOC<-gsub("%","",REIT$Netherlands.GPR.250.NL...GPNLLOC)
REIT$UK.S.P.Property...SBBCUKPROP04<-gsub("%","",REIT$UK.S.P.Property...SBBCUKPROP04)
REIT$US.GPR.250...GPUSLOC<-gsub("%","",REIT$US.GPR.250...GPUSLOC)
REIT$India.S.P.Property...INSBBCINPROP<-gsub("%","",REIT$India.S.P.Property...INSBBCINPROP)
REIT[2:8]<-apply(REIT[2:8],2,as.numeric)
REIT<-na.omit(REIT[,-1])
Date1<-seq(as.Date("2007/03/28"), by = "3 month", length.out =56 )
rownames(REIT)<-Date1


direct<-read.csv("C:/Users/hp/Downloads/DirectReturns.csv")
colnames(direct)<-direct[1,]
direct<-direct[-1,-9]
direct$`Australia National Commercial`<-gsub("%","",direct$`Australia National Commercial`)
direct$`Hong Kong National Commercial`<-gsub("%","",direct$`Hong Kong National Commercial`)
direct$`Japan National Commercial`<-gsub("%","",direct$`Japan National Commercial`)
direct$`Netherlands National Commercial`<-gsub("%","",direct$`Netherlands National Commercial`)
direct$`United Kingdom National Commercial`<-gsub("%","",direct$`United Kingdom National Commercial`)
direct$`United States National Commercial`<-gsub("%","",direct$`United States National Commercial`)
direct$`India National Office`<-gsub("%","",direct$`India National Office`)
direct[2:8]<-apply(direct[2:8],2,as.numeric)
direct<-na.omit(direct[,-1])
rownames(direct)<-Date1

#Jennrich Test
Stock_Vs_REIT<-cortest.jennrich(stock,REIT)
Stock_Vs_Direct<-cortest.jennrich(stock,direct)
rbind(Stock_Vs_REIT,Stock_Vs_Direct)

#Sharpe Ratio
View(stock)
a<-SharpeRatio(timeSeries(stock),FUN = "StdDev")
colnames(a)<-c("Australian","Hang.Seng","Nikkei.225","Amsterdam.AEX","FTSE.100","S.P.500","BSE.500")
kable(a,caption = "Sharpe Ratio for Stocks")%>%
  kable_styling(bootstrap_options = "striped")
b<-SharpeRatio(timeSeries(REIT),FUN = "StdDev")
colnames(b)<-c("Australian","Hong.Kong","Japan","Netherlands","UK","US","India")
kable(b,caption = "Sharpe Ratio for REITs")%>%
  kable_styling(bootstrap_options = "striped")
c<-SharpeRatio(timeSeries(direct),FUN = "StdDev")
kable(c,caption = "Sharpe Ratio for Direct Real Estate")%>%
  kable_styling(bootstrap_options = "striped")
