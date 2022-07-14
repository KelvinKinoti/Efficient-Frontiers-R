library(fPortfolio)
library(timeSeries)
library(quantmod)
library(dplyr)
library(PerformanceAnalytics)
library(ggplot2)
library(lubridate)
library(reshape2)
library(ggplot2)
library(psych)

# LOading and data processing 
stocks<-read.csv("C:/Users/hp/Desktop/New folder/Index Data.csv")
stocks<-stocks[,7:15]
stocks[,1] <-seq(as.Date("2007/01/28"), by = "month", length.out = 169)
colnames(stocks)[1]<-"Dates"
stocksxts<-as.xts(stocks[,-1],order.by =stocks$Dates)
stock.returns<-Return.calculate(as.numeric(stocksxts,drop=FALSE))
stock.returns.y<-to.yearly(stock.returns,indexAt='yearmon',drop.time=TRUE)
stock<-stock.returns.y
direct.estate<-read.csv("C:/Users/hp/Desktop/New folder/Direct Real Estate.csv")
direct.estate<-direct.estate[c(27:82),-2]
direct.estate<-direct.estate[,c(1,2,5,6,9,10,14)]
direct.estate[,1] <-seq(as.Date("2007/03/28"), by = "3 month", length.out = 56)
colnames(direct.estate)[1]<-"DATE"

direct.estatexts<-as.xts(direct.estate[,-1],order.by= direct.estate$DATE)
direct.returns<-Return.calculate(as.numeric(direct.estatexts,drop=FALSE))
direct.returns.y<-to.yearly(direct.returns,indexAt='yearmon',drop.time=TRUE)
direct<-direct.returns.y
return.merge<-merge(direct,stock[-15,])
return.mat<-as.matrix(merge(direct,stock[-15,]))
correlation.matrix<-cor(return.mat)#Portfolio A correlation matrix
plot(correlation.matrix)
meltmat<-melt(correlation.matrix)
ggplot(data=meltmat,
       aes(x=Var1,y=Var2,fill=value))+geom_point(alpha=0.7)
portfolioA.returns<-Return.portfolio(return.merge,rebalance_on = "year")#stocks and Direct real estate portfolio
txt<-write.table(as.data.frame(portfolioA.returns),"C:/Users/hp/Desktop/New folder/PORTReturnsA.txt",sep = ",")
plot(portfolioA.returns)
return.mat=as.timeSeries(return.mat)
efficient.frontier=portfolioFrontier(return.mat,constraints="LongOnly")
#efficient frontier
#1:Eficient frontier
#2:Global minimum variance portfolio
#3:Tangent(Optimal) Portfolio
#4:Risk/Return of each asset
#4:Equal weights Portfolios
#5:Two Assets frontier
#7:Monte Carlo Portfolio
#Sharpe Ratio
plot(efficient.frontier,c(1))
plot(efficient.frontier,c(1,2,3,7,8))#Efficient Frontier for Portfolio A


REITs<-read.csv("C:/Users/hp/Desktop/New folder/GPR 250 REIT Country index.csv")
REITs$China[is.na(REITs$China)]<-mean(REITs$China, na.rm=TRUE)
REITs$Germany[is.na(REITs$Germany)]<-mean(REITs$Germany, na.rm=TRUE)
REITs<-REITs[c(1947:5600) ,]
REITs<-REITs[,c(1,3,6,8,13,18,19)]
Date<-REITs[,1] <-seq(as.Date("2007/01/01"), by = "2 day", length.out = 3654)
REITs<-REITs[c(1:2557) ,]
Date<-REITs$Date
REITsxts<-as.xts((REITs[,-1]), order.by=Date)
REITs.returns<-Return.calculate(as.numeric(REITsxts,drop=FALSE))
REITs.returns.y<-to.yearly(REITs.returns, indexAt='yearmon',drop.time=TRUE)
reit.mergeB<-merge(REITs.returns.y,stock.returns.y[-15,])
return.matB<-as.matrix(reit.mergeB)
cor.matrixB<-cor(return.matB)#Portfolio A correlation matrix
plot(cor.matrixB)
meltmatB<-melt(cor.matrixB)
ggplot(data=meltmatB,
       aes(x=Var1,y=Var2,fill=value))+geom_tile()
portfolioB.returns<-Return.portfolio(reit.mergeB,rebalance_on = "year")#stocks and Direct real estate portfolio
plot(portfolioB.returns )
txt1<-write.table(as.data.frame(portfolioB.returns),"C:/Users/hp/Desktop/New folder/PORTReturnsB.txt",sep = ",")
return.matB<-as.timeSeries(return.matB)
efficient.frontierB=portfolioFrontier(return.matB,constraints="LongOnly")

plot(efficient.frontier,c(1,2,3,4))
plot(efficient.frontierB,c(1,2,3,7,8))#Efficient Frontier for Portfolio B


#Apply the Jennrich test

cortest.jennrich(return.mat[,c(1:4)], return.mat[,c(5:8)])
cortest.jennrich(return.matB[,c(1:4)], return.matB[,c(5:8)])
