#收益率走势图

#install.packages("devtools")
#library(devtools)
#install_github('qutke/qutke')

library('lubridate')
library(qutke)
key<-"faca4c8ff4dc1502d87944ea9cfd74f19918f0aef37fde6746b61d4d339bfcf3"
init(key)

#date <- Sys.Date()-1

#某只股票收益率 = （当天的股价-买入价）/ 买入价 *100
#需要参数：1、选定股票种类；2、买入的日期、买入价；3、买入之后到如今的日期，以及当天股价；4、收益率计算

#收益率是按单个股票来说的

#1、选定001979.SZ的招商蛇口，002142.SZ的宁波银行
#http://xueqiu.com/p/ZH078564
qtid <- c('002230.SZ','002715.SZ')
#qtid <- '002230.SZ'

#2、设定买入日期是2015-10-01，获取当天收盘价（假定买入价为收盘价）.均选为收盘价
date <- '2015-10-01'
#股票日间行情(前复权)
dailyQuote <- getDailyQuote(data='mktFwdDaily',qtid=qtid,startdate=date,enddate=Sys.Date(),key=key)
#当日收盘价(前复权)
stock1 <-dailyQuote[which(dailyQuote$qtid==qtid[1]),]
stock2 <-dailyQuote[which(dailyQuote$qtid==qtid[2]),]

#x轴，作为时间轴。取买入日期至今。
tradingDay <- getDate(data='tradingDay',startdate=stock1$date[1],enddate=Sys.Date(),key=key)

#归一化
#dat1 <- as.character(tradingDay[1])
i <- 1
length <- length(tradingDay)
fwdAdjClose1 <- c()
fwdAdjClose2 <- c()
while(i<=length){
  fwdAdjClose1 <- c(fwdAdjClose1,stock1[i,]$fwdAdjClose)
  if(is.null(fwdAdjClose1[i])||is.na(fwdAdjClose1[i])) 
    fwdAdjClose1[i] <- fwdAdjClose1[i-1]
  
  fwdAdjClose2 <- c(fwdAdjClose2,stock2[i,]$fwdAdjClose)
  if(is.null(fwdAdjClose2[i])||is.na(fwdAdjClose2[i])) 
    fwdAdjClose2[i] <- fwdAdjClose2[i-1]
  
  i=i+1
}
print(fwdAdjClose1[length])

#计算
fwdAdjClose1 <- (fwdAdjClose1-fwdAdjClose1[1])/fwdAdjClose1[1]*100
fwdAdjClose2 <- (fwdAdjClose2-fwdAdjClose2[1])/fwdAdjClose2[1]*100

#形成dataframe
yieldChart <- data.frame('date'=tradingDay,'2'=fwdAdjClose1,'3'=fwdAdjClose2)
postData(yieldChart,name='yieldChart',key=key)



#'001979.SZ'not find
#industry <- getIndustry(data='industryType',date='2016-01-08',qtid=c('001979.SZ','002142.SZ'),key=key)

#MD <- getMD(data='keyMap',qtid=qtid,key=key)

#getDailyQuote(data='mktFwdDaily',qtid=c('001979.SZ','002142.SZ'),key=key)

#getQtStock(data='financialIndex',qtid=qtid,key=key)

#即第一列应为日期向量，其他列为数值向量
#yieldChart <- data.frame('date'=c(as.Date('2016-1-1'),as.Date('2016-1-2'),as.Date('2016-1-3')),'2'=c(1,2,3),'3'=c(3,2,1))
#names(yieldChart)<-c('代码','名称','日期')

