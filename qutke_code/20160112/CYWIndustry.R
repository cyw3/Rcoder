library('lubridate')

library(qutke)
key<-"faca4c8ff4dc1502d87944ea9cfd74f19918f0aef37fde6746b61d4d339bfcf3"
init(key)

date <- Sys.Date()-1

#进行筛选,判断其SW1
sw1 <- '商业贸易'

#使用循环，对所有qtid进行计算
industry <- getIndustry(data='industryType',date=date,SW1=sw1,key=key)
qtid <- industry$qtid

#YalesonChan test c()
#qtid <- c('000001.SZ','000002.SZ')

#获取交易日期
lastYearDate <- as.Date(paste(year(date)-1,month(date),day(date),sep='-'))
tradingDay <- getDate(data='tradingDay',startdate=lastYearDate,enddate=date,key=key)
length <- length(tradingDay)

FirstDay <- as.Date(paste(year(date)-1,'1','1',sep='-'))
FirstDay <- (getDate(data='tradingDay',startdate=FirstDay,enddate=date,key=key))[1]

mktDaily <- getDailyQuote(data='mktDaily',qtid = qtid,startdate=date,enddate=date,key=key)

qtid <- mktDaily$qtid

#SecuAbbr
SecuAbbr <- (getIndustry(data='industryType',qtid = qtid,date=date,SW1=sw1,key=key))$SecuAbbr

#当日收盘价
close<- mktDaily$close
#当日交易量
volume <- mktDaily$volume/10000
#上一交易日的收盘价
prevClose <- mktDaily$prevClose
#当日涨跌幅
quoteChangeDaily <- (close-prevClose)/prevClose*100

#先用最简单的日期计算方法
mktWeek <- getDailyQuote(data='mktDaily',qtid = qtid,startdate=tradingDay[length-5],enddate=tradingDay[length-5],key=key)
mktMonth <- getDailyQuote(data='mktDaily',qtid = qtid,startdate=tradingDay[length-20],enddate=tradingDay[length-20],key=key)
mktYear <- getDailyQuote(data='mktDaily',qtid = qtid,startdate=tradingDay[1],enddate=tradingDay[1],key=key)

mktFYear <- getDailyQuote(data='mktDaily',qtid = qtid,startdate=FirstDay,enddate=FirstDay,key=key)

#近一周的涨跌幅（相邻的5个交易日）
quoteChangeWeek <- (close-mktWeek$close)/mktWeek$close*100
quoteChangeMonth <- (close-mktMonth$close)/mktMonth$close*100
quoteChangeYear <- (close-mktYear$close)/mktYear$close*100
quoteChangeFYear <- (close-mktFYear$close)/mktFYear$close*100


#test<-c(1:length(qtid))

#'代码'=qtid,'名称'=industry$ChiName,'日期'=as.qtDate(industry$date),as.numeric(close)
stock1 <- data.frame('1'=qtid,'2'=SecuAbbr,'3'=paste(year(date),month(date),day(date),sep='-'),'4'=close,
                     '5'=quoteChangeDaily,'6'=volume,'7'=quoteChangeWeek,
                     '8'=quoteChangeMonth,'9'=quoteChangeYear,
                     '10'=quoteChangeFYear,'11'=sw1)

#stock1 <- data.frame('1'=c(2.1,1.1,3.4),'2'=c(1.2,2.1,3.2),'3'=c('代码','日期','名称'))
#names(stock1)<-c('吗','带','日')
#names(stock1)<-c('日','x','e')

names(stock1)[1:11]<-c('代码','名称','日期','收盘价','涨跌幅(%)','成交量(万元)','周涨跌幅(%)','月涨跌幅(%)','年涨跌幅(%)','年初至今涨跌幅（%）','SW1')
#stock1$date<-as.qtDate(stock1$'日期')
postData(stock1,name='stock1',key=key)

