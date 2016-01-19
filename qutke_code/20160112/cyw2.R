install.packages('lubridate')
library('lubridate')

library(qutke)
key<-"faca4c8ff4dc1502d87944ea9cfd74f19918f0aef37fde6746b61d4d339bfcf3"
init(key)

date <- Sys.Date()

#YalesonChan test c()
#qtid <- c('000001.SZ','000002.SZ')

#获取交易日期
lastYearDate <- as.Date(paste(year(date)-1,month(date),day(date),sep='-'))
tradingDay <- getDate(data='tradingDay',startdate=lastYearDate,enddate=date,key=key)
length <- length(tradingDay)
FirstDay <- as.Date(paste(year(date)-1,'1','13',sep='-'))

date <- tradingDay[length]

#进行筛选,判断其SW1
sw1 <- '房地产'
#使用循环，对所有qtid进行计算
industry <- getIndustry(data='industryType',date=date,SW1=sw1,key=key)
qtid <- industry$qtid
  
mktDaily <- getDailyQuote(data='mktDaily',qtid = qtid,startdate=date,enddate=date,key=key)

qtid <- mktDaily$qtid

#ChiName
ChiName <- (getIndustry(data='industryType',qtid = qtid,date=date,SW1=sw1,key=key))$ChiName

#当日收盘价
close<- mktDaily$close
#当日交易量
volume <- mktDaily$volume
#上一交易日的收盘价
prevClose <- mktDaily$prevClose
#当日涨跌幅
quoteChangeDaily <- (close-prevClose)/prevClose

#先用最简单的日期计算方法
mktWeek <- getDailyQuote(data='mktDaily',qtid = qtid,startdate=tradingDay[length-5],enddate=tradingDay[length-5],key=key)
mktMonth <- getDailyQuote(data='mktDaily',qtid = qtid,startdate=tradingDay[length-20],enddate=tradingDay[length-20],key=key)
mktYear <- getDailyQuote(data='mktDaily',qtid = qtid,startdate=tradingDay[1],enddate=tradingDay[1],key=key)

mktFYear <- getDailyQuote(data='mktDaily',qtid = qtid,startdate=FirstDay,enddate=FirstDay,key=key)

#近一周的涨跌幅（相邻的5个交易日）
quoteChangeWeek <- (close-mktWeek$close)/mktWeek$close
quoteChangeMonth <- (close-mktMonth$close)/mktMonth$close
quoteChangeYear <- (close-mktYear$close)/mktYear$close
quoteChangeFYear <- (close-mktFYear$close)/mktFYear$close
  
if(length(qtid)==0){
  date <- c()
  sw1 <- c()
}


#'代码'=qtid,'名称'=industry$ChiName,'日期'=industry$date,
dataFrame <- data.frame('代码'=qtid,'名称'=ChiName,'日期'=date,'当日收盘价'=close,
                        '当日涨跌幅'=quoteChangeDaily,'当日成交量'=volume,'近一周涨跌幅'=quoteChangeWeek,
                        '近一月涨跌幅'=quoteChangeMonth,'近一年涨跌幅'=quoteChangeYear,
                        '年初至今的涨跌幅'=quoteChangeFYear,'SW1'=sw1)
