install.packages('lubridate')
library('lubridate')

library(qutke)
key<-"faca4c8ff4dc1502d87944ea9cfd74f19918f0aef37fde6746b61d4d339bfcf3"
init(key)

date <- Sys.Date()-2
date


#进行筛选,判断其SW1
#使用循环，对所有qtid进行计算
#industry <- getIndustry(data='industryType',date=date,SW1='商业贸易',key=key)

#shiyong list() junshi 
#qtid <- industry$qtid

#YalesonChan test shiyong c()
qtid <- list('000001.SZ','000002.SZ')
close <- list()
volume <- list()
prevClose <- list()
quoteChangeDaily <- list()
quoteChangeWeek <- list()
quoteChangeMonth <- list()
quoteChangeYear <- list()
quoteChangeFYear <- list()

#获取交易日期
lastYearDate <- as.Date(paste(year(date)-1,month(date),day(date),sep='-'))
tradingDay <- getDate(data='tradingDay',startdate=lastYearDate,enddate=date,key=key)
length <- length(tradingDay)
FirstDay <- as.Date(paste(year(date)-1,'1','13',sep='-'))

for(x in qtid){
  
  count <- 1
  x <- '000001.SZ'
  
  mktDaily <- getDailyQuote(data='mktDaily',qtid = x,startdate=date,enddate=date,key=key)
  
  #当日收盘价
  close[count]<- mktDaily$close
  #当日交易量
  volume[count] <- mktDaily$volume
  #上一交易日的收盘价
  prevClose[count] <- mktDaily$prevClose
  #当日涨跌幅
  quoteChangeDaily[count] <- (close[[count]]-prevClose[[count]])/prevClose[[count]]
  
  #先用最简单的日期计算方法
  mktWeek <- getDailyQuote(data='mktDaily',qtid = x,startdate=tradingDay[length-5],enddate=tradingDay[length-5],key=key)
  mktMonth <- getDailyQuote(data='mktDaily',qtid = x,startdate=tradingDay[length-20],enddate=tradingDay[length-20],key=key)
  mktYear <- getDailyQuote(data='mktDaily',qtid = x,startdate=tradingDay[1],enddate=tradingDay[1],key=key)
  
  mktFYear <- getDailyQuote(data='mktDaily',qtid = x,startdate=FirstDay,enddate=FirstDay,key=key)
  
  #近一周的涨跌幅（相邻的5个交易日）
  quoteChangeWeek[count] <- (close[[count]]-mktWeek$close)/mktWeek$close
  quoteChangeMonth[count] <- (close[[count]]-mktMonth$close)/mktMonth$close
  quoteChangeYear[count] <- (close[[count]]-mktYear$close)/mktYear$close
  quoteChangeFYear[count] <- (close[[count]]-mktFYear$close)/mktFYear$close
  
  count <- count+1
  
}

#'代码'=industry$qtid,'名称'=industry$ChiName,'日期'=industry$date,
stock1 <- data.frame('当日收盘价'=close,
                        '当日涨跌幅'=quoteChangeDaily,'当日成交量'=volume,'近一周涨跌幅'=quoteChangeWeek,
                        '近一月涨跌幅'=quoteChangeMonth,'近一年涨跌幅'=quoteChangeYear,
                        '年初至今的涨跌幅'=quoteChangeFYear)

#stock1$date<-as.qtDate(stock1$'日期')
#postData(stock1,name='stock1',key=key)
