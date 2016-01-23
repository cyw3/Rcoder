
library(quantmod)
library(plyr)
library(TTR)
library(scales)
library(ggplot2)
library(qutke)

#library(devtools)
#install_github('qutke/qutke')

key <- 'faca4c8ff4dc1502d87944ea9cfd74f19918f0aef37fde6746b61d4d339bfcf3'
init(key)

#1、设定参数（包括股票组）
#股票组20，取最高的5支，时间是2015-01-01至今

#试验qtid
qtid <- c('002230.SZ','002715.SZ')
#qtid <- c('000001.SZ','000002.SZ')

sDate<-as.Date("2015-1-1") #开始日期
eDate<-Sys.Date() #结束日期


#2、取每支股票的每交易日收盘价
#3、数据处理，移动均值ma

#已有MACD
dailyQuote <- getDailyQuote(data='mktFwdDaily',qtid=qtid,startdate=sDate,enddate=eDate,key=key)
ldata <- dailyQuote[,grep("qtid|date|close|ma5|ma20", names(dailyQuote)) ]
#为true时候，补全ldata中是na的数值，以最近日期的数据 进行赋值 
ldata<-na.locf(ldata, fromLast=TRUE)


#4、标定UP、Down
# 散点数据
genPoint<-function(arg=c(),ldata){
  if(length(arg)>2){
    stop('The length of args is two.')
  }
  if(length(arg)<2){
    arg <- c(arg,2)
  }
  #[,c(1,arg[1])]
  pdata <- ldata[which(ldata[,arg[2]]>ldata[,arg[1]]),]
  pdata <- cbind(pdata,'op'=c('Down')) #添加op列
  
  xdata <- ldata[which(ldata[,arg[2]]<=ldata[,arg[1]]),]
  xdata <- cbind(xdata,'op'='UP')
    
  return(rbind(pdata,xdata))
} 

separateQtid<-function(pdata,qtid){
  if(length(qtid)<=0){
    stop('The length of qtid is at least one.')
  }
  
  xdata <- list()
  for(id in qtid){
    xdata[[id]] <- pdata[which(pdata$qtid==id),]
  }
  
  return(xdata)
}


pdata<-genPoint(c(5,4),ldata)
pdata <- separateQtid(pdata,qtid)

for(id in qtid){
  pdata[[id]] <- pdata[[id]][order(pdata[[id]]$date,decreasing=F),]
}

#5、取交易点
#dataFrame按列排序
#pdata[order(pdata[,1],decreasing=F),]
Signal<-function(ldata,pdata,qtid){
  
  if(length(qtid)<=0){
    stop('The length of qtid is at least one.')
  }
  op1 <- list()
  for(id in qtid){
    pdata[[id]] <- pdata[[id]][order(pdata[[id]]$date,decreasing=F),]
    
    op <- pdata[[id]][1,]
    tmp <- op$op
    i <- 2
    nrow <- nrow(pdata[[id]])
    while(i<=nrow){
      if(tmp!=pdata[[id]][i,]$op){
        op <- rbind(op,pdata[[id]][i,])
      }
      tmp <- pdata[[id]][i,]$op
      i=i+1
    }
    op1[[id]] <- op
  }
  return(op1)
}
tdata<-Signal(ldata,pdata,qtid)

#6、模拟交易
#利用交易信号数据，进行模拟交易
#设定交易参数，以$10W为本金，满仓买入或卖出，手续为0，传入交易信号。
#参数：交易信号tdata ,本金capital,持仓比例position,手续费比例fee
trade<-function(tdata,capital=100000,position=1,fee=0){
  
  value <- as.numeric(tdata$close) 

  asset <- capital
  if(tdata[1,]$op=='Down'){
    amount <- (asset*position)%/%value[1]
    cash <- (asset*position)%%value[1]
    #奇数为0，偶数为asset减去上一次的。
    diff <- 0.00
  }else if(tdata[1,]$op=='UP'){
    amount <- 0
    cash <- asset
    diff <- 0.00
  }

  i<-2
  nrow <- nrow(tdata)
  while(i<=nrow){
    #B 买入
    if(tdata[i,]$op=='Down'){
      asset <- c(asset,tail(asset,1))
      amount <- c(amount,(tail(asset,1)*position)%/%value[i])
      cash <- c(cash,(tail(asset,1)*position)%%value[i]+(tail(asset,1)*(1-position)))
      diff <- c(diff,0.00)
    }else if(tdata[i,]$op=='UP'){
      #S 卖出
      cash <- c(cash,(tail(amount,1)*value[i])+tail(cash,1))
      amount <- c(amount,0)
      asset <- c(asset,tail(amount,1)*value[i]+tail(cash,1))
      diff <- c(diff,tail(asset,1)-tail(asset,2)[1])
    }
    i=i+1
  }
  
  result <- list()
  result[['ticks']] <- cbind(tdata,cash,amount,asset,diff)
  
  indexRise <- c()
  indexFall <- c()
  i <- 2
  while(i<=nrow){
    if(result[['ticks']][i,]$diff > 0){
      indexRise <- c(indexRise,i-1,i)
    }else if(result[['ticks']][i,]$diff < 0){
      indexFall <- c(indexFall,i-1,i)
    }
    i <- i+1
  }
  
  result[['rise']] <- result$ticks[indexRise,]
  result[['fall']] <- result$ticks[indexFall,]
  return(result)
  
} 

# 查看每笔交易，将会变成一个list，保存每一个qtid的交易记录result
#每一个qtid的交易记录
tradeList <- list()
for(id in qtid){
  tradeList[[id]] <- trade(tdata[[id]],100000)
}

#7、计算收益率，并记录在一个列表里面，生成dataframe，信息包含
#日期，代码，名称，收益率，涨跌幅，近1周涨跌，近1月涨跌，年初至今涨跌。

income <- c()
quoteChangeDaily <- c()
quoteChangeWeek <- c()
quoteChangeMonth <- c() 
quoteChangeFYear <- c()

for(id in qtid){
  
  #C(),收益率。收益率 = （当天的股价-买入价）/ 买入价 *100%
  income <- c(income,(tail(tradeList[[id]]$ticks$asset,1)-tradeList[[id]]$ticks$asset[1])/tradeList[[id]]$ticks$asset[1]*100)
  
  mktYest <- getDailyQuote(data='mktFwdDaily',qtid = id,startdate=pdata[[id]]$date[nrow(pdata[[id]])-1],enddate=pdata[[id]]$date[nrow(pdata[[id]])-1],key=key)
  mktWeek <- getDailyQuote(data='mktFwdDaily',qtid = id,startdate=pdata[[id]]$date[nrow(pdata[[id]])-5],enddate=pdata[[id]]$date[nrow(pdata[[id]])-5],key=key)
  mktMonth <- getDailyQuote(data='mktFwdDaily',qtid = id,startdate=pdata[[id]]$date[nrow(pdata[[id]])-20],enddate=pdata[[id]]$date[nrow(pdata[[id]])-20],key=key)
  mktFYear <- getDailyQuote(data='mktFwdDaily',qtid = id,startdate=pdata[[id]]$date[1],enddate=pdata[[id]]$date[1],key=key)
  
  
  #涨跌幅 .涨跌幅的计算公式是：(当前最新成交价（或收盘价）-开盘参考价)÷开盘参考价×100%  [which(mktYest$qtid==id),]
  quoteChangeDaily <- c(quoteChangeDaily,(as.numeric(tail(pdata[[id]], 1)$close)-mktYest$close)/mktYest$close*100)
  #近1周涨跌
  quoteChangeWeek <- c(quoteChangeWeek,(as.numeric(tail(pdata[[id]], 1)$close)-mktWeek$close)/mktWeek$close*100)
  #近1月涨跌
  quoteChangeMonth <- c(quoteChangeMonth,(as.numeric(tail(pdata[[id]], 1)$close)-mktMonth$close)/mktMonth$close*100)
  #年初至今涨跌
  quoteChangeFYear <- c(quoteChangeFYear,(as.numeric(tail(pdata[[id]], 1)$close)-mktFYear$close)/mktFYear$close*100)

}

ChiAbbr <- getMD(data='keyMap',qtid=qtid,key=key)$ChiAbbr

maDF <- data.frame('1'=tail(dailyQuote,1)$date,'2'=qtid,'3'=ChiAbbr,
                        '4'=income,'5'=quoteChangeDaily,'6'=quoteChangeWeek,'7'=quoteChangeMonth,
                   '8'=quoteChangeFYear)
#排序
maDF <- maDF[order(maDF$X4,decreasing=T),]

#8、#CSI300指数(沪深300指数的qtid代码就是‘0003000.SH’) ：hs300<-getDailyQuote(data='mktDataIndex',qtid=c('000300.SH'),key=key) 
#查不到这个qtid。取得的代码在mktFwdDaily也查不到
#getDailyQuote(data='mktDataIndex',qtid=qtid,startdate=tail(dailyQuote,1)$date,enddate=tail(dailyQuote,1)$date,key=key)
hs300<-getDailyQuote(data='mktDataIndex',qtid=c('000300.SH'),startdate=sDate,enddate=eDate,key=key) 
id <- c('000300.SH')
mktWeek <- getDailyQuote(data='mktDataIndex',qtid = id,startdate=hs300$date[nrow(hs300)-5],enddate=hs300$date[nrow(hs300)-5],key=key)
mktMonth <- getDailyQuote(data='mktDataIndex',qtid = id,startdate=hs300$date[nrow(hs300)-20],enddate=hs300$date[nrow(hs300)-20],key=key)
mktFYear <- getDailyQuote(data='mktDataIndex',qtid = id,startdate=hs300$date[1],enddate=hs300$date[1],key=key)

quoteChangeDaily <- (tail(hs300,1)$close-mktYest$close)/mktYest$close*100
quoteChangeWeek <- (tail(hs300,1)$close-mktWeek$close)/mktWeek$close*100
quoteChangeMonth <- (tail(hs300,1)$close-mktMonth$close)/mktMonth$close*100
quoteChangeFYear <- (tail(hs300,1)$close-mktFYear$close)/mktFYear$close*100

#0.3
hs300Dt <- data.frame('1'=tail(dailyQuote,1)$date,'2'='000300.SH','3'='沪深300','4'=0.3,'5'=quoteChangeDaily,'6'=quoteChangeWeek,'7'=quoteChangeMonth,'8'=quoteChangeFYear)

maDF <- rbind(hs300Dt,maDF)

names(maDF)<-c('日期','代码','名称','收益率(%)','涨跌幅(%)','周涨跌幅(%)','月涨跌幅(%)','年初至今涨跌幅（%）')

postData(maDF,name='maDataF',key=key)

