#library('lubridate')
#library(qutke)
#key<-"faca4c8ff4dc1502d87944ea9cfd74f19918f0aef37fde6746b61d4d339bfcf3"
#init(key)



getYieldChart<-function(date,qtid=c(),key){
  
  qtid <- c('002230.SZ','002715.SZ')
  date <- '2015-10-01'
  if(length(qtid)<=0){
    stop("Qtid is more than one.")
  }
  
  #股票基本信息
  md <- getMD(data='keyMap',qtid=qtid,key=key)
  
  #股票日间行情(前复权)
  dailyQuote <- getDailyQuote(data='mktFwdDaily',qtid=qtid,startdate=date,enddate=Sys.Date(),key=key)
  #当日收盘价(前复权)
  
  stock <- list()
  qtidlength <- length(qtid)
  i <- 1
  while(i<=qtidlength){
    ChiAbbr <- md[i,]$ChiAbbr
    qtidSt <- md[i,]$qtid
    stock[[ChiAbbr]] <- dailyQuote[which(dailyQuote$qtid==qtidSt),]
    i <- i+1
  }
  
  #x轴，作为时间轴。取买入日期至今。
  tradingDay <- getDate(data='tradingDay',startdate=stock[[1]]$date[1],enddate=Sys.Date(),key=key)
  
  fwdAdjClose <- list()
  fwdAdjClose[['date']] <- tradingDay
  
  #归一化
  i <- 1
  length <- length(tradingDay)
  while(i<=qtidlength){
    ChiAbbr <- md[i,]$ChiAbbr
    j <- 1
    while(j<=length){
      fwdAdjClose[[ChiAbbr]] <- c(fwdAdjClose[[ChiAbbr]],stock[[ChiAbbr]][j,]$fwdAdjClose)
      
      if(is.null(fwdAdjClose[[ChiAbbr]][j])||is.na(fwdAdjClose[[ChiAbbr]][j])) 
        fwdAdjClose[[ChiAbbr]][j] <- fwdAdjClose[[ChiAbbr]][j-1]
      
      j <- j+1
    }
    
    #计算
    fwdAdjClose[[ChiAbbr]] <- (fwdAdjClose[[ChiAbbr]]-fwdAdjClose[[ChiAbbr]][1])/fwdAdjClose[[ChiAbbr]][1]*100
    i=i+1
  }
  
  #形成dataframe
  yieldChart <-data.frame(fwdAdjClose)
  postData(yieldChart,name='yieldChart',key=key)
} 
