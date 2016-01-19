# Get getKeyMap data
getKeyMap<-function(qtid,SecuCode=NULL,CompanyCode=NULL,ChiName=NULL,SecuMarket=NULL,key){  
  
  #c() become a Vector。将参数组合成一个向量
  qtid <- c(qtid)
  if(is.null(qtid)){
    stop("Please input parameter qtid.")
  }
  
  #这是在为参数列表进行复制，并且对未给的参数进行判断
  args<-list(data='keyMap',key=key,qtid=qtid)
  if(!is.null(SecuCode)) args[['SecuCode']]<-SecuCode
  if(!is.null(CompanyCode)) args[['CompanyCode']]<-CompanyCode
  if(!is.null(ChiName)) args[['ChiName']]<-ChiName   
  if(!is.null(SecuMarket)) args[['SecuMarket']]<-SecuMarket   
  
  qtid<-qtid2c(qtid)
  dl<-list()
  for(x in qtid){
    print('x')
    cat(x,fill=TRUE)
    args[['qtid']]<-x
    dl[[x]]<-getData(args)
  }
  print('dl')
  print(dl)
  
  df<-do.call(rbind, lapply(dl, data.frame, stringsAsFactors=FALSE))
  print('df')
  print(df)
  
  print('row')
  print(row.names(df))
  
  row.names(df)<-NULL
  return(df)
}

# Get getIndustryType data
getIndustryType<-function(date=NULL,qtid=c(),CompanyCode=NULL,sw1=NULL,sw2=NULL,sw3=NULL,key){  
  
  if(is.null(date)&is.null(qtid)){
    stop('At least input one parameter date, qtid.')
  }
  
  
  args<-list(data='industryType',key=key)
  #args[['date']]<-as.character(date)
  if(!is.null(date)) args[['date']]<-date
  #if(!is.null(qtid)) args[['qtid']]<-qtid
  if(!is.null(CompanyCode)) args[['CompanyCode']]<-CompanyCode
  if(!is.null(sw1)) args[['sw1']]<-sw1
  if(!is.null(sw2)) args[['sw2']]<-sw2
  if(!is.null(sw3)) args[['sw3']]<-sw3
  
  if(!is.null(qtid)){
    #需要支持qtid为一向量
    qtid<-qtid2c(qtid)
    dl<-list()
    for(x in qtid){
      print('x')
      cat(x,fill=TRUE)
      args[['qtid']]<-x
      dl[[x]]<-getData(args)
    }
    df<-do.call(rbind, lapply(dl, data.frame, stringsAsFactors=FALSE))
    row.names(df)<-NULL
  }else{
    df<-getData(args)
  }
  return(df)
}

# Get getFinancialIndex data
getFinancialIndex<-function(qtid=c(),date=c(),key){
  if(is.null(qtid)&is.null(date)){
    stop('At least input one parameter date, qtid.')
  }
  
  return(getDaily('financialIndex',qtid,date,key))
}

# Get getTradingDay
getTradingDay<-function(key){
  args<-list(data='tradingDay',key=key)
  df<-getData(args)
  return(as.Date(df$busDate))
}

# Get getStockBeta data
getStockBeta<-function(qtid=c(),date=c(),key){
  if(is.null(qtid)&is.null(date)){
    stop('At least input one parameter date, qtid.')
  }
  return(getDaily('stockBeta',qtid,date,key))
}

getSecuritiesMargin<-function(date=c(),SecuMarket=NULL,key){  
  args<-list(data='securitiesMargin',key=key)
  
  if(!is.null(SecuMarket)) args[['SecuMarket']]<-SecuMarket
  if(!is.null(date)) args[['date']]<-date
  
  df<-getData(args)
  df$TradingDay<-as.Date(df$TradingDay)
  
  return(df)
}

getIndexWeight<-function(date=c(), key){
  if(is.null(date)){
    stop("Need to input parameter date")
  }
  
  args<-list(data="indexWeight",key=key)
  
  dl<-list()
  for(x in as.character(date)){
    cat(x,fill=TRUE)
    args[['date']]<-x
    dl[[x]]<-getData(args)
  }
  
  df<-do.call(rbind, lapply(dl, data.frame, stringsAsFactors=FALSE))
  row.names(df)<-NULL
  return(df)
}

getStockShare<-function(CompanyCode, date, key){
  if(is.null(date) & is.null(CompanyCode)){
    stop("At least input one parameter CompanyCode or date")
  }
  
  args<-list(data='shareStru',key=key)
  args[['date']]<-as.character(date)
  
  df<-getData(args)
  
  if(!is.null(CompanyCode)) {
    df<-df[which(df$CompanyCode==CompanyCode),]
  }
  
  return(df)
}

getMktDaily<-function(qtid=c(),date=c(),key){
  if(is.null(qtid)&is.null(date)){
    stop('At least input one parameter date, qtid.')
  }
  return(getDaily('mktDaily',qtid,date,key))
}

getFwdMktDaily<-function(qtid=c(),date=c(),key){
  if(is.null(qtid)&is.null(date)){
    stop('At least input one parameter date, qtid.')
  }
  return(getDaily('mktFwdDaily',qtid,date,key))
}

# Get MktDataIndex data
getMktDataIndex<-function(qtid=c(),date=c(),key){
  if(is.null(qtid)&is.null(date)){
    stop('At least input one parameter date, qtid.')
  }
  return(getDaily('mktDataIndex',qtid,date,key))
}

# 多个qtid与多个date读取
getDaily<-function(data,qtid=c(),date=c(),key){  
  args<-list(data=data,key=key)
  
  dl<-list()
  if(!is.null(qtid) & !is.null(date)){
    if(length(qtid)<=length(date)){
      for(x in as.character(qtid)){
        cat(x,fill=TRUE)
        args[['qtid']]<-x
        dl[[x]]<-getData(args)
        dl[[x]]<-dl[[x]][which(as.Date(dl[[x]]$date) %in% date),]
      }
      
    }else{
      for(x in as.character(date)){
        cat(x,fill=TRUE)
        args[['date']]<-x
        dl[[x]]<-getData(args)
        dl[[x]]<-dl[[x]][which(dl[[x]]$qtid %in% qtid),]
      }
    }    
  }
  
  
  if(!is.null(qtid) & is.null(date)){
    for(x in as.character(qtid)){
      cat(x,fill=TRUE)
      args[['qtid']]<-x
      dl[[x]]<-getData(args)
    }
  }  
  
  if(!is.null(date) & is.null(qtid)){
    for(x in as.character(date)){
      cat(x,fill=TRUE)
      args[['date']]<-x
      dl[[x]]<-getData(args)
    }
  }
  
  df<-do.call(rbind, lapply(dl, data.frame, stringsAsFactors=FALSE))
  row.names(df)<-NULL
  return(df)
}


# 获取qutke的数据
getData<-function(args){ 
  query<-compose_query(args)
  
  # 将第一参数与第二参数，以seq为分隔符连接起来
  url<-paste(apiurl,'opendata',sep="/")
  # HTTP请求网址，得到的是日期数据
  addr<-URLencode(paste(url,query,sep="?"))
  #print(addr)
  return(read.table(addr,sep=",",quote='\"',header=TRUE,fileEncoding="utf-8"))
}