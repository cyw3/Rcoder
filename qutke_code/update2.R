library(devtools)
#install_github('qutke/ruibe')
library(qutke)
library(ruibe)
library(xts)
library(plyr)
key<-'ff5ed58edf645c6581e8148db1130dc310fbab5fdccc4b2a9ea0be30f4128ace'
init(key)

#前期准备
day0<-Sys.Date()           #最新交易日
day1<-as.Date('2014-01-01') #开始日期
# 获得交易日期，直接是date类型的
tdate<-getDate(data='tradingDay',key=key)

#获取在最新交易日与开始日期之间的日期数据
dates<-tdate[which(tdate>=day1 & tdate<=day0)]

dl<-list()
for(dat in as.character(dates)){
	dl[[dat]]<-getDailyQuote(data='mktFwdDaily',startdate=dat,enddate=dat,key=key)
}
#do.call构造和执行一个函数调用，从一个名称或一个函数传递给它的参数列表。矩阵
df0<-do.call(rbind, lapply(dl, data.frame, stringsAsFactors=FALSE))
df0$date<-as.Date(df0$date)
# 从当前数据中，定位日期   读出最后一行,最近一天的
dates2<-dates[which(dates<=tail(df0$date,1))]
dlen<-length(dates2)
dateRange<-data.frame(
  x1=rev(tail(dates2,dlen-10)),
  x2=rev(head(dates2,dlen-10))
)


# 计算持仓
result2<-apply(dateRange,1,function(row){
  cat(row[1],fill=TRUE)
  d1<-as.Date(row[1])
  d2<-as.Date(row[2])  
  stock<- df0[which(df0$date<=d1 & df0$date>=d2),]
  s1<-ddply(stock,.(qtid),summarise,hi=max(fwdAdjHi[1:10]),lo=min(fwdAdjLo[1:10]),
            inf=(fwdAdjClose[10]-lo)/(hi-lo),vol=volume[10]/mean(volume[1:10]),
            yesRet=ret[10],ret=last(ret))
  #计算一段时间内上涨股票支数
  numa<-nrow(stock[which(stock$ret>0),])
  #计算一段时间内下跌股票支数
  numb<-nrow(stock[which(stock$ret<0),])
  if (numb>numa)                      #如果空方占优势（注意此处是唯一需要修改的部分）
  {s2<-s1[which(s1$inf>0.9),]         #追高         
  s3<-s2[-which(s2$yesRet>=0.095),]   #去掉涨停股票
  }
  else                                   #如果多方占优势
  {s2<-s1[which(s1$inf<0.2&s1$vol>1.5),] # 抄底        
  s3<-s2[-which(s2$yesRet<=-0.095),] }   #去掉跌停股票
  #  s3$date<-d1 
  return(s3)
})
#后期处理

# 计算每日的平均收益率
for(i in 1:nrow(dateRange)){
  if(nrow(result[[i]])>0) result[[i]]$date = dateRange$x1[i]
}
df1<-do.call(rbind, lapply(result, data.frame, stringsAsFactors=FALSE))
df2<-ddply(df1,.(date),summarise,ret=mean(ret))

idx300<-getDailyQuote(data='mktDataIndex',qtid='000300.SH',key=key)

x1<-as.xts(df2$ret,order.by=as.Date(df2$date))
x2<-as.xts(idx300$ret,order.by=as.Date(idx300$date))
xd<-merge(x1,x2)
xd$x1[which(is.na(xd$x1))]<-0  #NA处理
xd$x2[which(is.na(xd$x2))]<-0  #NA处理
df<-as.data.frame(merge(cumprod(xd$x1+1),cumprod(xd$x2+1)))
df<-cbind(date=row.names(df),df)
df$date<-as.qtDate(df$date)



