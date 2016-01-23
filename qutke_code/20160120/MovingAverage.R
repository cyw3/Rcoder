#实现 http://blog.fens.me/finance-stock-ma/
#MA模型


#R语言本身提供了丰富的金融函数工具包，quantmod包就是最常用的一个，
#另外还要配合时间序列包zoo和xts，指标计算包TTR，可视包ggplot2等一起使用。

install.packages('quantmod')
##install.packages('TTR')
install.packages('ggplot2')
##install.packages('scales')


#加载工具包
library(plyr)

library(quantmod)
library(TTR)
library(ggplot2)
library(scales)

#1、首先利用quantmod包，从互联网下载股票数据，并以CSV格式保存到本地。
#下载数据 方法
download<-function(stock,from="2010-01-01"){
   #返回xts格式
   #利用quantmod包的getSymbols()函数，默认会通过Yahoo的金融开放API下载数据
   df<-getSymbols(stock,from=from,env=environment(),auto.assign=FALSE)  #下载数据
   names(df)<-c("Open","High","Low","Close","Volume","Adjusted")
   #保存在文件之中
   write.zoo(df,file=paste(stock,".csv",sep=""),sep=",",quote=FALSE) #保存到本地
  }


#本地读数据 方法
read<-function(stock){  
    #从当地文件读取数据
    as.xts(read.zoo(file=paste(stock,".csv",sep=""),header = TRUE,sep=",", format="%Y-%m-%d"))
  }


#选择IBM的股票数据，从2010-01-01到今天2014-07-09的4年多的日间交易数据。
stock<-"IBM"
download(stock,from='2010-01-01')
IBM<-read(stock)

# 查看数据类型
class(IBM)
#[1] "xts" "zoo"

#数据类型为xts格式的时间序列，数据包括7个列，以日期做索引列，其他6列
#分别为 开盘价(Open), 最高价(High), 最低价(Low), 收盘价(Close), 交易量(Volume), 调整价(Adjusted)。
# 查看前6条数据
head(IBM)


#2 实现简单的蜡烛图
#直接使用quantmod包的chartSeries()函数，我们可以画出可视化效果还不错的蜡烛图。
#简单的蜡烛图
chartSeries(IBM)

#带指标的蜡烛图
chartSeries(IBM,TA = "addVo(); addSMA(); addEnvelope();addMACD(); addROC()")

#非常简单的2个函数，就可以实现股票数据的可视化。当然，这个功能是封装好的通用的函数，
#如果我们要自定策略模型，就需要自己写代码来实现了，比如 自定义的支持量化机(SVM)分类器模型




#3 自定义均线图
#我们需要自定义均线指标：
#1）日期时间序列为索引
#2）收盘价做为价格指标
#3）不考虑成交量及其他维度字段
#4）取2010-01-01至2012-01-01，形成趋势的数据
#5）画出价格曲线，5日均线，20日均线，60日是均线


#移动平均MA，数据清理。分别为收盘价、5日、20日、60日
ma<-function(cdata,mas=c(5,20,60)){ 
       ldata<-cdata
       for(m in mas){
             #依次 添加列。计算时候，前m-1个数值是NA。因为前面没有数据了。需要凑够5、20、60
             ldata<-merge(ldata,SMA(cdata,m))
       }
       #为true时候，补全ldata中是na的数值，以最近日期的数据 进行赋值
       ldata<-na.locf(ldata, fromLast=TRUE)
       names(ldata)<-c('Value',paste('ma',mas,sep=''))
       return(ldata)
   }

# 均线图
drawLine<-function(ldata,titie="Stock_MA",sDate=min(index(ldata)),eDate=max(index(ldata)),out=FALSE){
       #fortify 转成"data.frame"格式；将data数据传给aes方法：Index Series  Value;  1   2010-01-04  Value 132.45
       #画出 坐标轴
       g<-ggplot(aes(x=Index, y=Value),data=fortify(ldata[,1],melt=TRUE))
       #画上k线。黑色线
       g<-g+geom_line()
       #ldata[,-1]  是说去掉第一列
       #画上其他线 并取不同颜色
       g<-g+geom_line(aes(colour=Series),data=fortify(ldata[,-1],melt=TRUE))
       #加标签 取范围
       g<-g+scale_x_date(labels=date_format("%Y-%m"),breaks=date_breaks("2 months"),limits = c(sDate,eDate))
       g<-g+xlab("") + ylab("Price")+ggtitle(title)
       
       #out是true时候，就保存；为false时候，就输出g
       if(out) ggsave(g,file=paste(titie,".png",sep=""))
       else g
   }

# 运行程序，提取收盘价，索引依然是日期
cdata<-IBM['2010/2012']$Close
title<-"Stock_IBM"         #图片标题
sDate<-as.Date("2010-1-1") #开始日期
eDate<-as.Date("2012-1-1") #结束日期

#通过自己封装的移动平均函数和可视化函数，就实现了
#与交易软件中类似的日K线图和多条均线结合的可视化输出。
ldata<-ma(cdata,c(5,20,60))  #选择滑动平均指标
drawLine(ldata,title,sDate,eDate) #画图


#4 一条均线的交易策略
#自己的交易模型
#模型设计思路：股价即是收盘价吧
#1. 以股价和20日均线的交叉，进行交易信号的判断。
#2. 当股价上穿20日均线则买入(红色)，下穿20日均线卖出(蓝色)。
#当k线大于20日均线的第一个点买入，为红。当k线小于20日均线的第一个点买入，为蓝。

#画出股价和20日均线图
ldata<-ma(cdata,c(20))  #选择滑动平均指标
drawLine(ldata,title,sDate,eDate) #画图

#以散点覆盖20日均线，红色点为买入持有，蓝色点为卖出空仓。
# 均线图+散点
drawPoint<-function(ldata,pdata,titie,sDate,eDate){
    g<-ggplot(aes(x=Index, y=Value),data=fortify(ldata[,1],melt=TRUE))
    g<-g+geom_line()
    g<-g+geom_line(aes(colour=Series),data=fortify(ldata[,-1],melt=TRUE))
    #加三点
    g<-g+geom_point(aes(x=Index,y=Value,colour=Series),data=fortify(pdata,melt=TRUE))
    g<-g+scale_x_date(labels=date_format("%Y-%m"),breaks=date_breaks("2 months"),limits = c(sDate,eDate))
    g<-g+xlab("") + ylab("Price")+ggtitle(title)
    g
}

# 散点数据
# 计算pdata,
genPoint<-function(args=c(),ldata){
  #添加列 如： ldata<-merge(ldata,'se'=c(2))
  #列数 length(ldata)
  #ldata<-merge(ldata,SMA(cdata,m))
  #nrow(ldata[which(ldata$Value>ldata$ma20)]$ma20)
  
  if(length(args)>2){
    stop('The length of args is two.')
  }
  if(length(args)<2){
    args <- c(args,1)
  }
  
  #以列进行排序  行数*列数
  #fortify(ldata[,-1],melt=TRUE)
  #转了之后。，合并行，形成一个 rbind(student1,student2)
  pdata <- xts()
  
  #pdata <- ldata[which(ldata$Value>ldata$ma20)]$ma20
  pdata <- ldata[which(ldata[,args[2]]>ldata[,args[1]])][,args[1]]
  names(pdata)<-'Down'
  pdata <- fortify(pdata,melt=TRUE)
  
  xdata <- ldata[which(ldata[,args[2]]<ldata[,args[1]])][,args[1]]
  names(xdata) <- 'UP'
  xdata <- fortify(xdata,melt=TRUE)
  
  pdata <- rbind(pdata,xdata)
  return(pdata)
  
} #代码省略

pdata<-genPoint(c(2),ldata)
head(pdata)

#此时，value是ma20
#       Index Series    Value
#1 2010-01-04   down 128.7955
#2 2010-01-05   down 128.7955
#3 2010-01-06   down 128.7955
#4 2010-01-07   down 128.7955
#5 2010-01-08   down 128.7955
#6 2010-01-11   down 128.7955

drawPoint(ldata,pdata,title,sDate,eDate) #画图

#用股价和20日均线价格做比较，把股价大于均线的部分用蓝色表示，股价小于均线的部分用红色表示。
#可以在每次红色出现的第一个点买入股票，然后在蓝色的第一个点卖出股票

#交易信号 cdata:索引+收盘价
#dataFrame按列排序
#pdata[order(pdata[,1],decreasing=F),]

#  xdata <- ldata
#xdata <- merge(xdata,'OP'=c('OP'))
#xdata[which(xdata$Value>xdata$ma20)]$ma20 <- 'Down'

#1为B  2为S
Signal<-function(cdata,pdata){
  
  pdata <- pdata[order(pdata[,1],decreasing=F),]
  #op <- factor(c('B','S','N'))
  
  op <- pdata[1,]
  tmp <- op$Series
  i <- 2
  nrow <- nrow(pdata)
  while(i<=nrow){
    if(tmp!=pdata[i,]$Series){
      op <- rbind(op,pdata[i,])
    }
    tmp <- pdata[i,]$Series
    i=i+1
  }
  
  
  xdata <- cdata[op$Index]
  names(xdata) <- 'Value'
  return(merge(xdata,'op'=as.factor(op$Series)))
} #代码省略

tdata<-Signal(cdata,pdata)
#row.names  行的序号
#tdata<-tdata[which(as.Date(row.names(tdata)))]
head(tdata)

#B :买入  S: 卖出     value是股价k线   cdata[as.Date('2010-01-27')]  factor(sample)
#                               Value op
#                   2010-01-04 132.45  B
#                   2010-01-22 125.50  S
#                   2010-02-17 126.33  B
#                   2010-03-09 125.55  S
#                   2010-03-11 127.60  B
#                   2010-04-08 127.61  S

# 交易记录
nrow(tdata)
#[1] 72   对的。这个是到2012-01-01的

#tdata<-tdata[1:72]

#利用交易信号数据，进行模拟交易
#设定交易参数，以$10W为本金，满仓买入或卖出，手续为0，传入交易信号。

#模拟交易
#参数：交易信号tdata ,本金capital,持仓比例position,手续费比例fee
trade<-function(tdata,capital=100000,position=1,fee=0){
  
  value <- fortify(tdata$Value,melt=TRUE)$Value
  # %/%  %%
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
  #奇数为0，偶数为asset减去上一次的。S(2)-B(1)
  i<-2
  nrow <- nrow(tdata)
  while(i<=nrow){
    #B 买入
    if(tdata[i,]$op==1){
      asset <- c(asset,tail(asset,1))
      amount <- c(amount,(tail(asset,1)*position)%/%value[i])
      cash <- c(cash,(tail(asset,1)*position)%%value[i]+(tail(asset,1)*(1-position)))
      diff <- c(diff,0.00)
    }else if(tdata[i,]$op==2){
      #S 卖出
      cash <- c(cash,(tail(amount,1)*value[i])+tail(cash,1))
      amount <- c(amount,0)
      asset <- c(asset,tail(amount,1)*value[i]+tail(cash,1))
      diff <- c(diff,tail(asset,1)-tail(asset,2)[1])
    }
    i=i+1
  }
  
  result <- list()
  result[['ticks']] <- merge(tdata,cash,amount,asset,diff)
  
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
  
  result[['rise']] <- result$ticks[indexRise]
  result[['fall']] <- result$ticks[indexFall]
  return(result)
  
} #代码省略
result1<-trade(tdata,100000)

# 查看每笔交易
head(result1$ticks)

#cash是现金,amount是持股数,asset是总财富(包含股票),diff是与上一次买入卖出的亏损或收益

#             Value op     cash amount     asset     diff
#2010-01-04 132.45  B     0.25    755 100000.00     0.00
#2010-01-22 125.50  S 94752.75      0  94752.75 -5247.25
#2010-02-17 126.33  B     5.25    750  94752.75     0.00
#2010-03-09 125.55  S 94167.75      0  94167.75  -585.00
#2010-03-11 127.60  B   126.55    737  94167.75     0.00
#2010-04-08 127.61  S 94175.12      0  94175.12     7.37

# 盈利的交易
head(result1$rise)
#            Value op     cash amount    asset     diff
#2010-03-11 127.60  B   126.55    737 94167.75     0.00
#2010-04-08 127.61  S 94175.12      0 94175.12     7.37
#2010-07-22 127.47  B   108.79    633 80797.30     0.00
#2010-08-12 128.30  S 81322.69      0 81322.69   525.39
#2010-09-09 126.36  B   120.40    632 79979.92     0.00
#2010-11-16 142.24  S 90016.08      0 90016.08 10036.16

# 亏损的交易
head(result1$fall)
#             Value op     cash amount     asset     diff
#2010-01-04 132.45  B     0.25    755 100000.00     0.00
#2010-01-22 125.50  S 94752.75      0  94752.75 -5247.25
#2010-02-17 126.33  B     5.25    750  94752.75     0.00
#2010-03-09 125.55  S 94167.75      0  94167.75  -585.00
#2010-04-09 128.76  B    51.56    731  94175.12     0.00
#2010-04-12 128.36  S 93882.72      0  93882.72  -292.40

#精确地算出每笔交易的盈利情况

#查看最后的资金情况。
tail(result1$ticks,1)
#             Value op     cash amount    asset     diff
#2011-12-21 181.47  S 96363.76      0 96363.76 -3063.87

#画出资金曲线，可以查找亏损的原因。
#由于我们把赚到利润继续投资，增大了投资，
#以至于2011年底的震荡市让模型失效，从而赔了更多的钱。

# 股价+现金流量
drawCash<-function(ldata,adata){
     g<-ggplot(aes(x=Index, y=Value),data=fortify(ldata[,1],melt=TRUE))
     g<-g+geom_line()
     g<-g+geom_line(aes(x=as.Date(Index), y=Value,colour=Series),data=fortify(adata,melt=TRUE))
     g<-g+facet_grid(Series ~ .,scales = "free_y")
     g<-g+scale_y_continuous(labels = dollar)
     g<-g+scale_x_date(labels=date_format("%Y-%m"),breaks=date_breaks("2 months"),limits = c(sDate,eDate))
     g<-g+xlab("") + ylab("Price")+ggtitle(title)
     g
   }

# 现金流量B==1;S==2
adata<-as.xts(result1$ticks[which(result1$ticks$op==2),]$cash)
drawCash(ldata,adata)



#5 二条均线的交易策略
#一条均线模型，在大的趋势下是可以稳定赚钱的，但由于一条均线对于波动非常敏感性，
#如果小波动过于频繁，不仅会增加交易次数，而且会让模型失效。然后，就有二条均线的策略模型，
#可以减低对波动的敏感性。

#二条均线策略模型，与一条均线模型思路类似，以5日均线价格替换股价，
#是通过5日均线和20日均线交叉来进行信号交易的。

#1）首先画出股价，5日均线和20日均线图。
ldata<-ma(cdata,c(5,20))  #选择滑动平均指标
drawLine(ldata,title,sDate,eDate) #画图

#以散点覆盖20日均线，红色点为买入持有，紫色点为卖出空仓。
# 散点数据
pdata<-genPoint(c(3,2),ldata)
head(pdata)
#        Index Series    Value
#1 2010-01-04   down 128.7955
#2 2010-01-05   down 128.7955
#3 2010-01-06   down 128.7955
#4 2010-01-07   down 128.7955
#5 2010-01-08   down 128.7955
#6 2010-01-11   down 128.7955
drawPoint(ldata,pdata,title,sDate,eDate) #画图


#量化计算
tdata<-Signal(cdata,pdata)
#tdata<-tdata[which(as.Date(row.names(tdata)), 
head(tdata)
#             Value op
#2010-01-04 132.45  B
#2010-01-26 125.75  S
#2010-02-18 127.81  B
#2010-03-10 125.62  S
#2010-03-16 128.67  B
#2010-04-12 128.36  S

# 交易记录
nrow(tdata)

#tdata <- tdata[1:72]

#模拟交易
result2<-trade(tdata,100000)
head(result2$ticks)
head(result2$rise)
head(result2$fall)
tail(result2$ticks,1)

#资金曲线
adata<-as.xts(result1$ticks[which(result1$ticks$op==2),]$cash)
#adata<-as.xts(result2$ticks[which(result2$ticks$op=='S'),]['cash'])
drawCash(ldata,adata)

#进一步对比两个模型的盈利情况，找出两个模型中所有赚钱的交易。
# 盈利的交易
rise<-merge(as.xts(result1$rise[,1]),as.xts(result2$rise[,1]))
names(rise)<-c("plan1","plan2")
# 查看数据情况
rise

#画一下盈利部分的交易区间
# 均线图+交易区间
drawRange<-function(ldata,plan,titie="Stock_2014",sDate=min(index(ldata)),eDate=max(index(ldata)),out=FALSE){
     g<-ggplot(aes(x=Index, y=Value),data=fortify(ldata[,1],melt=TRUE))
     g<-g+geom_line()
     g<-g+geom_line(aes(colour=Series),data=fortify(ldata[,-1],melt=TRUE))
     
     #交易区间
     #g<-g+geom_rect(aes(NULL, NULL,xmin=start,xmax=end,fill=plan),ymin = yrng[1], ymax = yrng[2],data=plan)
     g<-g+geom_rect(aes(NULL, NULL,xmin=start,xmax=end,fill=plan),ymin = 0, ymax = Inf,data=plan)
     #g<-g+geom_rect(aes(NULL, NULL,xmin=start,xmax=end,fill=plan),ymin = 0, ymax = Inf,data=plan[2])
     
     g<-g+scale_fill_manual(values =alpha(c("blue", "red"), 0.2))#0.2是透明度
     g<-g+scale_x_date(labels=date_format("%Y-%m"),breaks=date_breaks("2 months"),limits = c(sDate,eDate))
     g<-g+xlab("") + ylab("Price")+ggtitle(title)
     
     if(out) ggsave(g,file=paste(titie,".png",sep=""))
     else g
  }

#盈利区间
#data <- data.frame(start=as.Date(c('1997-01-01','2003-01-01')),end=as.Date(c('2002-12-30','2012-01-20')),plan=c('jiang','hu'))
#yrng <- range(as.vector(close))
comPlan <- function(ldata,result1,result2){
  
  rise1 <- fortify(result1$rise[,1],melt = TRUE)
  rise2 <- fortify(result2$rise[,1],melt = TRUE)
  
  start1 <- rise1[which(index(rise1)%%2==1),]$Index
  end1 <- rise1[which(index(rise1)%%2==0),]$Index
  
  start2 <- rise2[which(index(rise2)%%2==1),]$Index
  end2 <- rise2[which(index(rise2)%%2==0),]$Index
  #end <- list(rise1[which(index(rise1)%%2==0),]$Index,rise2[which(index(rise2)%%2==0),]$Index)
  
  rise2[which(index(rise2)%%2==1),]$Index <- end1 
  rise1[which(index(rise1)%%2==0),]$Index <- start2
  
  return(data.frame(start=rise1$Index,end=rise2$Index,plan=c('plan1','plan2')))
  #return(list(data.frame(start=start1,end=end1,plan=c('plan1')),data.frame(start=start2,end=end2,plan=c('plan2'))))

}# 代码省略
plan<-comPlan(ldata,result1,result2)
#drawRange(ldata,plan,title,sDate,eDate) #画图
drawRange(ldata,plan,title,sDate,eDate)


#R画图：http://www.open-open.com/lib/view/open1410081720011.html
