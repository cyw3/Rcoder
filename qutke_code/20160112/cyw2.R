install.packages('lubridate')
library('lubridate')

library(qutke)
key<-"faca4c8ff4dc1502d87944ea9cfd74f19918f0aef37fde6746b61d4d339bfcf3"
init(key)

date <- Sys.Date()

#YalesonChan test c()
#qtid <- c('000001.SZ','000002.SZ')

#��ȡ��������
lastYearDate <- as.Date(paste(year(date)-1,month(date),day(date),sep='-'))
tradingDay <- getDate(data='tradingDay',startdate=lastYearDate,enddate=date,key=key)
length <- length(tradingDay)
FirstDay <- as.Date(paste(year(date)-1,'1','13',sep='-'))

date <- tradingDay[length]

#����ɸѡ,�ж���SW1
sw1 <- '���ز�'
#ʹ��ѭ����������qtid���м���
industry <- getIndustry(data='industryType',date=date,SW1=sw1,key=key)
qtid <- industry$qtid
  
mktDaily <- getDailyQuote(data='mktDaily',qtid = qtid,startdate=date,enddate=date,key=key)

qtid <- mktDaily$qtid

#ChiName
ChiName <- (getIndustry(data='industryType',qtid = qtid,date=date,SW1=sw1,key=key))$ChiName

#�������̼�
close<- mktDaily$close
#���ս�����
volume <- mktDaily$volume
#��һ�����յ����̼�
prevClose <- mktDaily$prevClose
#�����ǵ���
quoteChangeDaily <- (close-prevClose)/prevClose

#������򵥵����ڼ��㷽��
mktWeek <- getDailyQuote(data='mktDaily',qtid = qtid,startdate=tradingDay[length-5],enddate=tradingDay[length-5],key=key)
mktMonth <- getDailyQuote(data='mktDaily',qtid = qtid,startdate=tradingDay[length-20],enddate=tradingDay[length-20],key=key)
mktYear <- getDailyQuote(data='mktDaily',qtid = qtid,startdate=tradingDay[1],enddate=tradingDay[1],key=key)

mktFYear <- getDailyQuote(data='mktDaily',qtid = qtid,startdate=FirstDay,enddate=FirstDay,key=key)

#��һ�ܵ��ǵ��������ڵ�5�������գ�
quoteChangeWeek <- (close-mktWeek$close)/mktWeek$close
quoteChangeMonth <- (close-mktMonth$close)/mktMonth$close
quoteChangeYear <- (close-mktYear$close)/mktYear$close
quoteChangeFYear <- (close-mktFYear$close)/mktFYear$close
  
if(length(qtid)==0){
  date <- c()
  sw1 <- c()
}


#'����'=qtid,'����'=industry$ChiName,'����'=industry$date,
dataFrame <- data.frame('����'=qtid,'����'=ChiName,'����'=date,'�������̼�'=close,
                        '�����ǵ���'=quoteChangeDaily,'���ճɽ���'=volume,'��һ���ǵ���'=quoteChangeWeek,
                        '��һ���ǵ���'=quoteChangeMonth,'��һ���ǵ���'=quoteChangeYear,
                        '���������ǵ���'=quoteChangeFYear,'SW1'=sw1)