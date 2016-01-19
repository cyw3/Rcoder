library('lubridate')

library(qutke)
key<-"faca4c8ff4dc1502d87944ea9cfd74f19918f0aef37fde6746b61d4d339bfcf3"
init(key)

date <- Sys.Date()-1

#����ɸѡ,�ж���SW1
sw1 <- '��ҵó��'

#ʹ��ѭ����������qtid���м���
industry <- getIndustry(data='industryType',date=date,SW1=sw1,key=key)
qtid <- industry$qtid

#YalesonChan test c()
#qtid <- c('000001.SZ','000002.SZ')

#��ȡ��������
lastYearDate <- as.Date(paste(year(date)-1,month(date),day(date),sep='-'))
tradingDay <- getDate(data='tradingDay',startdate=lastYearDate,enddate=date,key=key)
length <- length(tradingDay)

FirstDay <- as.Date(paste(year(date)-1,'1','1',sep='-'))
FirstDay <- (getDate(data='tradingDay',startdate=FirstDay,enddate=date,key=key))[1]

mktDaily <- getDailyQuote(data='mktDaily',qtid = qtid,startdate=date,enddate=date,key=key)

qtid <- mktDaily$qtid

#SecuAbbr
SecuAbbr <- (getIndustry(data='industryType',qtid = qtid,date=date,SW1=sw1,key=key))$SecuAbbr

#�������̼�
close<- mktDaily$close
#���ս�����
volume <- mktDaily$volume/10000
#��һ�����յ����̼�
prevClose <- mktDaily$prevClose
#�����ǵ���
quoteChangeDaily <- (close-prevClose)/prevClose*100

#������򵥵����ڼ��㷽��
mktWeek <- getDailyQuote(data='mktDaily',qtid = qtid,startdate=tradingDay[length-5],enddate=tradingDay[length-5],key=key)
mktMonth <- getDailyQuote(data='mktDaily',qtid = qtid,startdate=tradingDay[length-20],enddate=tradingDay[length-20],key=key)
mktYear <- getDailyQuote(data='mktDaily',qtid = qtid,startdate=tradingDay[1],enddate=tradingDay[1],key=key)

mktFYear <- getDailyQuote(data='mktDaily',qtid = qtid,startdate=FirstDay,enddate=FirstDay,key=key)

#��һ�ܵ��ǵ��������ڵ�5�������գ�
quoteChangeWeek <- (close-mktWeek$close)/mktWeek$close*100
quoteChangeMonth <- (close-mktMonth$close)/mktMonth$close*100
quoteChangeYear <- (close-mktYear$close)/mktYear$close*100
quoteChangeFYear <- (close-mktFYear$close)/mktFYear$close*100

#'����'=qtid,'����'=industry$ChiName,'����'=as.qtDate(industry$date),as.numeric(as.character())
stock1 <- data.frame('����'=qtid,'����'=SecuAbbr,'����'=as.Date(date),'���̼�'=close,
                     '�ǵ���(%)'=quoteChangeDaily,'�ɽ���(��Ԫ)'=volume,'���ǵ���(%)'=quoteChangeWeek,
                     '���ǵ���(%)'=quoteChangeMonth,'���ǵ���(%)'=quoteChangeYear,
                     '��������ǵ�����%��'=quoteChangeFYear,'SW1'=sw1)


#stock1$date<-as.qtDate(stock1$'����')
#postData(stock1,name='stock1',key=key)
#postData(industry0,name='industry0',key=key)
