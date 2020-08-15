rm(list=ls());gc()

library(tidyverse)
library(ggplot2)
library(data.table)
library(lubridate)
setwd("C:/Users/USER/Desktop/FinanceManagement/財策/hw4")
load("C:/Users/USER/Desktop/FinanceManagement/財策/hw4/HW4.rdata")
Industry<-fread("Industry.txt")
colnames(Industry)<-c("Code","Name","Ind","Date","Price")
Industry$Code<-Industry$Code %>% as.numeric()
Industry$Ind<-Industry$Ind %>% as.numeric()
Industry$Price<-Industry$Price %>% as.numeric()
Industry$Date<-paste0(Industry$Date %>%as.character(),"01")  %>% ymd()
Industry<-Industry %>% filter(Ind!=17)

Recession<-Recession %>% rename(Date=年月)
Recession$Date<-ymd(Recession$Date)
colnames(accounting)<-c("Code","Name","Date","Quarter","EBT","SaleNP","CostNP","Nonusual","DebtR")
accounting$Date<-ymd(paste0(as.character(accounting$Date),'01'))
accounting$DebtR<-as.numeric(accounting$DebtR)

Ind_PriceTrend<-Industry %>% left_join(Recession[,c(2,7:9)],by="Date") %>% na.omit()
Ind_Trend<-Ind_PriceTrend %>% left_join(accounting[,c(2,3,9)],by=c("Name","Date")) %>% na.omit()
Inds<-Ind_Trend$Ind %>% unique()
# Ind_Trend2<-Ind_Trend %>% group_by(Name) %>% filter(n()>=40) %>% ungroup()
allindTemp<-Ind_Trend %>% 
         group_by(Date) %>% 
         summarise(Ind_Debt=median(DebtR),Inv=mean(Invertion),Rec_TW=mean(Recession_TW),Rec_US=mean(Recession_US))
                   
ggplot(data=allindTemp,aes(x=Date,y=Ind_Debt))+
  geom_line()+
  # geom_col(data=allindTemp,aes(y=Rec_TW))+
  geom_col(data=allindTemp,aes(y=Inv*40),col="yellow")+ggsave("inveranddebt.jpg")
  
# for(i in 1:length(unique(Ind_Trend$Ind))){
#   temp<-Ind_Trend %>%
#     filter(Ind==unique(Ind_Trend$Ind)[i]) %>%
#     group_by(Date) %>% 
#     summarise(Ind_Debt=median(DebtR),Inv=mean(Invertion))
#   plot_title=paste0(as.character(unique(Ind_Trend$Ind)[i]),"th產業負債比跟殖利率反轉")
#   plotname<-paste0(as.character(unique(Ind_Trend$Ind)[i]),"th產業負債比跟殖利率反轉.jpg")
#   
#   ggplot(data = temp,aes(x=Date,y=Inv*40))+labs(x="時間",y="負債比率")+
#     geom_col(size=0.1,col="cornsilk")+
#     geom_line(aes(y=Ind_Debt))+
#     ggtitle(plot_title)+ggsave(plotname)
# }

stkdata<-fread("stkdata.csv")

stkdata<-stkdata %>% select(年月日,簡稱,證券代碼,`收盤價(元)`,`CAPM_Beta 一年`,Q_Y9999_ret,`台灣-第一銀行-一年期定期儲蓄利率`,`市值(百萬元)`)
colnames(stkdata)<-c("Date","Name","Code","Close","Beta","Mktret","interestrate","volume")
stkdata$Date<-ymd(stkdata$Date)
stkdata$Beta<-as.numeric(stkdata$Beta)
stkdata.sum<-stkdata %>% na.omit %>% mutate(yr=year(Date),mth=month(Date)) %>% group_by(Name,mth,yr) %>% summarise(Bet=median(Beta),MK=median(Mktret),Interest=median(interestrate),VOL=median(volume)) 

Ind_Trend.v2<-Ind_Trend %>% mutate(yr=year(Date),mth=month(Date))%>% left_join(stkdata.sum,by=c("mth","yr","Name")) %>% na.omit()
Ind_Trend.v2<-Ind_Trend.v2 %>% mutate(ke=Interest/100+Bet*(MK-Interest)/100)

ind_trend_DC<-Ind_Trend.v2 %>% ungroup() %>% group_by(Name,yr) %>% summarise(DB=mean(DebtR)) %>% ungroup()
ind_trend_DC<-ind_trend_DC %>%  filter(yr>=2005&yr<=2006) %>% group_by(Name) %>% summarise(Rise=ifelse((DB[2]-DB[1])>0,1,0))
Ind_Trend.v3<-Ind_Trend.v2 %>% left_join(ind_trend_DC,by="Name") %>% na.omit
Ind_Trend.v4<-Ind_Trend.v3 %>% group_by(Name)%>% mutate(Ret=Price/lag(Price)-1) %>% na.omit()
RisePlot<-Ind_Trend.v4 %>% filter(Rise==1)%>% ungroup() %>% group_by(Date) %>% summarise(Mret=median(R))
DownPlot<-Ind_Trend.v4 %>% filter(Rise==0) %>% ungroup() %>% group_by(Date) %>% summarise(Mret=median(ROA))
ggplot(RisePlot,aes(x=Date,y=Mret))+geom_line()
ggplot(DownPlot,aes(x=Date,y=Mret))+geom_line()
library(lmtest)
grangertest(Ind_Trend.v3$Invertion,Ind_Trend.v3$DebtR,order=1)
install.packages("stringr")
library(stringr)
ROA<-fread("ROA.csv",data.table = F)
ROA$Date<-ymd(ROA$Date)
ROA$Name<-str_trim(ROA$Name)
Ind_Trend.v3$Date<-ymd(Ind_Trend.v3$Date)
Ind_Trend.v3.1<-Ind_Trend.v3 %>% left_join(ROA[,c(3,5,6)],by=c("Name","Date")) %>% na.omit()
Ind_Trend.v4.1<-Ind_Trend.v3.1 %>% group_by(Name)%>% mutate(Ret=Price/lag(Price)) %>% na.omit()
yieldinvert$Year<-as.numeric(as.character(yieldinvert$Year))
yieldinvert <-yieldinvert %>% rename(yr=Year)
Ind_Trend.v4.1<-Ind_Trend.v4.1 %>% mutate(Quarter=quarter(Date)) %>% left_join(yieldinvert[,c(4,5,7)],by=c("Quarter","yr"))
Ind_Trend.v4.2<-Ind_Trend.v4.1 %>% filter(Date>ymd(20040101)&Date<ymd(20081231))
grangertest(Ind_Trend.v4.2$long_low,Ind_Trend.v4.2$DebtR,order=1)


RisePlot<-Ind_Trend.v4.1 %>% filter(Rise==1)%>% ungroup() %>% group_by(Date) %>% summarise(Mret=median(ROA),Inv=median(Invertion))
DownPlot<-Ind_Trend.v4.1 %>% filter(Rise==0) %>% ungroup() %>% group_by(Date) %>% summarise(Mret=median(ROA),Inv=median(Invertion))

ggplot(RisePlot,aes(x=Date,y=Mret,col="Rise"))+geom_line()+geom_line(data=DownPlot,aes(y=Mret,col="Decrease"))+geom_col(data=DownPlot,aes(x=Date,y=Inv))+scale_color_manual(values = c(Rise="red",Decrease="blue"))+ggsave("ROAandinver.jpg")

longshort<-fread("ShortLongDebt.txt")

colnames(longshort)<-c("Code","Name","Date","SD","LD","Asset")
longshort$Date<-ymd(paste0(as.character(longshort$Date),"01"))
longshort$SD<-as.numeric(longshort$SD)
longshort$LD<-as.numeric(longshort$LD)
longshort$Asset<-as.numeric(longshort$Asset)
# volume<-volume[,c(4,5,10)]
# colnames(volume)<-c("Date","Name","Volume")
# volume$Date<-as.character(volume$Date)
# volume<-volume %>% mutate(yr=year(Date),mth=month(Date)) 
# %>% mutate(Date=ymd(paste0(as.character(yr),as.character(mth),"01")))
# # volume[,1] = as.character(volume[,1])
Ind_Trend.v4.1<-Ind_Trend.v4.1 %>% left_join(accounting[,c(2,3,5)],by=c("Name","Date"))
Ind_Trend.v4.1$EBT<-as.numeric(Ind_Trend.v4.1$EBT)
Ind_Trend.v4.1<-Ind_Trend.v4.1 %>% group_by(Name)%>% mutate(EVA=EBT-lag(ke,1)*lag(MK,1))
# ggplot()
Lower<-Ind_Trend.v2 %>% ungroup() %>% group_by(Name,yr) %>% summarise(DB=mean(DebtR)) %>% ungroup() %>%  filter(yr>=2008&yr<=2009) %>% group_by(Name) %>% summarise(change=DB[2]-DB[1])%>% arrange(change) %>% na.omit() %>% slice(1:50) 
Upper<-Ind_Trend.v2 %>% ungroup() %>% group_by(Name,yr) %>% summarise(DB=mean(DebtR)) %>% ungroup() %>%  filter(yr>=2008&yr<=2009) %>% group_by(Name) %>% summarise(change=DB[2]-DB[1])%>% arrange(change,) %>% na.omit()%>% slice(1074:1123) 

Ind_Trend.v4.2<-Ind_Trend.v4.1 %>% filter(Name%in%Upper$Name)%>% na.omit() %>% group_by(Name) %>% mutate(evaret=EVA/lag(EVA)-1) %>% na.omit() %>% ungroup() %>% group_by(Date) %>% summarise(Mevaret=mean(evaret)) %>% filter(Date<ymd(20090101)&Date>ymd(20050101))
ggplot(data=Ind_Trend.v4.2,aes(x=Date,y=evaret,col=Name))+geom_line()

Ind_Trend.v4.3<-Ind_Trend.v4.1 %>% filter(Name%in%Lower$Name) %>% na.omit() %>% group_by(Name)%>% mutate(evaret=EVA/lag(EVA)-1) %>% na.omit() %>% ungroup() %>% group_by(Date) %>% summarise(Mevaret=mean(evaret))%>% filter(Date<ymd(20090101)&Date>ymd(20050101))

ggplot(data=Ind_Trend.v4.3,aes(x=Date,y=Mevaret))+geom_line(col="red")+geom_line(data=Ind_Trend.v4.2,col="blue")+ggsave("evaretcomparison.jpg")


LowerName<-Lower$Name
UpperName<-Upper$Name

longshort<-longshort %>% mutate(SDA=SD/Asset,LDA=LD/Asset)
DebtU<-longshort %>% filter(Name%in%UpperName) %>% mutate(yr=year(Date)) %>% group_by(Name,yr) %>% summarise(SDA=mean(SDA),LDA=mean(LDA)) %>% ungroup() %>%  filter(yr>=2008&yr<=2009) %>% group_by(Name) %>% summarise(changeS=SDA[2]-SDA[1],changeL=LDA[2]-LDA[1]) %>% na.omit() 
DebtD<-longshort %>% filter(Name%in%LowerName) %>% mutate(yr=year(Date)) %>% group_by(Name,yr) %>% summarise(SDA=mean(SDA),LDA=mean(LDA)) %>% ungroup() %>%  filter(yr>=2008&yr<=2009) %>% group_by(Name) %>% summarise(changeS=SDA[2]-SDA[1],changeL=LDA[2]-LDA[1])%>% na.omit()

Ind_Trend.v4.1$Ret<-Ind_Trend.v4.1$Ret-1

Ind_Trend.v4.1<-Ind_Trend.v4.1 %>% mutate(AR=Ret-ke) 

MARKETREC<-MARKET %>% filter(Date>=ymd(20071201),Date<=ymd(20091231))
CARup<-Ind_Trend.v4.1 %>% ungroup() %>% filter(Name%in%UpperName) %>% group_by(Date) %>% summarise(MedianAR=median(AR)) %>% filter(Date>=ymd(20070901),Date<=ymd(20091231)) %>% mutate(CAR=cumsum(MedianAR))
CARdown<-Ind_Trend.v4.1 %>% ungroup() %>% filter(Name%in%LowerName) %>% group_by(Date) %>% summarise(MedianAR=median(AR)) %>% filter(Date>=ymd(20070901),Date<=ymd(20091231))%>% mutate(CAR=cumsum(MedianAR))
reversal<-DownPlot %>% filter(Date>ymd(20050101))
ggplot(data=CARup,aes(x=Date,y=MedianAR,col="Rise"))+geom_line()+geom_line(data=CARdown,aes(x=Date,y=MedianAR,col="Decrease"))+scale_colour_manual(values = c(Rise="red",Decrease="blue"))+
  # geom_col(data=reversal,aes(x=Date,y=Inv*0.05))+
  geom_line(data=MARKETREC,aes(x=Date,y=-0.2+(Close-mean(Close))/(5*sd(Close))))+
  ggtitle("衰退期負債比例上升或下降公司累積報酬率")+labs(y="累積報酬率")+
  ggsave("殖利率倒掛與負債上升下降公司的CAR走勢(各50家).jpg")

library(MASS)
Variables<-fread("variable_merge.csv")[,-1]
Variables<-Variables[,c(1,2,3,5,7,8,9,10,11,12,15,17,18,19)]
colnames(Variables)<-c("Name","Date","Code","Ind","RD","Sales","Credit","Debt","ReInv","GPM","Age","Inner","Individual","boardSize")

Variables$Date<-ymd(paste0(as.character(Variables$Date),"01"))
Variables$Ind<-as.factor(Variables$Ind)
Variables$RD<-as.numeric(Variables$RD)
Variables$Sales<-as.numeric(Variables$Sales)
Variables$Credit<-as.numeric(Variables$Credit)
Variables$Debt<-as.numeric(Variables$Debt)
Variables$ReInv<-as.numeric(Variables$ReInv)
Variables$GPM<-as.numeric(Variables$GPM)

ind_trend_DC2<-ind_trend_DC %>%  filter(yr>=2008&yr<=2009) %>% group_by(Name) %>% summarise(RECRise=ifelse((DB[2]-DB[1])>0,1,0))

Ind_Trend.v4.1<-Ind_Trend.v4.1 %>% left_join(Ind_Trend.v4.1,by=)
RGSdata<-Ind_Trend.v4.1 %>% filter(Date>ymd(20050101),Date<ymd(20090101))  %>% left_join(Variables[,c(1,2,5:14)],by=c("Date","Name"))%>% na.omit()
RGSdata$Ind<-as.factor(RGSdata$Ind)
model1<-glm(formula=Rise~., data=RGSdata[,c(17,26:28,30:35)], family=binomial(link="probit"), na.action=na.exclude) 
A<-summary(model1)
ARESULT<-A$coefficients
write.csv(ARESULT,"nolndregressionresult.csv")

RGSdata$Ind<-as.factor(RGSdata$Ind)
model1<-glm(formula=Rise~., data=RGSdata[,c(3,17,26:28,30:35)], family=binomial(link="probit"), na.action=na.exclude) 
B<-summary(model1)
BRESULT<-B$coefficients
write.csv(BRESULT,"indlogitregressionresult.csv")



indup<-Industry %>% filter(Name%in%UpperName) %>% group_by(Name) %>% slice(1) %>% ungroup() %>% group_by(Ind) %>% summarise(num=n())
inddown<-Industry %>% filter(Name%in%LowerName) %>% group_by(Name) %>% slice(1) %>% ungroup() %>% group_by(Ind) %>% summarise(num=n())
indresult<-inddown %>% full_join(indup,by="Ind") %>% rename(Down=num.x,Up=num.y)
indresult[is.na(indresult)]<-0


LSresult1<-DebtD %>% summarise(ShortMean=mean(changeS),LongMean=mean(changeL)) %>% cbind(DebtD %>% summarise(ShortMedian=median(changeS),LongMedian=median(changeL)))
LSresult2<-DebtU %>% summarise(ShortMean=mean(changeS),LongMean=mean(changeL)) %>% cbind(DebtU %>% summarise(ShortMedian=median(changeS),LongMedian=median(changeL)))
LSresult<-LSresult2 %>% rbind(LSresult1)
rownames(LSresult)<-c("Up","Down")

write.csv(LSresult,"負債長短概要.csv")

MARKET<-fread("Y9999.txt")
colnames(MARKET)<-c("Code","Name","Date","Close")
MARKET$Date<-ymd(MARKET$Date)

ggplot(data=MARKET,aes(x=Date,y=Close))+geom_line()+ggsave("Market.jpg")
