library(tidyverse)
library(ggplot2)
library(data.table)
library(lubridate)
library(data.table)
library(plm)
setwd("C:/Users/USER/Desktop/FinanceManagement/Graduate_S2/財策/HW7")
wholetable<-fread("Wholetable.csv",data.table = F)
ebitda<-fread("EBITDA.txt",data.table = F)

load("HW7.RData")
for(i in 4:11){
  ebitda[,i]<-ebitda[,i]  %>% as.numeric()
}

ebitda$`年/月`<-ymd(paste0(ebitda$`年/月` %>% as.numeric(),"01")) 
ebitda<-ebitda %>% mutate(Yr=year(`年/月`))
EBITDA_sum<-ebitda %>% group_by(簡稱,Yr) %>% filter(n()==4) %>% summarise(Ad=sum(`營業費用─推銷費用`),
                                                                        ebitda=sum(稅前息前折舊前淨利),
                                                                        revenue=sum(營業收入淨額),
                                                                        SalesGrowthRate=prod(100+營業毛利成長率)**(1/4)-100,
                                                                        OperatingIncomeGrowthRate=prod(100+營業利益成長率)**(1/4)-100,
                                                                        pretaxNIGrowthRate=prod(100+稅前淨利成長率)**(1/4)-100
                                                                        )
wholetable2<-wholetable %>% left_join(EBITDA_sum,by=c("Name"="簡稱","YM"="Yr"))

wholetable2<-wholetable2 %>% select(-c(EVA))

library(tibbletime)

MA_5Y<-rollify(mean,window = 5)

wholetable2<-wholetable2 %>% mutate(WACC_5Y=MA_5Y(WACC))
investedcap<-fread("InvestedCap.txt",data.table = F)
for(i in 4:7){
  investedcap[,i]<-investedcap[,i] %>% as.numeric()
}
investedcap$`年/月`<-ymd(paste0(as.character(investedcap$`年/月`),"01"))

investedcap<-investedcap %>% mutate(Yr=year(`年/月`),month=month(`年/月`)) %>% filter(month==12)

wholetable2<-wholetable2 %>% left_join(investedcap,by=c("YM"="Yr","Name"="簡稱"))
wholetable2<-wholetable2 %>%mutate(investedCap=M資本公積++M普通股股本+M長期負債+M特別股股本)
wholetable2$TaxRate<-as.numeric(wholetable2$TaxRate)
wholetable2$MV<-wholetable2$MV*1000
wholetable2<-wholetable2 %>% mutate(EVA_5Y=revenue*(1-TaxRate*0.01)-WACC_5Y*0.01*(MV+M長期負債))

wholetable.na<-wholetable2[,-c(35:39)]
RESULTDATA<-wholetable2  %>%  ungroup() %>% group_by(Name) %>% filter(n()>=12)
RESULTDATA<-RESULTDATA %>% mutate(EV=MV+M長期負債) %>% mutate(FGV=EV-EVA_5Y/WACC_5Y-investedCap) %>% mutate(COV=EVA_5Y/WACC_5Y+investedCap)

MarketValue<-RESULTDATA%>% ungroup() %>% arrange(YM) %>% group_by(Name) %>%  slice(n()) %>% ungroup() %>% group_by(Industry) %>% arrange(MV) %>% filter(n()>=10) %>% slice((n()-9):n())
names<-unique(MarketValue$Name)
top10names<-MarketValue %>% pull(Name)



RESULT_FOOD<-RESULTDATA %>% ungroup() %>% filter(Industry==ind_list[10]) %>% filter(Name%in%names) %>% select(YM,Name,FGV) %>%  filter(YM>2007) %>% spread(YM,FGV)
write.csv(RESULT_FOOD,"food.csv")
RESULT_semi<-RESULTDATA %>% ungroup() %>% filter(Industry==ind_list[3]) %>% filter(Name%in%names) %>% select(YM,Name,FGV) %>%  filter(YM>2007) %>% spread(YM,FGV)
write.csv(RESULT_semi,"semi.csv")
RESULT_steel<-RESULTDATA %>% ungroup() %>% filter(Industry==ind_list[12]) %>% filter(Name%in%names) %>% select(YM,Name,FGV) %>%  filter(YM>2007) %>% spread(YM,FGV)
write.csv(RESULT_steel,"steel.csv")

RESULTDATA<-RESULTDATA %>% mutate(fgv_ratio=FGV/EV)
RESULTDATA_food<-RESULTDATA %>% ungroup()  %>% filter(Industry==ind_list[10]) %>% filter(Name%in%names)%>% group_by(YM)%>% mutate(MFGVR=mean(fgv_ratio,na.rm=T))
ggplot(data=RESULTDATA_food,aes(x=YM,y=MFGVR))+geom_line()+labs(x="Year",y="FGV占EV比重")+ggtitle("食品業前十大公司FGV占EV比重走勢")+ggsave("FOODCOM.jpg")

RESULTDATA_semiconductor<-RESULTDATA %>% ungroup()  %>% filter(Industry==ind_list[3]) %>% filter(Name%in%names)%>% group_by(YM)%>% mutate(MFGVR=mean(fgv_ratio,na.rm=T))
ggplot(data=RESULTDATA_semiconductor,aes(x=YM,y=MFGVR))+geom_line()+labs(x="Year",y="FGV占EV比重")+ggtitle("半導體前十大公司FGV占EV比重走勢")+ggsave("SEMICOM.jpg")

RESULTDATA_STEEL<-RESULTDATA %>% ungroup()  %>% filter(Industry==ind_list[12]) %>% filter(Name%in%names)%>% group_by(YM)%>% mutate(MFGVR=mean(fgv_ratio,na.rm=T))
ggplot(data=RESULTDATA_STEEL,aes(x=YM,y=MFGVR))+geom_line()+labs(x="Year",y="FGV占EV比重")+ggtitle("鋼鐵業前十大公司FGV占EV比重走勢")+ggsave("STEELCOM.jpg")



RESULTDATA_sum<-RESULTDATA %>% ungroup() %>% group_by(Name) %>% mutate(std.FGV=FGV/sd(FGV,na.rm = T))
RESULTDATA_food<-RESULTDATA_sum %>% filter(Industry==ind_list[10]) %>% filter(Name%in%names)%>% select(YM,Name,std.FGV)  %>% ungroup() %>%group_by(YM) %>% summarise(mean.FGV=mean(std.FGV)) %>% filter(YM>=2007)
ggplot(data=RESULTDATA_food,aes(x=YM,y=mean.FGV))+geom_line()+labs(x="Year",y="FGV走勢")+ggtitle("食品業前十大公司FGV走勢")+ggsave("FOOD.jpg")

RESULTDATA_semiconductor<-RESULTDATA_sum %>% filter(Industry==ind_list[3]) %>% filter(Name%in%names)%>% select(YM,Name,std.FGV)  %>% ungroup() %>%group_by(YM) %>% summarise(mean.FGV=mean(std.FGV)) %>% filter(YM>=2007)
ggplot(data=RESULTDATA_semiconductor,aes(x=YM,y=mean.FGV))+geom_line()+labs(x="Year",y="FGV走勢")+ggtitle("半導體前十大公司FGV走勢")+ggsave("semiconductor.jpg")

RESULTDATA_STEEL<-RESULTDATA_sum %>% filter(Industry==ind_list[12])%>% filter(Name%in%names) %>% select(YM,Name,std.FGV)  %>% ungroup() %>%group_by(YM) %>% summarise(mean.FGV=mean(std.FGV)) %>% filter(YM>=2007)
ggplot(data=RESULTDATA_STEEL,aes(x=YM,y=mean.FGV))+geom_line()+labs(x="Year",y="FGV走勢")+ggtitle("鋼鐵工業前十大公司FGV走勢")+ggsave("STEEL.jpg")

library(Hmisc)
RESULTDATA$RnD<-as.numeric(RESULTDATA$RnD)*0.01
RESULTDATA2<-RESULTDATA %>% ungroup()%>% group_by(Name)%>%arrange(YM) %>%  mutate(lag_eva =Lag(EVA_5Y, shift = 1)) %>% mutate(d_eva=EVA_5Y-lag_eva) %>%
  mutate(d_FGV=FGV-Lag(FGV,shift=1),i_d.sale=revenue-Lag(revenue,shift = 1),eva_pos.dummy=ifelse(d_eva>=0,d_eva,0),
                                   eva_neg.dummy=ifelse(d_eva<=0,d_eva,0))%>% mutate(i2=i_d.sale*eva_pos.dummy,
                                                                                   i3=(RnD*revenue-Lag(RnD*revenue,shift = 1))/WACC_5Y,
                                                                                   i4=(Ad-Lag(Ad,shift = 1))/WACC_5Y,
                                                                                   i5=(ebitda-Lag(ebitda,shift=1))/WACC_5Y,
                                                                                   i6=eva_neg.dummy/WACC_5Y,
                                                                                   i7=eva_pos.dummy/WACC_5Y,
                                                                                  i8=eva_pos.dummy*log(1+0.01*SalesGrowthRate)/WACC_5Y,
                                                                                   i9=Lag(FGV,shift=5),
                                                                                  i10=Lag(investedCap,shift=5))
REGDATA<-RESULTDATA2 %>% select(FGV,Ret,d_FGV,EVA_5Y,Industry,Name,YM,i_d.sale,i2,i3,i4,i5,i6,i7,i8,i9,i10,FGV,WACC_5Y,d_eva,MV,M長期負債,Close,NIpretaxGrowth,SalesGrowthRate,ebitda) %>% na.omit() 
RESULTDATA2[mapply(is.nan, RESULTDATA2)] = NA
RESULTDATA2[mapply(is.infinite, RESULTDATA2)] = NA
REGDATA = REGDATA %>% na.omit()
regnames<-unique(REGDATA$Name)

REGRESSOR<-lm(data=REGDATA,formula=d_FGV~i2+i3+i4+i5+i6+i7+i8+i9+i10)
REGresult<-summary(REGRESSOR)
REGDATA<-REGDATA %>% na.omit()
ind_name<-unique(REGDATA$Industry)
library(foreach)

temp=REGDATA %>% ungroup() %>% filter(Industry==ind_list[10])
regpart<-lm(data=temp,formula=d_FGV~i3+i4+i5+i6+i7+i8+i9+i10)
sumreg<-summary(regpart)
RESULTFOOD<-sumreg$coefficients
write.csv(RESULTFOOD,"foodreg.csv")

temp=REGDATA %>% ungroup() %>% filter(Industry==ind_list[3])
regpart<-lm(data=temp,formula=d_FGV~i3+i4+i5+i6+i7+i8+i9+i10)
sumreg<-summary(regpart)
RESULTsemi<-sumreg$coefficients
write.csv(RESULTsemi,"semireg.csv")


temp=REGDATA %>% ungroup() %>% filter(Industry==ind_list[12])
regpart<-lm(data=temp,formula=d_FGV~i_d.sale+ i2+i3+i4+i5+i6+i7+i8+i9+i10)
sumreg<-summary(regpart)
RESULTsteel<-sumreg$coefficients
write.csv(RESULTsteel,"steelreg.csv")

DATA<-foreach(i = c(1:22,24:length(ind_name)),.combine = "rbind")%do%{
  temp=REGDATA %>% ungroup() %>% filter(Industry==ind_list[i])

  regpart<-lm(data=temp,formula=d_FGV~i_d.sale+i2+i3+i4+i5+i6+i7+i8+i9)
  sumreg<-summary(regpart)
  temp<-temp %>% mutate(abnormal_d_FGV=sumreg$residual)
  
}
DATA<-DATA %>% mutate(EI=(0.01*WACC_5Y*FGV-d_FGV)*(1+0.01*WACC_5Y)/(0.01*WACC_5Y))

DATA<-DATA  %>% mutate(ER=(1+0.01*WACC_5Y)/0.01*WACC_5Y*(d_eva-EI)+abnormal_d_FGV)

write.csv(DATA,"HW7.csv")
# result<-RESULTDATA %>% select(FGV,YM,Name,Industry,EVA_5Y,WACC_5Y,) 
# ind_list<-unique(result$Industry)
# result1<-result %>% filter(Industry==ind_list[1]) %>% spread(YM,FGV)
# 
# a<-result %>%filter(Name%in%top10names) %>%  group_by(Industry) %>% na.omit%>% summarise(med=median(FGV),q1=quantile(FGV)[2],q2=quantile(FGV)[4])
# B<-RESULTDATA %>% select(EV,FGV,COV,EVA_5Y,WACC_5Y,revenue,TaxRate)
# 
# currentdata %>% result %>%filter(Name%in%top10names)
pretaxNI<-fread("pretaxNI.txt",data.table = F)
colnames(pretaxNI)<-c("code","Name","YM","pretaxNI")
pretaxNI$YM<-as.numeric(pretaxNI$YM)%/%100
pretaxNI<- pretaxNI%>% group_by(Name,YM) %>% filter(n()>=4) %>% slice(4)
DATA<-DATA %>% left_join(pretaxNI,by=c("YM","Name"))
DATA$pretaxNI<-as.numeric(DATA$pretaxNI)


DATA<-DATA %>%mutate(PretaxROIC=pretaxNI/(MV+M長期負債)) 
DATA<-DATA %>% mutate(d_ebitda=ebitda-Lag(ebitda,shift = 1))

# pretaxROICreg<-lm(data=DATA,formula=FGV~PretaxROIC)
# summary(pretaxROICreg)
# SalesGrowthRatereg<-lm(data=DATA,formula=FGV~SalesGrowthRate)
# summary(SalesGrowthRatereg)
# d_ebitdareg<-lm(data=DATA,formula=FGV~d_ebitda)
# summary(d_ebitdareg)
# 
# pretaxROICreg1<-lm(data=DATA,formula=EVA_5Y~PretaxROIC)
# summary(pretaxROICreg1)
# SalesGrowthRatereg1<-lm(data=DATA,formula=EVA_5Y~SalesGrowthRate)
# summary(SalesGrowthRatereg1)
# d_ebitdareg1<-lm(data=DATA,formula=EVA_5Y~d_ebitda)
# summary(d_ebitdareg1)
# 
# pretaxROICreg2<-lm(data=DATA,formula=ER~PretaxROIC)
# summary(pretaxROICreg2)
# SalesGrowthRatereg2<-lm(data=DATA,formula=ER~SalesGrowthRate)
# summary(SalesGrowthRatereg2)
# d_ebitdareg2<-lm(data=DATA,formula=ER~d_ebitda)
# summary(d_ebitdareg2)

name_ind<-RESULTDATA %>% select(Name,Industry) %>% filter(Name%in%names)

semis<-name_ind %>% filter(Industry==ind_list[3]) %>% pull(Name)
foods<-name_ind %>% filter(Industry==ind_list[10]) %>% pull(Name)
steels<-name_ind %>% filter(Industry==ind_list[12]) %>% pull(Name)

DATAFOOD<-DATA %>%group_by(Name) %>% mutate(std.ER=ER/sd(ER,na.rm = T)) %>% ungroup() %>%  filter(Name%in%foods) %>% group_by(YM) %>% summarise(ERtrend=mean(std.ER))
DATASTEEL<-DATA %>%group_by(Name) %>% mutate(std.ER=ER/sd(ER,na.rm = T)) %>% ungroup() %>%  filter(Name%in%steels) %>% group_by(YM) %>% summarise(ERtrend=mean(std.ER))
DATASEMI<-DATA %>%group_by(Name) %>% mutate(std.ER=ER/sd(ER,na.rm = T)) %>% ungroup() %>%  filter(Name%in%semi) %>% group_by(YM) %>% summarise(ERtrend=mean(std.ER))

ggplot(data=DATASEMI,aes(x=YM,y=ERtrend))+geom_line()+labs(x="Year",y="ER走勢")+ggtitle("半導體前十大公司ER走勢")+ggsave("SEMIER.jpg")
ggplot(data=DATASTEEL,aes(x=YM,y=ERtrend))+geom_line()+labs(x="Year",y="ER走勢")+ggtitle("鋼鐵前十大公司ER走勢")+ggsave("SteelER.jpg")
ggplot(data=DATAFOOD,aes(x=YM,y=ERtrend))+geom_line()+labs(x="Year",y="ER走勢")+ggtitle("食品前十大公司ER走勢")+ggsave("foodER.jpg")

DATA<-DATA %>%group_by(Name) %>%  mutate(lag_fgv=Lag(FGV,shift = 1),lag_eva=Lag(EVA_5Y,shift=1),lag_er=lag(ER,shift=1),lag_roic=Lag(PretaxROIC,shift=1),lag_sgr=Lag(SalesGrowthRate,shift=1),lag_d_ebitda=Lag(d_ebitda,shift = 1))
FGVret<-lm(data=DATA,formula=Ret~FGV)
EVAret<-lm(data=DATA,formula=Ret~EVA_5Y)
ERret<-lm(data=DATA,formula=Ret~ER)
ROICret<-lm(data=DATA,formula=Ret~PretaxROIC)
SRret<-lm(data=DATA,formula=Ret~SalesGrowthRate)
D_EBITDA<-lm(data=DATA,formula=Ret~d_ebitda)

LFGVret<-lm(data=DATA,formula=Ret~lag_fgv)
LEVAret<-lm(data=DATA,formula=Ret~lag_eva)
LERret<-lm(data=DATA,formula=Ret~lag_er)
LROICret<-lm(data=DATA,formula=Ret~lag_roic)
LSRret<-lm(data=DATA,formula=Ret~lag_sgr)
LD_EBITDA<-lm(data=DATA,formula=Ret~lag_d_ebitda)

reglist=c("FGVret","EVAret","ERret","ROICret","SRret","D_EBITDA","LFGVret","LEVAret","LERret","LROICret","LSRret","LD_EBITDA")
EXPLAIN<-data.frame()
for(i in 1:length(reglist)){
  eval(parse(text=paste0("temp=summary(",reglist[i],")")))
  temp1<-data.frame(name=reglist[i],adj.rsquared=temp$adj.r.squared,estimate=temp$coefficients[2,1],pvalue=temp$coefficients[2,4])
  EXPLAIN<-EXPLAIN %>% rbind(temp1)
}
write.csv(EXPLAIN,"ans7.csv")


save.image(file = "HW7.RData")

