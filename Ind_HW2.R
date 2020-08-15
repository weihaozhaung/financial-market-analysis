rm(list=ls());gc()

library(lubridate)
library(tidyverse)
library(StatMeasures)
library(tibbletime)
library(foreach)
library(ggplot2)
library(data.table)
#---------------------------------#
ind<-fread("Ind_db_ratio.txt")
ind<-ind[,-c(10,12)]
colnames(ind)<-c("code","name","ind","ipodate","ym","Qrt","Mth","Dbr","Ir","Tr")
PId<-fread("PIboard.txt")
colnames(PId)<-c("code","name","ym","MngrBr")
ind$code<-as.numeric(ind$code) 
Xclu<-fread("興櫃.txt")
XcluNm<-unique(Xclu$簡稱)
ComData<-ind %>% left_join(PId,by=c("code","name","ym")) %>% filter(!name%in%XcluNm)
ComDataNA<-ComData %>% na.omit()


ComDataNA$ym<-paste0(ComDataNA$ym %>% as.character,"01")
ComDataNA$ym<-ymd(ComDataNA$ym)
indname<-fread("indname.txt")[,1:3]
colnames(indname)<-c("code","name","TEJind")
indname<-unique(indname[1:nrow(indname),])
ComDataNA<-ComDataNA %>% left_join(indname,by=c("code","name"))
a<-a %>% left_join(general,by="ind_num")
ComDataNA<-ComDataNA %>% left_join(a[,-2],by=c("TEJind"="Oind"))
IndNum<-unique(ComDataNA$Gind) %>% as.character()
#---------------------------------#
foreach( i = 1:length(IndNum))%do%{
  Gname<-paste0(IndNum[i],"_DB.jpg")
  Gtitle<-paste0(IndNum[i],"_DB_ratio")
  temp<-ComDataNA %>% filter(Gind==IndNum[i])
  temp$Dbr<-as.numeric(temp$Dbr)
  Tmed<-temp %>% group_by(ym) %>% summarise(DBratio=median(Dbr))
  Tmed$DBratio<-as.numeric(Tmed$DBratio)
  ggplot(data=Tmed,aes(x=ym,y=DBratio))+labs(x="時間")+geom_line()+ggtitle(Gtitle)+ggsave(Gname)
}
#---------------------------------#
data1<-fread("FTS.csv",data.table = F)
currentcode<-unique(ComDataNA$code)
forcombine<-data1 %>% filter(公司%in%currentcode)
forcombine<-forcombine[,-c(1,2)]
forcombine$ym<-ymd(paste0(as.character(forcombine$ym),'01'))

ComData.t<-ComDataNA %>% left_join(forcombine,by=c("name","ym"))
#--------------------------------------------------------------------------------------#

comdata<-read.csv("com.csv")
library(lmtest)
i_list=c(8:10,12:13,18:20)
gtest4<-foreach(i = 1:length(i_list),.combine = "rbind")%do%{
temp=data.frame(y=comdata[,11] %>% as.character() %>% as.numeric(),x=comdata[,i_list[i]] %>% as.character() %>% as.numeric()) %>% na.omit()
a<-grangertest(data=temp,y~x,order=4)

data.frame(factor=colnames(comdata)[i_list[i]],pvalue=a$`Pr(>F)`[2])
}

gtest_rev4<-foreach(i = 1:length(i_list),.combine = "rbind")%do%{
  temp=data.frame(y=comdata[,i_list[i]] %>% as.character() %>% as.numeric(),x=comdata[,11] %>% as.character() %>% as.numeric()) %>% na.omit()
  a<-grangertest(data=temp,y~x,order=4)
  
  data.frame(factor=colnames(comdata)[i_list[i]],pvalue=a$`Pr(>F)`[2])
}
# gtest2<-foreach(i = 1:length(i_list),.combine = "rbind")%do%{
#   temp=data.frame(y=comdata[,11] %>% as.character() %>% as.numeric(),x=comdata[,i_list[i]] %>% as.character() %>% as.numeric()) %>% na.omit()
#   a<-grangertest(data=temp,y~x,order=2)
#   
#   data.frame(factor=colnames(comdata)[i_list[i]],pvalue=a$`Pr(>F)`[2])
# }
# gtest_rev2<-foreach(i = 1:length(i_list),.combine = "rbind")%do%{
#   temp=data.frame(y=comdata[,i_list[i]] %>% as.character() %>% as.numeric(),x=comdata[,11] %>% as.character() %>% as.numeric()) %>% na.omit()
#   a<-grangertest(data=temp,y~x,order=2)
#   
#   data.frame(factor=colnames(comdata)[i_list[i]],pvalue=a$`Pr(>F)`[2])
# }
# temp=data.frame(y=comdata[,9] %>% as.character() %>% as.numeric(),x=comdata[,10] %>% as.character() %>% as.numeric()) %>% na.omit()
# aa<-lm(data=temp,y~x)
# temp2<-temp %>% mutate(b=aa$residuals)
# a<-grangertest(data=temp2,y~b,order=1)
# a$`Pr(>F)`
plotdata<-comdata %>% group_by(ym) %>% summarise(m=median(IndInvtr %>% as.character() %>% as.numeric() %>% na.omit),m.DB=median(DBrate   %>% as.character() %>% as.numeric() %>% na.omit))
plotdata$ym<-ymd(as.character(plotdata$ym))
ggplot(data=plotdata,aes(x=ym,y=m.DB))+geom_line()
