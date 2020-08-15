#---------Setting----------------#
rm(list=ls());gc()
setwd("C:/Users/USER/Desktop/FinanceManagement/Graduate_S2/財策/HW6/data")

library(tidyverse)
library(ggplot2)
library(data.table)
library(lubridate)
library(data.table)

#--------Read files--------------#

KeyVariables<-fread("KeyVar.csv")[,-c(1,4)]
colnames(KeyVariables)<-c("Name","YM","Code","WACC","ROA","EVA","CIV","Industry")

Ratios<-fread("Ratio_yr.txt")[,c(2,3,5,8,9,10,11,12,14,15,16,17,18,19,21,23,24,25)]
colnames(Ratios)<-c("Name","YM","TobinQ","CashDiv","OpeLev","FinanLev","CashReinv","InterestGuarante","SalesGrowth","OpeBenefitGrowth","NIpretaxGrowth","RnD","InterestRate","TaxRate","Staff","CFope","CFinv","CFfinan")

Mkt<-fread("Mkt_yr.txt")[,c(1,2,9,13,14,15,16,17,18,19,22,23)]
colnames(Mkt)<-c("Code","Name","YM","Close","Volume","Value","Ret","Turnover","SharesOutStanding","MV","MBratio","DividendYield")

merge_ind=fread("Merge_Ind.csv")[-1,-1]
colnames(merge_ind)<-c("industry","ind")

CIV<-fread("KeyVar2.csv")[,c(2,3,22)]
colnames(CIV)<-c("YM","Name","CIV")
#------Join Tables-------------#
KeyVariables<-KeyVariables %>% left_join(merge_ind,by=c("Industry"="industry"))
Wholetable<-KeyVariables %>% left_join(Mkt[,-1],by=c("Name","YM"))
Wholetable<-Wholetable %>% left_join(Ratios,by=c("Name","YM"))
#--revise CIV--#
Wholetable<-Wholetable[,-7]
Wholetable<-Wholetable %>% left_join(CIV,by=c("YM","Name"))
#--------Listing---------------#
industrys<-unique(KeyVariables$Industry)


#--------Main-----------------#
#-WACC industry-#

WACC_summary<-Wholetable %>%
  filter(!ind=="金融") %>% 
  filter(WACC>=0) %>% 
  group_by(ind) %>%
  summarise(WACCyr=median(WACC)) %>%
#  spread(key=YM,value=WACCyr) %>%
  na.omit()

ggplot(data=WACC_summary,aes(x=ind,y=WACCyr))+
  geom_col()+
  labs(x="產業",y="WACC")+
  ggtitle("產業WACC")+
  ggsave("產業WACC.jpg")
#-ROA industry-#

ROA_summary<-Wholetable %>%
  filter(!ind=="金融") %>% 
  # filter(WACC>=0) %>% 
  group_by(ind) %>%
  summarise(ROAyr=median(ROA)) %>%
 # spread(key=YM,value=ROAyr) %>%
  na.omit()
ggplot(data=ROA_summary,aes(x=ind,y=ROAyr))+
  geom_col()+
  labs(x="產業",y="ROA")+
  ggtitle("產業ROA")+
  ggsave("產業ROA.jpg")

#-WACC firm size-#
WACC_size_summary<-quantileStatics(data=Wholetable %>%
                                     filter(!ind=="金融")%>% 
                                     filter(WACC>=0)  ,
                                   column = WACC,quantile = MV, slice_unit = 5 )

ggplot(data=WACC_size_summary,aes(x=Quantile,y=median))+
  geom_col()+
  labs(x="Size",y="WACC")+
  ggtitle("Size&WACC")+
  ggsave("SizenWACC.jpg")
#-ROA firm size-#
ROA_size_summary<-quantileStatics(data=Wholetable %>% 
                                    # filter(WACC>=0) %>%
                                    filter(!ind=="金融"),
                                  column = ROA,quantile = MV, slice_unit = 5 )

ggplot(data=ROA_size_summary,aes(x=Quantile,y=median))+
  geom_col()+
  labs(x="Size",y="ROA")+
  ggtitle("Size&ROA")+
  ggsave("SizenROA.jpg")
#-WACC sales growth-#
Wholetable$SalesGrowth<-as.numeric(Wholetable$SalesGrowth)
WACC_SaleGrowth_summary<-quantileStatics(data=Wholetable %>%
                                           filter(!ind=="金融")%>% 
                                           filter(WACC>=0),
                                         column = WACC,quantile = SalesGrowth, slice_unit = 5 )

ggplot(data=WACC_SaleGrowth_summary,aes(x=Quantile,y=median))+
  geom_col()+
  labs(x="SalesGrowth",y="WACC")+
  ggtitle("SalesGrowth&WACC")+
  ggsave("SalesGrowthnWACC.jpg")
#-ROA sales growth-#
ROA_SaleGrowth_summary<-quantileStatics(data=Wholetable %>% 
                                          filter(WACC>=0)%>%
                                          filter(!ind=="金融"),
                                        column = ROA,quantile = SalesGrowth, slice_unit = 5 )
ggplot(data=ROA_SaleGrowth_summary,aes(x=Quantile,y=median))+
  geom_col()+
  labs(x="SalesGrowth",y="ROA")+
  ggtitle("SalesGrowth&ROA")+
  ggsave("SalesGrowthnROA.jpg")
#-WACC Price-#
WACC_price_summary<-quantileStatics(data=Wholetable %>%
                                           filter(!ind=="金融")%>% 
                                           filter(WACC>=0),
                                         column = WACC,quantile = Close, slice_unit = 5 )

ggplot(data=WACC_price_summary,aes(x=Quantile,y=median))+
  geom_col()+
  labs(x="Price",y="WACC")+
  ggtitle("Price&WACC")+
  ggsave("PricenWACC.jpg")
#-ROA Price-#
ROA_price_summary<-quantileStatics(data=Wholetable %>% 
                                      # filter(WACC>=0)%>%
                                      filter(!ind=="金融"),
                                    column = ROA,quantile = Close, slice_unit = 5 )

ggplot(data=ROA_price_summary,aes(x=Quantile,y=median))+
  geom_col()+
  labs(x="Price",y="ROA")+
  ggtitle("Price&ROA")+
  ggsave("PricenROA.jpg")

#-WACC Ret-#
WACC_ret_summary<-quantileStatics(data=Wholetable %>%
                                      filter(!ind=="金融")%>% 
                                      filter(WACC>=0),
                                    column = WACC,quantile = Ret, slice_unit = 5 )

ggplot(data=WACC_ret_summary,aes(x=Quantile,y=median))+
  geom_col()+
  labs(x="Return",y="WACC")+
  ggtitle("Return&WACC")+
  ggsave("ReturnnWACC.jpg")

#-ROA Ret-#
ROA_ret_summary<-quantileStatics(data=Wholetable %>% 
                                     # filter(WACC>=0)%>%
                                     filter(!ind=="金融"),
                                   column = ROA,quantile = Ret, slice_unit = 5 )

ggplot(data=ROA_ret_summary,aes(x=Quantile,y=median))+
  geom_col()+
  labs(x="Return",y="ROA")+
  ggtitle("Return&ROA")+
  ggsave("ReturnROA.jpg")


#-WACC time-#

WACC_time_summary<-Wholetable %>%
  filter(!ind=="金融") %>% 
   filter(WACC>=0) %>% 
  group_by(YM) %>%
  summarise(WACCyr=median(WACC)) 

ggplot(data=WACC_time_summary,aes(x=YM,y=WACCyr))+
  geom_col()+
  labs(x="Date",y="WACC")+
  ggtitle("Date&WACC")+
  ggsave("DatenWACC.jpg")
#-ROA time -#

ROA_time_summary<-Wholetable %>%
  filter(!ind=="金融") %>% 
   filter(WACC>=0) %>% 
  group_by(YM) %>%
  summarise(ROAyr=median(ROA)) 

ggplot(data=ROA_time_summary,aes(x=YM,y=ROAyr))+
  geom_col()+
  labs(x="Date",y="ROA")+
  ggtitle("Date&ROA")+
  ggsave("DatenROA.jpg")

#---------EVA-------------------------#
#--Industry--#
EVA_summary<-Wholetable %>%
  filter(!ind=="金融") %>% 
  # filter(WACC>=0) %>% 
  group_by(ind) %>%
  summarise(EVAyr=median(EVA)) 

ggplot(data=EVA_summary,aes(x=ind,y=EVAyr))+
  geom_col()+
  labs(x="產業",y="EVA")+
  ggtitle("產業EVA")+
  ggsave("產業EVA.jpg")
#--Size--#
EVA_size_summary<-quantileStatics(data=Wholetable %>%
                                          # filter(WACC>=0)%>%
                                          filter(!ind=="金融"),
                                        column = EVA,quantile = MV, slice_unit = 5 )
ggplot(data=EVA_size_summary,aes(x=Quantile,y=median))+
  geom_col()+
  labs(x="Size",y="EVA")+
  ggtitle("Size&EVA")+
  ggsave("SizenEVA.jpg")

#--SalesGrowth--#
EVA_SaleGrowth_summary<-quantileStatics(data=Wholetable %>%
                                           # filter(WACC>=0)%>%
                                          filter(!ind=="金融"),
                                        column = EVA,quantile = SalesGrowth, slice_unit = 5 )
ggplot(data=EVA_SaleGrowth_summary,aes(x=Quantile,y=median))+
  geom_col()+
  labs(x="SalesGrowth",y="EVA")+
  ggtitle("SalesGrowth&EVA")+
  ggsave("SalesGrowthnEVA.jpg")
#--Others--#
EVA_SaleGrowth_summary<-quantileStatics(data=Wholetable %>%
                                          # filter(WACC>=0)%>%
                                          filter(!ind=="金融"),
                                        column = EVA,quantile = SalesGrowth, slice_unit = 5 )
ggplot(data=EVA_SaleGrowth_summary,aes(x=Quantile,y=median))+
  geom_col()+
  labs(x="SalesGrowth",y="EVA")+
  ggtitle("SalesGrowth&EVA")+
  ggsave("SalesGrowthnEVA.jpg")

