library(e1071)
library(cwhmisc)
library(party)
library(rpart)
library(dplyr)
library(stringr)
library(ggplot2)

df<-read.csv("us_perm_visas_SMALL.csv")
new<-df[!df$us_economic_sector=="",]
new3<- df[df$country_of_citizenship=='INDIA' | df$country_of_citizenship=='CHINA'| df$country_of_citizenship=='SOUTH KOREA' | df$country_of_citizenship=='CANADA' |df$country_of_citizenship== 'PHILIPINES'|df$country_of_citizenship=='MEXICO'|df$country_of_citizenship=='UNITED KINGDOM'|df$country_of_citizenship=='TAIWAN'|df$country_of_citizenship=='PAKISTAN'|df$country_of_citizenship=='JAPAN'|df$country_of_citizenship=='FRANCE'|df$country_of_citizenship=='NEPAL'|df$country_of_citizenship=='BRAZIL'|df$country_of_citizenship=='TURKEY'|df$country_of_citizenship=='RUSSIA'|df$country_of_citizenship=='GERMANY',]

new$yearly=0;
new<-new[!new$wage_offer_from_9089=="",]
new<-new[!new$wage_offer_unit_of_pay_9089=="",]
new$wage_offer_from_9089<- as.numeric(as.character(new$wage_offer_from_9089))
new<-new[!(new$wage_offer_from_9089>200 & new$wage_offer_unit_of_pay_9089=='hr'),]
new<-new[!(new$wage_offer_from_9089>200 & new$wage_offer_unit_of_pay_9089=='Hour'),]


new$yearly<-ifelse((new$wage_offer_unit_of_pay_9089=='Year'|new$wage_offer_unit_of_pay_9089=='yr'),
                   (new$wage_offer_from_9089),ifelse((new$wage_offer_unit_of_pay_9089=='Hour'|new$wage_offer_unit_of_pay_9089=='hr'),(2000*new$wage_offer_from_9089),
                                                     ifelse((new$wage_offer_unit_of_pay_9089=='bi'|new$wage_offer_unit_of_pay_9089=='Bi-Weekly'),(26*new$wage_offer_from_9089),
                                                            ifelse((new$wage_offer_unit_of_pay_9089=='Month'|new$wage_offer_unit_of_pay_9089=='mth'),(12*new$wage_offer_from_9089),
                                                                   ifelse((new$wage_offer_unit_of_pay_9089=='week'|new$wage_offer_unit_of_pay_9089=='wk'),(52*new$wage_offer_from_9089),
                                                                          0)))))

new<-new[!(new$yearly>100000),]

summary(new$yearly)
a=which.max(new$yearly)
a
new[a,]

g2<-ggplot(new, aes(x=new$us_economic_sector)) + geom_bar(aes(y = (..count..)*100/sum(..count..),fill=factor(new$case_status))) + labs(title="Case status Vs Economic Sector")+ylab("Percentage") + xlab("Economic Sector")+ coord_flip()
g2

g3<-ggplot(new3, aes(x=new3$country_of_citizenship)) + geom_bar(aes(y = (..count..)*100/sum(..count..),fill=factor(new3$case_status))) + labs(title=" Highest Application by Citizenship")+ylab("Percentage") + xlab("Citizenship")+ coord_flip()
g3

gg4<- ggplot(new, aes(x=new$us_economic_sector, y=new$yearly)) + stat_summary(fun.y="mean", geom="bar") +ylab("Average Yearly Income") + xlab("US Economic Sector")
gg4 + coord_flip()

a=1:nrow(new)
s<-sample(a,0.1*length(a))
new_data<-new[s,]
new_data<- new_data[,c(2,3,4,5,6,7,8,10,17,18,19)]
tree_1<-ctree(new_data$case_status~ new_data$us_economic_sector,data=new_data)
new_data$final_value<-predict(tree_1,new_data[,])

adil2$corr<-ifelse(adil2$corr==TRUE,1,0)
