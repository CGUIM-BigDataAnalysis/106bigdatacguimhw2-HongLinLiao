library(readr)
library(knitr)
library(kableExtra)
library(dplyr)
library(ggplot2)
library(ggmap)
library(choroplethr)

OutsideL <- as.data.frame(read_csv("https://ws.moe.edu.tw/Download.ashx?u=C099358C81D4876CC7586B178A6BD6D5062C39FB76BDE7EC7685C1A3C0846BCDD2B4F4C2FE907C3E7E96F97D24487065577A728C59D4D9A4ECDFF432EA5A114C8B01E4AFECC637696DE4DAECA03BB417&n=4E402A02CE6F0B6C1B3C7E89FDA1FAD0B5DDFA6F3DA74E2DA06AE927F09433CFBC07A1910C169A1845D8EB78BD7D60D7414F74617F2A6B71DC86D17C9DA3781394EF5794EEA7363C&icon=..csv"))

OutsideL$X4 <- NULL
OutsideL$X5 <- NULL
OutsideL$X6 <- NULL

StudyInTW103Cou <-as.data.frame(read_csv("http://stats.moe.gov.tw/files/detail/103/103_ab103_C.csv"))
StudyInTW103Uni <-as.data.frame(read_csv("http://stats.moe.gov.tw/files/detail/103/103_ab103_S.csv"))
StudyInTW104Cou <-as.data.frame(read_csv("http://stats.moe.gov.tw/files/detail/104/104_ab104_C.csv"))
StudyInTW104Uni <-as.data.frame(read_csv("http://stats.moe.gov.tw/files/detail/104/104_ab104_S.csv")) 
StudyInTW105Cou <-as.data.frame(read_csv("http://stats.moe.gov.tw/files/detail/105/105_ab105_C.csv"))
StudyInTW105Uni <-as.data.frame(read_csv("http://stats.moe.gov.tw/files/detail/105/105_ab105_S.csv")) 
StudyInTW106Cou <-as.data.frame(read_csv("http://stats.moe.gov.tw/files/detail/106/106_ab105_C.csv"))
StudyInTW106Uni <-as.data.frame(read_csv("http://stats.moe.gov.tw/files/detail/106/106_ab105_S.csv"))

StudyOutsideData <- as.data.frame(read_csv("C:/Users/ASUS/Downloads/Student_RPT_07.csv"))
StudyOutsideData <- StudyOutsideData[c(-1,-2),]
colnames(StudyOutsideData) <- c("學年度","學期","設立別","學校類別","學校代碼",
                               "學校名稱","系所代碼","系所名稱","學制","對方學校國別",
                               "對方學校名稱","英文名稱","交流人數","男","女")

countryname<-read_csv("C:/Users/ASUS/Downloads/CountriesComparisionTable.csv")
colnames(countryname)<-c("ISO3","English","國別")


for(i in 3:11){
  StudyInTW103Cou[,i] <- as.numeric(StudyInTW103Cou[,i])
  StudyInTW104Cou[,i] <- as.numeric(StudyInTW104Cou[,i])
  StudyInTW105Cou[,i] <- as.numeric(StudyInTW105Cou[,i])
  StudyInTW106Cou[,i] <- as.numeric(StudyInTW106Cou[,i])
}

StudyInTW103Cou$total103<-rowSums(StudyInTW103Cou[,3:11],na.rm = T)
StudyInTW104Cou$total104<-rowSums(StudyInTW104Cou[,3:11],na.rm = T)
StudyInTW105Cou$total105<-rowSums(StudyInTW105Cou[,3:11],na.rm = T)
StudyInTW106Cou$total106<-rowSums(StudyInTW106Cou[,3:11],na.rm = T)

StudyInTW103Uni$`非學位生-大陸研修生` <- gsub("…",NA,StudyInTW103Uni$`非學位生-大陸研修生`)
StudyInTW104Uni$`非學位生-大陸研修生` <- gsub("…",NA,StudyInTW104Uni$`非學位生-大陸研修生`)

for(n in 4:12){
  StudyInTW103Uni[,n]<-as.numeric(StudyInTW103Uni[,n])
  StudyInTW104Uni[,n]<-as.numeric(StudyInTW104Uni[,n])
  StudyInTW105Uni[,n]<-as.numeric(StudyInTW105Uni[,n])
  StudyInTW106Uni[,n]<-as.numeric(StudyInTW106Uni[,n])
}

StudyInTW103Uni$total103<-rowSums(StudyInTW103Uni[,4:12],na.rm = T)
StudyInTW104Uni$total104<-rowSums(StudyInTW104Uni[,4:12],na.rm = T)
StudyInTW105Uni$total105<-rowSums(StudyInTW105Uni[,4:12],na.rm = T)
StudyInTW106Uni$total106<-rowSums(StudyInTW106Uni[,4:12],na.rm = T)


##Q1-1
StudyInTWAllCou <- full_join(StudyInTW103Cou,StudyInTW104Cou,by="國別")
StudyInTWAllCou <- full_join(StudyInTWAllCou,StudyInTW105Cou,by="國別")
StudyInTWAllCou <- full_join(StudyInTWAllCou,StudyInTW106Cou,by="國別")
StudyInTWAllCou$總人數 <- StudyInTWAllCou$total103+StudyInTWAllCou$total104+StudyInTWAllCou$total105+StudyInTWAllCou$total106
StudyInTWAllCou <- StudyInTWAllCou[order(StudyInTWAllCou$總人數,decreasing = T),]
StudyInTWAllCou <- select(StudyInTWAllCou,"洲別.x","國別","總人數")
colnames(StudyInTWAllCou) <- c("洲別","國別","總人數")

kable(head(StudyInTWAllCou,10))


##Q1-2
StudyInTWAllUni <- full_join(StudyInTW103Uni,StudyInTW104Uni,by="學校代碼")
StudyInTWAllUni <- full_join(StudyInTWAllUni,StudyInTW105Uni,by="學校代碼")
StudyInTWAllUni <- full_join(StudyInTWAllUni,StudyInTW106Uni,by="學校代碼")
StudyInTWAllUni$總人數 <- StudyInTWAllUni$total103+StudyInTWAllUni$total104+StudyInTWAllUni$total105+StudyInTWAllUni$total106
StudyInTWAllUni <- StudyInTWAllUni[order(StudyInTWAllUni$總人數,decreasing = T),]
StudyInTWAllUni <- select(StudyInTWAllUni,"學校類型.x","學校名稱.x","總人數")  
colnames(StudyInTWAllUni) <- c("學校類型","學校名稱","總人數")

kable(head(StudyInTWAllUni,10))


##Q2
Q2Data <- group_by(StudyInTWAllCou,國別) %>%
  tally(總人數, sort = TRUE) %>%
  group_by(國別 = factor(c(國別[1:10], rep("Other", n() - 10)),
                            levels = c(國別[1:10], "Other"))) %>%
  tally(n) 

colnames(Q2Data) <- c("國別","總人數")

ggplot()+geom_bar(data=Q2Data,aes(x=國別,y=總人數), stat = "identity",fill="#7700BB") + theme_get()
  
##Q3
ETotalCountry<-merge(StudyInTWAllCou,countryname,by="國別")
colnames(ETotalCountry)<-c("國別","洲別","value","ISO3","region")

ETotalCountry[5,3]<-ETotalCountry[5,3]+ETotalCountry[91,3]+ETotalCountry[159,3]
ETotalCountry[107,3]<-ETotalCountry[107,3]+ETotalCountry[108,3]
ETotalCountry<-ETotalCountry%>%
  subset(region!="Unmatch")%>%
  subset(國別!="索馬利蘭共和國")
ans3<-country_choropleth(ETotalCountry)+scale_color_continuous(low = "yellow",high="red")
ans3


##Q4-1
StudyOutsideCou <- group_by(StudyOutsideData,對方學校國別)%>%summarise(CountyTotal = sum(as.numeric(交流人數)))%>%
  arrange(desc(CountyTotal))
colnames(StudyOutsideCou) <- c("學校國別","總計人數")
kable(head(StudyOutsideCou,10))


##Q4-2
StudyOutsideUni <- group_by(StudyOutsideData,對方學校名稱)%>%summarise(CountyTotal = sum(as.numeric(交流人數)))%>%
  arrange(desc(CountyTotal))
colnames(StudyOutsideUni) <- c("學校名稱","總計人數")
kable(head(StudyOutsideUni,10))


##Q5
colnames(StudyOutsideCou) <- c("國別","總人數")

Q5Data <- group_by(StudyOutsideCou,國別) %>%
  tally(總人數, sort = TRUE) %>%
  group_by(國別 = factor(c(國別[1:10], rep("Other", n() - 10)),
                       levels = c(國別[1:10], "Other"))) %>%
  tally(n) 

colnames(Q5Data) <- c("國別","總人數")

ggplot()+geom_bar(data=Q5Data,aes(x=國別,y=總人數),stat = "identity",fill="#7700BB")+theme_get()


##Q6
colnames(StudyOutsideCou) <- c("國別","總計人數")
WTotalCountry<-merge(StudyOutsideCou,countryname,by="國別")
colnames(WTotalCountry)<-c("國別","value","ISO3","region")

WTotalCountry[5,2]<-WTotalCountry[5,2]+WTotalCountry[91,2]+WTotalCountry[159,2]
WTotalCountry[107,2]<-WTotalCountry[107,2]+WTotalCountry[108,2]
WTotalCountry<-WTotalCountry%>%
  subset(region!="Unmatch")%>%
  subset(國別!="索馬利蘭共和國")
ans6<-country_choropleth(WTotalCountry)
ans6


##Q7
OutsideL <- OutsideL[order(OutsideL$總人數,decreasing = T),]
kable(head(OutsideL,10))

##Q8
colnames(OutsideL) <- c("洲別","國別","總計人數")
MTotalCountry<-merge(OutsideL,countryname,by="國別")
colnames(MTotalCountry)<-c("國別","洲別","value","ISO3","region")

MTotalCountry[5,3]<-MTotalCountry[5,3]+MTotalCountry[91,3]+MTotalCountry[159,3]
MTotalCountry[107,3]<-MTotalCountry[107,3]+MTotalCountry[108,3]
MTotalCountry<-MTotalCountry%>%
  subset(region!="Unmatch")%>%
  subset(國別!="索馬利蘭共和國")
ans8<-country_choropleth(MTotalCountry)
ans8


##Q9
Q2Data <- group_by(StudyInTWAllCou,國別) %>%
  tally(總人數, sort = TRUE) %>%
  group_by(國別 = factor(c(國別[1:10], rep("Other", n() - 10)),
                       levels = c(國別[1:10], "Other"))) %>%
  tally(n) 

colnames(Q2Data) <- c("國別","總人數")

ggplot()+geom_bar(data=Q2Data,aes(x=國別,y=總人數), stat = "identity",fill="#7700BB") + theme_get()

testData <- group_by(OutsideL,國別) %>%
  tally(總計人數, sort = TRUE) %>%
  group_by(國別 = factor(c(國別[1:10], rep("Other", n() - 10)),
                       levels = c(國別[1:10], "Other"))) %>%
  tally(n) 

colnames(testData) <- c("國別","總人數")

ggplot()+geom_bar(data=testData,aes(x=國別,y=總人數), stat = "identity",fill="#7700BB") + theme_get()









