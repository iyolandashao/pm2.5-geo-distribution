library(ade4)
library(adespatial)
library(adegraphics)
library(sp)
library(spdep)
library(dplyr)
setwd(".../Result/Multispati")
summer<-read.csv(file = ".../Result/Multispati/tab_summer.csv",header = T)
winter<-read.csv(file = ".../Result/Multispati/tab_winter.csv",header = T)
annual<-read.csv(file = ".../Result/Multispati/tab_region.csv",header = T)
Time<-NULL
Time<-"summer"
summer<-cbind(summer,Time)
summer<-summer[order(summer$c),]
s_first<-summer[1:50,]%>%group_by(Time,Region)%>%tally()
s_last<-summer[101:150,]%>%group_by(Time,Region)%>%tally()
s_first$n<-s_first$n/c(20,22,49,17,42)
s_last$n<-s_last$n/c(20,22,49,17,42)
Time<-NULL
Time<-"winter"
winter<-cbind(winter,Time)
winter<-winter[order(winter$d),]
w_first<-winter[1:50,]%>%group_by(Time,Region)%>%tally()
w_last<-winter[101:150,]%>%group_by(Time,Region)%>%tally()
w_first$n<-w_first$n/c(20,22,49,17,42)
w_last$n<-w_last$n/c(20,22,49,17,42)
Time=NULL
Time<-"annual"
annual<-cbind(annual,Time)
annual<-annual[order(annual$c),]
a_first<-annual[1:50,]%>%group_by(Time,Region)%>%tally()
a_last<-annual[101:150,]%>%group_by(Time,Region)%>%tally()
a_first$n<-a_first$n/c(20,22,49,17,42)
a_last$n<-a_last$n/c(20,22,49,17,42)
first<-rbind(a_first,s_first,w_first)
criteria<-NULL
criteria<-as.data.frame(c("homogeneous_50","homogeneous_50","homogeneous_50","homogeneous_50","homogeneous_50"))
first<-cbind(criteria,first)
last<-rbind(a_last,s_last,w_last)
criteria<-NULL
criteria<-as.data.frame(c("heterogeneous_50","heterogeneous_50","heterogeneous_50","heterogeneous_50","heterogeneous_50"))
last<-cbind(criteria,last)
data<-rbind(first,last)
ggplot(data,aes(Region,fill=Time))+
  geom_bar(aes(Region,n),stat = "identity")
ggplot(last,aes(Region,fill=Time))+
  geom_bar(aes(Region,n),stat = "identity")
write.csv(first,file="first.csv")
write.csv(last,file="last.csv")
