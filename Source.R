library(zoo)
library(lubridate)
library(dplyr)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(ggmap)
setwd(".../Result/Source apportionment")
winter<-read.csv(file = ".../Result/Source apportionment/spp_winter.csv",header = T)
summer<-read.csv(file = ".../Result/Source apportionment/spp_summer.csv",header = T)
nw<-read.csv(file = ".../Result/Source apportionment/spp.csv",header = T)
nw[,c(1,13:18)]<-NULL
Time<-NULL
Time<-"Annual"
nw<-cbind(nw,Time)
source_nw<-melt(nw,id.vars=c("Longitude","Latitude","Elevation","SiteCode","SiteName","State","Time"))
summer[,c(1,13:18)]<-NULL
Time<-NULL
Time<-"Summer"
summer<-cbind(summer,Time)
source_summer<-melt(summer,id.vars=c("Longitude","Latitude","Elevation","SiteCode","SiteName","State","Time"))
winter[,c(1,13:18)]<-NULL
Time<-NULL
Time<-"Winter"
winter<-cbind(winter,Time)
source_winter<-melt(winter,id.vars=c("Longitude","Latitude","Elevation","SiteCode","SiteName","State","Time"))

us <- c(left = -127, bottom = 35, right = -90, top = 49)
map <- get_stamenmap(us, zoom = 5, maptype = "toner-background")
p<-ggmap(map,extent="normal")
s1<-source_summer%>%filter(variable== "Soil"|variable=="Industry")
w1<-source_winter%>%filter(variable== "Soil"|variable=="Industry")
nw1<-source_nw%>%filter(variable== "Soil"|variable=="Industry")
p1<-rbind(s1,w1,nw1)
s2<-source_summer%>%filter(!(variable== "Soil"|variable=="Industry"))
w2<-source_winter%>%filter(!(variable== "Soil"|variable=="Industry"))
nw2<-source_nw%>%filter(!(variable== "Soil"|variable=="Industry"))
p2<-rbind(s2,w2,nw2)

p +geom_point(aes(p1$Longitude,p1$Latitude),data = p1,size=p1[,9]*4,col="blue")+
  facet_grid(variable~Time)
p +geom_point(aes(p2$Longitude,p2$Latitude),data = p2,size=p2[,9]*10,col="blue")+
  facet_grid(variable~Time)