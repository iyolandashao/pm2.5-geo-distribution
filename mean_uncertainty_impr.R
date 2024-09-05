library(zoo)
library(lubridate)
library(dplyr)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(ggmap)
#register_google(key = "AIzaSyBsYUUJQULNh24SadWpkLLdl9pw4lw3oOA", write = TRUE)
setwd(".../Result/impr")
options(stringsAsFactors=FALSE) #to be string
pmdata<-read.csv(file = ".../Raw data/impr.txt",header = T, sep = ";")
pmdata[pmdata==-999]<-NA #missing value
pmdata<-pmdata%>%filter(!(State=="AK"|State=="HI"|State=="VI"|State=="VT"|State=="AB"|State=="ON"))
pmdata$Date<-as.Date(pmdata$Date)
pmdata$Longitude<-pmdata$Longitude*(-1)
pmdata[pmdata<0]<-0
pmdata$Longitude<-pmdata$Longitude*(-1)
# remove the negative value
pmdata<-pmdata %>%filter_at(vars(-Dataset,-SiteCode,-POC,-Date,-SiteName,-Longitude,-Latitude,-Elevation,-State,-CountyFIPS,-EPACode), all_vars(.>=0 | is.na(.)))
pmdata<-pmdata[,c(1:9,12:13,16:21,23,25:28,30:31,14:15,24,29,22)]
# Delete observations
pmdata<-pmdata%>%
  group_by(Latitude,Longitude,Elevation)%>%
  filter(n()>=60)
# Fire
pmdata<-dplyr::filter(pmdata, !grepl('2015-01-0[1-3]|2015-07-0[4-6]', Date))
# Exclude value> mean+12*sd
pm_avg=aggregate(x = pmdata[,10:29],
                 list(pmdata$SiteCode),
                 mean,na.rm=TRUE)
pm_sd=aggregate(x = pmdata[,10:29],
                list(pmdata$SiteCode),
                sd,na.rm=TRUE)
names(pm_avg) = c(c("SiteCode"), names(pm_avg[,2:21]))
names(pm_sd) = c(c("SiteCode"), names(pm_sd[,2:21]))
pm_avg_sd=merge(pm_avg,pm_sd, by=c("SiteCode"))
pm_avg_sd[,42:61]<-pm_avg[,2:21] + pm_sd[,2:21]*12
P0<-pmdata %>% 
  group_by(SiteCode) %>%
  tally()
matr<-NULL
for(i in 1:nrow(P0)){
  matr <- rbind(matr,pm_avg_sd[rep(i,each=P0$n[i]),])
}
pmdata[,30:49]<- matr[,42:61]-pmdata[,10:29]
pmdata<-pmdata%>%
  filter_at(vars(-Latitude,-Longitude,Elevation,-SiteCode,-Date), all_vars(.>=0 | is.na(.)))
pmdata[,30:49]<-NULL
pm=aggregate(x = pmdata[,10:29],
             list(pmdata$Dataset,
                  pmdata$Longitude,
                  pmdata$Latitude,
                  pmdata$Elevation,
                  pmdata$Date,
                  pmdata$SiteCode,
                  pmdata$SiteName,
                  pmdata$State),
             mean,na.rm=TRUE)
names(pm) = c(c("Dataset","Longitude","Latitude","Elevation","Date","SiteCode","SiteName","State","As","Ca","Cl","Cu","Fe","Pb","Mg","Mn","Ni","K","Se","Si","Na","V","Zn","EC","OC","NO3","S","PM2.5"))
pm<-pm[complete.cases(pm),]
#pmdata by season
pm$Month<-month(pm$Date)
pm$Season<-NULL
for (i in 1:nrow(pm)){
  if (pm$Month[i]%in% c(4:9)) {
    pm$Season[i]<- "Summer"
  }
  else {
    pm$Season[i]<- "Winter"
  } 
}
Region <-NULL
pm$Longitude<-pm$Longitude*(-1)
for(i in 1:nrow(pm)){
  if ((pm$Longitude[i]> 92) & (pm$Latitude[i]> 39) & (pm$State[i]!="CA")){
    Region[i]<- "Northwest"
  }
  else if ((pm$Longitude[i]> 92) & (pm$Latitude[i]< 39) & (pm$State[i]!="CA")){
    Region[i]<- "Southwest"
  }
  else if ((pm$Longitude[i]< 92) & (pm$Latitude[i]> 39) & (pm$State[i]!="CA")){
    Region[i]<- "Northeast"
  }
  else if ((pm$Longitude[i]< 92) & (pm$Latitude[i]< 39) & (pm$State[i]!="CA")){
    Region[i]<- "Southeast"
  }
  else {
    Region[i]<- "California"
  }  
}
Region<-as.data.frame(Region)
pm<-cbind(pm,Region)
pm$Longitude<-pm$Longitude*(-1)
df<-pm
df[,9:28]<-df[,9:28]*1000
id.vars <- c("Dataset","Longitude", "Latitude", "Elevation","Date","SiteCode", "SiteName", "State","Month","Season", "Region")
lf <- melt(df, id.vars=id.vars)
ComputeStats <- function(x) {
  x <- na.omit(x)
  m <- mean(x)
  s <- sd(x)
  n <- length(x)
  t <- qt(.975, n - 1)
  data.frame(mean=m,
             conf.lower=m-t*s/sqrt(n),
             conf.upper=m+t*s/sqrt(n),
             sd.lower=m-s,
             sd.upper=m+s)
}
mystats <-lf%>%
  group_by(variable)%>%
  do(ComputeStats(.[["value"]]))
ggp <- ggplot(mystats)+
  geom_bar(aes(x=variable, y=mean), stat="identity", fill="gray")+ scale_y_log10(limits = c(0.01,10000))+
  geom_errorbar(aes(x=variable,
                    ymin=conf.lower,
                    ymax=conf.upper),
                width=0.1)+
  theme(plot.title = element_text(size=12,hjust = 0.5),axis.text = element_text(size = 10))+
  #ggtitle("Mean concentration and uncertainty") + 
  labs(x="")+ labs(y="Mass concentration")
print(ggp)