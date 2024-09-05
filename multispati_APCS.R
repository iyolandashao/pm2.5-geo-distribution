library(ade4)
library(adespatial)
library(adegraphics)
library(sp)
library(spdep)
library(dplyr)
library(SpatialEpi)
setwd(".../Result/Multispati")
apcs<-read.csv(file = ".../Result/Variogram/APCS.csv",header = T)
pm<-apcs[,2:14]
pm2<-pm
pm_vari<-as.data.frame(scale(pm[,5:13]))
pm_name<-as.factor(pm[,3])
pm_region<-as.factor(pm[,4])
coordinates(pm) <- ~Longitude + Latitude
coord<-latlong2grid(coordinates(pm))
pm<- cbind(coord,pm2[,3:13])
coordinates(pm) <- ~x + y
xy.coord<-SpatialPoints(coordinates(pm), proj4string=CRS(as.character(NA)), bbox = NULL)
xym<- as.matrix(coordinates(pm))
nb<- graph2nb(gabrielneigh(xym),sym=TRUE)
invdist<-lapply(nbdists(nb,xym), function(x) 1/(x+0.0001))
lw<- nb2listw(nb,glist=invdist,style="W",zero.policy = TRUE)
pca_vari <- dudi.pca(pm_vari, scannf = FALSE, nf = 3)
ms<-multispati(pca_vari,lw,scannf=FALSE, nfposi = 3,nfnega = 0)
summary(ms)
plot(ms)
s.arrow(ms$c1)
s.match(ms$li,ms$ls,arrows=TRUE,facet=pm_region)
s.match(ms$li[c(4,112,92,13,83),],
        ms$ls[c(4,112,92,13,83),],
        label=pm_name[c(4,112,92,13,83)])
a<-ms$li[,1]-ms$ls[,1]
b<-ms$li[,2]-ms$ls[,2]
c<-ms$li[,3]-ms$ls[,3]
d<-sqrt(a^2+b^2+c^2)
tab<-cbind(ms$li,ms$ls,a,b,c,d,pm$Region)
names(tab)<-c("a1","b1","c1","a2","b2","c2","a","b","c","d","Region")
order(tab$d)
write.csv(tab,file="tab_APCS.csv")

pm<-impr%>%filter(Season=="Summer")
write.csv(pm, file="summer.csv")
pm<-pm[,c(1:6,8:28)]
pm_vari<-as.data.frame(scale(pm[,8:27]))
pm_name<-as.factor(pm[,5])
pm_region<-as.factor(pm[,7])
coordinates(pm)<-~Longitude + Latitude
xy.coord<-SpatialPoints(coordinates(pm), proj4string=CRS(as.character(NA)), bbox = NULL)
xym<- as.matrix(coordinates(pm))
nb<- graph2nb(gabrielneigh(xym),sym=TRUE)
invdist<-lapply(nbdists(nb,xym), function(x) 1/(x+0.0001))
lw<- nb2listw(nb,glist=invdist,style="W",zero.policy = TRUE)
pca_vari <- dudi.pca(pm_vari, scannf = FALSE, nf = 2)
ms<-multispati(pca_vari,lw,scannf=FALSE, nfposi = 2,nfnega = 0)
summary(ms)
plot(ms)
s.arrow(ms$c1)
s.match(ms$li,ms$ls,arrows=TRUE,facet=pm_region)
s.match(ms$li[c(13,120,83,51,125,95,57,41),],
        ms$ls[c(13,120,83,51,125,95,57,41),],
        label=pm_name[c(13,120,83,51,125,95,57,41)])

a<-ms$li[,1]-ms$ls[,1]
b<-ms$li[,2]-ms$ls[,2]
c<-sqrt(a^2+b^2)
tab<-cbind(ms$li,ms$ls,a,b,c,pm$Region)
names(tab)<-c("a1","b1","a2","b2","a","b","c","Region")
order(tab$c)
write.csv(tab,file="tab_summer.csv")

Ord<-NULL
for (i in 9:28){
 Ord<-cbind(Ord, as.data.frame(order(pm[,i])))
}
o1<-as.data.frame(order(pm$As))
o2<-as.data.frame(order(pm$Ca))
o3<-as.data.frame(order(pm$Cl))
o4<-as.data.frame(order(pm$Cu))
o5<-as.data.frame(order(pm$Fe))
o6<-as.data.frame(order(pm$Pb))
o7<-as.data.frame(order(pm$Mg))
o8<-as.data.frame(order(pm$Mn))
o9<-as.data.frame(order(pm$Ni))
o10<-as.data.frame(order(pm$K))
o11<-as.data.frame(order(pm$Se))
o12<-as.data.frame(order(pm$Si))
o13<-as.data.frame(order(pm$Na))
o14<-as.data.frame(order(pm$V))
o15<-as.data.frame(order(pm$Zn))
o16<-as.data.frame(order(pm$EC))
o17<-as.data.frame(order(pm$OC))
o18<-as.data.frame(order(pm$NO3))
o19<-as.data.frame(order(pm$S))
o20<-as.data.frame(order(pm$PM2.5))
o<-cbind(o1,o2,o3,o4,o5,o6,o7,o8,o9,o10,o11,o12,o13,o14,o15,o16,o17,o18,o19,o20)
names(o)<-c(names(pm[,9:28]))
write.csv(o,file="order_winter.csv")