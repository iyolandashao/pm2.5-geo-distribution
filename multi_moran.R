library(ade4)
library(adespatial)
library(adegraphics)
library(sp)
library(spdep)
library(dplyr)
library(RColorBrewer)
setwd(".../Result/Multispati")
impr<-read.csv(file = ".../Result/impr/pmavg_season_region_impr.csv",header = T)
O<-read.csv(file = ".../Result/impr/pmavg_region_impr.csv",header = T)
pm<-O[,3:29]
df<-pm[,8:26]
As<-pm[,8]
S<-pm[,26]
pm_vari<-as.data.frame(scale(pm[,8:27]))
pm_name<-as.factor(pm[,5])
pm_region<-as.factor(pm[,7])
coordinates(pm)<-~Longitude + Latitude
xy.coord<-SpatialPoints(coordinates(pm), proj4string=CRS(as.character(NA)), bbox = NULL)
xym<- as.matrix(coordinates(pm))
nb<- graph2nb(gabrielneigh(xym),sym=TRUE)
invdist<-lapply(nbdists(nb,xym), function(x) 1/(x+0.0001))
lw<- nb2listw(nb,glist=invdist,style="W")
s.label(xym,nb=nb, ppoints.cex=1.5, plabels.cex=0) #neighbors
plot(lw,xym,add=TRUE,pch=20,lwd=2,cex=2)
l1<-lapply(df,moran.mc,lw,999)
As.lag<-lag.listw(lw,As)
S.lag<-lag.listw(lw,S)
par(mfrow=c(1,2))
moran.plot(As,lw)
moran.plot(S,lw)
text(x[5],x.lag[5],pm_name[5],pos=1,cex=0.8)
me<-mem(lw)
pca_vari <- dudi.pca(pm_vari, scannf = FALSE, nf = 2)
ms<-multispati(pca_vari,lw,scannf=FALSE, nfposi = 2,nfnega = 0)
summary(ms)
plot(ms)
s.arrow(ms$c1)
par(mfrow=c(1,2))
col_region <- colors()[c(149, 254, 468, 552, 26)]
s.match(ms$li,ms$ls,arrows=TRUE,pm_region=col.dep)
s.match(ms$li[c(146,80,43,131,55,124,145,122,54,126,125,150,148,117,60,78,48,56),],
        ms$ls[c(146,80,43,131,55,124,145,122,54,126,125,150,148,117,60,78,48,56),],
        facet = col_region)
       # label=pm_name[c(146,80,43,131,55,124,145,122)])
s.match(ms$li[c(32,139,11,8,2,19,141,6,1,24),],
        ms$ls[c(32,139,11,8,2,19,141,6,1,24),],
        label=pm_name[c(32,139,11,8,2,19,141,6,1,24)])
a<-ms$li[,1]-ms$ls[,1]
b<-ms$li[,2]-ms$ls[,2]
c<-sqrt(a^2+b^2)
tab<-cbind(ms$li,ms$ls,a,b,c,O$SiteName,O$Region)
names(tab)<-c("a1","b1","a2","b2","a","b","c","SiteName","region")
order(tab$c)
write.csv(tab,file="tab.csv")


col.region <- brewer.pal(5, "Set1")
col.dep <- col.region[pm_region]
g.lab.pca.u <- update(g.lab.pca.u, paxes.draw = TRUE,
                      pgrid.draw = FALSE, plabels.col = col.dep, plot = FALSE)