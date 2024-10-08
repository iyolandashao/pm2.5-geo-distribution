library(ade4)
library(adespatial)
library(adegraphics)
library(sp)
library(spdep)
library(geoR)
library(lattice)
library(SpatialEpi)
library(gstat)
setwd(".../Result/Variogram")
pm <-read.csv(file = ".../Result/impr/pmavg_impr.csv",header = T)
df<-pm[,c(3:4,9:24)]
coordinates(df) <- ~Longitude + Latitude
coord<-latlong2grid(coordinates(df))
df<- cbind(coord,pm[,9:24])
coordinates(df) <- ~x + y
shapiro.test(log(df$As))
shapiro.test(log(df$Se))
shapiro.test(log(df$Pb))
shapiro.test(log(df$Zn))
v1<-variogram(log(df$As)~1,df)
vm1 <- fit.variogram(v1, vgm(0.8, "Gau", 2000, 0.3))
plot(gamma~dist,v1,xlim = c(0,max(v1$dist)), ylim = c(0, max(v1$gamma)),col='blue', ylab = 'semivariance', xlab = 'distance', main="Experiment variogram fitting") 
lines(variogramLine(vm1, 1850), col='blue')
v2<-variogram(log(df$Se)~1,df)
vm2 <- fit.variogram(v2, vgm(1, "Gau", 1000, 0.1))
plot(gamma~dist,v2,xlim = c(0,max(v2$dist)), ylim = c(0, max(v2$gamma)),col='blue', ylab = 'semivariance', xlab = 'distance', main="Experiment variogram fitting") 
lines(variogramLine(vm2, 1850), col='blue')
v3<-variogram(log(df$Pb)~1,df)
vm3 <- fit.variogram(v3, vgm(0.5, "Gau", 1500, 0.1))
plot(gamma~dist,v3,xlim = c(0,max(v3$dist)), ylim = c(0, max(v3$gamma)),col='blue', ylab = 'semivariance', xlab = 'distance', main="Experiment variogram fitting") 
lines(variogramLine(vm3, 1850), col='blue')
v4<-variogram(log(df$Zn)~1,df)
vm4 <- fit.variogram(v4, vgm(1.25, "Sph",10000, 0.25)) #?this model is not perfect, exp as well
plot(gamma~dist,v4,xlim = c(0,max(v4$dist)), ylim = c(0, max(v4$gamma)),col='blue', ylab = 'semivariance', xlab = 'distance', main="Experiment variogram fitting") 
lines(variogramLine(vm4, 1850), col='blue')

shapiro.test(log(df$Si))
shapiro.test(log(df$Ca))
shapiro.test(log(df$Fe))
shapiro.test(log(df$Mn))
v5<-variogram(log(df$Si)~1,df)
vm5 <- fit.variogram(v5, vgm(0.4, "Gau", 1500, 0.15))
plot(gamma~dist,v5,xlim = c(0,max(v5$dist)), ylim = c(0, max(v5$gamma)),col='blue', ylab = 'semivariance', xlab = 'distance', main="Experiment variogram fitting") 
lines(variogramLine(vm5, 1850), col='blue')
v6<-variogram(log(df$Ca)~1,df)
vm6 <- fit.variogram(v6, vgm(0.7, "Gau", 1500, 0.1))
plot(gamma~dist,v6,xlim = c(0,max(v6$dist)), ylim = c(0, max(v6$gamma)),col='blue', ylab = 'semivariance', xlab = 'distance', main="Experiment variogram fitting") 
lines(variogramLine(vm6, 1850), col='blue')
v7<-variogram(log(df$Fe)~1,df)
vm7 <- fit.variogram(v7, vgm(0.3, "Sph",3000,0.2))
plot(gamma~dist,v7,xlim = c(0,max(v7$dist)), ylim = c(0, max(v7$gamma)),col='blue', ylab = 'semivariance', xlab = 'distance', main="Experiment variogram fitting") 
lines(variogramLine(vm7, 1850), col='blue')
v8<-variogram(log(df$Mn)~1,df)
vm8 <- fit.variogram(v8, vgm(0.4, "Sph",4500, 0.2))
plot(gamma~dist,v8,xlim = c(0,max(v8$dist)), ylim = c(0, max(v8$gamma)),col='blue', ylab = 'semivariance', xlab = 'distance', main="Experiment variogram fitting") 
lines(variogramLine(vm8, 1850), col='blue')

shapiro.test(log(df$Na))
shapiro.test(log(df$Mg))
shapiro.test(log(df$Cl))
v9<-variogram(log(df$Na)~1,df)
vm9 <- fit.variogram(v9, vgm(1, "Gau", 1500, 0.1))
plot(gamma~dist,v9,xlim = c(0,max(v9$dist)), ylim = c(0, max(v9$gamma)),col='blue', ylab = 'semivariance', xlab = 'distance', main="Experiment variogram fitting") 
lines(variogramLine(vm9, 1850), col='blue')
v10<-variogram(log(df$Mg)~1,df)
vm10 <- fit.variogram(v10, vgm(1, "Sph", 1500, 0.1))
plot(gamma~dist,v10,xlim = c(0,max(v10$dist)), ylim = c(0, max(v10$gamma)),col='blue', ylab = 'semivariance', xlab = 'distance', main="Experiment variogram fitting") 
lines(variogramLine(vm10, 1850), col='blue')
v11<-variogram(log(df$Cl)~1,df)
vm11 <- fit.variogram(v11, vgm(2.5, "Sph", 1500, 0.4))
plot(gamma~dist,v11,xlim = c(0,max(v11$dist)), ylim = c(0, max(v11$gamma)),col='blue', ylab = 'semivariance', xlab = 'distance', main="Experiment variogram fitting") 
lines(variogramLine(vm11, 1850), col='blue')

shapiro.test(log(df$Ni))
shapiro.test(log(df$V))
v12<-variogram(log(df$Ni)~1,df)
vm12 <- fit.variogram(v12, vgm(0.3, "Sph", 4500, 0.15))
plot(gamma~dist,v12,xlim = c(0,max(v12$dist)), ylim = c(0, max(v12$gamma)),col='blue', ylab = 'semivariance', xlab = 'distance', main="Experiment variogram fitting") 
lines(variogramLine(vm12, 1850), col='blue')
v13<-variogram(log(df$V)~1,df)
vm13 <- fit.variogram(v13, vgm(0.4, "Gau", 1100, 0.1))
plot(gamma~dist,v13,xlim = c(0,max(v13$dist)), ylim = c(0, max(v13$gamma)),col='blue', ylab = 'semivariance', xlab = 'distance', main="Experiment variogram fitting") 
lines(variogramLine(vm13, 1850), col='blue')

shapiro.test(log(df$K))
v14<-variogram(log(df$K)~1,df)
vm14 <- fit.variogram(v14, vgm(0.05, "Sph", 100, 0.1))
plot(gamma~dist,v14,xlim = c(0,max(v14$dist)), ylim = c(0, max(v14$gamma)),col='blue', ylab = 'semivariance', xlab = 'distance', main="Experiment variogram fitting") 
lines(variogramLine(vm14, 1850), col='blue')

shapiro.test(log(df$EC))
shapiro.test(log(df$Cu))
v15<-variogram(log(df$EC)~1,df)
vm15 <- fit.variogram(v15, vgm(0.2, "Sph", 100, 0.1))
plot(gamma~dist,v15,xlim = c(0,max(v15$dist)), ylim = c(0, max(v15$gamma)),col='blue', ylab = 'semivariance', xlab = 'distance', main="Experiment variogram fitting") 
lines(variogramLine(vm15, 1850), col='blue')
v16<-variogram(log(df$Cu)~1,df)
vm16 <- fit.variogram(v16, vgm(0.5, "Gau", 1000, 0.5))
plot(gamma~dist,v16,xlim = c(0,max(v16$dist)), ylim = c(0, max(v16$gamma)),col='blue', ylab = 'semivariance', xlab = 'distance', main="Experiment variogram fitting") 
lines(variogramLine(vm16, 1850), col='blue')


plot(gamma~dist,v1,xlim = c(0,max(v1$dist)), ylim = c(0, max(v1$gamma)),col='blue', ylab = 'semivariance', xlab = 'distance', main="Experiment variogram fitting") 
lines(variogramLine(vm1, 1850), col='blue')
points(gamma~dist,v2, col='red') 
lines(variogramLine(vm2, 1850), col='red')
points(gamma~dist,v3, col='yellow') 
lines(variogramLine(vm3, 1850), col='yellow')
points(gamma~dist,v4, col='green') 
lines(variogramLine(vm4, 1850), col='green')

plot(gamma~dist,v5,xlim = c(0,max(v5$dist)), ylim = c(0, max(v5$gamma)),col='blue', ylab = 'semivariance', xlab = 'distance', main="Experiment variogram fitting") 
lines(variogramLine(vm5, 1850), col='blue')
points(gamma~dist,v6, col='red') 
lines(variogramLine(vm6, 1850), col='red')
points(gamma~dist,v7, col='yellow') 
lines(variogramLine(vm7, 1850), col='yellow')
points(gamma~dist,v8, col='green') 
lines(variogramLine(vm8, 1850), col='green')

plot(gamma~dist,v9,xlim = c(0,max(v9$dist)), ylim = c(0, max(v9$gamma)),col='blue', ylab = 'semivariance', xlab = 'distance', main="Experiment variogram fitting") 
lines(variogramLine(vm9, 1850), col='blue')
points(gamma~dist,v10, col='red') 
lines(variogramLine(vm10, 1850), col='red')
points(gamma~dist,v11, col='yellow') 
lines(variogramLine(vm11, 1850), col='yellow')
