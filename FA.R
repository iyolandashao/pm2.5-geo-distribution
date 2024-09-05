library(psych)
library(dplyr)
library(reshape2)
library(ggplot2)
library(magrittr)
setwd(".../Result/Source apportionment")
pm <-read.csv(file = ".../Result/impr/pm_impr.csv",header = T)
O<-pm[,10:25]
f_O <- fa(O,nfactors = 5,rotate = "varimax")
print(f_O)
l_O <-read.csv(file = ".../Result/Source apportionment/f_O.csv",header = T)
id.vars <- c("composition")
lf <- melt(l_O,id.vars=id.vars)
ggp_us<-ggplot(lf,aes(composition,value))+
  facet_grid(variable~.,scales="free_y")+
  geom_bar(aes(composition,value),stat="identity", position=position_dodge())+
  xlab("US")+ylab(" ")+theme(axis.text = element_text(size = 8),strip.text.y = element_text(size = 8))
print(ggp_us)

pm <-read.csv(file = ".../Result/impr/pm_impr.csv",header = T)
pm<-pm%>%filter(Region =="Northeast")
O<-pm[,10:25]
f_ne<- fa(O,nfactors = 5,rotate = "varimax")
print(f_ne)
l_ne <-read.csv(file = ".../Result/Source apportionment/f_ne.csv",header = T)
id.vars <- c("composition")
lf <- melt(l_ne,id.vars=id.vars)
ggp_ne<-ggplot(lf,aes(composition,value))+
  facet_grid(variable~.,scales="free_y")+
  geom_bar(aes(composition,value),stat="identity", position=position_dodge())+
  xlab("Northeast")+ylab(" ")+theme(axis.text = element_text(size = 8),strip.text.y = element_text(size = 8))
print(ggp_ne)

pm <-read.csv(file = ".../Result/impr/pm_impr.csv",header = T)
pm<-pm%>%filter(Region=="Northwest")
O<-pm[,10:25]
f_nw<- fa(O,nfactors = 5,rotate = "varimax")
print(f_nw)
l_nw <-read.csv(file = ".../Result/Source apportionment/f_nw.csv",header = T)
id.vars <- c("composition")
lf <- melt(l_nw,id.vars=id.vars)
ggp_nw<-ggplot(lf,aes(composition,value))+
  facet_grid(variable~.,scales="free_y")+
  geom_bar(aes(composition,value),stat="identity", position=position_dodge())+
  xlab("Northwest")+ylab(" ")+theme(axis.text = element_text(size = 8),strip.text.y = element_text(size = 8))
print(ggp_nw)

pm <-read.csv(file = ".../Result/impr/pm_impr.csv",header = T)
pm<-pm%>%filter(Region=="Southeast")
O<-pm[,10:25]
f_se <- fa(O,nfactors = 5,rotate = "varimax")
print(f_se)
l_se <-read.csv(file = ".../Result/Source apportionment/f_se.csv",header = T)
id.vars <- c("composition")
lf <- melt(l_se,id.vars=id.vars)
ggp_se<-ggplot(lf,aes(composition,value))+
  facet_grid(variable~.,scales="free_y")+
  geom_bar(aes(composition,value),stat="identity", position=position_dodge())+
  xlab("Southeast")+ylab(" ")+theme(axis.text = element_text(size = 8),strip.text.y = element_text(size = 8))
print(ggp_se)

pm <-read.csv(file = ".../Result/impr/pm_impr.csv",header = T)
pm<-pm%>%filter(Region=="Southwest")
O<-pm[,10:25]
f_sw <- fa(O,nfactors = 5,rotate = "varimax")
print(f_sw)
l_sw <-read.csv(file = ".../Result/Source apportionment/f_sw.csv",header = T)
id.vars <- c("composition")
lf <- melt(l_sw,id.vars=id.vars)
ggp_sw<-ggplot(lf,aes(composition,value))+
  facet_grid(variable~.,scales="free_y")+
  geom_bar(aes(composition,value),stat="identity", position=position_dodge())+
  xlab("Southwest")+ylab(" ")+theme(axis.text = element_text(size = 8),strip.text.y = element_text(size = 8))
print(ggp_sw)

pm <-read.csv(file = ".../Result/impr/pm_impr.csv",header = T)
pm<-pm%>%filter(Region=="California")
O<-pm[,10:25]
f_ca <- fa(O,nfactors = 5,rotate = "varimax")
print(f_ca)
l_ca <-read.csv(file = ".../Result/Source apportionment/f_ca.csv",header = T)
id.vars <- c("composition")
lf <- melt(l_ca,id.vars=id.vars)
ggp_ca<-ggplot(lf,aes(composition,value))+
  facet_grid(variable~.,scales="free_y")+
  geom_bar(aes(composition,value),stat="identity", position=position_dodge())+
  xlab("California")+ylab(" ")+theme(axis.text = element_text(size = 8),strip.text.y = element_text(size = 8))
print(ggp_ca)
pdf("Fa.pdf")
gridExtra::grid.arrange(ggp_us,ggp_ne,ggp_nw,ggp_se,ggp_sw,ggp_ca, nrow = 3)
dev.off()