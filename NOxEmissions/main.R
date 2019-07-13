#set working directory for the file
path <- rstudioapi::getActiveDocumentContext()$path
Encoding(path) <- "UTF-8"
setwd(dirname(path))
getwd()
#-------------------------------------

library(locfit)

## first we read in the data
ethanol <- read.csv("ethanol.csv")
#OldFaithful <- read.csv("https://www.biz.uiowa.edu/faculty/jledolter/DataMining/ethanol.csv")
ethanol[1:3,]

## density histogram
hist(ethanol$NOx,freq=FALSE)
## smoothed density histogram
fit <- locfit(~lp(NOx),data=ethanol)
plot(fit)

## experiment with the parameters of locfit
fit <- locfit(~lp(NOx,deg=1),data=ethanol)
plot(fit)
fit <- locfit(~lp(NOx,nn=0.7,deg=1),data=ethanol)
plot(fit)
fit <- locfit(~lp(NOx,nn=0.5,deg=1),data=ethanol)
plot(fit)

fit <- locfit(~lp(NOx,deg=2),data=ethanol)
plot(fit)
fit <- locfit(~lp(NOx,nn=0.7,deg=2),data=ethanol)
plot(fit)
fit <- locfit(~lp(NOx,nn=0.5,deg=2),data=ethanol)
plot(fit)

fit <- locfit(~lp(NOx,deg=3),data=ethanol)
plot(fit)
fit <- locfit(~lp(NOx,nn=0.7,deg=3),data=ethanol)
plot(fit)
fit <- locfit(~lp(NOx,nn=0.5,deg=3),data=ethanol)
plot(fit)

## standard regression of NOx on the equivalence ratio
plot(NOx~EquivRatio,data=ethanol)
fitreg=lm(NOx~EquivRatio,data=ethanol)
plot(NOx~EquivRatio,data=ethanol)
abline(fitreg)

## local polynomial regression of NOx on the equivalence ratio
## fit with a 50% nearest neighbor bandwidth.
fit <- locfit(NOx~lp(EquivRatio,nn=0.5),data=ethanol)
plot(fit)

## experiment with the parameters of locfit
fit <- locfit(NOx~lp(EquivRatio,nn=0.2),data=ethanol)
plot(fit)
fit <- locfit(NOx~lp(EquivRatio,nn=0.8),data=ethanol)
plot(fit)
fit <- locfit(NOx~lp(EquivRatio,deg=1),data=ethanol)
plot(fit)
fit <- locfit(NOx~lp(EquivRatio,deg=2),data=ethanol)
plot(fit)
## cross-validation
alpha<-seq(0.20,1,by=0.01)
n1=length(alpha)
g=matrix(nrow=n1,ncol=4)
for (k in 1:length(alpha)) {
  g[k,]<-gcv(NOx~lp(EquivRatio,nn=alpha[k]),data=ethanol)
}
g
plot(g[,4]~g[,3],ylab="GCV",xlab="degrees of freedom")

nn=alpha[which.min(g[,4])]

f1=locfit(NOx~lp(EquivRatio,nn=nn0),data=ethanol)
f1
plot(f1)

## local polynomial regression on both E and C
plot(NOx~EquivRatio,data=ethanol)
plot(NOx~CompRatio,data=ethanol)
fit <- locfit(NOx~lp(EquivRatio,CompRatio,scale=TRUE),data=ethanol)
plot(fit)

## experiment with the parameters of locfit
fit <- locfit(NOx~lp(EquivRatio,CompRatio,nn=0.5,scale=TRUE),data=ethanol)
plot(fit)
fit <- locfit(NOx~lp(EquivRatio,CompRatio,deg=0,scale=TRUE),data=ethanol)
plot(fit)



