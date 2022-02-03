library(locfit)  
## first we read in the data
## first we read in the data
#OldFaithful <- read.csv("https://www.biz.uiowa.edu/faculty/jledolter/DataMining/OldFaithful.csv")


df <-data.frame(read.csv("OldFaithful.csv"))
df[1:10,]

## density histograms and smoothed density histograms
## time of eruption
plot.new()
#par(mfrow=c(1,1))
#par(mar=c(4,4,1,1)+0.1,mfrow=c(1,2),bg="white",cex = 1, cex.main = 1)
hist(df$TimeEruption,freq=FALSE)

library(ggplot2)

df %>%
  ggplot(aes(x=TimeEruption))+
  geom_histogram(aes(y=..density..))

df %>%
  ggplot(aes(x=TimeWaiting))+
  geom_histogram(aes(y=..density..))


#use locfit https://www.rdocumentation.org/packages/locfit/versions/19980714-2/topics/locfit
fit1 <- locfit(~lp(TimeEruption),data=df)
#lp function https://www.rdocumentation.org/packages/lpSolve/versions/5.6.13.1/topics/lp
#lp is a local polynomial model term for Locfit models.
#https://www.rdocumentation.org/packages/locfit/versions/1.5-9.1/topics/lp
plot(fit1)
dev.copy(png,'oldfaithful_timeeruption01.png',width = 800, height = 400)
dev.off()


library(locfit)
qplot(hwy, cty, data=mpg) + 
  geom_smooth(method="locfit")





## waiting time to next eruption
hist(df$TimeWaiting,freq=FALSE)
fit2 <- locfit(~lp(TimeWaiting),data=df)
plot(fit2)
dev.copy(png,'oldfaithful_TimeWaiting01.png',width = 800, height = 400)
dev.off()

#------------------------------
## experiment with different smoothing constants
fit3 <- locfit(~lp(TimeWaiting,nn=0.9,deg=2),data=df) #nn is the nearest neighbour component, and deg is the degree of polynomial. default value of nn is 0.6 and deg is 2.
plot(fit3)
fit4 <- locfit(~lp(TimeWaiting,nn=0.3,deg=2),data=df)
plot(fit4)
dev.copy(png,'oldfaithful_TimeWaiting02.png',width = 800, height = 400)
dev.off()


## cross-validation of smoothing constant 
## for waiting time to next eruption
alpha<-seq(0.20,1,by=0.01)
n1=length(alpha)
g=matrix(nrow=n1,ncol=4)
for (k in 1:length(alpha)) {
  g[k,]<-gcv(~lp(TimeWaiting,nn=alpha[k]),data=df)
}
g
#gcv is used to estimate the penalty coefficient from the generalized cross-validation criteria. https://www.rdocumentation.org/packages/SpatialExtremes/versions/2.0-7/topics/gcv

plot(g[,4]~g[,3],ylab="GCV",xlab="degrees of freedom")
#the minimum point of the curve indicate the best value of nn. In this case, we can find the minimum value point.
which.min(g[,4])
#This indicate 
nn=alpha[which.min(g[,4])] #this is the value of the minimum nn.

fit5 <- locfit(~lp(TimeWaiting,nn=nn,deg=2),data=df)
plot(fit5)
dev.copy(png,'oldfaithful_TimeWaiting03.png',width = 800, height = 400)
dev.off()


#------------------------
## local polynomial regression of TimeEruption on TimeWaiting
plot(TimeWaiting~TimeEruption,data=df)
# standard regression fit
fitreg=lm(TimeWaiting~TimeEruption,data=df)
plot(TimeWaiting~TimeEruption,data=df)
abline(fitreg)
dev.copy(png,'oldfaithful_TimeWaitingvseruption01.png',width = 800, height = 400)
dev.off()

#-----------------------------------
# fit with nearest neighbor bandwidth

plot.new()
#par(mfrow=c(1,1))
par(mar=c(4,4,1,1)+0.1,mfrow=c(2,2),bg="white",cex = 1, cex.main = 1)

fit6 <- locfit(TimeWaiting~lp(TimeEruption),data=df)
plot(fit6)
fit7 <- locfit(TimeWaiting~lp(TimeEruption,deg=1),data=df)
plot(fit7)
fit8 <- locfit(TimeWaiting~lp(TimeEruption,deg=0),data=df)
plot(fit8)  
hist(df$TimeEruption,freq=FALSE)
dev.copy(png,'oldfaithful_TimeWaitingvseruption02.png',width = 800, height = 800)
dev.off()
