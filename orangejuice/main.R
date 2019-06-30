## Install packages from CRAN; use any USA mirror
library(lattice)
#oj <- read.csv("https://www.biz.uiowa.edu/faculty/jledolter/DataMining/oj.csv")
oj <- read.csv("oj.csv")
oj$store <- factor(oj$store) #change numberic value of store into categorical data
oj[1:2,]
t1=tapply(oj$logmove,oj$brand,FUN=mean,na.rm=TRUE) #calculate the mean of each brand using logmove value.
t1
t2=tapply(oj$logmove,INDEX=list(oj$brand,oj$week),FUN=mean,na.rm=TRUE) #calculate the mean of logmove value based on index lists per week.
t2
#plot each graph as time serieas data per week.
plot.new()
par(mar=c(4.5,4.3,1,1)+0.1,mfrow=c(2,2),bg="white",cex = 1, cex.main = 0.6)
plot(t2[1,],type= "l",xlab="week",ylab="dominicks",ylim=c(7,12),cex.axis = 1,las = 1)
plot(t2[2,],type= "l",xlab="week",ylab="minute.maid",ylim=c(7,12),cex.axis = 1,las = 1)
plot(t2[3,],type= "l",xlab="week",ylab="tropicana",ylim=c(7,12),cex.axis = 1,las = 1)
dev.copy(png,'oj_weekmean01.png',width = 1600, height = 600)
dev.off()
#-------------------------------
#now we combine the three above graphs into one single graphs for ease of comparison
logmove=c(t2[1,],t2[2,],t2[3,])
week1=c(40:160)
week=c(week1,week1,week1)
brand1=rep(1,121)
brand2=rep(2,121)
brand3=rep(3,121)
brand=c(brand1,brand2,brand3)
plot.new()
xyplot(logmove~week|factor(brand),type= "l",layout=c(1,3),col="black")
dev.copy(png,'oj_weekmean02.png',width = 1000, height = 600)
dev.off()
#-----------------------------
plot.new()
par(mfrow=c(1,1))
#par(mar=c(4.5,4.3,1,1)+0.1,mfrow=c(2,2),bg="white",cex = 1, cex.main = 0.6)
boxplot(logmove~brand,data=oj) # compare logmove of 3 branch using boxplot
dev.copy(png,'oj_logmovebrandboxplot.png',width = 800, height = 600)
dev.off()
histogram(~logmove|brand,data=oj,layout=c(1,3)) # compare logmove of 3 branch using histogram
dev.copy(png,'oj_logmovebrandhist.png',width = 1000, height = 600)
dev.off()
a1=densityplot(~logmove|brand,data=oj,layout=c(1,3),plot.points=FALSE) # compare logmove of 3 branch using density plot
a2=densityplot(~logmove,groups=brand,data=oj,plot.points=FALSE) ## compare logmove of 3 branch using density plot in a one frame
#using xyplot to see the spartial distribution of data weekly
library(gridExtra) #this package allows to plot multiple graphs in the same plot despite the difference in plotting engines (e.g. ggplot or barchart)
grid.arrange(a1, a2,  ncol = 2) #display the two plot a and p
dev.copy(png,'oj_logmovedensity.png',width = 1000, height = 500)
dev.off()

#-------------------------------------------------
xyplot(logmove~week,data=oj,col="black")
dev.copy(png,'oj_logmoveweekspartial.png',width = 1000, height = 500)
dev.off()

#---------------------------------
xyplot(logmove~week|brand,data=oj,layout=c(1,3),col="black")
dev.copy(png,'oj_logmoveweekspartialbrand.png',width = 1000, height = 500)
dev.off()

#---------------------------------
xyplot(logmove~price,data=oj,col="black")
dev.copy(png,'oj_logmoveprice.png',width = 1000, height = 500)
dev.off()

#---------------------------------
xyplot(logmove~price|brand,data=oj,layout=c(1,3),col="black")
dev.copy(png,'oj_logmovepricebrand.png',width = 1000, height = 500)
dev.off()

#---------------------------------
smoothScatter(oj$price,oj$logmove)
dev.copy(png,'oj_logmovepricesmooth.png',width = 1000, height = 500)
dev.off()
#---------------------------------
a1=densityplot(~logmove,groups=feat, data=oj, plot.points=FALSE)
a2=xyplot(logmove~price,groups=feat, data=oj)
grid.arrange(a1, a2,  ncol = 2) #display the two plot a and p
dev.copy(png,'oj_logmovepricegroupfeat.png',width = 1200, height = 500)
dev.off()

#------------------------------------------------
oj1=oj[oj$store == 5,]
xyplot(logmove~week|brand,data=oj1,type="l",layout=c(1,3),col="black")
dev.copy(png,'oj_logmovebrand.png',width = 1200, height = 500)
dev.off()

xyplot(logmove~price,data=oj1,col="black")
dev.copy(png,'oj_logmovepricexyplot.png',width = 800, height = 500)
dev.off()


xyplot(logmove~price|brand,data=oj1,layout=c(1,3),col="black")
dev.copy(png,'oj_logmovepricebrandxyplot.png',width = 1000, height = 500)
dev.off()

densityplot(~logmove|brand,groups=feat,data=oj1,plot.points=FALSE)
dev.copy(png,'oj_logmovebranddenst.png',width = 1200, height = 500)
dev.off()

xyplot(logmove~price|brand,groups=feat,data=oj1)
dev.copy(png,'oj_logmovepricebrandxyplot.png',width = 1200, height = 500)
dev.off()

#----------------------------
t21=tapply(oj$INCOME,oj$store,FUN=mean,na.rm=TRUE)
t21
t21[t21==max(t21)]
t21[t21==min(t21)]

oj1=oj[oj$store == 62,]
oj2=oj[oj$store == 75,]
oj3=rbind(oj1,oj2)

#----------------------------------------
a1=xyplot(logmove~price|store,data=oj3)
a2=xyplot(logmove~price|store,groups=feat,data=oj3)
grid.arrange(a1, a2,  ncol = 1) #display the two plot a and p
dev.copy(png,'oj_logmovexyplotprice.png',width = 500, height = 1000)
dev.off()

## store in the wealthiest neighborhood
plot.new()
par(mar=c(4,4,1,1)+0.1,mfrow=c(1,2),bg="white",cex = 1, cex.main = 1)
mhigh=lm(logmove~price,data=oj1)
summary(mhigh)
plot(logmove~price,data=oj1,xlim=c(0,4),ylim=c(0,13), main="62 = wealthiest store")
abline(mhigh)
## store in the poorest neighborhood
mlow=lm(logmove~price,data=oj2)
summary(mlow)
plot(logmove~price,data=oj2,xlim=c(0,4),ylim=c(0,13), main="75 = poorest store")
abline(mlow)
dev.copy(png,'oj_logmovepriceoj2.png',width = 1000, height = 300)
dev.off()
