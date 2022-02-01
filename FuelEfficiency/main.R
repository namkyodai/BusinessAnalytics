## first we read in the data
df <- data.frame(read.csv("https://www.biz.uiowa.edu/faculty/jledolter/DataMining/FuelEfficiency.csv"))
#FuelEff <- read.csv("FuelEfficiency.csv")

#GPM-->Gallon per 100 miles
#MPG--> miles per Gallons
#WT --> weigt of the car
#DIS --> Cubic displacement
#NC --> No. of cylinders
#HP --> Hourse power
#ACC --> Acceleration
# ET --> engine type

#----------------------------
plot.new()
#par(mfrow=c(1,1))
par(mar=c(4,4,1,1)+0.1,mfrow=c(3,3),bg="white",cex = 1, cex.main = 1)
plot(GPM~MPG,data=df)
plot(GPM~WT,data=df)
plot(GPM~DIS,data=df)
plot(GPM~NC,data=df)
plot(GPM~HP,data=df)
plot(GPM~ACC,data=df)
plot(GPM~ET,data=df)

dev.copy(png,'fueleff_xyplot.png',width = 800, height = 800)
dev.off()

#draw using ggplot2
library(ggplot2)
library(dplyr)
require(gridExtra)
library(ggpubr)
plot.new()
#par(mfrow=c(1,1))

plot01 <- df%>%
  ggplot(aes(x=MPG, y=GPM))+
  geom_point()

plot02 <- df%>%
  ggplot(aes(x=WT, y=GPM))+
  geom_point()

plot03 <- df%>%
  ggplot(aes(x=DIS, y=GPM))+
  geom_point()

plot04 <- df%>%
  ggplot(aes(x=NC, y=GPM))+
  geom_point()

plot05 <- df%>%
  ggplot(aes(x=HP, y=GPM))+
  geom_point()

plot06 <- df%>%
  ggplot(aes(x=ACC, y=GPM))+
  geom_point()

plot07 <- df%>%
  ggplot(aes(x=ET, y=GPM))+
  geom_point()

#way 1 to draw all graphs in one pallete
grid.arrange(                       # First row with one plot spaning over 2 columns
             arrangeGrob(plot01, plot02, plot03, plot04, plot05, plot06, plot07, ncol = 3), # Second row with 2 plots in 2 different columns
             nrow = 1) 


#way 2 to draw all graphs in one pallete
grid.arrange(plot01, plot02, plot03, plot04, plot05, plot06, plot07, ncol = 3, nrow = 3)


df=df[-1] #ignore the MPG column
df

## regression on all data
m1=lm(GPM~.,data=df)
summary(m1)

cor(df)

## best subset regression in R
library(leaps)

X=df[,2:7]
y=df[,1]

#use the regsubsets function from package leaps to compute regression of the subsets
#https://www.rdocumentation.org/packages/leaps/versions/2.1-1/topics/regsubsets


out=summary(regsubsets(X,y,nbest=2,nvmax=ncol(X)))
tab=cbind(out$which,out$rsq,out$adjr2,out$cp)
tab

m2=lm(GPM~WT,data=df)
summary(m2)

## cross-validation (leave one out) for the model on all six regressors
n=length(df$GPM) #compute how many numbers of observation in the data.


for (k in 1:n) {
  train1=c(1:n)
  train=train1[train1!=k]
  ## the R expression "train1[train1!=k]" picks from train1 those
  ## elements that are different from k and stores those elements in the
  ## object train.
  ## For k=1, train consists of elements that are different from 1; that
  ## is 2, 3, …, n.
  m1=lm(GPM~.,data=df[train,])
  pred=predict(m1,newdat=df[-train,]) #adding the new data, which is ignored earlier
  obs=df$GPM[-train]
  diff[k]=obs-pred
  percdiff[k]=abs(diff[k])/obs
}
me=mean(diff)
rmse=sqrt(mean(diff**2))
mape=100*(mean(percdiff))
me   # mean error
rmse # root mean square error
mape # mean absolute percent error

## cross-validation (leave one out) for the model on weight only
n=length(df$GPM)

for (k in 1:n) {
  train1=c(1:n)
  train=train1[train1!=k]
  m2=lm(GPM~WT,data=df[train,])
  pred=predict(m2,newdat=df[-train,])
  obs=df$GPM[-train]
  diff[k]=obs-pred
  percdiff[k]=abs(diff[k])/obs
}
me=mean(diff)
rmse=sqrt(mean(diff**2))
mape=100*(mean(percdiff))
me   # mean error
rmse # root mean square error
mape # mean absolute percent error

