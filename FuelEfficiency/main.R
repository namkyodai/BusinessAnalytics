## first we read in the data
FuelEff <- read.csv("https://www.biz.uiowa.edu/faculty/jledolter/DataMining/FuelEfficiency.csv")
#FuelEff <- read.csv("FuelEfficiency.csv")

FuelEff

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
plot(GPM~MPG,data=FuelEff)
plot(GPM~WT,data=FuelEff)
plot(GPM~DIS,data=FuelEff)
plot(GPM~NC,data=FuelEff)
plot(GPM~HP,data=FuelEff)
plot(GPM~ACC,data=FuelEff)
plot(GPM~ET,data=FuelEff)

dev.copy(png,'fueleff_xyplot.png',width = 800, height = 800)
dev.off()


FuelEff=FuelEff[-1] #ignore the MPG column
FuelEff

## regression on all data
m1=lm(GPM~.,data=FuelEff)
summary(m1)

cor(FuelEff)

## best subset regression in R
library(leaps)  
X=FuelEff[,2:7]
y=FuelEff[,1]

#use the regsubsets function from package leaps to compute regression of the subsets
#https://www.rdocumentation.org/packages/leaps/versions/2.1-1/topics/regsubsets


out=summary(regsubsets(X,y,nbest=2,nvmax=ncol(X)))
tab=cbind(out$which,out$rsq,out$adjr2,out$cp)
tab

m2=lm(GPM~WT,data=FuelEff)
summary(m2)

## cross-validation (leave one out) for the model on all six regressors
n=length(FuelEff$GPM)
diff=dim(n)
percdiff=dim(n)
for (k in 1:n) {
  train1=c(1:n)
  train=train1[train1!=k]
  ## the R expression "train1[train1!=k]" picks from train1 those 
  ## elements that are different from k and stores those elements in the
  ## object train. 
  ## For k=1, train consists of elements that are different from 1; that 
  ## is 2, 3, …, n.
  m1=lm(GPM~.,data=FuelEff[train,])
  pred=predict(m1,newdat=FuelEff[-train,]) #adding the new data, which is ignored earlier
  obs=FuelEff$GPM[-train]
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
n=length(FuelEff$GPM)
diff=dim(n)
percdiff=dim(n)
for (k in 1:n) {
  train1=c(1:n)
  train=train1[train1!=k]
  m2=lm(GPM~WT,data=FuelEff[train,])
  pred=predict(m2,newdat=FuelEff[-train,])
  obs=FuelEff$GPM[-train]
  diff[k]=obs-pred
  percdiff[k]=abs(diff[k])/obs
}
me=mean(diff)
rmse=sqrt(mean(diff**2))
mape=100*(mean(percdiff))
me   # mean error
rmse # root mean square error
mape # mean absolute percent error

