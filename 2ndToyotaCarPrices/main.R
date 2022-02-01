## first we read in the data
df <- data.frame(read.csv("https://www.biz.uiowa.edu/faculty/jledolter/DataMining/ToyotaCorolla.csv"))
#toyota <- read.csv("ToyotaCorolla.csv")
df[1:3,]
summary(df)
hist(df$Price)

library(ggplot2)
df %>%
  ggplot(aes(x=Price))+
  geom_density()+
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white", alpha=0.3)

## FuelType with its three nominal outcomes: CNG, Diesel, and Petrol
v1=rep(1,length(df$FuelType))

v2=rep(0,length(df$FuelType))

df$FuelType1=ifelse(df$FuelType=="CNG",v1,v2) #return value of CNG to v1, otherwise 0


df$FuelType2=ifelse(df$FuelType=="Diesel",v1,v2) #return value of Diesel to v1, otherwise 0
auto=df[-4] #ignore column 4 (Fueltype)
auto[1:3,]

plot.new()
#par(mfrow=c(1,1))
par(mar=c(4,4,1,1)+0.1,mfrow=c(4,2),bg="white",cex = 0.7, cex.main = 1)

plot(Price~Age,data=auto)
plot(Price~KM,data=auto)
plot(Price~HP,data=auto)
plot(Price~MetColor,data=auto)
plot(Price~Automatic,data=auto)
plot(Price~CC,data=auto)
plot(Price~Doors,data=auto)
plot(Price~Weight,data=auto)

dev.copy(png,'toyota_xyplot.png',width = 500, height = 800)
dev.off()



library(dplyr)
require(gridExtra)
library(ggpubr)
plot.new()


plot01 <- df%>%
  ggplot(aes(x=Age, y=Price))+
  geom_point()

plot02 <- df%>%
  ggplot(aes(x=KM, y=Price))+
  geom_point()

plot03 <- df%>%
  ggplot(aes(x=HP, y=Price))+
  geom_point()

plot04 <- df%>%
  ggplot(aes(x=MetColor, y=Price))+
  geom_point()

plot05 <- df%>%
  ggplot(aes(x=Automatic, y=Price))+
  geom_point()

plot06 <- df%>%
  ggplot(aes(x=CC, y=Price))+
  geom_point()

plot07 <- df%>%
  ggplot(aes(x=Doors, y=Price))+
  geom_point()

plot08 <- df%>%
  ggplot(aes(x=Weight, y=Price))+
  geom_point()


grid.arrange(plot01, plot02, plot03, plot04, plot05, plot06, plot07, plot07,ncol = 2, nrow = 4)


## regression on all data
m1=lm(Price~.,data=auto)
summary(m1)

set.seed(1)
## fixing the seed value for the random selection guarantees the 
## same results in repeated runs
n=length(auto$Price)
n1=1000
n2=n-n1
train=sample(1:n,n1) # generate random n1 integer number of data among the size from 1 to n.

## regression on training set
m1=lm(Price~.,data=auto[train,])
summary(m1)
pred=predict(m1,newdat=auto[-train,]) #adding a set of n2 number into regression model
obs=auto$Price[-train]
diff=obs-pred
percdiff=abs(diff)/obs
me=mean(diff)
rmse=sqrt(sum(diff**2)/n2)
mape=100*(mean(percdiff))
me   # mean error
rmse # root mean square error
mape # mean absolute percent error 

## cross-validation (leave one out)
n=length(auto$Price)
for (k in 1:n) {
  train1=c(1:n)
  train=train1[train1!=k]
  m1=lm(Price~.,data=auto[train,])
  pred=predict(m1,newdat=auto[-train,])
  obs=auto$Price[-train]
  diff[k]=obs-pred
  percdiff[k]=abs(diff[k])/obs
}
me=mean(diff)
rmse=sqrt(mean(diff**2))
mape=100*(mean(percdiff))
me   # mean error
rmse # root mean square error
mape # mean absolute percent error 

## cross-validation (leave one out): Model with just Age
n=length(auto$Price)
diff=dim(n)
percdiff=dim(n)
for (k in 1:n) {
  train1=c(1:n)
  train=train1[train1!=k]
  m1=lm(Price~Age,data=auto[train,])
  pred=predict(m1,newdat=auto[-train,])
  obs=auto$Price[-train]
  diff[k]=obs-pred
  percdiff[k]=abs(diff[k])/obs
}
me=mean(diff)
rmse=sqrt(mean(diff**2))
mape=100*(mean(percdiff))
me   # mean error
rmse # root mean square error
mape # mean absolute percent error 

## Adding the squares of Age and KM to the model
auto$Age2=auto$Age^2
auto$KM2=auto$KM^2
m11=lm(Price~Age+KM,data=auto)
summary(m11)
m12=lm(Price~Age+Age2+KM+KM2,data=auto)
summary(m12)
m13=lm(Price~Age+Age2+KM,data=auto)
summary(m13)

#----------------
plot.new()
#par(mfrow=c(1,1))
par(mar=c(4,4,1,1)+0.1,mfrow=c(2,2),bg="white",cex = 0.7, cex.main = 1)
plot(m11$res~m11$fitted)
hist(m11$res)
plot(m12$res~m12$fitted)
dev.copy(png,'toyota_m11m12.png',width = 800, height = 500)
dev.off()
