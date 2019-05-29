#this subroutine is used to estimate the cashflow of an investment for pump stations
Npumps = 6 #total number of pumps
TPeriod=10 #this infers 5 years
TC1<-matrix(nrow=TPeriod,ncol = Npumps) #Option 1
TC2<-matrix(nrow=TPeriod,ncol = Npumps) #Option 2
CAPEX<-matrix(nrow=TPeriod,ncol = Npumps) #Captical investment cost
OPEX<-matrix(nrow=TPeriod,ncol = Npumps) #Operational Cost
delta<-matrix(nrow=TPeriod,ncol = Npumps) #decrease in efficiency
epsilonold<-matrix(nrow=TPeriod,ncol = Npumps) #decrease in efficiency
epsilonnew<-matrix(nrow=TPeriod,ncol = Npumps) #decrease in efficiency
u<-matrix(nrow=TPeriod,ncol = Npumps) #ultilization
rho=0.085 #discount factor
pricee=6.5
hoursepower=500
utilization=0.98
Newpumpcost=5000000
#Option 1 - Stagging investment for pump
for (t in 1: TPeriod){
  if (t<2){
    for (i in 1:Npumps){
      epsilonold[t,i]<-1.05
    }
  } else {
    for (i in 1:Npumps){
    epsilonold[t,i]<-epsilonold[t-1,i]*1.05
    }
  }
}
for (t in 1:TPeriod){
  for (i in 1:Npumps){
    delta[,]<-0
    delta[1,1]<-1
    epsilonnew[1,1]<-1
    delta[2,2]<-1
    epsilonnew[2,2]<-1
    delta[3,3]<-1
    epsilonnew[3,3]<-1
    delta[4,4]<-1
    epsilonnew[4,4]<-1
    delta[5,5]<-1
    epsilonnew[5,5]<-1
    delta[6,6]<-1
    epsilonnew[6,6]<-1
    CAPEX[t,i]<-Newpumpcost #peso
    u[t,i]<-utilization
    OPEX[t,i]<- pricee*hoursepower*365*24*0.746*u[t,i] #peso
      }
}
#this part is used to set the parameter value for epsilon
for (t in 1: TPeriod){
  for (i in 1:Npumps){
    if (delta[t,i]==1){
      epsilonnew[t,i]<-1
    } else if (i==1 & t==1){
        epsilonnew[t,i]<-1
    } else if (t>1){
      epsilonnew[t,i]<-epsilonnew[t-1,i]*1
    }
  }
}
epsilonnew[is.na(epsilonnew)] <- 0
#redefine the value of epsilonold based on new value of epsilonnew
for (t in 1: TPeriod){
  for (i in 1:Npumps){
    if (epsilonnew[t,i]<1){
      epsilonold[t,i]<-epsilonold[t,i]
    } else if (epsilonnew[t,i]==1){
       epsilonold[t,i]<-0
     } 
   }
 }
for (i in 1:Npumps){
  for (t in 1:TPeriod){
    TC1[t,i]<-(((CAPEX[t,i]*delta[t,i]+OPEX[t,i]*epsilonnew[t,i]))+(1-delta[t,i])*OPEX[t,i]*epsilonold[t,i])/(1+rho)**t
  }
}
cat("Replacement of pumps \n")
print(TC1)
#stop("dd")
#Option 2 - Do Nothing
for (t in 1:TPeriod){
  for (i in 1:Npumps){
    delta[,]<-0
    CAPEX[t,i]<-0 #peso
    u[t,i]<-utilization
    epsilonnew[t,i]<-1.0
      OPEX[t,i]<- pricee*hoursepower*365*24*0.746*u[t,i] #peso
  }
}
for (t in 1: TPeriod){
  if (t<2){
    for (i in 1:Npumps){
      epsilonold[t,i]<-1.05
    }
  } else {
    for (i in 1:Npumps){
      epsilonold[t,i]<-epsilonold[t-1,i]*1.05
    }
  }
}
for (i in 1:Npumps){
  for (t in 1:TPeriod){
    TC2[t,i]<-(((CAPEX[t,i]+OPEX[t,i]*epsilonnew[t,i])*delta[t,i])+(1-delta[t,i])*OPEX[t,i]*epsilonold[t,i])/(1+rho)**t
  }
}
cat("Do Nothing \n")
print(TC2)

cat("The difference of investment \n")
print(TC1-TC2)
#plot the graph for comparison
library(ggplot2)
time=c(1:TPeriod)
x<-data.frame(dat=TC1[,1],IS=rep("TC1"))
y<-data.frame(dat=TC2[,1],IS=rep("TC2"))
x<-cbind(time,x)
y<-cbind(time,y)
xy <- rbind(x, y)
ggplot(xy, aes(fill=IS, y=dat, x=factor(time))) + geom_bar(position="dodge", stat="identity")+labs(y="Peso", x="years") 
dev.copy(png,'ROIpump_roi.png',width = 800, height = 500)
dev.off()

stop("")
TC1<-data.frame(TC1,IS=rep("TC1"))
TC2<-data.frame(TC2,IS=rep("TC2"))
#TC2<-data.frame(TC2=TC2[,1])
#TC1<-data.frame(TC1)
#TC2<-data.frame(TC2)
TC1<-cbind(time,TC1)
TC2<-cbind(time,TC2)
library(reshape)
mdataTC1 <- melt(TC1,id=c("time","IS"))
mdataTC2 <- melt(TC2,id=c("time","IS"))
#joint the two data
total <- rbind(mdataTC1, mdataTC2)
ggplot(total, aes(fill=IS, y=value, x=factor(time)),variable=X1) + geom_bar(position="dodge", stat="identity")
#ggplot(total, aes(fill=variable, y=value, x=factor(time)),variable=X1) + geom_bar(position="fill", stat="identity")

#ggplot(total, aes(y=value, x=variable, color=IS, fill=IS)) + geom_bar( stat="identity") +   facet_wrap(~factor(time))
