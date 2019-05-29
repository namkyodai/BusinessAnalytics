library(lattice)
library(ggplot2)

#----1. Data

don <- read.csv("contribution.csv")
#or read directly from the web
#don <- read.csv("https://www.biz.uiowa.edu/faculty/jledolter/DataMining/contribution.csv")
#or

don[1:5,]

table(don$Class.Year)
barchart(table(don$Class.Year),horizontal=FALSE,xlab="Class Year",col="black")
#plot the same barchart using ggplot2
dev.copy(png,'alumni_classyear_bar.png',width = 800, height = 500)
dev.off()

p=ggplot(data.frame(table(don$Class.Year)), aes(x=Var1, y=Freq))+labs(y="Freq", x="Class Year") + geom_bar(stat="identity",width=0.8,color="blue",fill="steelblue")+geom_text(aes(label=Freq), vjust=-0.3, size=3.5)
p
dev.copy(png,'alumni_classyear_barggplot.png',width = 800, height = 500)
dev.off()


#ggplot(don, aes(x=don$Marital.Status,fill=don$Marital.Status))+  geom_bar()

#ggplot(don, aes(x=don$Marital.Status,fill=don$Marital.Status))+  geom_bar()+ facet_grid(don$Major ~ .)

#ggplot(don, aes(x=don$Major))+  geom_bar()+coord_flip()
#+ geom_bar(stat="bin", fill="steelblue")

don$TGiving=don$FY00Giving+don$FY01Giving+don$FY02Giving+don$FY03Giving+don$FY04Giving
mean(don$TGiving)
sd(don$TGiving)
quantile(don$TGiving,probs=seq(0,1,0.05))
quantile(don$TGiving,probs=seq(0.95,1,0.01))


#---------------------
plot.new()
par(mar=c(4.5,4.3,1,1)+0.1,mfrow=c(2,2))
hist(don$TGiving,main=NULL,xlab="Total Contribution")
hist(don$TGiving[don$TGiving!=0][don$TGiving[don$TGiving!=0]<=1000],main=NULL,xlab="Total Contribution")
boxplot(don$TGiving,horizontal=TRUE,xlab="Total Contribution")
boxplot(don$TGiving,outline=FALSE,horizontal=TRUE,xlab="Total Contribution")
dev.copy(png,'alumni_contributionplot.png',width = 800, height = 500)
dev.off()



ddd=don[don$TGiving>=30000,]
ddd
ddd1=ddd[,c(1:5,12)]
ddd1
ddd1[order(ddd1$TGiving,decreasing=TRUE),]



plot.new()
par(mar=c(4.5,4.3,1,1)+0.1,mfrow=c(2,2))
boxplot(TGiving~Class.Year,data=don,outline=FALSE, xlab="year")
boxplot(TGiving~Gender,data=don,outline=FALSE, xlab="sex")
boxplot(TGiving~Marital.Status,data=don,outline=FALSE,xlab="Marital status")
boxplot(TGiving~AttendenceEvent,data=don,outline=FALSE,xlab="Attend event or not")

dev.copy(png,'alumni_distribution_boxplot.png',width = 800, height = 500)
dev.off()

plot.new()
#par(mar=c(4.5,4.3,1,1)+0.1,mfrow=c(1,1)) #--> this doesnt work for barchart function

t4=tapply(don$TGiving,don$Major,mean,na.rm=TRUE)
t4
t5=table(don$Major)
t5
t6=cbind(t4,t5)
t7=t6[t6[,2]>10,]
t7[order(t7[,1],decreasing=TRUE),]


plot(barchart(t7[,1],col="black"))
dev.copy(png,'alumni_major_barplot.png',width = 800, height = 500)
dev.off()


#dat2<-data.frame("Type",t7)

#ggplot(data.frame(t7), aes(x=data.frame(t7)[,1]))+  geom_bar()

#ggplot(don, aes(x=data.frame(t7)[,1]))+  geom_bar()

#ggplot(don, aes(x=don$Major))+  geom_bar()+coord_flip()



t4=tapply(don$TGiving,don$Next.Degree,mean,na.rm=TRUE)
t4
t5=table(don$Next.Degree)
t5
t6=cbind(t4,t5)
t7=t6[t6[,2]>10,]
t7[order(t7[,1],decreasing=TRUE),]
plot(barchart(t7[,1],col="black"))
dev.copy(png,'alumni_degree_barplot.png',width = 800, height = 500)
dev.off()


#ggplot(don, aes(x=don$Next.Degree))+  geom_bar()+coord_flip()





