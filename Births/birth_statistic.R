library(lattice)
library(nutshell)
data(births2006.smpl)
births2006.smpl[1:5,]
births.dow=table(births2006.smpl$DOB_WK)
#print(births.dow)
births.dow

barchart(births.dow,ylab="Day of Week",col="black")
dev.copy(png,'births_dow.png')
dev.off()


#Mu???n xem li???t kê theo phuong pháp d???

dob.dm.tbl=table(WK=births2006.smpl$DOB_WK,MM=births2006.smpl$DMETH_REC)
dob.dm.tbl

dob.dm.tbl=dob.dm.tbl[,-2]
dob.dm.tbl

#trellis.device()
barchart(dob.dm.tbl,ylab="Day of Week")
dev.copy(png,'births_dow_dmeth_rec.png')
dev.off()


barchart(dob.dm.tbl,horizontal=F,groups=FALSE, xlab="Day of Week",col="black")
dev.copy(png,'births_dow_dmeth_rec_fre.png')
dev.off()



histogram(~DBWT|DPLURAL,data=births2006.smpl,layout=c(1,5), col="black")
dev.copy(png,'births_dplural_dis.png')
dev.off()

histogram(~DBWT|DMETH_REC,data=births2006.smpl,layout=c(1,3), col="black")
dev.copy(png,'births_dmethrec_dis.png')
dev.off()



densityplot(~DBWT|DPLURAL,data=births2006.smpl,layout=c(1,5), plot.points=FALSE,col="black")
dev.copy(png,'births_dmethrec_density.png')
dev.off()


densityplot(~DBWT,groups=DPLURAL,data=births2006.smpl, plot.points=FALSE)
dev.copy(png,'births_DPLURAL_densityall.png')
dev.off()



dotplot(~DBWT|DPLURAL,data=births2006.smpl,layout=c(1,5), plot.points=FALSE,col="black")
dev.copy(png,'births_DPLURAL_dotplot.png')
dev.off()



xyplot(DBWT~DOB_WK,data=births2006.smpl,col="black")
dev.copy(png,'births_DBWTDOB_xy.png')
dev.off()



xyplot(DBWT~DOB_WK|DPLURAL,data=births2006.smpl,layout=c(1,5), col="black")
dev.copy(png,'births_DBWTDOB_xydplural.png')
dev.off()



xyplot(DBWT~WTGAIN,data=births2006.smpl,col="black")
dev.copy(png,'births_DBWTwtgain_xy.png')
dev.off()



xyplot(DBWT~WTGAIN|DPLURAL,data=births2006.smpl,layout=c(1,5), col="black")
dev.copy(png,'births_DBWTwtgaindplural_xy.png')
dev.off()



smoothScatter(births2006.smpl$WTGAIN,births2006.smpl$DBWT)
dev.copy(png,'births_WTGAIN_scatter.png')
dev.off()



## boxplot is the command for a box plot in the standard graphics
## package
boxplot(DBWT~APGAR5,data=births2006.smpl,ylab="DBWT", xlab="AGPAR5")
dev.copy(png,'births_PGAR5box.png')
dev.off()


boxplot(DBWT~DOB_WK,data=births2006.smpl,ylab="DBWT", xlab="Day of Week")
dev.copy(png,'births_DOB_WKbox.png')
dev.off()


## bwplot is the command for a box plot in the lattice graphics
## package. There you need to declare the conditioning variables
## as factors
bwplot(DBWT~factor(APGAR5)|factor(SEX),data=births2006.smpl,
        xlab="AGPAR5")
dev.copy(png,'births_APGAR5bwplot.png')
dev.off()



bwplot(DBWT~factor(DOB_WK),data=births2006.smpl,
        xlab="Day of Week")
dev.copy(png,'births_DOB_WKbwplot.png')
dev.off()



fac=factor(births2006.smpl$DPLURAL)
res=births2006.smpl$DBWT
t4=tapply(res,fac,mean,na.rm=TRUE)

t4

t5=tapply(births2006.smpl$DBWT,INDEX=list(births2006.smpl$DPLURAL,
                                           births2006.smpl$SEX),FUN=mean,na.rm=TRUE)

barplot(t4,ylab="DBWT")
dev.copy(png,'births_DBWTt4barplot.png')
dev.off()

barplot(t5,beside=TRUE,ylab="DBWT")
dev.copy(png,'births_DBWTt5barplot.png')
dev.off()


t5=table(births2006.smpl$ESTGEST)


new=births2006.smpl[births2006.smpl$ESTGEST != 99,]

t51=table(new$ESTGEST)
t51

t6=tapply(new$DBWT,INDEX=list(cut(new$WTGAIN,breaks=10),
                               cut(new$ESTGEST,breaks=10)),FUN=mean,na.rm=TRUE)
t6
levelplot(t6,scales = list(x = list(rot = 90)))
dev.copy(png,'births_levelplott6.png')
dev.off()


contourplot(t6,scales = list(x = list(rot = 90)))
dev.copy(png,'births_contourplott6.png')
dev.off()














