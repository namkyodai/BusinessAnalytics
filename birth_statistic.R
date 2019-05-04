library(lattice)
library(nutshell)
data(births2006.smpl)
births2006.smpl[1:5,]
births.dow=table(births2006.smpl$DOB_WK)
#print(births.dow)
births.dow

barchart(births.dow,ylab="Day of Week",col="black")

