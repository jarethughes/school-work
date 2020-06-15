#APA Final Project
#By Jaret Hughes

#Import Data
rdat = read.csv("PPPdataV2.csv",header=TRUE)

#Summary Stats
summary(rdat)

#Plot
plot(rdat$time,rdat$error,main="Figure 1: Error in Polls Over Time",
     xlab="Days Until Election Day",ylab="Difference from Final Popular Vote Margin",xlim=c(min(rdat$time)-sd(rdat$time)/2,max(rdat$time)+sd(rdat$time)/2),
     ylim=c(min(rdat$error)-sd(rdat$error)/2,max(rdat$error)+sd(rdat$error)/2),col="blue",
     lty = 1,lwd=1,pch=1)

#Regression
reg=lm(error~time,data=rdat)
summary(reg)
abline(reg)
