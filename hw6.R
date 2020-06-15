getwd()
setwd("C://Users/Jaret/Documents/School Work/College/Senior Year/Fall/e307/")
getwd()

rdat = read.table("p4_8.prn", header=TRUE)

library(car)
library(e1071)

rdat$ln.time = log(rdat$time)
rdat$time2 = (rdat$time)^2

#
#PRELIMINARY STATISTICS
#

summary(rdat)
var(rdat$chapman)
sd(rdat$chapman)
#These are preliminary reminders of ways to get important statistics from the data directly 
#Especially summary() should be done for all vectors you're analyzing

h_chap = hist(rdat$chapman,freq = TRUE,
               main="Figure 1: Histogram of Yields in Chapman",
               xlab="Yield (tonnes/hectare)",
               breaks=seq(0,2.5,0.25))
#These histograms are a way to visualize the data and its distribution

plot_chap = plot(rdat$time,rdat$chapman,
                  main="Figure 2: Yields in Chapman over Time",
                  xlab="Year",ylab="Yield (tonnes/hectare)",xlim=c(0,50),
                  ylim=c(0,2.5),col="black",lty=1,lwd=1,pch=1)
#This is to plot the data on separate tables

#
#LINEAR REGRESSION
#

reg_chap = lm(chapman~time, data = rdat)
summary(reg_chap)
#This creates our linear regression equations for each region

plot(rdat$time,rdat$chapman,
                 main="Figure 1: Regression of Yields in Chapman over Time - Model 1",
                 xlab="Year",ylab="Yield (tonnes/hectare)",xlim=c(0,50),
                 ylim=c(0,2.5),col="black",lty=1,lwd=1,pch=1)
abline(reg_chap, col = "red")
confint(reg_chap,level=0.95)

summary(reg_chap$residuals)
sd(reg_chap$residuals)

plot(reg_chap$fitted.values,reg_chap$residuals,main="Figure 2: Residuals against Predicted Yield - Model 1",
     xlab="Predicted Yield",ylab="Estimated Residuals",xlim=c(0.6,1.5),ylim=c(-0.7,0.7),col="black",
     lty=1,lwd=1,pch=1)
abline(h=0,col="red")

h_res = hist(reg_chap$residuals, freq = TRUE, breaks = seq(-0.75,0.75,0.1),
                  main = "Figure 3: Histogram of Estimated Residuals", xlab = "Estimated Residuals",
                  xlim = c(-0.75,0.75))

ehat_sk = skewness(reg_chap$residuals,type=1)
ehat_ku = kurtosis(reg_chap$residuals,type=1)

JBstat = (48/6)*((ehat_sk^2) + (ehat_ku^2/4))
JBstat
qchisq(0.95,2)

#
#LINEAR-LOG REGRESSION
#

reg_lnchap = lm(chapman~ln.time, data = rdat)
summary(reg_lnchap)
#This creates our linear regression equations for each region

plot(rdat$time,rdat$chapman,
     main="Figure 4: Regression of Yields in Chapman over Time - Model 2",
     xlab="Year",ylab="Yield (tonnes/hectare)",xlim=c(0,50),
     ylim=c(0,2.5),col="black",lty=1,lwd=1,pch=1)
lines(rdat$time,reg_lnchap$fitted.values, col = "red")
confint(reg_lnchap,level=0.95)

##Residual Analysis
summary(reg_lnchap$residuals)
sd(reg_lnchap$residuals)

plot(reg_lnchap$fitted.values,reg_lnchap$residuals,main="Figure 5: Residuals against Predicted Yield - Model 2",
     xlab="Predicted Yield",ylab="Estimated Residuals",xlim=c(0.6,1.5),ylim=c(-0.7,0.7),col="black",
     lty=1,lwd=1,pch=1)
abline(h=0,col = "red")

h_res.ln = hist(reg_lnchap$residuals, freq = TRUE, breaks = seq(min(reg_lnchap$residuals)-sd(reg_lnchap$residuals),max(reg_lnchap$residuals)+sd(reg_lnchap$residuals),(sd(reg_lnchap$residuals)/2)),
             main = "Figure 6: Histogram of Estimated Residuals", xlab = "Estimated Residuals",
             xlim = c(-0.75,0.75))

ehatln_sk = skewness(reg_lnchap$residuals,type=1)
ehatln_ku = kurtosis(reg_lnchap$residuals,type=1)

JBstatln = (48/6)*((ehatln_sk^2) + (ehatln_ku^2/4))
JBstatln
qchisq(0.95,2)

#
#SQUARED REGRESSION
#

reg_chap2 = lm(chapman~time2, data = rdat)
summary(reg_chap2)
#This creates our linear regression equations for each region

plot(rdat$time,rdat$chapman,
     main="Figure 7: Regression of Yields in Chapman over Time - Model 3",
     xlab="Year",ylab="Yield (tonnes/hectare)",xlim=c(0,50),
     ylim=c(0,2.5),col="black",lty=1,lwd=1,pch=1)
lines(rdat$time,reg_chap2$fitted.values, col = "red")
confint(reg_chap2,level=0.95)

##Residual Analysis
summary(reg_chap2$residuals)
sd(reg_chap2$residuals)

plot(reg_chap2$fitted.values,reg_chap2$residuals,main="Figure 8: Residuals against Predicted Yield - Model 3",
     xlab="Predicted Yield",ylab="Estimated Residuals",xlim=c(0.6,1.5),ylim=c(-0.7,0.7),col="black",
     lty=1,lwd=1,pch=1)
abline(h=0,col = "red")

h_res2 = hist(reg_chap2$residuals, freq = TRUE, breaks = seq(min(reg_chap2$residuals)-sd(reg_chap2$residuals),max(reg_chap2$residuals)+sd(reg_chap2$residuals),(sd(reg_chap2$residuals)/2)),
             main = "Figure 9: Histogram of Estimated Residuals", xlab = "Estimated Residuals",
             xlim = c(-0.75,0.75))

ehat2_sk = skewness(reg_chap2$residuals,type=1)
ehat2_ku = kurtosis(reg_chap2$residuals,type=1)

JBstat2 = (48/6)*((ehat2_sk^2) + (ehat2_ku^2/4))
JBstat2
qchisq(0.95,2)

#Hypothesis Testing

Ho1 = c("time=0")
Ho2 = c("ln.time=0")
Ho3 = c("time2=0")

linearHypothesis(reg_chap, Ho1)
linearHypothesis(reg_lnchap, Ho2)
linearHypothesis(reg_chap2, Ho3)

summary(reg_chap)$coefficients[,3]
summary(reg_lnchap)$coefficients[,3]
summary(reg_chap2)$coefficients[,3]
qt(0.95,47)

#Predicting after the sample timeperiod
pred.val = data.frame(time = 49)
pred.val.ln = data.frame(ln.time = 49)
pred.val.2 = data.frame(time2 = 49)
predict(reg_chap,pred.val,interval="confidence",level=0.95)
predict(reg_lnchap,pred.val.ln,interval="confidence",level=0.95)
predict(reg_chap2,pred.val.2,interval="confidence",level=0.95)

