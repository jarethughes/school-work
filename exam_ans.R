getwd()
setwd("C://Users/Jaret/Documents/School Work/College/Senior Year/Fall/e307/")
getwd()
library(e1071)

rdat = read.table("exam_dat.csv", header=TRUE, sep=",")

#
#PRELIMINARY STATISTICS
#

summary(rdat)
var(rdat$price)
sd(rdat$price)
var(rdat$sqft)
sd(rdat$sqft)

#These are preliminary reminders of ways to get important statistics from the data directly 
#Especially summary() should be done for all vectors you're analyzing

h_p = hist(rdat$price,freq = TRUE,
               main="Figure 1: Histogram of Price",
               xlab="Price (USD)",
               breaks=seq(0,900000,42000))
h_pp = hist(rdat$price,freq = FALSE,
           main="Figure 1: Histogram of Price",
           xlab="Price (USD)",
           breaks=seq(0,900000,42000))

#This is the distribution that we are particularly interested in

h_a = hist(rdat$sqft,freq = TRUE,
              main="Figure 2: Histogram of Square Footage",
              xlab="Square Feet",
              breaks=seq(0,8000,500))
#These histograms are a way to visualize the data and its distribution

plot_p = plot(rdat$sqft,rdat$price,
                  main="Figure 6: Price versus Square Footage",
                  xlab="Square Feet",ylab="Price (USD)",xlim=c(0,8000),
                  ylim=c(0,900000),col="blue",lty=1,lwd=1,pch=1)

#This is to plot the data

#
#LINEAR REGRESSIONS
#

reg_price = lm(price~sqft, data=rdat)
summary(reg_price)
plot(rdat$sqft,rdat$price,
     main="Figure 2: Regresssion of Prices versus Square Footage",
     xlab="Square Feet",ylab="Price (USD)",xlim=c(0,8000),
     ylim=c(0,900000),col="blue",lty=1,lwd=1,pch=1)
abline(reg_price, col = "red")
#This is the regression model that we are concerned with

confint(reg_price,level=0.95)

pv_sqft = data.frame(sqft=2200)
predict(reg_price,pv_sqft,interval="confidence", level=0.95)
predict(reg_price,pv_sqft,interval="prediction", level=0.95)

#
#Analysis of the Residuals

plot(reg_price$fitted.values,reg_price$residuals,main="Figure 3: Residuals against Predicted Values",
     xlab="Predicted Values",ylab="Estimated Residuals",xlim=c(0,500000),ylim=c(-300000,300000),col="blue",
     lty = 1,lwd=1,pch=1)

abline(h=0,col="red")

h_err = hist(reg_price$residuals, freq = FALSE, breaks = seq(-600000,600000,25000),
                 main = "Figure 4: Histogram of Estimated Residuals", xlab = "Estimated Residuals")

err_sk = skewness(reg_price$residuals,type=1)
err_ku = kurtosis(reg_price$residuals,type=1)

JBstat = (1074/6)*((err_sk^2) + (err_ku^2/4))
JBstat
qchisq(0.95,2)
#If JBstat > qchisq(0.95,2), then the residuals are not normally distributed

gtest = (69.55-100)/1.671
gtest
qt(0.95,1072)
#Hypothesis test if B1 is no more than 100
#H0 rejected if gtest > qt

rdatwf = subset(rdat, Waterfront==1)
rdatnwf = subset(rdat, Waterfront==0)

reg_wf = lm(price~sqft, data = rdatwf)
plot(rdatwf$sqft,rdatwf$price,
     main="Figure 5: Regresssion of Prices versus Square Footage",
     xlab="Square Feet",ylab="Price (USD)",xlim=c(0,8000),
     ylim=c(0,900000),col="blue",lty=1,lwd=1,pch=1)
abline(reg_wf, col = "red")

reg_nwf = lm(price~sqft, data = rdatnwf)
plot(rdatnwf$sqft,rdatnwf$price,
     main="Figure 6: Regresssion of Prices versus Square Footage",
     xlab="Square Feet",ylab="Price (USD)",xlim=c(0,8000),
     ylim=c(0,900000),col="blue",lty=1,lwd=1,pch=1)
abline(reg_nwf, col = "red")

summary(reg_wf)
summary(reg_nwf)
