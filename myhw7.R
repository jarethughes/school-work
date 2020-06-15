#Homework 7

#Prelim
setwd("C:/Users/Jaret/Documents/School Work/College/Senior Year/Fall/e307/")

library(e1071)
library(car)
library(perturb)

rdat = read.table("clothes.prn", header = TRUE)
rdat$osq = rdat$output^2
rdat$ocb = rdat$output^3
rdat = rdat[order(rdat$output),]

summary(rdat)
cor(rdat,method="pearson")

#Question 1
#Plot total cost against output
plot(rdat$output,rdat$cost,main="Figure 1: Total Cost Over Output",
     xlab="Output",ylab="Total Cost",xlim=c(min(rdat$output)-sd(rdat$output)/2,max(rdat$output)+sd(rdat$output)/2),
     ylim=c(min(rdat$cost)-sd(rdat$cost)/2,max(rdat$cost)+sd(rdat$cost)/2),col="blue",
     lty = 1,lwd=1,pch=1,cex=0.4)

#Question 2
#Estimate the polynomial equation
lm <- lm(cost ~ output+osq+ocb, data = rdat)
summary(lm)

plot(rdat$output,rdat$cost,main="Figure 2: Regression of Total Cost Over Output",
     xlab="Output",ylab="Total Cost",xlim=c(min(rdat$output)-sd(rdat$output)/2,max(rdat$output)+sd(rdat$output)/2),
     ylim=c(min(rdat$cost)-sd(rdat$cost)/2,max(rdat$cost)+sd(rdat$cost)/2),col="blue",
     lty = 1,lwd=1,pch=1,cex=0.4)
lines(rdat$output,lm$fitted.values,col="red")

#Question 3
#Plot the prediction intervals

#Adapted from Instructor's hw7 file
rdatmat = as.matrix(rdat)

pv_level = data.frame(output=rdat[1:28,2],osq=rdat[1:28,3],ocb=rdat[1:28,4])

prm <- predict(lm,pv_level,interval="confidence",level=0.95)
pri <- predict(lm,pv_level,interval="prediction",level=0.95)

lm_p <- cbind(rdatmat[1:28,2],rdatmat[1:28,2],prm[1:28,1:3],pri[1:28,2:3])

matplot(lm_p[1:28,1],lm_p[1:28,2:7], type = c("p","l","l","l","l","l"), 
        lwd=c(1,2,2,2,2,2), lty = c(1,1,2,2,3,3),pch=1,cex=0.5,
        col = c("black","blue","red","red","purple","purple"), 
        main = "Figure 3: Prediction Invervals of Regression of Cost over Output",
        xlab = "Output", ylab = "Cost", xlim=c(min(rdat$output)-sd(rdat$output)/2,max(rdat$output)+sd(rdat$output)/2),
        ylim=c(min(rdat$cost)-sd(rdat$cost)/2,max(rdat$cost)+sd(rdat$cost)/2))

#Question 4
#Evaluate validity of classical assumptions
summary(lm$residuals)

plot(lm$fitted.values,lm$residuals,main="Figure 4: Estimated Residuals against Predicted Cost",
     xlab="Predicted Cost",ylab="Estimated Residuals",xlim=c(min(lm$fitted.values)-sd(lm$fitted.values)/2,max(lm$fitted.values)+sd(lm$fitted.values)/2),ylim=c(min(lm$residuals)-sd(lm$residuals)/2,max(lm$residuals)+sd(lm$residuals)/2),col="blue",
     lty = 1,lwd=1,pch=1)
abline(h=0,col="red")

h_res = hist(lm$residuals, freq = TRUE, breaks = seq(min(lm$residuals)-sd(lm$residuals),max(lm$residuals)+sd(lm$residuals),sd(lm$residuals)/2),
                  main = "Figure 5: Histogram of Estimated Residuals", xlab = "Estimated Residuals")

ehat_sk <- skewness(lm$residuals,type=1)
ehat_ku <- kurtosis(lm$residuals,type=1)
JBstat <- (28/6)*((ehat_sk^2) + (ehat_ku^2/4))
JBstat
qchisq(0.95,2)

#Question 5
#Is there multicolinearity?
cor(rdat,method="pearson")
colldiag(lm, scale = FALSE, center = FALSE, add.intercept = TRUE)

#Question 6
#Test Ho: B3,B4 = 0 @ 5% for lm
Ho1 = c("osq=0","ocb=0")
linearHypothesis(lm,Ho1)

#Question 7
#Test Ho: B4 = 0 @ 5% for lm
Ho2 = c("ocb=0")
linearHypothesis(lm,Ho2)

#Question 8
#Marginal Cost
rdat$mc = 57.79 - 2*11.03*rdat$output + 3*1.14*rdat$osq
print("57.79 - 2*11.03*rdat$output + 3*1.14*rdat$osq")

#Question 9
#Average Cost
rdat$ac = 134.65*(1/rdat$output) + 57.79 - 11.03*rdat$output + 1.14*rdat$osq
print("134.65*(1/rdat$output) + 57.79 - 11.03*rdat$output + 1.14*rdat$osq")

#Question 10
#Plots of both
matplot(rdat$output,cbind(rdat$mc,rdat$ac),type="b",main="Figure 5: Output Against Marginal Cost and Average Cost",
        xlab="Output",ylab="Marginal Cost/Average Cost",col=c("blue","red"),lty = 1,lwd=1,pch=1,cex=0.4)
#Lowest level of price such that profit > 0
min(rdat$ac)

#Number of firms in the sample that produce output levels that yield a net profit
# ANSWER : 17 firms produce less than the level of output where MC=AC, or the minimum amount of output where there is a net profit

#Question 11
#What restrictions on the model's coefficients imply that AC is linear? Test those restrictions. 
# ANSWER : B1 and B4 both = 0 
Ho3 = c("(Intercept)=0","ocb=0")
linearHypothesis(lm,Ho3)