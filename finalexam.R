#Final Exam
#ECON 307
#
#Jaret Hughes

setwd("C://Users/Jaret/Documents/School Work/College/Senior Year/Fall/e307")

library(car)
library(e1071)
library(perturb)

rdat = read.table("vacation.csv",header=TRUE,sep=",")

#Question 1
#Part A
#Comment on the distribution of the length of trips (Y) based on the sample of 200 households
summary(rdat$miles)
sd(rdat$miles)
h_miles = hist(rdat$miles, freq = TRUE, breaks = seq(min(rdat$miles)-sd(rdat$miles),max(rdat$miles)+sd(rdat$miles),sd(rdat$miles)/2),
               main = "Figure 1: Histogram of Miles Travelled", xlab = "Miles Travelled")

#Part B
#Summarize the results for the estimated regression with appropriate interpretation of all important calculated statistics
reg_trav = lm(miles~income+age+kids, dat=rdat)
summary(reg_trav)

#Part C
#Calculate 95% confidence interval for all the regression parameters, and provide a suitable interpretation for at least one of the estimated intervals
confint(reg_trav, level=0.95)

#Part D
#You would like to show that income has a positive effect on length of trips during vacation. Clearly state the null and alternative hypothesis and the underlying distributional result for the test statistic, calculate the test-statistic, illustrate the acceptance and rejection regions on a well-labeled graph using a 5% signifcance level, and discuss the inference that you would draw based on the available data
summary(reg_trav)$coefficients[,3]
qt(0.95,196)

#Part E
#Test the null hypothesis that the average age of all adults and the number of kids have no effect on the number of miles traveled. Use the 5% signifcance level
#Show calculations for the F-statistic
Ho1 = c("age=0","kids=0")
linearHypothesis(reg_trav,Ho1)
qf(0.95,2,196)

#Part F
#Calculate the predicted value and the 99% prediction intervals from this model for a household that has an income of $70,000, an average adult age of 45, and 2 kids. Provide a suitable interpretation of the calculated intervals
pred.val = data.frame(income = 70, age = 45, kids = 2)
predict(reg_trav,pred.val,interval="prediction",level=0.99)

#Part G
#Comment on the Validity of the classical assumptions for the error term in the population regression line. Clearly explain your answer
summary(reg_trav$residuals)

plot(reg_trav$fitted.values,reg_trav$residuals,main="Figure 2: Residuals against Predicted Value",
     xlab="Predicted Value",ylab="Estimated Residuals",xlim=c(min(reg_trav$fitted.values)-sd(reg_trav$fitted.values),max(reg_trav$fitted.values)+sd(reg_trav$fitted.values)),ylim=c(min(reg_trav$residuals)-sd(reg_trav$residuals),max(reg_trav$residuals)+sd(reg_trav$residuals)),col="black",
     lty=1,lwd=1,pch=1)
abline(h=0,col = "red")

h_res = hist(reg_trav$residuals, freq = TRUE, breaks = seq(min(reg_trav$residuals)-sd(reg_trav$residuals),max(reg_trav$residuals)+sd(reg_trav$residuals),(sd(reg_trav$residuals)/2)),
              main = "Figure 3: Histogram of Estimated Residuals", xlab = "Estimated Residuals")

ehat_sk = skewness(reg_trav$residuals,type=1)
ehat_ku = kurtosis(reg_trav$residuals,type=1)

JBstat = (200/6)*((ehat_sk^2) + (ehat_ku^2/4))
JBstat
qchisq(0.95,2)

#Part H
#Is there any evidence of multicollinearity in the estimated regression model? Clearly explain your answer
cor(rdat,method="pearson")
colldiag(reg_trav, scale = FALSE, center = FALSE, add.intercept = TRUE)
