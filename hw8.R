# hw8.R

#install.packages("e1071")
library(e1071)
#install.packages("car")
library(car)
#install.packages("perturb")
library(perturb)

options(digits=2)
# Creating a data frame from an external text file

hw8dat <- read.table("manuf.txt",header = TRUE)

hw8dat$lnk <- log(hw8dat$k)
hw8dat$lnl <- log(hw8dat$l)
hw8dat$lne <- log(hw8dat$e)
hw8dat$lnm <- log(hw8dat$m)
hw8dat$lny <- log(hw8dat$y)

# Summary Statistics for Consumption, Income, and Saving
summary(hw8dat)

mean(hw8dat$k)
median(hw8dat$k)
var(hw8dat$k)
sd(hw8dat$k)
skewness(hw8dat$k)
kurtosis(hw8dat$k)

mean(hw8dat$lnk)
median(hw8dat$lnk)
var(hw8dat$lnk)
sd(hw8dat$lnk)
skewness(hw8dat$lnk)
kurtosis(hw8dat$lnk)

cor(hw8dat,method="pearson")

# Multiple Linear Regression

#model1 <- lm(log(y) ~ log(k) + log(l) + log(e) + log(m) , data = hw8dat)
model1 <- lm(lny ~ lnk + lnl + lne + lnm, dat = hw8dat)
summary(model1)


# Confidence Interval Estimates

confint(model1)
confint(model1,level=0.95)



# Diagnostics on the error term

summary(model1$residuals)


plot(model1$fitted.values,model1$residuals,main="Figure 1: Residuals against Predicted Values",
     xlab="Predicted Values",ylab="Estimated Residuals",xlim=c(0,0.8),ylim=c(-0.12,0.12),col="blue",
     lty = 1,lwd=1,pch=1)

abline(h=0,col="red")


hist_res <- hist(model1$residuals, freq = FALSE, breaks = seq(-0.12,0.12000,0.02),
                 main = "Figure 2: Histogram of Estimated Residuals", xlab = "Estimated Residuals",
                 xlim = c(-0.12,0.12))




ehat_sk <- skewness(model1$residuals,type=1)
ehat_ku <- kurtosis(model1$residuals,type=1)

ehat_sk
ehat_ku

JBstat <- (25/6)*((ehat_sk^2) + (ehat_ku^2/4))
JBstat
qchisq(0.95,2)




H01 <- c("lnk=0")
linearHypothesis(model1,H01)

H02 <- c("lnk=0","lnl=0")
linearHypothesis(model1,H02)

H03 <- c("lnk=0","lne=0")
linearHypothesis(model1,H03)

H04 <- c("lnk=0","lnl=0","lne=0")
linearHypothesis(model1,H04)

H05 <- c("lnk+lnl+lne+lnm=1")
linearHypothesis(model1,H05)


hw8_cor <- cor(hw8dat,method="pearson")
print(hw8_cor[6:10,6:10])


colldiag(model1, scale = FALSE, center = FALSE, add.intercept = TRUE)




model2 <- lm(lny ~ lnk + lnl , dat = hw8dat)
summary(model2)
colldiag(model2, scale = FALSE, center = FALSE, add.intercept = TRUE)




model3 <- lm(lny ~ lnk , dat = hw8dat)
summary(model3)




model4 <- lm(lny ~ lnl , dat = hw8dat)
summary(model4)

yhat4 <- exp(model4$fitted.values)

plot(hw8dat$l,hw8dat$y,main="Figure 3: Output against Labour",
     xlab="Labour",ylab="Output",xlim=c(0.9,1.7),ylim=c(0.5,2.5),col="blue",
     lty = 1,lwd=1,pch=1)

lines(hw8dat$l,yhat4,col="red")

# Diagnostics on the error term in Model 4

summary(model4$residuals)

plot(model4$fitted.values,model4$residuals,main="Figure 4: Residuals against Predicted Values",
     xlab="Predicted Values",ylab="Estimated Residuals",xlim=c(0,0.8),ylim=c(-0.15,0.15),col="blue",
     lty = 1,lwd=1,pch=1)

abline(h=0,col="red")


hist_res <- hist(model4$residuals, freq = FALSE, breaks = seq(-0.15,0.15,0.02),
                 main = "Figure 5: Histogram of Estimated Residuals", xlab = "Estimated Residuals",
                 xlim = c(-0.15,0.15))




ehat4_sk <- skewness(model4$residuals,type=1)
ehat4_ku <- kurtosis(model4$residuals,type=1)

ehat4_sk
ehat4_ku

JBstat4 <- (25/6)*((ehat4_sk^2) + (ehat4_ku^2/4))
JBstat4
qchisq(0.95,2)

