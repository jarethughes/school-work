# lab4.R

#install.packages("e1071")
library(e1071)


# Creating a data frame from an external text file

lab1dat <- read.table("lab1_dat.prn",header = TRUE)


lab1dat

lab1dat$Income
lab1dat$Consumption
lab1dat$Saving <- lab1dat$Income - lab1dat$Consumption

lab1dat$Saving

str(lab1dat)
head(lab1dat)


save(lab1dat,file = "lab1.RData")

pdf(file = "Figure1.pdf", width = 5, height = 3.5)
plot(lab1dat$Income,lab1dat$Consumption,main="Figure 1: Consumption Against Income",
     xlab="Income",ylab="Consumption",xlim=c(60,280),ylim=c(40,160),col="blue",
     lty = 1,lwd=1,pch=1)

dev.off()

plot(lab1dat$Income,lab1dat$Consumption)

plot(lab1dat$Income,lab1dat$Consumption,main="Figure 1: Consumption Against Income",
      xlab="Income",ylab="Consumption",xlim=c(60,280),ylim=c(40,160),col="blue",
     lty = 1,lwd=1,pch=1)

plot(lab1dat$Income,lab1dat$Saving,main="Figure 2: Saving Against Income",
     xlab="Income",ylab="Saving",xlim=c(60,280),ylim=c(0,80),col="red",
     lty = 1,lwd=1,pch=1)

cons_sav <- cbind(lab1dat$Consumption,lab1dat$Saving)

matplot(lab1dat$Income, cons_sav, type = "l", lwd=c(1,2), lty = c(1,2),
        col = c("green","blue"), main = "Consumption and Saving Against Income",
        xlab = "Income", ylab = "Consumption/Saving", xlim = c(60,280),
        ylim = c(0,160))



# Summary Statistics for Consumption, Income, and Saving
summary(lab1dat)
mean(lab1dat$Consumption)
median(lab1dat$Consumption)
var(lab1dat$Consumption)
sd(lab1dat$Consumption)
skewness(lab1dat$Consumption)
kurtosis(lab1dat$Consumption)

cov(lab1dat$Consumption,lab1dat$Income)
cor(lab1dat$Consumption,lab1dat$Income)

mean(lab1dat$Income)
median(lab1dat$Income)
var(lab1dat$Income)
sd(lab1dat$Income)
skewness(lab1dat$Income)
kurtosis(lab1dat$Income)


hist(lab1dat$Consumption,freq = FALSE)
hist(lab1dat$Consumption,freq = TRUE)

hist_cons <- hist(lab1dat$Consumption, freq = FALSE, breaks = c(60,75,90,105,120,135,150,165),
     main = "Figure 3: Histogram of Consumption", xlab = "Consumption",
     xlim = c(60,175))

hist_cons


# Simple Linear Regression

lm(lab1dat$Consumption~lab1dat$Income)

rci_out <- lm(Consumption~Income, data = lab1dat)


names(rci_out)

rci_out$df.residual

summary(rci_out)


# Simple Linear Regression through the origin

rci_out2 <- lm(Consumption ~ 0 + Income, data=lab1dat)
rci_out2

# Simple Linear Regression on a intercept only

rci_out3 <- lm(Consumption ~ 1, data=lab1dat)
rci_out3

plot(lab1dat$Income,lab1dat$Consumption,main="Figure 1: Consumption Against Income",
     xlab="Income",ylab="Consumption",xlim=c(60,280),ylim=c(40,160),col="blue",
     lty = 1,lwd=1,pch=1)

abline(rci_out, col = "blue")
abline(rci_out2, col = "red")
abline(rci_out3, col = "yellow")


# Confidence Interval Estimates

confint(rci_out)
confint(rci_out,level=0.99)


# Prediction Interval Calculations
# the option interval = "confidence" yields the mean Prediction Interval
# the option interval = "prediction" yields the individual Prediction Interval


pv_level1 <- data.frame(Income=150)
predict(rci_out,pv_level1,interval="confidence",level=0.95)
predict(rci_out,pv_level1,interval="prediction",level=0.95)


lab1mat = as.matrix(lab1dat)
pv_level2 <- data.frame(Income=lab1mat[1:10,1])

prm <- predict(rci_out,pv_level2,interval="confidence",level=0.95)
pri <- predict(rci_out,pv_level2,interval="prediction",level=0.95)

rci_p <- cbind(lab1mat[1:10,1:2],prm[1:10,1:3],pri[1:10,2:3])

matplot(rci_p[1:10,1],rci_p[1:10,2:7], type = c("p","l","l","l","l","l"), 
        lwd=c(1,1,1,1,1,1), lty = c(1,1,2,2,3,3),pch=1,cex=0.5,
        col = c("black","blue","red","red","purple","purple"), 
        main = "Consumption Against Income",
        xlab = "Income", ylab = "Consumption", xlim = c(60,280),ylim = c(40,180))


# Calculation of Critical Values from a t-distribution and a Chi-squared distribution

qt(0.025,8)
qt(0.975,8)

qchisq(0.025,8)
qchisq(0.975,8)





# Diagnostics on the error term

plot(rci_out$fitted.values,rci_out$residuals,main="Figure 2: Residuals against Predicted Values",
     xlab="Predicted Values",ylab="Estimated Residuals",xlim=c(60,160),ylim=c(-12,12),col="blue",
     lty = 1,lwd=1,pch=1)

abline(h=0,col="red")


hist_res <- hist(rci_out$residuals, freq = FALSE, breaks = c(-14,-10,-6,-2,2,6,10,14),
                 main = "Figure 5: Histogram of Estimated Residuals", xlab = "Estimated Residuals",
                 xlim = c(-14,14))




ehat_sk <- skewness(rci_out$residuals,type=1)
ehat_ku <- kurtosis(rci_out$residuals,type=1)

ehat_sk
ehat_ku

JBstat <- (10/6)*((ehat_sk^2) + (ehat_ku^2/4))
JBstat
qchisq(0.95,2)



