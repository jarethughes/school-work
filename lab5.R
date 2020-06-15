# lab5.R

#install.packages("e1071")
library(e1071)


# Creating a data frame from an external text file

lab5dat <- read.table("lab5.prn",header = TRUE)


lab5dat

lab5dat$QR
lab5dat$PR
lab5dat$PC
lab5dat$Income


str(lab5dat)
head(lab5dat)


save(lab5dat,file = "lab5.RData")


# Summary Statistics for Consumption, Income, and Saving
summary(lab5dat)

mean(lab5dat$QR)
median(lab5dat$QR)
var(lab5dat$QR)
sd(lab5dat$QR)
skewness(lab5dat$QR)
kurtosis(lab5dat$QR)

mean(lab5dat$PR)
median(lab5dat$PR)
var(lab5dat$PR)
sd(lab5dat$PR)
skewness(lab5dat$PR)
kurtosis(lab5dat$PR)

mean(lab5dat$PC)
median(lab5dat$PC)
var(lab5dat$PC)
sd(lab5dat$PC)
skewness(lab5dat$PC)
kurtosis(lab5dat$PC)

mean(lab5dat$Income)
median(lab5dat$Income)
var(lab5dat$Income)
sd(lab5dat$Income)
skewness(lab5dat$Income)
kurtosis(lab5dat$Income)


cor(lab5dat,method="pearson")



hist_QR <- hist(lab5dat$QR, freq = TRUE, breaks = seq(3000,12000,1000),
                  main = "Figure 1: Histogram of Quantity of Roses", xlab = "Quantity of Roses",
                  xlim = c(3000,12000))

hist_PR <- hist(lab5dat$PR, freq = TRUE, breaks = seq(2.25,4.25,0.5),
                main = "Figure 2: Histogram of Price of Roses", xlab = "Price of Roses",
                xlim = c(2.25,4.25))

hist_PC <- hist(lab5dat$PC, freq = TRUE, breaks = seq(2.80,4.20,0.2),
                main = "Figure 3: Histogram of Price of Carnations", xlab = "Price of Carnations",
                xlim = c(2.80,4.20))

hist_Income <- hist(lab5dat$Income, freq = TRUE, breaks = seq(155,200,5),
                main = "Figure 4: Histogram of Income", xlab = "Income",
                xlim = c(155,200))


#pdf(file = "Figure1.pdf", width = 5, height = 3.5)
plot(lab5dat$PR,lab5dat$QR,main="Figure 5: Quantity of Roses against Price of Roses",
     xlab="Price of Roses",ylab="Quantity of Roses",ylim=c(3000,12000),xlim=c(2,4.5),col="blue",
     lty = 1,lwd=1,pch=1)

#dev.off()

plot(lab5dat$PC,lab5dat$QR,main="Figure 6: Quantity of Roses against Price of Carnations",
     xlab="Price of Carnations",ylab="Quantity of Roses",ylim=c(3000,12000),xlim=c(2.5,4.5),col="blue",
     lty = 1,lwd=1,pch=1)


plot(lab5dat$Income,lab5dat$QR,main="Figure 7: Quantity of Roses against Income",
     xlab="Income",ylab="Quantity of Roses",ylim=c(3000,12000),xlim=c(150,210),col="blue",
     lty = 1,lwd=1,pch=1)



# Multiple Linear Regression


model1 <- lm(QR~PR+PC+Income, data = lab5dat)
summary(model1)


# Confidence Interval Estimates

confint(model1)
confint(model1,level=0.99)


# Prediction Interval Calculations
# the option interval = "confidence" yields the mean Prediction Interval
# the option interval = "prediction" yields the individual Prediction Interval


pv_level1 <- data.frame(PR=4,PC=3.5,Income=190)
predict(model1,pv_level1,interval="confidence",level=0.95)
predict(model1,pv_level1,interval="prediction",level=0.95)


# Diagnostics on the error term

summary(model1$residuals)


plot(model1$fitted.values,model1$residuals,main="Figure 8: Residuals against Predicted Values",
     xlab="Predicted Values",ylab="Estimated Residuals",xlim=c(3500,12000),ylim=c(-2000,2000),col="blue",
     lty = 1,lwd=1,pch=1)

abline(h=0,col="red")


hist_res <- hist(model1$residuals, freq = FALSE, breaks = seq(-2000,2000,500),
                 main = "Figure 5: Histogram of Estimated Residuals", xlab = "Estimated Residuals",
                 xlim = c(-2000,2000))




ehat_sk <- skewness(model1$residuals,type=1)
ehat_ku <- kurtosis(model1$residuals,type=1)

ehat_sk
ehat_ku

JBstat <- (16/6)*((ehat_sk^2) + (ehat_ku^2/4))
JBstat
qchisq(0.95,2)


model2 <- lm(QR~PR, data = lab5dat)
summary(model2)

#install.packages("car")
library(car)
H01 <- c("PC=0","Income=0")
linearHypothesis(model1,H01)


model3 <- lm(QR~PR+PC, data = lab5dat)
summary(model3)

qt(0.025,12)
qf(0.95,df1=3,df2=12)
