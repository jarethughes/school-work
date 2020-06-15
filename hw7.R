# hw7.R


#install.packages("e1071")
library(e1071)
#install.packages("car")
library(car)
#install.packages("perturb")
library(perturb)

# Creating a data frame from an external text file

hw7dat <- read.table("clothes.txt",header = TRUE)

hw7dat

hw7dat <- hw7dat[order(output),]

hw7dat

save(hw7dat,file = "hw7dat.RData")

hw7dat <- hw7dat[order(hw7dat$output),]
# Summary Statistics

summary(hw7dat)

mean(hw7dat$output)
median(hw7dat$output)
var(hw7dat$output)
sd(hw7dat$output)
skewness(hw7dat$output)
kurtosis(hw7dat$output)

plot(hw7dat$output,hw7dat$cost,main="Figure 1: Output Against Cost",
     xlab="Output",ylab="Cost",col="blue",lty = 1,lwd=1,pch=1,cex=0.4)



cor(hw7dat,method="pearson")


# Multiple Linear Regression

hw7dat$outputsq <- hw7dat$output*hw7dat$output
hw7dat$outputcu <- hw7dat$output*hw7dat$output*hw7dat$output

model1 <- lm(cost ~ output + outputsq + outputcu, dat = hw7dat)
summary(model1)


# Confidence Interval Estimates

confint(model1)

# Prediction Interval Calculations
# the option interval = "confidence" yields the mean Prediction Interval
# the option interval = "prediction" yields the individual Prediction Interval




hw7mat <- as.matrix(hw7dat)

pv_level <- data.frame(output=hw7mat[1:28,2],outputsq=hw7mat[1:28,3],outputcu=hw7mat[1:28,4])

prm <- predict(model1,pv_level,interval="confidence",level=0.95)
pri <- predict(model1,pv_level,interval="prediction",level=0.95)
prm
pri

model1_p <- cbind(hw7mat[1:28,2],hw7mat[1:28,2],prm[1:28,1:3],pri[1:28,2:3])

matplot(model1_p[1:28,1],model1_p[1:28,2:7], type = c("p","l","l","l","l","l"), 
        lwd=c(1,2,2,2,2,2), lty = c(1,1,2,2,3,3),pch=1,cex=0.5,
        col = c("black","blue","red","red","purple","purple"), 
        main = "Figure 4: Regression of Cost on a Third Degree Polynomial of Ouput",
        xlab = "Output", ylab = "Cost", xlim = c(0,10),ylim = c(150,750))



# Diagnostics on the error term

summary(model1$residuals)


plot(model1$fitted.values,model1$residuals,main="Figure 1: Residuals against Predicted Values",
     xlab="Predicted Values",ylab="Estimated Residuals",col="blue",
     lty = 1,lwd=1,pch=1)

abline(h=0,col="red")


hist_res <- hist(model1$residuals, freq = FALSE,
                 main = "Figure 2: Histogram of Estimated Residuals", xlab = "Estimated Residuals")




ehat_sk <- skewness(model1$residuals,type=1)
ehat_ku <- kurtosis(model1$residuals,type=1)

ehat_sk
ehat_ku

JBstat <- (28/6)*((ehat_sk^2) + (ehat_ku^2/4))
JBstat
qchisq(0.95,2)


colldiag(model1, scale = FALSE, center = FALSE, add.intercept = TRUE)


#White's Test for Heteroskedasticity
# n*R^2 from model 2
# compare to a chi-squared distribution with df = number of regressors excluding the constant


hw7dat$ehat <- model1$residuals
hw7dat$ehatsq <- model1$residuals*model1$residuals
hw7dat$outputfour <- hw7dat$output*hw7dat$outputcu
hw7dat$outputfive <- hw7dat$output*hw7dat$outputfour
hw7dat$outputsix <- hw7dat$output*hw7dat$outputfive


model2 <- lm(ehatsq ~ output + outputsq + outputcu + outputfour + outputfive + outputsix, dat = hw7dat)
summary(model2)

qchisq(0.95,6)

hw7dat$ac = 134.65*(1/hw7dat$output) + 57.79 - 11.03*hw7dat$output + 1.14*hw7dat$outputsq
hw7dat$mc = 57.79 - 2*11.03*hw7dat$output + 3*1.14*hw7dat$outputsq

matplot(hw7dat$output,cbind(hw7dat$mc,hw7dat$ac),main="Figure 1: Output Against Marginal Cost and Average Cost",
     xlab="Output",ylab="Marginal Cost/Average Cost",col=c("blue","red"),lty = 1,lwd=1,pch=1,cex=0.4)

min(hw7dat$ac)



H01 <- c("outputsq=0","outputcu=0")
H02 <- c("outputcu=0")
H03 <- c("intercept=0","outputcu=0")

linearHypothesis(model1,H01)
linearHypothesis(model1,H02)
linearHypothesis(model1,H03)



