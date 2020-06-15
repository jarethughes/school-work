getwd()
setwd("C://Users/Jaret/Documents/School Work/College/Senior Year/Fall/e307/")
getwd()

p2_15 = read.table("p2_15.prn", header=TRUE)
p2_15$Fe_wage = (p2_15$female)*(p2_15$wage)

summary(p2_15)
mean(p2_15$educ)
median(p2_15$educ)
var(p2_15$educ)
sd(p2_15$educ)



#These are preliminary reminders of ways to get important statistics from the data directly 
#Especially summary() should be done for all vectors you're analyzing
#Q1

#HW3

h_educ = hist(p2_15$educ,freq = TRUE,
               main="Figure 1: Histogram of Education",
               xlab="Education",
               breaks=seq(0,22.5,1.5))

#This histogram is a way to visualize the data and its distribution, as Q1 is to discuss the distribution of EDUC

#Q2

h_wage = hist(p2_15$wage,freq = TRUE,
               main="Figure 2: Histogram of Wages",
               xlab="Wage",
               breaks=seq(0,78+6,6))

#This does the same as in Q2, but for WAGE

#Q3

plot(p2_15$educ,p2_15$wage,
     main="Figure 3: Wages versus Education",
     xlab="Education",ylab="Wages",xlim=c(0,22),
     ylim=c(0,80),col="blue",lty=1,lwd=1,pch=1)

#This is to plot the relationship between WAGE and EDUC

cor(p2_15$educ,p2_15$wage)

#This tells us numerically the strength of relationship between the two variables, as well as the direction of the relationship

#HW5
#Q1

rwe_out = lm(wage~educ, data = p2_15)

names(rwe_out)
summary(rwe_out)
rwe_out$df.residual

#This creates a linear regression equation with educ as x and wage as y

plot(p2_15$educ,p2_15$wage,main="Figure 3: Regression of Wages Against Education",
     xlab="Education",ylab="Wage",xlim=c(0,22),ylim=c(0,80),col="blue",
     lty = 1,lwd=1,pch=1)

abline(rwe_out, col = "red")

#This creates a plot with the regression line on it

#Q7

hw5datm = subset(p2_15, female==0)
hw5datf = subset(p2_15, female==1)
hw5datb = subset(p2_15, black==1)
hw5datnb = subset(p2_15, black==0)
hw5datw = subset(hw5datnb, asian==0)

rwem_out = lm(wage~educ, data = hw5datm)
rwef_out = lm(wage~educ, data = hw5datf)
rweb_out = lm(wage~educ, data = hw5datb)
rwew_out = lm(wage~educ, data = hw5datw)
rwef_more = lm(wage~educ+female+Fe_wage, data = p2_15)

summary(rwem_out)
summary(rwef_out)
summary(rweb_out)
summary(rwew_out)

#This generates subsets of the data utilizing different characteristics of each datapoint.
#This allows us to run regressions for each subset, getting a different regression equation for each one.