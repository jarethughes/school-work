getwd()
setwd("C://Users/Jaret/Documents/School Work/College/Senior Year/Fall/e307/")
getwd()

p2_15 = read.table("p2_15.prn", header=TRUE)

summary(p2_15)
mean(p2_15$educ)
median(p2_15$educ)
var(p2_15$educ)
sd(p2_15$educ)

#This is preliminary reminders of ways to get important statistics from the data directly 
#Especially summary() should be done for all vectors you're analyzing

#Q1

h_educ <- hist(p2_15$educ,freq = TRUE,
               main="Figure 1: Histogram of Education",
               xlab="Education",
               breaks=seq(0,22.5,1.5))

#This histogram is a way to visualize the data and its distribution, as Q1 is to discuss the distribution of EDUC

#Q2

h_wage <- hist(p2_15$wage,freq = TRUE,
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
