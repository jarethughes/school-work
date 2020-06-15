# Lab1.R
# The first step is to link the class directory
# getwd()
# setwd("/Users/amitsen/Desktop/econ_307/R_software")
# getwd()

x <- 5
x^2
(x <- 5)
(y <- 3)
(z <- y^x)

curve(x^2,-5,5)

# The function exists will let us know if an object exists all ready
exists('x')

#the function rm will remove an object
rm(x)

# Analyze the example of consumption and income that was discussed in class
# First, I am creating a vector for consumption called vec_cons

vec_cons <- c(70,65,90,95,110,115,120,140,155,150)

# The data on income is created using the seq command....

vec_inc <- seq(80,260,20)

(vec_sav <- vec_inc - vec_cons)


min(vec_cons)
max(vec_cons)
length(vec_cons)
sum(vec_cons)
sort(vec_cons)
prod(vec_cons)


inds <- c("ind1","ind2","ind3","ind4","ind5","ind6","ind7","ind8","ind9","ind10")
names(vec_cons) <- inds
names(vec_inc) <- inds
names(vec_sav) <- inds

mat_dat1 <- cbind(vec_cons,vec_inc,vec_sav)

lab1dat1 <- as.data.frame(mat_dat1)

mat_dat1
lab1dat1


colMeans(lab1dat1)


# Creating a data frame from an external text file

lab1dat <- read.table("lab1_dat.txt",header = TRUE)


lab1dat

lab1dat$Income
lab1dat$Consumption
lab1dat$Saving <- lab1dat$Income - lab1dat$Consumption

lab1dat$Saving

str(lab1dat)
head(lab1dat)

rm(lab1dat1)

summary(lab1dat)
mean(lab1dat$Consumption)
median(lab1dat$Consumption)
var(lab1dat$Consumption)
sd(lab1dat$Consumption)
cov(lab1dat$Consumption,lab1dat$Income)
cor(lab1dat$Consumption,lab1dat$Income)

mean(lab1dat$Income)
median(lab1dat$Income)
var(lab1dat$Income)
sd(lab1dat$Income)


plot(lab1dat$Income,lab1dat$Consumption,
      main="Figure 1: Consumption versus Income",
     xlab="Income",ylab="Consumption",xlim=c(80,260),
     ylim=c(40,160),col="blue",lty=1,lwd=1,pch=1)

plot(lab1dat$Income,lab1dat$Saving,
     main="Figure 1: Saving versus Income",
     xlab="Income",ylab="Saving",xlim=c(80,260),
     ylim=c(0,120),col="green",lty=1,lwd=1,pch=1)


cons_sav <- cbind(lab1dat$Consumption,lab1dat$Saving)

matplot(lab1dat$Income,cons_sav,
     main="Figure 3: Consumption and Saving versus Income",
     xlab="Income",ylab="Consumption/Saving",xlim=c(80,260),
     ylim=c(0,160),col=c("blue","green"),type = "l",
    lwd=c(1,2),lty=c(1,2))


h_cons <- hist(lab1dat$Consumption,freq = TRUE,
               main="Figure 3: Histogram of Consumption",
               xlab="Consumption",
               breaks=c(60,75,90,105,120,135,150,165))


    
    

