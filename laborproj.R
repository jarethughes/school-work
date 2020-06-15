#ECON 460-01 Research Project
#By: Jaret Hughes

#Clerical
getwd()
setwd("C://Users/Jaret/Documents/School Work/College/Senior Year/Fall/Labor/Research Project/")
getwd()

library(car)
library(e1071)

mem.dat = read.table("prop_mem.csv",header = TRUE,sep = ",")
rep.dat = read.table("prop_rep.csv",header = TRUE,sep = ",")
cpi.dat = read.table("cpi.csv",header = TRUE,sep = ",")

#Summary Stats
summary(mem.dat)
summary(rep.dat)
summary(cpi.dat)

cor(mem.dat)
cor(rep.dat)
cor(cpi.dat)

#Graphs and plots
h_mem.ne = hist(mem.dat$Northeast,freq = TRUE,
               main="Figure 1: Histogram of Union Membership in the Northeast",
               xlab="Proportion of workers who are members of a union",
               breaks=seq(min(mem.dat$Northeast)-sd(mem.dat$Northeast),max(mem.dat$Northeast)+sd(mem.dat$Northeast),(sd(mem.dat$Northeast)/2)))
h_mem.mw = hist(mem.dat$Midwest,freq = TRUE,
                main="Figure 2: Histogram of Union Membership in the Midwest",
                xlab="Proportion of workers who are members of a union",
                breaks=seq(min(mem.dat$Midwest)-sd(mem.dat$Midwest),max(mem.dat$Midwest)+sd(mem.dat$Midwest),(sd(mem.dat$Midwest)/2)))
h_mem.so = hist(mem.dat$South,freq = TRUE,
                main="Figure 3: Histogram of Union Membership in the South",
                xlab="Proportion of workers who are members of a union",
                breaks=seq(min(mem.dat$South)-sd(mem.dat$South),max(mem.dat$South)+sd(mem.dat$South),(sd(mem.dat$South)/2)))
h_mem.we = hist(mem.dat$West,freq = TRUE,
                main="Figure 4: Histogram of Union Membership in the West",
                xlab="Proportion of workers who are members of a union",
                breaks=seq(min(mem.dat$West)-sd(mem.dat$West),max(mem.dat$West)+sd(mem.dat$West),(sd(mem.dat$West)/2)))

h_rep.ne = hist(rep.dat$Northeast,freq = TRUE,
                main="Figure 5: Histogram of Union Representation in the Northeast",
                xlab="Proportion of workers who are represented by a union",
                breaks=seq(min(rep.dat$Northeast)-sd(rep.dat$Northeast),max(rep.dat$Northeast)+sd(rep.dat$Northeast),(sd(rep.dat$Northeast)/2)))
h_rep.mw = hist(rep.dat$Midwest,freq = TRUE,
                main="Figure 6: Histogram of Union Representation in the Midwest",
                xlab="Proportion of workers who are represented by a union",
                breaks=seq(min(rep.dat$Midwest)-sd(rep.dat$Midwest),max(rep.dat$Midwest)+sd(rep.dat$Midwest),(sd(rep.dat$Midwest)/2)))
h_rep.so = hist(rep.dat$South,freq = TRUE,
                main="Figure 7: Histogram of Union Representation in the South",
                xlab="Proportion of workers who are represented by a union",
                breaks=seq(min(rep.dat$South)-sd(rep.dat$South),max(rep.dat$South)+sd(rep.dat$South),(sd(rep.dat$South)/2)))
h_rep.we = hist(rep.dat$West,freq = TRUE,
                main="Figure 8: Histogram of Union Representation in the West",
                xlab="Proportion of workers who are represented by a union",
                breaks=seq(min(rep.dat$West)-sd(rep.dat$West),max(rep.dat$West)+sd(rep.dat$West),(sd(rep.dat$West)/2)))

h_cpi.ne = hist(cpi.dat$Northeast,freq = TRUE,
                main="Figure 9: Histogram of CPI values in the Northeast",
                xlab="CPI Value",
                breaks=seq(min(cpi.dat$Northeast)-sd(cpi.dat$Northeast),max(cpi.dat$Northeast)+sd(cpi.dat$Northeast),(sd(cpi.dat$Northeast)/2)))
h_cpi.mw = hist(cpi.dat$Midwest,freq = TRUE,
                main="Figure 10: Histogram of CPI values in the Midwest",
                xlab="CPI Value",
                breaks=seq(min(cpi.dat$Midwest)-sd(cpi.dat$Midwest),max(cpi.dat$Midwest)+sd(cpi.dat$Midwest),(sd(cpi.dat$Midwest)/2)))
h_cpi.so = hist(cpi.dat$South,freq = TRUE,
                main="Figure 11: Histogram of CPI values in the South",
                xlab="CPI Value",
                breaks=seq(min(cpi.dat$South)-sd(cpi.dat$South),max(cpi.dat$South)+sd(cpi.dat$South),(sd(cpi.dat$South)/2)))
h_cpi.we = hist(cpi.dat$West,freq = TRUE,
                main="Figure 12: Histogram of CPI values in the West",
                xlab="CPI Value",
                breaks=seq(min(cpi.dat$West)-sd(cpi.dat$West),max(cpi.dat$West)+sd(cpi.dat$West),(sd(cpi.dat$West)/2)))

p_mem = plot(mem.dat$mem,mem.dat$cpi,
                main="Figure 3: CPI values versus Union Membership Proportion",
                xlab="Proportion of workers in a union",ylab="CPI",xlim=c(min(mem.dat$mem)-sd(mem.dat$mem),max(mem.dat$mem)+sd(mem.dat$mem)),
                ylim=c(min(mem.dat$cpi)-sd(mem.dat$cpi),max(mem.dat$cpi)+sd(mem.dat$cpi)),col="blue",lty=1,lwd=1,pch=1)

p_mem.ne = plot(mem.dat$Year,mem.dat$Northeast,
                main="Figure 13: Northeast union membership over time",
                xlab="Year",ylab="Proportion of workers in a union",xlim=c(2006,2018),
                ylim=c(min(mem.dat$Northeast)-sd(mem.dat$Northeast),max(mem.dat$Northeast)+sd(mem.dat$Northeast)),col="blue",lty=1,lwd=1,pch=1)
p_mem.mw = plot(mem.dat$Year,mem.dat$Midwest,
                main="Figure 14: Midwest union membership over time",
                xlab="Year",ylab="Proportion of workers in a union",xlim=c(2006,2018),
                ylim=c(min(mem.dat$Midwest)-sd(mem.dat$Midwest),max(mem.dat$Midwest)+sd(mem.dat$Midwest)),col="blue",lty=1,lwd=1,pch=1)
p_mem.so = plot(mem.dat$Year,mem.dat$South,
                main="Figure 15: South union membership over time",
                xlab="Year",ylab="Proportion of workers in a union",xlim=c(2006,2018),
                ylim=c(min(mem.dat$South)-sd(mem.dat$South),max(mem.dat$South)+sd(mem.dat$South)),col="blue",lty=1,lwd=1,pch=1)
p_mem.we = plot(mem.dat$Year,mem.dat$West,
                main="Figure 16: West union membership over time",
                xlab="Year",ylab="Proportion of workers in a union",xlim=c(2006,2018),
                ylim=c(min(mem.dat$West)-sd(mem.dat$West),max(mem.dat$West)+sd(mem.dat$West)),col="blue",lty=1,lwd=1,pch=1)

p_rep.ne = plot(rep.dat$Year,rep.dat$Northeast,
                main="Figure 17: Northeast union representation over time",
                xlab="Year",ylab="Proportion of workers represented by a union",xlim=c(2006,2018),
                ylim=c(min(rep.dat$Northeast)-sd(rep.dat$Northeast),max(rep.dat$Northeast)+sd(rep.dat$Northeast)),col="blue",lty=1,lwd=1,pch=1)
p_rep.mw = plot(rep.dat$Year,rep.dat$Midwest,
                main="Figure 18: Midwest union representation over time",
                xlab="Year",ylab="Proportion of workers represented by a union",xlim=c(2006,2018),
                ylim=c(min(rep.dat$Midwest)-sd(rep.dat$Midwest),max(rep.dat$Midwest)+sd(rep.dat$Midwest)),col="blue",lty=1,lwd=1,pch=1)
p_rep.so = plot(rep.dat$Year,rep.dat$South,
                main="Figure 19: South union representation over time",
                xlab="Year",ylab="Proportion of workers represented by a union",xlim=c(2006,2018),
                ylim=c(min(rep.dat$South)-sd(rep.dat$South),max(rep.dat$South)+sd(rep.dat$South)),col="blue",lty=1,lwd=1,pch=1)
p_rep.we = plot(rep.dat$Year,rep.dat$West,
                main="Figure 20: West union representation over time",
                xlab="Year",ylab="Proportion of workers represented by a union",xlim=c(2006,2018),
                ylim=c(min(rep.dat$West)-sd(rep.dat$West),max(rep.dat$West)+sd(rep.dat$West)),col="blue",lty=1,lwd=1,pch=1)

p_cpi.ne = plot(cpi.dat$Year,cpi.dat$Northeast,
                main="Figure 21: Northeast CPI values over time",
                xlab="Year",ylab="Proportion of workers in a union",xlim=c(2006,2018),
                ylim=c(min(cpi.dat$Northeast)-sd(cpi.dat$Northeast),max(cpi.dat$Northeast)+sd(cpi.dat$Northeast)),col="blue",lty=1,lwd=1,pch=1)
p_cpi.mw = plot(cpi.dat$Year,cpi.dat$Midwest,
                main="Figure 22: Midwest CPI values over time",
                xlab="Year",ylab="Proportion of workers in a union",xlim=c(2006,2018),
                ylim=c(min(cpi.dat$Midwest)-sd(cpi.dat$Midwest),max(cpi.dat$Midwest)+sd(cpi.dat$Midwest)),col="blue",lty=1,lwd=1,pch=1)
p_cpi.so = plot(cpi.dat$Year,cpi.dat$South,
                main="Figure 23: South CPI values over time",
                xlab="Year",ylab="Proportion of workers in a union",xlim=c(2006,2018),
                ylim=c(min(cpi.dat$South)-sd(cpi.dat$South),max(cpi.dat$South)+sd(cpi.dat$South)),col="blue",lty=1,lwd=1,pch=1)
p_cpi.we = plot(cpi.dat$Year,cpi.dat$West,
                main="Figure 24: West CPI values over time",
                xlab="Year",ylab="Proportion of workers in a union",xlim=c(2006,2018),
                ylim=c(min(cpi.dat$West)-sd(cpi.dat$West),max(cpi.dat$West)+sd(cpi.dat$West)),col="blue",lty=1,lwd=1,pch=1)

p_cvm.ne = plot(mem.dat$Northeast,cpi.dat$Northeast,
                main="Figure 25: Northeast CPI values over union membership proportion",
                xlab="Proportion of workers in a union",ylab="CPI",xlim=c(min(mem.dat$Northeast)-sd(mem.dat$Northeast),max(mem.dat$Northeast)+sd(mem.dat$Northeast)),
                ylim=c(min(cpi.dat$Northeast)-sd(cpi.dat$Northeast),max(cpi.dat$Northeast)+sd(cpi.dat$Northeast)),col="blue",lty=1,lwd=1,pch=1)
p_cvr.ne = plot(rep.dat$Northeast,cpi.dat$Northeast,
                main="Figure 26: Northeast CPI values over union representation proportion",
                xlab="Proportion of workers represented by a union",ylab="CPI",xlim=c(min(rep.dat$Northeast)-sd(rep.dat$Northeast),max(rep.dat$Northeast)+sd(rep.dat$Northeast)),
                ylim=c(min(cpi.dat$Northeast)-sd(cpi.dat$Northeast),max(cpi.dat$Northeast)+sd(cpi.dat$Northeast)),col="blue",lty=1,lwd=1,pch=1)

p_cvm.mw = plot(mem.dat$Midwest,cpi.dat$Midwest,
                main="Figure 27: Midwest CPI values over union membership proportion",
                xlab="Proportion of workers in a union",ylab="CPI",xlim=c(min(mem.dat$Midwest)-sd(mem.dat$Midwest),max(mem.dat$Midwest)+sd(mem.dat$Midwest)),
                ylim=c(min(cpi.dat$Midwest)-sd(cpi.dat$Midwest),max(cpi.dat$Midwest)+sd(cpi.dat$Midwest)),col="blue",lty=1,lwd=1,pch=1)
p_cvr.mw = plot(rep.dat$Midwest,cpi.dat$Midwest,
                main="Figure 28: Northeast CPI values over union representation proportion",
                xlab="Proportion of workers represented by a union",ylab="CPI",xlim=c(min(rep.dat$Midwest)-sd(rep.dat$Midwest),max(rep.dat$Midwest)+sd(rep.dat$Midwest)),
                ylim=c(min(cpi.dat$Midwest)-sd(cpi.dat$Midwest),max(cpi.dat$Midwest)+sd(cpi.dat$Midwest)),col="blue",lty=1,lwd=1,pch=1)

p_cvm.so = plot(mem.dat$South,cpi.dat$South,
                main="Figure 29: South CPI values over union membership proportion",
                xlab="Proportion of workers in a union",ylab="CPI",xlim=c(min(mem.dat$South)-sd(mem.dat$South),max(mem.dat$South)+sd(mem.dat$South)),
                ylim=c(min(cpi.dat$South)-sd(cpi.dat$South),max(cpi.dat$South)+sd(cpi.dat$South)),col="blue",lty=1,lwd=1,pch=1)
p_cvr.so = plot(rep.dat$South,cpi.dat$South,
                main="Figure 30: South CPI values over union representation proportion",
                xlab="Proportion of workers represented by a union",ylab="CPI",xlim=c(min(rep.dat$South)-sd(rep.dat$South),max(rep.dat$South)+sd(rep.dat$South)),
                ylim=c(min(cpi.dat$South)-sd(cpi.dat$South),max(cpi.dat$South)+sd(cpi.dat$South)),col="blue",lty=1,lwd=1,pch=1)

p_cvm.we = plot(mem.dat$West,cpi.dat$West,
                main="Figure 31: West CPI values over union membership proportion",
                xlab="Proportion of workers in a union",ylab="CPI",xlim=c(min(mem.dat$West)-sd(mem.dat$West),max(mem.dat$West)+sd(mem.dat$West)),
                ylim=c(min(cpi.dat$West)-sd(cpi.dat$West),max(cpi.dat$West)+sd(cpi.dat$West)),col="blue",lty=1,lwd=1,pch=1)
p_cvr.we = plot(rep.dat$West,cpi.dat$West,
                main="Figure 32: West CPI values over union representation proportion",
                xlab="Poportion of workers represented by a union",ylab="CPI",xlim=c(min(rep.dat$West)-sd(rep.dat$West),max(rep.dat$West)+sd(rep.dat$West)),
                ylim=c(min(cpi.dat$West)-sd(cpi.dat$West),max(cpi.dat$West)+sd(cpi.dat$West)),col="blue",lty=1,lwd=1,pch=1)

p_cpi.year = plot(mem.dat$year,mem.dat$cpi,
                  main="Figure 3: CPI values over time",
                  xlab="Year",ylab="CPI Values",xlim=c(2006,2018),
                  ylim=c(min(mem.dat$cpi)-sd(mem.dat$cpi),max(mem.dat$cpi)+sd(mem.dat$cpi)),col="blue",lty=1,lwd=1,pch=1)
p_cvm = plot(mem.dat$mem,mem.dat$cpi,
                main="Figure 3: CPI values over union membership proportion",
                xlab="Proportion of workers in a union",ylab="CPI",xlim=c(min(mem.dat$mem)-sd(mem.dat$mem),max(mem.dat$mem)+sd(mem.dat$mem)),
                ylim=c(min(mem.dat$cpi)-sd(mem.dat$cpi),max(mem.dat$cpi)+sd(mem.dat$cpi)),col="blue",lty=1,lwd=1,pch=1)
p_cvr = plot(rep.dat$rep,rep.dat$cpi,
                main="Figure 4: CPI values over union representation proportion",
                xlab="Proportion of workers represented by a union",ylab="CPI",xlim=c(min(rep.dat$rep)-sd(rep.dat$rep),max(rep.dat$rep)+sd(rep.dat$rep)),
                ylim=c(min(rep.dat$cpi)-sd(rep.dat$cpi),max(rep.dat$cpi)+sd(rep.dat$cpi)),col="blue",lty=1,lwd=1,pch=1)
p_mem.year = plot(mem.dat$year,mem.dat$mem,
                main="Figure 4: Union membership over time",
                xlab="Year",ylab="Proportion of workers in a union",xlim=c(2006,2018),
                ylim=c(min(mem.dat$mem)-sd(mem.dat$mem),max(mem.dat$mem)+sd(mem.dat$mem)),col="blue",lty=1,lwd=1,pch=1)
p_rep.year = plot(rep.dat$year,rep.dat$rep,
                  main="Figure 5: Union representation over time",
                  xlab="Year",ylab="Proportion of workers in a union",xlim=c(2006,2018),
                  ylim=c(min(rep.dat$rep)-sd(rep.dat$rep),max(rep.dat$rep)+sd(rep.dat$rep)),col="blue",lty=1,lwd=1,pch=1)

#Regression Analysis
#Model 1 (mem w/o time)
model1 = lm(cpi~mem+ismw+ismw*mem+isso+isso*mem+iswe+iswe*mem, data = mem.dat)
summary(model1)

p_mem = plot(mem.dat$mem,mem.dat$cpi,
             main="Figure 1: CPI values versus Union Membership Proportion",
             xlab="Proportion of workers in a union",ylab="CPI",xlim=c(min(mem.dat$mem)-sd(mem.dat$mem),max(mem.dat$mem)+sd(mem.dat$mem)),
             ylim=c(min(mem.dat$cpi)-sd(mem.dat$cpi),max(mem.dat$cpi)+sd(mem.dat$cpi)),col="blue",lty=1,lwd=1,pch=1)
p_rep = plot(rep.dat$rep,rep.dat$cpi,
             main="Figure 2: CPI values versus Union Representation Proportion",
             xlab="Proportion of workers represented by a union",ylab="CPI",xlim=c(min(rep.dat$rep)-sd(rep.dat$rep),max(rep.dat$rep)+sd(rep.dat$rep)),
             ylim=c(min(rep.dat$cpi)-sd(rep.dat$cpi),max(rep.dat$cpi)+sd(rep.dat$cpi)),col="blue",lty=1,lwd=1,pch=1)
lines(mem.dat$mem,model1$fitted.values,col="red")


#Diagnostics on residual term
summary(model1$residuals)
sd(model1$residuals)

plot(model1$fitted.values,model1$residuals,main="Figure 33: Residuals against Predicted Values - Model 1",
     xlab="Predicted Value",ylab="Estimated Residuals",xlim=c(min(model1$fitted.values)-sd(model1$fitted.values),max(model1$fitted.values)+sd(model1$fitted.values)),
     ylim=c(min(model1$residuals)-sd(model1$residuals),max(model1$residuals)+sd(model1$residuals)),col="black",
     lty=1,lwd=1,pch=1)
abline(h=0,col="red")

h_res.model1 = hist(model1$residuals, freq = TRUE, breaks = seq(min(model1$residuals)-sd(model1$residuals)/2,max(model1$residuals)+sd(model1$residuals)/2,sd(model1$residuals)/2),
             main = "Figure 34: Histogram of Estimated Residuals", xlab = "Estimated Residuals",
             xlim = c(min(model1$residuals)-sd(model1$residuals),max(model1$residuals)+sd(model1$residuals)))

ehat_sk.model1 = skewness(model1$residuals,type=1)
ehat_ku.model1 = kurtosis(model1$residuals,type=1)

JBstat.model1 = (44/6)*((ehat_sk.model1^2) + (ehat_ku.model1^2/4))
JBstat.model1
qchisq(0.95,2)

#Model 2 (mem w/ time)
model2 = lm(cpi~mem+ismw+ismw*mem+isso+isso*mem+iswe+iswe*mem+year+year*mem, data = mem.dat)
summary(model2)

#Diagnostics on residual term
summary(model2$residuals)
sd(model2$residuals)

plot(model2$fitted.values,model2$residuals,main="Figure 35: Residuals against Predicted Values - Model 2",
     xlab="Predicted Value",ylab="Estimated Residuals",xlim=c(min(model2$fitted.values)-sd(model2$fitted.values),max(model2$fitted.values)+sd(model2$fitted.values)),
     ylim=c(min(model2$residuals)-sd(model2$residuals),max(model2$residuals)+sd(model2$residuals)),col="black",
     lty=1,lwd=1,pch=1)
abline(h=0,col="red")

h_res.model2 = hist(model2$residuals, freq = TRUE, breaks = seq(min(model2$residuals)-sd(model2$residuals)/2,max(model2$residuals)+sd(model2$residuals)/2,sd(model2$residuals)/2),
                    main = "Figure 36: Histogram of Estimated Residuals", xlab = "Estimated Residuals",
                    xlim = c(min(model2$residuals)-sd(model2$residuals),max(model2$residuals)+sd(model2$residuals)))

ehat_sk.model2 = skewness(model2$residuals,type=1)
ehat_ku.model2 = kurtosis(model2$residuals,type=1)

JBstat.model2 = (44/6)*((ehat_sk.model2^2) + (ehat_ku.model2^2/4))
JBstat.model2
qchisq(0.95,2)

#Model 3 (rep w/o time)
model3 = lm(cpi~rep+ismw+ismw*rep+isso+isso*rep+iswe+iswe*rep, data = rep.dat)
summary(model3)

#Diagnostics on residual term
summary(model3$residuals)
sd(model3$residuals)

plot(model3$fitted.values,model3$residuals,main="Figure 33: Residuals against Predicted Values - Model 3",
     xlab="Predicted Value",ylab="Estimated Residuals",xlim=c(min(model3$fitted.values)-sd(model3$fitted.values),max(model3$fitted.values)+sd(model3$fitted.values)),
     ylim=c(min(model3$residuals)-sd(model3$residuals),max(model3$residuals)+sd(model3$residuals)),col="black",
     lty=1,lwd=1,pch=1)
abline(h=0,col="red")

h_res.model3 = hist(model3$residuals, freq = TRUE, breaks = seq(min(model3$residuals)-sd(model3$residuals)/2,max(model3$residuals)+sd(model3$residuals)/2,sd(model3$residuals)/2),
                    main = "Figure 34: Histogram of Estimated Residuals", xlab = "Estimated Residuals",
                    xlim = c(min(model3$residuals)-sd(model3$residuals),max(model3$residuals)+sd(model3$residuals)))

ehat_sk.model3 = skewness(model3$residuals,type=1)
ehat_ku.model3 = kurtosis(model3$residuals,type=1)

JBstat.model3 = (44/6)*((ehat_sk.model3^2) + (ehat_ku.model3^2/4))
JBstat.model3
qchisq(0.95,2)

#Model 2 (rep w/ time)
model4 = lm(cpi~rep+ismw+ismw*rep+isso+isso*rep+iswe+iswe*rep+year+year*rep, data = rep.dat)
summary(model4)

#Diagnostics on residual term
summary(model4$residuals)
sd(model4$residuals)

plot(model4$fitted.values,model4$residuals,main="Figure 35: Residuals against Predicted Values - Model 4",
     xlab="Predicted Value",ylab="Estimated Residuals",xlim=c(min(model4$fitted.values)-sd(model4$fitted.values),max(model4$fitted.values)+sd(model4$fitted.values)),
     ylim=c(min(model4$residuals)-sd(model4$residuals),max(model4$residuals)+sd(model4$residuals)),col="black",
     lty=1,lwd=1,pch=1)
abline(h=0,col="red")

h_res.model4 = hist(model4$residuals, freq = TRUE, breaks = seq(min(model4$residuals)-sd(model4$residuals)/2,max(model4$residuals)+sd(model4$residuals)/2,sd(model4$residuals)/2),
                    main = "Figure 36: Histogram of Estimated Residuals", xlab = "Estimated Residuals",
                    xlim = c(min(model4$residuals)-sd(model4$residuals),max(model4$residuals)+sd(model4$residuals)))

ehat_sk.model4 = skewness(model4$residuals,type=1)
ehat_ku.model4 = kurtosis(model4$residuals,type=1)

JBstat.model4 = (44/6)*((ehat_sk.model4^2) + (ehat_ku.model4^2/4))
JBstat.model4
qchisq(0.95,2)


#old regressions
mem.dat$ne.time = mem.dat$Northeast*mem.dat$Year
mem.dat$mw.time = mem.dat$Midwest*mem.dat$Year
mem.dat$so.time = mem.dat$South*mem.dat$Year
mem.dat$we.time = mem.dat$West*mem.dat$Year
rep.dat$ne.time = rep.dat$Northeast*rep.dat$Year
rep.dat$mw.time = rep.dat$Midwest*rep.dat$Year
rep.dat$so.time = rep.dat$South*rep.dat$Year
rep.dat$we.time = rep.dat$West*rep.dat$Year
mem.dat$

r_cvm.ne = lm(cpi.dat$Northeast~mem.dat$Northeast)
r_cvm.mw = lm(cpi.dat$Midwest~mem.dat$Midwest)
r_cvm.so = lm(cpi.dat$South~mem.dat$South)
r_cvm.we = lm(cpi.dat$West~mem.dat$West)

summary(r_cvm.ne)
summary(r_cvm.mw)
summary(r_cvm.so)
summary(r_cvm.we)

r_cvr.ne = lm(cpi.dat$Northeast~rep.dat$Northeast)
r_cvr.mw = lm(cpi.dat$Midwest~rep.dat$Midwest)
r_cvr.so = lm(cpi.dat$South~rep.dat$South)
r_cvr.we = lm(cpi.dat$West~rep.dat$West)

summary(r_cvr.ne)
summary(r_cvr.mw)
summary(r_cvr.so)
summary(r_cvr.we)

r_cvmt.ne = lm(cpi.dat$Northeast~mem.dat$Northeast+mem.dat$Year)
r_cvmt.mw = lm(cpi.dat$Midwest~mem.dat$Midwest+mem.dat$Year)
r_cvmt.so = lm(cpi.dat$South~mem.dat$South+mem.dat$Year)
r_cvmt.we = lm(cpi.dat$West~mem.dat$West+mem.dat$Year)

summary(r_cvmt.ne)
summary(r_cvmt.mw)
summary(r_cvmt.so)
summary(r_cvmt.we)

r_cvmt.multi.ne = lm(cpi.dat$Northeast~mem.dat$Northeast+mem.dat$Year+mem.dat$ne.time)
r_cvmt.multi.mw = lm(cpi.dat$Midwest~mem.dat$Midwest+mem.dat$Year+mem.dat$mw.time)
r_cvmt.multi.so = lm(cpi.dat$South~mem.dat$South+mem.dat$Year+mem.dat$so.time)
r_cvmt.multi.we = lm(cpi.dat$West~mem.dat$West+mem.dat$Year+mem.dat$we.time)

summary(r_cvmt.multi.ne)
summary(r_cvmt.multi.mw)
summary(r_cvmt.multi.so)
summary(r_cvmt.multi.we)
