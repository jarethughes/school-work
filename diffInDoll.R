rdat = read.csv("diffInDollData.csv",header=TRUE)

#Create DID vars
rdat$time=ifelse(rdat$year >= 2001,1,0)
rdat$treated=ifelse(rdat$country=="E"|
                    rdat$country=="S",1,0)
rdat$did=rdat$time*rdat$treated

#Need to sub-section out the two different experiments so that they dont interfere with each other in the regression models
rdat_south = subset(rdat,country=="E"|
                    country=="V")
rdat_central = subset(rdat,country=="G"|
                      country=="S")
rdat_E = subset(rdat,country=="E")
rdat_S = subset(rdat,country=="S")
rdat_V = subset(rdat,country=="V")
rdat_G = subset(rdat,country=="G")

#Summary Stats
summary(rdat)
summary(rdat_south)
summary(rdat_central)
summary(rdat_E)
summary(rdat_V)
summary(rdat_S)
summary(rdat_G)

plot(rdat_E$year,rdat_E$inflation,main="Figure 1: Inflation Rate per year, Ecuador",
    xlab="Year",ylab="Inflation",xlim=c(min(rdat_E$year)-sd(rdat_E$year)/2,max(rdat_E$year)+sd(rdat_E$year)/2),
    ylim=c(min(rdat_E$inflation)-sd(rdat_E$inflation)/2,max(rdat_E$inflation)+sd(rdat_E$inflation)/2),col="blue",
    lty = 1,lwd=1,pch=1)
plot(rdat_V$year,rdat_V$inflation,main="Figure 2: Inflation Rate per year, Venezuela",
     xlab="Year",ylab="Inflation",xlim=c(min(rdat_V$year)-sd(rdat_V$year)/2,max(rdat_V$year)+sd(rdat_V$year)/2),
     ylim=c(min(rdat_V$inflation)-sd(rdat_V$inflation)/2,max(rdat_V$inflation)+sd(rdat_V$inflation)/2),col="blue",
     lty = 1,lwd=1,pch=1)
plot(rdat_S$year,rdat_S$inflation,main="Figure 3: Inflation Rate per year, El Salvador",
     xlab="Year",ylab="Inflation",xlim=c(min(rdat_S$year)-sd(rdat_S$year)/2,max(rdat_S$year)+sd(rdat_S$year)/2),
     ylim=c(min(rdat_S$inflation)-sd(rdat_S$inflation)/2,max(rdat_S$inflation)+sd(rdat_S$inflation)/2),col="blue",
     lty = 1,lwd=1,pch=1)
plot(rdat_G$year,rdat_G$inflation,main="Figure 4: Inflation Rate per year, Guatemala",
     xlab="Year",ylab="Inflation",xlim=c(min(rdat_G$year)-sd(rdat_G$year)/2,max(rdat_G$year)+sd(rdat_G$year)/2),
     ylim=c(min(rdat_G$inflation)-sd(rdat_G$inflation)/2,max(rdat_G$inflation)+sd(rdat_G$inflation)/2),col="blue",
     lty = 1,lwd=1,pch=1)

plot(rdat_E$year,rdat_E$inflation_change,main="Figure 5: Inflation Rate Change per year, Ecuador",
     xlab="Year",ylab="Inflation Change",xlim=c(min(rdat_E$year)-sd(rdat_E$year)/2,max(rdat_E$year)+sd(rdat_E$year)/2),
     ylim=c(min(rdat_E$inflation_change)-sd(rdat_E$inflation_change)/2,max(rdat_E$inflation_change)+sd(rdat_E$inflation_change)/2),col="blue",
     lty = 1,lwd=1,pch=1)
plot(rdat_V$year,rdat_V$inflation_change,main="Figure 6: Inflation Rate Change per year, Venezuela",
     xlab="Year",ylab="Inflation Change",xlim=c(min(rdat_V$year)-sd(rdat_V$year)/2,max(rdat_V$year)+sd(rdat_V$year)/2),
     ylim=c(min(rdat_V$inflation_change)-sd(rdat_V$inflation_change)/2,max(rdat_V$inflation_change)+sd(rdat_V$inflation_change)/2),col="blue",
     lty = 1,lwd=1,pch=1)
plot(rdat_S$year,rdat_S$inflation_change,main="Figure 7: Inflation Rate Change per year, El Salvador",
     xlab="Year",ylab="Inflation Change",xlim=c(min(rdat_S$year)-sd(rdat_S$year)/2,max(rdat_S$year)+sd(rdat_S$year)/2),
     ylim=c(min(rdat_S$inflation_change)-sd(rdat_S$inflation_change)/2,max(rdat_S$inflation_change)+sd(rdat_S$inflation_change)/2),col="blue",
     lty = 1,lwd=1,pch=1)
plot(rdat_G$year,rdat_G$inflation_change,main="Figure 8: Inflation Rate Change per year, Guatemala",
     xlab="Year",ylab="Inflation Change",xlim=c(min(rdat_G$year)-sd(rdat_G$year)/2,max(rdat_G$year)+sd(rdat_G$year)/2),
     ylim=c(min(rdat_G$inflation_change)-sd(rdat_G$inflation_change)/2,max(rdat_G$inflation_change)+sd(rdat_G$inflation_change)/2),col="blue",
     lty = 1,lwd=1,pch=1)

#Run regressions on DID vars, first inflation, then inflation volatility
reg_difS=lm(inflation~gdp+gdp_growth+pop+gdp_pc+gdp_pc_growth+year+treated+time+did,data=rdat_south)
summary(reg_difS)

reg_difC=lm(inflation~gdp+gdp_growth+pop+gdp_pc+gdp_pc_growth+year+treated+time+did,data=rdat_central)
summary(reg_difC)


reg_difS_delta=lm(inflation_change~gdp+gdp_growth+pop+gdp_pc+gdp_pc_growth+year+treated+time+did,data=rdat_south)
summary(reg_difS_delta)

reg_difC_delta=lm(inflation_change~gdp+gdp_growth+pop+gdp_pc+gdp_pc_growth+year+treated+time+did,data=rdat_central)
summary(reg_difC_delta)
