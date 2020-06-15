# Initializing various things we need for the script
setwd("C:/Users/Jaret/Documents")
library(poliscidata)
gss = gss

# Q1, Part A,B,C: Percent Liberal,Moderate,Conservative
freq(gss$polviews,gss$wtss)

#Q1, Part D
gss$polviews.n = as.numeric(gss$polviews)
freq(gss$polviews.n,gss$wtss)

#Q1, Part E
cutpnts = c(4,5,8)
gss$polview3 = cut2(gss$polviews.n,cutpnts)
freq(gss$polview3,gss$wtss)

#Q1, Part F
levels(gss$polview3) = c("Liberal", "Moderate", "Conservative")
gss$polview3 = as.ordered(gss$polview3)
freq(gss$polview3)
printC(freq(gss$polview3))

#Q2, Part A
gss$income06.n = as.numeric(gss$income06)
gss$income06.n4 = cut2(gss$income06.n,g=4)
levels(gss$income06.n4) = c("Low","MedLow","MedHigh","High")
gss$income06.n4 = as.ordered(gss$income06.n4)

freq(gss$income06.n4)

#Q2, Part B
printC(freq(gss$income06.n4))

#Q3, Part A
freq(gss$grass,gss$wtss)
