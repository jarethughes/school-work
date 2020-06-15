library(foreign)
mydata = read.dta("http://dss.princeton.edu/training/Panel101.dta")

mydata$time=ifelse(mydata$year >= 1994,1,0)
mydata$treated=ifelse(mydata$country=="E"|
                      mydata$country=="F"|
                      mydata$country=="G",1,0)
mydata$did=mydata$time*mydata$treated

didreg=lm(y~treated+time+did,data=mydata)
summary(didreg)