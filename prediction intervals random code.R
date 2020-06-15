#Attempts in "myhw7.R" to plot prediction intervals of a regression model

#From Internet
plot(rdat$output, rdat$cost, ylim=c(100, 200), xlab="QUET", ylab="SBP", main="Regression")
lines(rdat$output,lm$fitted.values,col="lightblue")

conf_interval <- predict(lm, newdata=data.frame(QUET=newx), interval="confidence",
                         level = 0.95)
lines(newx, conf_interval[,2], col="blue", lty=2)
lines(newx, conf_interval[,3], col="blue", lty=2)

pred_interval <- predict(model1, newdata=data.frame(QUET=newx), interval="prediction",
                         level = 0.95)
lines(newx, pred_interval[,2], col="orange", lty=2)
lines(newx, pred_interval[,3], col="orange", lty=2)

#From Instructor
rdatmat = as.matrix(rdat)

pv_level1 = data.frame(output=150)
predict(lm,pv_level1,interval="confidence",level=0.95)
predict(lm,pv_level1,interval="prediction",level=0.95)

pv_level2 = data.frame(cost=rdatmat[1:10,1])

prm = predict(lm,pv_level2,interval="confidence",level=0.95)
pri = predict(lm,pv_level2,interval="prediction",level=0.95)

rci_p = cbind(rdatmat[1:10,1:2],prm[1:10,1:3],pri[1:10,2:3])

matplot(rci_p[1:10,1],rci_p[1:10,2:7], type = c("p","l","l","l","l","l"), 
        lwd=c(1,1,1,1,1,1), lty = c(1,1,2,2,3,3),pch=1,cex=0.5,
        col = c("black","blue","red","red","purple","purple"), 
        main = "Consumption Against Income",
        xlab = "Income", ylab = "Consumption", xlim = c(60,280),ylim = c(40,180))