# Initializing various things we need for the script
setwd("C:/Users/Jaret/Documents")
library(poliscidata)

#Q1, Part B
compmeans(nes$fedspend_scale, nes$pid_x, nes$wt, plot = F)

#Q1, Part E
svyboxplot(fedspend_scale~pid_x,nesD,all.outliers=TRUE,
           xlab="Party ID",
           ylab="Pro-spending Scale Score",
           main="Pro-government Spending Scale, by Party ID",
           col="lightgray",
           varwidth=T)

#Q1,Part G
plotmeansC(nes, ~fedspend_scale, ~pid_x, fedspend_scale~pid_x, w=~wt, 
           xlab="Party ID",
           ylab="Pro-spending Scale Score",
           main="Pro-government Spending Scale, by Party ID",
           col="lightgray",
           varwidth=T)

#Q2, Part A
xtp(nes, gay_marry, libcon3, wt,
    xlab="Ideology",
    ylab="Opinion on Gay Marriage",
    main="Gay Marriage Opinions, by Ideology")

#Q2, Part D
nes$gay.marriage.yes = as.numeric(nes$gay_marry=="Yes")*100
plotmeansC(nes, ~gay.marriage.yes, ~libcon3, gay.marriage.yes~libcon3, w=~wt,
           xlab="Ideology",
           ylab="Percent Favorable to Gay Marriage",
           main="Percent Favorable to Gay Marriage, by Ideology")
