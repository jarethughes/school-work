# Initializing various things we need for the script
setwd("C:/Users/Jaret/Documents")
library(poliscidata)

#Q1, Part B
xtp(world, democ_regime, frac_eth3,
    xlab="Ethnic Heterogeneity",
    ylab="Percentage of democracies",
    main="Percentage of democracies by Ethnic Heterogeneity")

#Q1, Part E
xtabC(~democ_regime+frac_eth3+gdp_cap2,worldD)

#Q1, Part F
world$democ.yes=as.numeric(world$democ_regime=="Yes")*100
world=svydesign(id=~1,data=world)

#Q1, Part G
iplotC(~democ.yes,~frac_eth3+gdp_cap2,worldD,democ.yes~gdp_cap2+frac_eth3,
       xlab="Fractionalization",
       ylab="Percentage of Democracies",
       main=("Percentage of Democracies by \n Fractionalization and GDP"))
legend("topright",
       legend=c("Low GDP per capita","High GDP per capita"),
       lty=c(1,2),
       lwd=2,
       title="GDP per capita",
       inset=0.05,
       bty="n")

#Q2, Part A
imeansC(~ftgr_tea,~dhs_threat3+polknow3, nesD)

#Q2, Part F
xtp(nes, dhs_threat3, polknow3,
    xlab="Political Knowledge",
    ylab="Perceived Threat of Government",
    main="Perceived Threat of Government by Political Knowledge")

#Q2, Part H
iplotC(~ftgr_tea,~dhs_threat3+polknow3,nesD,ftgr_tea~polknow3+dhs_threat3,
       xlab="Perceived threat of Government",
       ylab="Ratings of Tea Party",
       main="Ratings of Tea Party, by Perceived Government Threat \n and Political Knowledge")
legend("topleft",
       legend=c("Low know","Mid know","High know"),
       lty=c(1,2,3),
       lwd=1,
       title="Political Knowledge",
       inset=0.05,
       bty="n")
