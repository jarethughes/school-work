# Initializing various things we need for the script
setwd("C:/Users/Jaret/Documents")
library(poliscidata)

#Q1, Part A
wtd.t.test(gss$egalit_scale,0,weight=gss$wtss)

#Q1, Part B
CI95(19.345,0.304)

#Q1, Part C
wtd.t.test(gss$egalit_scale,20.1,weight=gss$wtss)
CI95(-0.755,0.304)
wtd.t.test(gss$egalit_scale,18.8,weight=gss$wtss)
CI95(0.545,0.304)

#Q2, Part A
wtd.ttestC(~int_info_scale,~age2,gssD)

#Q3, Part A
wtd.ttestC(~sibs,~relig2,gssD)
wtd.ttestC(~authoritarianism,~sex,gssD)
