# Initializing various things we need for the script
setwd("C:/Users/Jaret/Documents")
library(poliscidata)

#Q1, Part D
xtabC(~abort_rank3+cook_index3,statesD)
xtabC(~gun_rank3+cook_index3,statesD)
svychisqC(~abort_rank3+cook_index3,statesD)
svychisqC(~gun_rank3+cook_index3,statesD)
somersD(~cook_index3+abort_rank3,statesD)
somersD(~cook_index3+gun_rank3,statesD)

#Q2, Part B
xtabC(~abhlth+sex,gssD)
xtabC(~femrole2+sex,gssD)
svychisqC(~abhlth+sex,gssD)
svychisqC(~femrole2+sex,gssD)
CramersV(2.1319,2,2,577.15+656.01)
CramersV(62.392,2,2,592.03+685.81)

#Q3, Part A
gssD.led = subset(gssD,educ_2=="0-12 yrs")
gssD.hed = subset(gssD,educ_2=="13+ yrs")

xtabC(~partyid_3+egalit_scale3,gssD.led)
xtabC(~partyid_3+egalit_scale3,gssD.hed)
svychisqC(~partyid_3+egalit_scale3,gssD.led)
svychisqC(~partyid_3+egalit_scale3,gssD.hed)
somersD(~egalit_scale3+partyid_3,gssD.led)
somersD(~egalit_scale3+partyid_3,gssD.hed)
