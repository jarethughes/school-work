# Initializing various things we need for the script
setwd("C:/Users/Jaret/Documents")
library(poliscidata)
world = world
gss = gss

# Question 1, part A: Summary Statistics
describe(world$women13)
wtd.hist(world$women13)

# Question 1, part D: Sorting a dataset by some variable
sortC(world, country, women13, TRUE)

# Question 1, part E: Histogram Labels
#    Built-in hist() throws "'from' must be a finite number". Suspect it's due to "NA" values in world$women13
wtd.hist(world$women13, xlab = "Percentage of Women in Legislature", main = "Percentage Women Legislators 90 Democracies")

#Question 2, part A: Summary Stats
describe(gss$science_quiz)
describe(gss$wordsum)
freqC(gss$science_quiz)
freqC(gss$wordsum)
wtd.sd(gss$science_quiz)
wtd.sd(gss$wordsum)

#Question 3, part C: Summary Stats(ish)
freqC(gss$attend)
wtd.mode(gss$attend)
wtd.median(gss$attend)

#Question 3, part F: Table creation
printC(freqC(gss$attend))
