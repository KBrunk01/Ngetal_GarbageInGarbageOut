
# Code to reproduce results from: 
# Garbage in may not equal garbage out: Sex mediates effects of 'junk food' in a synanthropic species
     # Elizabeth M. Ng, Anna Pidgeon, Elena H. West, M. Zachariah Peery, and Kristin M. Brunk
     # Submitted to Journal of Urban Ecology, December 2022

setwd("")

## Packages used in analysis
library(lme4)
library(emmeans)


####### Comparison of human food enrichment between HY and AHY individuals
AgeComp <- read.csv("d13C_AgeComparison.csv")
t.test(d13C ~ Age, var.equal=F, data=AgeComp)


####### Body Mass
# AHY
AHYmass <- read.csv("BodyMass_AHY.csv")
lmer1 <- lmer(Stmass ~ d13C * Sex + (1|BandID), REML = T, data=AHYmass) # Model
summary(lmer1)
confint(lmer1)
emtrends(lmer1, pairwise ~ Sex, var = "d13C") # Estimate of sex-specific trends

# HY
HYmass <- read.csv("BodyMass_HY.csv")
lm1 <- lm(Stmass ~ d13C * Sex, data=HYmass) # Model
summary(lm1)
emtrends(lm1, pairwise ~ Sex, var = "d13C") # Estimate of sex-specific trends


####### Growth Bar Width
# AHY
AHY_GB <- read.csv("GrowthBarWidth_AHY.csv")
lmer2 <- lmer(GB_TL ~ d13C * Sex + (1|BandCombo), REML = T, data=AHY_GB)
summary(lmer2)
confint(lmer2)
emtrends(lmer2, pairwise ~ Sex, var = "d13C")

# HY
HY_GB <- read.csv("GrowthBarWidth_HY.csv")
lm2 <- lm(GB_TL ~ d13C * Sex, data=HY_GB)
summary(lm2)

####### Fecundity (adult males only)
Fec <- read.csv("Fecundity_AHY_M.csv")
lm3 <- lm(d13C~fldg.grp, data=Fec)
summary(lm3)
