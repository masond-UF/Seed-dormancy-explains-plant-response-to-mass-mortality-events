## --------------- HEADER ------------------------------------------------------
## Script name: 2_Decay-rates-analysis.R
## Author: Abby Jones and David Mason
## Date Created: 2019-6-6
## Date Last Modified: 2025-8-22
## Copyright (c) Abby Jones, 2025
## Contact: david.mason@jonesctr.org
## Purpose of script: This script measures the rate of decay of different 
## MMEs and single carcasses over time

# Justification for stats test:
# ANOVA as also used in other papers focused on decay rate
# Also has both a categorical (treatment) and numerical (ADD) affecting variables

#Clear
rm(list=ls())

#Library downloads
library(dplyr)
library(plyr)
library(lme4)
library(car)
library(emmeans)

# Bring in the data
decay <- read.csv("Clean-data/Carrion-decomposition/Carrion-decay-rate.csv")

## --------------- Model 1: Early Decomposition --------------------------------

early <- decay |> filter(DECAY.STAGE == 'Early')
View(early)

hist(early$DAY.REACHED) # All the same?

shapiro.test(early$DAY.REACHED)
# Non-normal

decay.model <- glmer(DAY.REACHED~BIOMASS*EXCLUSION+(1|SITE),data=early,family=poisson)
anova(decay.model)
summary(decay.model)
#No significance, all one day so wouldn't run

rm(early)

## --------------- Model 2: Advanced Decomposition -----------------------------

# Open Decay Rate_AdvancedCalculations.csv
advanced <- decay |> filter(DECAY.STAGE == 'Advanced')
View(advanced)

hist(advanced$DAY.REACHED)

shapiro.test(advanced$DAY.REACHED)
# Non-normal

decay.model <- glmer(DAY.REACHED~BIOMASS*EXCLUSION+(1|SITE),data=advanced,family=poisson)
Anova(decay.model, type = 3)
summary(decay.model)

emmeans(decay.model,list (pairwise ~ BIOMASS*EXCLUSION),adjust="tukey")

emmeans(decay.model,list (pairwise ~ BIOMASS),adjust="tukey")
emmeans(decay.model,list (pairwise ~ EXCLUSION),adjust="tukey")

## --------------- Model 3: Skeletal Decomposition -----------------------------

# Open Decay Rate_Skeletal.csv
skeletal <- decay |> filter(DECAY.STAGE == 'Skeletal') # Missing data not included
View(skeletal)

hist(skeletal$DAY.REACHED)

shapiro.test(skeletal$DAY.REACHED)
# Non-normal

decay.model <- glmer(DAY.REACHED~BIOMASS*EXCLUSION+(1|SITE),data=skeletal,family=poisson)
anova(decay.model)
summary(decay.model)

emmeans(decay.model,list (pairwise ~ BIOMASS*EXCLUSION),adjust="tukey")

emmeans(decay.model,list (pairwise ~ BIOMASS),adjust="tukey")
emmeans(decay.model,list (pairwise ~ EXCLUSION),adjust="tukey")
