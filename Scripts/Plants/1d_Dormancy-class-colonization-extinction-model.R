## --------------- HEADER ------------------------------------------------------
## Script name: 1d_Dormancy-class-colonization-extinction-model.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliation: University of Florida
## Date Created: 2022-05-01
## Date Last modified: 2024-10-04
## Copyright (c) David S. Mason, 2022
## Contact: masond@ufl.edu, @EcoGraffito
## Purpose of script: This is a script for analyzing the binary colonization
## extinction data for plants belonging to seed dormancy classes.

## --------------- SET—UP WORKSPACE --------------------------------------------

# Clear the decks
rm(list=ls())

# Load the packages
library(tidyverse)
library(tidylog)
library(lubridate)
library(styler)
library(broom)
library(DataExplorer)
library(psych)
library(vtable)
library(lme4)
library(car)
library(emmeans)

# Convert scientific notation
options(scipen = 999)

# Bring in the data
dormancy.col <- read.csv("Clean-data/Plants/Dormancy-class-colonization.csv")
dormancy.ext <- read.csv("Clean-data/Plants/Dormancy-class-extinction.csv")

## --------------- COLONIZATION MODEL ------------------------------------------

# Model colonization
col.ever.m <- glm(Colonized.end ~ Site + Distance + DormancyClass * Treatment,
  data = dormancy.col, family = binomial
)

## Test model significance
Anova(col.ever.m, type = 3)
summary(col.ever.m)

library(performance)
check_model(col.ever.m)

check_singularity(col.ever.m) # False

# Good model fit (95% of values within confidence bands)
library(arm)
binnedplot(predict(col.ever.m, type="response", re.form=NA), 
					 resid(col.ever.m, type="response"), nclass=20)

library(DHARMa)
col.ever.sim <- simulateResiduals(col.ever.m)
plot(col.ever.sim, quantreg=T) # ok

testOutliers(col.ever.sim, type = 'bootstrap') # ok
plotResiduals(col.ever.sim, col.ever.sim$DormancyClass, quantreg = T) # Fail
plotResiduals(col.ever.sim, col.ever.sim$Treatment, quantreg = T) # ok 

# Overdispersion
testDispersion(col.ever.sim) # ok

# Calculate emmeans
col.ever.means <- emmeans(col.ever.m, ~ Treatment | DormancyClass,
  type = "response"
)

# Test signifigance
pairs(col.ever.means, adjust = "none")

# Save as dataframe
col.ever.means <- as.data.frame(col.ever.means)

# Rough visualization
ggplot(d = col.ever.means, aes(x = Treatment, y = prob)) +
  geom_errorbar(aes(ymin = prob - SE, ymax = prob + SE)) +
  geom_point() +
  facet_wrap(~DormancyClass, scales = "free_y")

# Save model outputs
col.model.coef <- tidy(col.ever.m)
write.csv(col.model.coef, "Analysis/Plants/Colonization-coef.csv",
  row.names = FALSE
)

col.model.summ <- glance(col.ever.m)
write.csv(col.model.summ, "Analysis/Plants/Colonization-summ.csv",
  row.names = FALSE
)

write.csv(col.ever.means, "Analysis/Plants/Colonization-means.csv",
  row.names = FALSE
)

## --------------- EXTINCTION MODEL --------------------------------------------

# Model colonization
ext.ever.m <- glm(Extirpated.ever ~ Site + Distance + Treatment,
  data = dormancy.ext, family = binomial
)

# Test model signifigance
Anova(ext.ever.m)
summary(ext.ever.m)

# Check residuals
plot(ext.ever.m)

# Calculate emmeans
ext.ever.means <- emmeans(ext.ever.m, ~Treatment,
  type = "response"
)

# Test significance
pairs(ext.ever.means, adjust = "none")

# Save as dataframe
ext.ever.means <- as.data.frame(ext.ever.means)

# Rough visualization
ggplot(d = ext.ever.means, aes(x = Treatment, y = prob)) +
  geom_errorbar(aes(ymin = prob - SE, ymax = prob + SE)) +
  geom_point()

check_model(ext.ever.m)

check_singularity(ext.ever.m) # False

# Good model fit (95% of values within confidence bands)
library(arm)
binnedplot(predict(ext.ever.m, type="response", re.form=NA), 
					 resid(ext.ever.m, type="response"), main='Without random effects', nclass=20)

library(DHARMa)
ext.ever.sim <- simulateResiduals(ext.ever.m)
plot(ext.ever.sim, quantreg=T) # ok

testOutliers(ext.ever.sim, type = 'bootstrap') # ok
plotResiduals(ext.ever.sim, ext.ever.sim$Treatment, quantreg = T) # ok 

# Overdispersion
testDispersion(ext.ever.sim) # ok

# Save model outputs
ext.ever.model.coef <- tidy(ext.ever.m)
write.csv(ext.ever.model.coef, "Analysis/Plants/Extinction-coef.csv",
  row.names = FALSE
)

ext.ever.model.summ <- glance(ext.ever.m)
write.csv(ext.ever.model.summ, "Analysis/Plants/Extinction-summ.csv",
  row.names = FALSE
)

write.csv(ext.ever.means, "Analysis/Plants/Extinction-means.csv",
  row.names = FALSE
)
