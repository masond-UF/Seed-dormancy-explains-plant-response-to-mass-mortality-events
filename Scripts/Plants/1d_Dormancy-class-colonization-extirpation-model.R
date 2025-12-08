## --------------- HEADER ------------------------------------------------------
## Script name: 1d_Dormancy-class-colonization-extinction-model.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliation: University of Florida
## Date Created: 2022-05-01
## Date Last modified: 2025-08-13
## Copyright (c) David S. Mason, 2025
## Contact: david.mason@jonesctr.org
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
dormancy.ext <- read.csv("Clean-data/Plants/Dormancy-class-extirpation.csv")

# Change column name to DORMANCY.CLASS
colnames(dormancy.col)[7] <- "DORMANCY.CLASS"

## --------------- COLONIZATION MODEL ------------------------------------------

dormancy.col$SITE <- as_factor(dormancy.col$SITE)
dormancy.col$TRANSECT <- as_factor(dormancy.col$TRANSECT)
dormancy.col$DISTANCE <- as_factor(dormancy.col$DISTANCE)

dormancy.col$TRANSECT.ID <- factor(paste(dormancy.col$SITE, dormancy.col$TRANSECT, sep = "_"))
dormancy.col$PlotID      <- paste(dormancy.col$SITE, dormancy.col$TREATMENT, sep = "_")
class(dormancy.col$PlotID)
dormancy.col$TransectID  <- paste(dormancy.col$PlotID, dormancy.col$TRANSECT, sep = "_")
class(dormancy.col$TransectID)
dormancy.col$PointID     <- paste(dormancy.col$TransectID, dormancy.col$DISTANCE, sep = "_")
class(dormancy.col$PointID)

str(dormancy.col)

# Random effects provide no value
mod <- glmer(COLONIZED.END ~ DORMANCY.CLASS * TREATMENT +
               (1 | SITE/PlotID/TransectID/PointID),
               data = dormancy.col, family = binomial)

mod <- glmer(COLONIZED.END ~ DORMANCY.CLASS * TREATMENT +
               (1 | SITE/PlotID/TransectID),
               data = dormancy.col, family = binomial)

mod <- glmer(COLONIZED.END ~ DORMANCY.CLASS * TREATMENT +
               (1 | SITE/PlotID),
               data = dormancy.col, family = binomial)

mod <- glmer(COLONIZED.END ~ DORMANCY.CLASS * TREATMENT +
               (1 | SITE),
               data = dormancy.col, family = binomial)

rm(mod)

col.ever.m <- glm(COLONIZED.END ~ DORMANCY.CLASS * TREATMENT,
  data = dormancy.col, family = binomial
)

## Test model significance
options(contrasts = c("contr.sum", "contr.poly"))
Anova(col.ever.m, type = 3)
summary(col.ever.m)

## Check residuals
plot(col.ever.m)

library(performance)
dev.new()
check_model(col.ever.m)

performance::r2(col.ever.m)

check_singularity(col.ever.m) # False

# Good model fit (95% of values within confidence bands)
library(arm)
binnedplot(predict(col.ever.m, type="response", re.form=NA), 
					 resid(col.ever.m, type="response"), main='Without random effects', nclass=20)

library(DHARMa)
col.ever.sim <- simulateResiduals(col.ever.m)
plot(col.ever.sim, quantreg=T) # ok

testOutliers(col.ever.sim, type = 'bootstrap') # ok
plotResiduals(col.ever.sim, col.ever.sim$DORMANCYCLASS, quantreg = T) # Fail
plotResiduals(col.ever.sim, col.ever.sim$TREATMENT, quantreg = T) # ok 

# Overdispersion
testDispersion(col.ever.sim) # ok

# Calculate emmeans
emmeans(col.ever.m, ~ TREATMENT,
  type = "response"
)

emmeans(col.ever.m, ~ DORMANCY.CLASS,
  type = "response"
)

col.ever.means <- emmeans(col.ever.m, ~ TREATMENT | DORMANCY.CLASS,
  type = "response"
)

pairs(emmeans(col.ever.m, ~ TREATMENT, type = "response"))
pairs(emmeans(col.ever.m, ~ DORMANCY.CLASS, type = "response"))


# Save as dataframe
col.ever.means <- as.data.frame(col.ever.means)

# Rough visualization
ggplot(d = col.ever.means, aes(x = TREATMENT, y = prob)) +
  geom_errorbar(aes(ymin = prob - SE, ymax = prob + SE)) +
  geom_point() +
  facet_wrap(~DORMANCY.CLASS, scales = "free_y")

write.csv(col.ever.means, "Analysis/Plants/Colonization-means.csv",
  row.names = FALSE
)

## --------------- EXTIRPATION MODEL --------------------------------------------

# Model loss using the same format for consistency
ext.ever.m <- glm(EXTIRPATED.EVER ~ TREATMENT,
  data = dormancy.ext, family = binomial
)

# Test model signifigance
Anova(ext.ever.m)
summary(ext.ever.m)

# Check residuals
plot(ext.ever.m)

# Calculate emmeans
ext.ever.means <- emmeans(ext.ever.m, ~TREATMENT,
  type = "response"
)

# Test significance
pairs(ext.ever.means, adjust = "none")

# Save as dataframe
ext.ever.means <- as.data.frame(ext.ever.means)

# Rough visualization
ggplot(d = ext.ever.means, aes(x = TREATMENT, y = prob)) +
  geom_errorbar(aes(ymin = prob - SE, ymax = prob + SE)) +
  geom_point()

check_model(ext.ever.m)

check_singularity(ext.ever.m) # False

# Check explanatory power of model.
performance::r2(ext.ever.m)

# Good model fit (95% of values within confidence bands)
library(arm)
binnedplot(predict(ext.ever.m, type="response", re.form=NA), 
					 resid(ext.ever.m, type="response"), main='Without random effects', nclass=20)

library(DHARMa)
ext.ever.sim <- simulateResiduals(ext.ever.m)
plot(ext.ever.sim, quantreg=T) # ok

testOutliers(ext.ever.sim, type = 'bootstrap') # ok
plotResiduals(ext.ever.sim, ext.ever.sim$TREATMENT, quantreg = T) # ok 

# Overdispersion
testDispersion(ext.ever.sim) # ok

# Save model outputs
write.csv(ext.ever.means, "Analysis/Plants/Extirpation-means.csv",
  row.names = FALSE
)
