## --------------- HEADER ------------------------------------------------------
## Script name: 1i_Traps-scat-analysis.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliaton: University of Florida
## Date Created: 2021-11-23
## Date Last modified: 2022-05-13
## Copyright (c) David S. Mason, 2021
## Contact: masond@ufl.edu, @EcoGraffito
## Purpose of script: This is a script for performing exploratory data analysis
## on the seed trap data.

## --------------- SET—UP WORKSPACE --------------------------------------------

rm(list=ls())

library(lubridate)
library(styler)
library(tidyverse)
library(tidylog)
library(fitdistrplus)
library(RVAideMemoire)

# Bring pre-cleaned data
trap.scat <- read.csv("Animals-plants-seeds/Clean-data/Seeds/Traps-scat.csv")

# Drop NAs
trap.scat <- drop_na(trap.scat)

# Round dates
trap.scat$DATE <- mdy(trap.scat$DATE)
trap.scat <- trap.scat %>% 
	mutate(Rounded.date = round_date(trap.scat$DATE, "week"))

## --------------- CHECK DISTRIBUTIONS -----------------------------------------

# Distributions
hist(trap.scat$SCAT)
descdist(trap.scat$SCAT, discrete = TRUE) # negative binomial

hist(trap.scat$BIRD)
descdist(trap.scat$BIRD, discrete = TRUE) # negative binomial

## --------------- SUMMARIZE THE DATA ------------------------------------------

rodent <- trap.scat %>% 
	group_by(SITE, Rounded.date, TREATMENT) %>% 
	summarize(Rodent.scat = sum(SCAT))

bird <- trap.scat %>% 
	group_by(SITE, Rounded.date, TREATMENT) %>% 
	summarize(Bird.scat = sum(BIRD))

## --------------- RODENT SCAT MODEL -------------------------------------------

rodent.mod <- glmer.nb(Rodent.scat ~ TREATMENT + (1|SITE/Rounded.date),
											 data = rodent)
summary(rodent.mod)
Anova(rodent.mod) # Not significant

rodent.mod.sim <- simulateResiduals(rodent.mod)
plot(rodent.mod.sim) ## Too few data points

perm.anova(Rodent.scat ~ TREATMENT + Rounded.date | SITE,
					 data = rodent) 

## --------------- Bird SCAT MODEL -------------------------------------------

bird.mod <- glmer.nb(Bird.scat ~ TREATMENT + (1|SITE/Rounded.date),
											 data = bird)
summary(bird.mod)
Anova(bird.mod) # Not significant

bird.mod.sim <- simulateResiduals(bird.mod)
plot(bird.mod.sim)

perm.anova(Bird.scat ~ TREATMENT + Rounded.date | SITE,
					 data = bird) # More scat in MME 
