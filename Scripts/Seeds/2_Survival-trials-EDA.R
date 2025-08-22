## --------------- HEADER ------------------------------------------------------
## Script name: 2_Seed-survival-EDA.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliation: University of Florida
## Date Last Modified: 2025-8-13
## Copyright (c) David S. Mason, 2025
## Contact: david.mason@jonesctr.org
## Purpose of script: This will be a script for conducting exploratory data 
## analysis on the seed survival data

## --------------- SET—UP WORKSPACE --------------------------------------------
library(tidyverse)
library(tidylog)
library(lubridate)
library(styler)

rm(list = ls())
seed.surv <- read.csv("Animals-plants-seeds/Clean-data/Seeds/Seed-survival.csv")

## --------------- CHECK THE DATA STRUCTURE ------------------------------------

levels(seed.surv$SITE)
levels(seed.surv$PLOT)
levels(seed.surv$BIOMASS)
levels(seed.surv$EXCLUSION)
levels(seed.surv$PACKET)
levels(seed.surv$DORMANCY.CLASS)
levels(seed.surv$SPECIES)

## --------------- CHECK THE DISTRIBUTION --------------------------------------

hist(seed.surv$FINAL.STATUS) 
# Most seeds were unable to germinate after the experiment

## --------------- CONTINGENCY TABLES ------------------------------------------

xtabs( ~ FINAL.STATUS + SITE, data = seed.surv)
xtabs( ~ FINAL.STATUS + PLOT, data = seed.surv)
xtabs( ~ FINAL.STATUS + BIOMASS, data = seed.surv)
xtabs( ~ FINAL.STATUS + TREATMENT, data = seed.surv)
xtabs( ~ FINAL.STATUS + PACKET, data = seed.surv)
xtabs( ~ FINAL.STATUS + DORMANCY.CLASS, data = seed.surv)
xtabs( ~ FINAL.STATUS + SPECIES, data = seed.surv)

## --------------- QUICK PLOT --------------------------------------------------

seed.surv <- seed.surv %>% filter(EXCLUSION != "Reference")
seed.surv <- seed.surv %>% filter(BIOMASS != "No carrion") # One of the GG reference
# rows is not being removed correctly with the filter

seed.surv <- seed.surv %>% separate(PACKET, c("TIMING", "LOCATION"))

seed.surv <- seed.surv %>% 
	group_by(SITE, PLOT, BIOMASS, EXCLUSION, TIMING, LOCATION, DORMANCY.CLASS, SPECIES) %>% 
	summarize(n = n(),
						surv = sum(FINAL.STATUS),
						prop = surv/n)

# Biomass and typE
seed.surv %>% 
	group_by(BIOMASS, DORMANCY.CLASS) %>% 
	dplyr::summarize(mean = mean(prop),
						n = n(),
						sd = sd(prop),
						se = sd/sqrt(n)) %>% 
	ggplot(aes(x = BIOMASS, y = mean, color = DORMANCY.CLASS))+
	geom_linerange(aes(ymin = mean-se, ymax = mean+se),
								 position = position_dodge(1))+
	geom_point(position = position_dodge(1))

# Exclusion and type
seed.surv %>% 
	group_by(EXCLUSION, DORMANCY.CLASS) %>% 
	dplyr::summarize(mean = mean(prop),
									 n = n(),
									 sd = sd(prop),
									 se = sd/sqrt(n)) %>% 
	ggplot(aes(x = EXCLUSION, y = mean, color = DORMANCY.CLASS))+
	geom_linerange(aes(ymin = mean-se, ymax = mean+se),
								 position = position_dodge(1))+
	geom_point(position = position_dodge(1))

# Biomass and exclusion
seed.surv %>% 
	group_by(BIOMASS, EXCLUSION) %>% 
	dplyr::summarize(mean = mean(prop),
									 n = n(),
									 sd = sd(prop),
									 se = sd/sqrt(n)) %>% 
	ggplot(aes(x = BIOMASS, y = mean, color = TREATMENT))+
	geom_linerange(aes(ymin = mean-se, ymax = mean+se),
								 position = position_dodge(1))+
	geom_point(position = position_dodge(1))

# Combined treatment
seed.surv$TREATMENT <- paste(seed.surv$BIOMASS, seed.surv$EXCLUSION)

seed.surv %>% 
	group_by(TREATMENT, DORMANCY.CLASS) %>% 
	dplyr::summarize(mean = mean(prop),
									 n = n(),
									 sd = sd(prop),
									 se = sd/sqrt(n)) %>% 
	ggplot(aes(x = TREATMENT, y = mean, color = DORMANCY.CLASS))+
	geom_linerange(aes(ymin = mean-se, ymax = mean+se),
								 position = position_dodge(1))+
	geom_point(position = position_dodge(1))+
	theme(axis.text.x = element_text(angle = 25, hjust=1))

	
# Location and type
seed.surv %>% 
	group_by(LOCATION, DORMANCY.CLASS) %>% 
	dplyr::summarize(mean = mean(prop),
									 n = n(),
									 sd = sd(prop),
									 se = sd/sqrt(n)) %>% 
	ggplot(aes(x = LOCATION, y = mean, color = DORMANCY.CLASS))+
	geom_linerange(aes(ymin = mean-se, ymax = mean+se),
								 position = position_dodge(1))+
	geom_point(position = position_dodge(1))

# Timing and type 
seed.surv %>% 
	group_by(TIMING, DORMANCY.CLASS) %>% 
	dplyr::summarize(mean = mean(prop),
									 n = n(),
									 sd = sd(prop),
									 se = sd/sqrt(n)) %>% 
	ggplot(aes(x = TIMING, y = mean, color = DORMANCY.CLASS))+
	geom_linerange(aes(ymin = mean-se, ymax = mean+se),
								 position = position_dodge(1))+
	geom_point(position = position_dodge(1))



