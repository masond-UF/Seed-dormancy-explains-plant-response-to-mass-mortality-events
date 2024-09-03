## --------------- HEADER ------------------------------------------------------
## Script name: 3b_Seed-survival-EDA.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliaton: University of Florida
## Date Created: 2021-11-22
## Copyright (c) David S. Mason, 2021
## Contact: masond@ufl.edu, @EcoGraffito
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
levels(seed.surv$TREATMENT)
levels(seed.surv$PACKET)
levels(seed.surv$TYPE)
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
xtabs( ~ FINAL.STATUS + TYPE, data = seed.surv)
xtabs( ~ FINAL.STATUS + SPECIES, data = seed.surv)

## --------------- QUICK PLOT --------------------------------------------------

seed.surv <- seed.surv %>% filter(TREATMENT != "Reference")
seed.surv <- seed.surv %>% filter(BIOMASS != "No carrion") # One of the GG reference
# rows is not being removed correctly with the filter

seed.surv <- seed.surv %>% separate(PACKET, c("TIMING", "LOCATION"))

seed.surv <- seed.surv %>% 
	group_by(SITE, PLOT, BIOMASS, TREATMENT, TIMING, LOCATION, TYPE, SPECIES) %>% 
	summarize(n = n(),
						surv = sum(FINAL.STATUS.NO.NA),
						prop = surv/n)

# Biomass and type *YES*
seed.surv %>% 
	group_by(BIOMASS, TYPE) %>% 
	dplyr::summarize(mean = mean(prop),
						n = n(),
						sd = sd(prop),
						se = sd/sqrt(n)) %>% 
	ggplot(aes(x = BIOMASS, y = mean, color = TYPE))+
	geom_linerange(aes(ymin = mean-se, ymax = mean+se),
								 position = position_dodge(1))+
	geom_point(position = position_dodge(1))

# Treatment and type *YES*
seed.surv %>% 
	group_by(TREATMENT, TYPE) %>% 
	dplyr::summarize(mean = mean(prop),
									 n = n(),
									 sd = sd(prop),
									 se = sd/sqrt(n)) %>% 
	ggplot(aes(x = TREATMENT, y = mean, color = TYPE))+
	geom_linerange(aes(ymin = mean-se, ymax = mean+se),
								 position = position_dodge(1))+
	geom_point(position = position_dodge(1))

# Biomass and treatment
seed.surv %>% 
	group_by(BIOMASS, TREATMENT) %>% 
	dplyr::summarize(mean = mean(prop),
									 n = n(),
									 sd = sd(prop),
									 se = sd/sqrt(n)) %>% 
	ggplot(aes(x = BIOMASS, y = mean, color = TREATMENT))+
	geom_linerange(aes(ymin = mean-se, ymax = mean+se),
								 position = position_dodge(1))+
	geom_point(position = position_dodge(1))

# Combined treatment?
seed.surv$COMB <- paste(seed.surv$BIOMASS, seed.surv$TREATMENT)

seed.surv %>% 
	group_by(COMB, TYPE) %>% 
	dplyr::summarize(mean = mean(prop),
									 n = n(),
									 sd = sd(prop),
									 se = sd/sqrt(n)) %>% 
	ggplot(aes(x = COMB, y = mean, color = TYPE))+
	geom_linerange(aes(ymin = mean-se, ymax = mean+se),
								 position = position_dodge(1))+
	geom_point(position = position_dodge(1))+
	theme(axis.text.x = element_text(angle = 25, hjust=1))

	
# Location and type *NO*
seed.surv %>% 
	group_by(LOCATION, TYPE) %>% 
	dplyr::summarize(mean = mean(prop),
									 n = n(),
									 sd = sd(prop),
									 se = sd/sqrt(n)) %>% 
	ggplot(aes(x = LOCATION, y = mean, color = TYPE))+
	geom_linerange(aes(ymin = mean-se, ymax = mean+se),
								 position = position_dodge(1))+
	geom_point(position = position_dodge(1))

# Timing and type *YES* 
seed.surv %>% 
	group_by(TIMING, TYPE) %>% 
	dplyr::summarize(mean = mean(prop),
									 n = n(),
									 sd = sd(prop),
									 se = sd/sqrt(n)) %>% 
	ggplot(aes(x = TIMING, y = mean, color = TYPE))+
	geom_linerange(aes(ymin = mean-se, ymax = mean+se),
								 position = position_dodge(1))+
	geom_point(position = position_dodge(1))



