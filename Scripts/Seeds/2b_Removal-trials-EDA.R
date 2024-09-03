## --------------- HEADER ------------------------------------------------------
## Script name: 2a_Seed-removal-EDA.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliaton: University of Florida
## Date Created: 2021-11-22
## Date Last Modified: 2022-05-11
## Copyright (c) David S. Mason, 2021
## Contact: masond@ufl.edu, @EcoGraffito
## Purpose of script: This is a script for conducting exploratory data analysis
## on the seed removal data

## --------------- SET—UP WORKSPACE --------------------------------------------
library(tidyverse)
library(tidylog)
library(lubridate)
library(styler)
library(fitdistrplus)

# Seed removal table (not in tidy format)
seed.rm <- read.csv("Animals-plants-seeds/Clean-data/Seeds/Seed-removal.csv")

# Seed removal survival table (tidy format)
surv.tbl <- read.csv("Animals-plants-seeds/Clean-data/Seeds/Seed-removal-survival-table.csv")

## --------------- LINE GRAPH --------------------------------------------------

# When trays ran out of seeds, the zeroes stopped being recorded. For EDA,
# lets treat them as zeros in summarize.
seed.rm[is.na(seed.rm)] <- 0

seed.rm$DATE <- mdy(seed.rm$DATE)

seed.rm %>% 
	filter(TRIAL == 1) %>% 
	group_by(TRIAL, TREATMENT, DATE) %>% 
	summarize(SEEDS = mean(SEEDS)) %>% 
	ggplot(aes(x = DATE, y = SEEDS, color = TREATMENT))+
	geom_line()

seed.rm %>% 
	filter(TRIAL == 2) %>% 
	group_by(TRIAL, TREATMENT, DATE) %>% 
	summarize(SEEDS = mean(SEEDS)) %>% 
	ggplot(aes(x = DATE, y = SEEDS, color = TREATMENT))+
	geom_line()

## --------------- DISTRIBUTION ------------------------------------------------

seed.rm.t1 <- seed.rm %>% 
	filter(TRIAL == 1)
descdist(seed.rm.t1$SEEDS, discrete = TRUE)

seed.rm.t2 <- seed.rm %>% 
	filter(TRIAL == 2)
descdist(seed.rm.t2$SEEDS, discrete = TRUE)

