## --------------- HEADER ------------------------------------------------------
## Script name: 1j_Traps-scat-figure.R
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
library(lubridate)
library(styler)
library(tidyverse)
library(tidylog)
library(fitdistrplus)
library(RVAideMemoire)

# Bring in the data
trap.scat <- read.csv("Animals-plants-seeds/Clean-data/Seeds/Traps-scat.csv")

## --------------- PREPARE DATA ------------------------------------------------

# Drop NAs
trap.scat <- drop_na(trap.scat)

# Round dates
trap.scat$DATE <- mdy(trap.scat$DATE)
trap.scat <- trap.scat %>% 
	mutate(ROUNDED.DATE = round_date(trap.scat$DATE, "week"))

# Put data in long form
trap.scat <- trap.scat %>% 
	dplyr::select(ROUNDED.DATE, SITE, TREATMENT, TRAP, SCAT, BIRD) %>%  
	pivot_longer(5:6, names_to = "TYPE", values_to = "DETECTIONS")
	

## --------------- CREATE FIGURE -----------------------------------------------

errbar_lims <- trap.scat %>% 
	group_by(TYPE, TREATMENT) %>% 
	summarize(mean=mean(DETECTIONS), se=sd(DETECTIONS)/sqrt(n()), 
						upper=mean+(2*se), lower=mean-(2*se))

trap.scat$ROUNDED.DATE <- as.factor(trap.scat$ROUNDED.DATE)

ggplot()+
	geom_dotplot(data = trap.scat, aes(x = TREATMENT, 
	y = DETECTIONS, fill = interaction(TREATMENT, ROUNDED.DATE)),
							 binaxis='y', stackdir='center', dotsize=1, alpha = 0.5)+
	scale_fill_manual(values = c("#ffeeac", "#c7e5f6",
															 "#4a3b00", "#0a2c3f"))+
	geom_errorbar(data = errbar_lims, aes(x = TREATMENT,
																				ymax = mean+se,
																				ymin = mean-se),
								width = 0.15, size = 1)+
	facet_wrap(~TYPE)+
	theme_bw()+
	theme(panel.grid.minor.y = element_blank()
)


