## --------------- HEADER ------------------------------------------------------
## Script name: 3b_Plant-nutrients-figure.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliation: University of Florida
## Date Created: 2022-6-26
## Date Last Modified: 2025-8-13
## Copyright (c) David S. Mason, 2025
## Contact: david.mason@jonesctr.org
## Purpose of script: This script creates a figure for the plant nutrient data

## --------------- SET—UP WORKSPACE --------------------------------------------
library(tidyverse)
library(tidylog)
library(lubridate)
library(styler)

# Clear the deck
rm(list = ls())

# Bring in the data
plant <- read.csv("Clean-data/Plants/Plant-nutrients-effect-size.csv")
source("Functions.R")

## --------------- SUMMARIZE DATA ----------------------------------------------
se <- function(x) sqrt(var(x)/length(x))

means <- plant %>% 
	group_by(DATE, BIOMASS, EXCLUSION, VARIABLE) %>% 
	summarize(Mean.es = mean(EFFECT.SIZE),
						se = se(EFFECT.SIZE),
						n = n()) %>% 
	mutate(LCL = lower_ci(mean = Mean.es, se = se, n = n),
				 UCL = upper_ci(mean = Mean.es, se = se, n = n))

## --------------- ADD NUDGE FACTOR --------------------------------------------

# Initialize column for Nudge factor (to separate points)
means["Nudge"] <- NA

for (i in 1:nrow(means)) {
	if (means$DATE[i] == "2020-08-01") {
		means$Nudge[i] <- 0.2
	}  else {
		means$Nudge[i] <- -0.2
	}
}

means$DATE <- as.factor(means$DATE)
means$DATE <- factor(means$DATE, c("2021-05-04",
																	 "2020-08-01"))

means$TREATMENT <- paste(means$BIOMASS, means$EXCLUSION)

means <- means %>% 
	filter(VARIABLE %in% c("K", "P", "N"))

means$VARIABLE <- factor(means$VARIABLE, c("N", "P", "K"))

means <- as.data.frame(means)
means$BIOMASS <- factor(x = means$BIOMASS, levels=c('Single carcass', 'MME'),
												ordered = TRUE)
means$EXCLUSION <- factor(means$EXCLUSION, levels=c('Open', 
																								'Herbivore',
																								'Scavenger'))

ggplot(means, aes(x = Mean.es, y = DATE))+
	geom_errorbar(aes(xmin = Mean.es - se, xmax = Mean.es + se),
								size = 0.5, width = 0.25
	) +
	geom_point(aes(fill = DATE),
						 color = "black",
						 pch = 21, size = 4
	) +
	scale_fill_manual(values = c("black", "grey"))+
	facet_wrap(~TREATMENT*VARIABLE, ncol = 3) +
	theme_bw() +
	theme(
		panel.grid.major.x = element_blank(),
		panel.grid.minor.x = element_blank(),
		text = element_text(size = 10),
		axis.title = element_text(face = "bold"),
		legend.position = "none",
		legend.title = element_blank()
	) +
	theme(axis.ticks.y = element_blank(),
				strip.background = element_blank())+
	xlab("Mean effect size") +
	ylab("")+
	theme(panel.spacing.y = unit(2, "lines"))

ggsave("Figures/Plants/Plant-tissue-nutrients.png")

