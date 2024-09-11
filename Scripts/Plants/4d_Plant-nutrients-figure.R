## --------------- HEADER ------------------------------------------------------
## Script name: 4d_Plant-nutrients-figure.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliaton: University of Florida
## Date Created: 2022-6-26
## Date Last Modified: 2022-6-26
## Copyright (c) David S. Mason, 2022
## Contact: masond@ufl.edu, @EcoGraffito
## Purpose of script: This script creates a figure for the plant nutrient data

## --------------- SET—UP WORKSPACE --------------------------------------------
library(tidyverse)
library(tidylog)
library(lubridate)
library(styler)

# Clear the deck
rm(list = ls())

# Bring in the data
plant <- read.csv("Animals-plants-seeds/Clean-data/Plants/Plant-nutrients-effect-size.csv")
source("Animals-plants-seeds/Functions.R")

## --------------- SUMMARIZE DATA ----------------------------------------------
se <- function(x) sqrt(var(x)/length(x))

means <- plant %>% 
	group_by(Date, Biomass, Fencing, Variable) %>% 
	summarize(Mean = mean(Effect.size),
						se = se(Effect.size),
						n = n()) %>% 
	mutate(LCL = lower_ci(mean = Mean, se = se, n = n),
				 UCL = upper_ci(mean = Mean, se = se, n = n))

## --------------- ADD NUDGE FACTOR --------------------------------------------

# Initialize column for Nudge factor (to separate points)
means["Nudge"] <- NA

for (i in 1:nrow(means)) {
	if (means$Date[i] == "2020-08-01") {
		means$Nudge[i] <- 0.2
	}  else {
		means$Nudge[i] <- -0.2
	}
}

means$Date <- as.factor(means$Date)
means$Date <- factor(means$Date, c("2021-05-04",
																	 "2020-08-01"))

means$Treatment <- paste(means$Biomass, means$Fencing)

means <- means %>% 
	filter(Variable %in% c("K", "P", "N"))

means$Variable <- factor(means$Variable, c("N", "P", "K"))

means <- as.data.frame(means)
means$Biomass <- factor(x = means$Biomass, levels=c('Single carcass', 'MME'),
												ordered = TRUE)
means$Fencing <- factor(means$Fencing, levels=c('Open control', 
																								'Herbivore exclusion',
																								'Scavenger exclusion'))

ggplot(means, aes(x = Mean, y = Date))+
	geom_errorbar(aes(xmin = Mean - se, xmax = Mean + se),
								size = 0.5, width = 0.25
	) +
	geom_point(aes(fill = Date),
						 color = "black",
						 pch = 21, size = 4
	) +
	scale_fill_manual(values = c("black", "grey"))+
	facet_wrap(~Treatment*Variable, ncol = 3) +
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

ggsave("Animals-plants-seeds/Figures/Plants/Plant-tissue-nutrients.png")

