## --------------- HEADER ------------------------------------------------------
## Script name: 4_Soil-nutrients-figures.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliaton: University of Florida
## Date Created: 2022-6-25
## Date Last Modified: 2022-6-25
## Copyright (c) David S. Mason, 2022
## Contact: masond@ufl.edu, @EcoGraffito
## Purpose of script: This script creates soil nutrient figures

## --------------- SET—UP WORKSPACE --------------------------------------------
library(tidyverse)
library(lubridate)
library(lme4)
library(sjmisc)

rm(list=ls())

soil <- read.csv("Animals-plants-seeds/Clean-data/Soils/Soil-nutrients-effect-size.csv")
source("Animals-plants-seeds/Functions.R")

se <- function(x) sqrt(var(x)/length(x))

## --------------- SUMMARIZE DATA ----------------------------------------------

means <- soil %>% 
	group_by(Treatment, Biomass, Fencing, Variable, Date, Measurement) %>% 
	summarize(Mean = mean(Effect.size),
						se = se(Effect.size),
						n = n()) %>% 
	mutate(LCL = lower_ci(mean = Mean, se = se, n = n),
				 UCL = upper_ci(mean = Mean, se = se, n = n))

## --------------- ADD NUDGE FACTOR --------------------------------------------

# Initialize column for Nudge factor (to separate points)
means["Nudge"] <- NA

for (i in 1:nrow(means)) {
	if (means$Measurement[i] == "1") {
		means$Nudge[i] <- 0.2
	}  
	if (means$Measurement[i] == "3") {
		means$Nudge[i] <- -0.2
	} 
	if (means$Measurement[i] == "2") {
		means$Nudge[i] <- 0.0
	}
}

means$Measurement <- as.factor(means$Measurement)
means$Date <- as.factor(means$Date)

means <- means %>% filter(Variable %in% c("Phosphorous", "Potassium", "Percent Nitrogen"))
means$Treatment <- factor(means$Treatment, c("No Carrion Reference", 
																				 "Single Carcass Open",
																				 "Single Carcass Scavenger Exclusion",
																				 "Single Carcass Herbivore Exclusion",
																				 "MME Open",
																				 "MME Scavenger Exclusion",
																				 "MME Herbivore Exclusion"))

means$Date <- factor(means$Date, c("2021-05-05",
																	 "2020-07-21",
																	 "2020-05-19"))

ggplot(means, aes(x = Mean, y = as.factor(Date)))+
	geom_errorbar(aes(xmin = Mean - se, xmax = Mean + se),
								size = 0.5, width = 0.25
	) +
	geom_point(aes(fill = Date),
						 color = "black",
						 pch = 21, size = 2,
	) +
	scale_fill_manual(values = c("black", "grey", "white"))+
	facet_wrap(~Treatment*Variable, ncol = 3) +
	theme_bw() +
	theme(
		panel.grid.major.x = element_blank(),
		panel.grid.minor.x = element_blank(),
		text = element_text(size = 22),
		axis.title = element_text(face = "bold"),
		legend.position = "none",
		legend.title = element_blank(),
		axis.text.y = element_blank(),
	) +
	theme(axis.ticks.y = element_blank(),
				strip.background = element_blank(),
				strip.text.x = element_blank())+
	xlab("Mean effect size") +
	ylab("")+
	theme(panel.spacing.y = unit(2, "lines"))

ggsave("Animals-plants-seeds/Figures/Soil/Soil-nutrients.png")


## Visualizing all the nutrients [not subsetted]

ggplot(means, aes(x = Mean, y = as.factor(Date)))+
	geom_errorbar(aes(xmin = Mean - se, xmax = Mean + se),
								size = 0.5, width = 0.25
	) +
	geom_point(aes(fill = Date),
						 color = "black",
						 pch = 21, size = 2,
	) +
	scale_fill_manual(values = c("black", "grey", "white"))+
	facet_wrap(~Treatment*Variable, ncol = 5) +
	theme_bw() +
	xlab("Mean effect size") +
	ylab("")+
	theme(panel.spacing.y = unit(2, "lines"))
