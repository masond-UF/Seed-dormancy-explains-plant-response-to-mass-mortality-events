## --------------- HEADER ------------------------------------------------------
## Script name: 1f_Traps-summary-stats-figures.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliaton: University of Florida
## Date Created: 2022-05-10
## Date Last Modified: 2022-05-10
## Copyright (c) David S. Mason, 2021
## Contact: masond@ufl.edu, @EcoGraffito
## Purpose of script:

## --------------- SET—UP WORKSPACE --------------------------------------------
library(tidyverse)
library(tidylog)
library(lubridate)
library(styler)

source("Animals-plants-seeds/Functions.R") # Load packages

rm(list=ls())

# Bring in the data
dorm.means <- read.csv("Animals-plants-seeds/Clean-data/Seeds/Trap-dormancy-means.csv")

## --------------- FIGURE ------------------------------------------------------

dorm.means$TREATMENT <- plyr::revalue(dorm.means$TREATMENT, c("CONTROL" = "Control",
																			"MME" = "Carrion deployment"))

ggplot(dorm.means,
  aes(x = TREATMENT, y = rate, fill = DormancyClass)) +
	geom_errorbar(aes(ymin = rate-SE, ymax = rate+SE),
								width = 0, size = 1.75,
								position = position_dodge(width = 0.5)) +
	geom_point(position = position_dodge(width = 0.5), size = 6,
						 shape = 21, stroke = 2) +
	scale_fill_manual(values = c("orangered", "mediumturquoise", "magenta"),
										labels = c("Non dormant", "Physiological dormancy",
															 "Physical dormancy"))+
	scale_y_continuous(limits = c(0,15))+
	guides(fill = guide_legend("Dormancy class"))+
	xlab("")+
	ylab("Mean seed detections")+
	ggtitle("Monthly seed arrival")+
	theme_bw()+
	theme(plot.title = element_text(hjust = 0.5, face = "bold"),
				axis.text.x = element_text(face = "bold"),
				axis.text.y = element_text(face = "bold"),
				axis.title.y = element_text(face = "bold"),
				text=element_text(size=25))
