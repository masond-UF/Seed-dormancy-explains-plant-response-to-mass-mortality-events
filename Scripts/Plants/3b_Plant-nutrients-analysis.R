## --------------- HEADER ------------------------------------------------------
## Script name: 3c_Plant-nutrients-analysis.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliation: University of Florida
## Date Created: 2022-7-10
## Date Last Modified: 2025-8-13
## Copyright (c) David S. Mason, 2025
## Contact: masond@ufl.edu, @EcoGraffito
## Purpose of script: This script analyzes plant tissue nutrients

## --------------- SET—UP WORKSPACE --------------------------------------------
library(tidyverse)
library(tidylog)
library(lubridate)
library(styler)
library(vegan)

# Clear the deck
rm(list = ls())

# Bring in the data
plant <- read.csv("Raw-data/Plants/Plant-tissue-nutrients.csv")

# Fix the date
plant$DATE <- mdy(plant$DATE)

pred <- plant[,1:4]
resp <- plant[,c(5,7,9)]
resp.hell <- decostand(resp, method = "hell")

pred$TREATMENT <- paste(pred$BIOMASS, pred$EXCLUSION)
## --------------- ADONIS ------------------------------------------------------

# Set the permutation to account for blocking design
perm <- how(nperm = 199)
setBlocks(perm) <- with(pred, SITE)

# Run the model
adonis2(resp.hell ~ TREATMENT*DATE,
				data = pred,
				permutations = perm
)

## --------------- MEANS -------------------------------------------------------

plant$Reference <- NA

for(i in 1:nrow(plant)){
	if(plant$BIOMASS[i] == 'Reference'){
		plant$REFERENCE[i] <- 'Yes'
	} else {
		plant$REFERENCE[i] <- 'No'
	}
}

plant.lg <- plant |>
	pivot_longer(cols = 5:15, names_to = "METRIC", values_to = "VALUE")

treat.vs.ref <- plant.lg |>
	group_by(REFERENCE, METRIC) |>
	summarize(Mean = mean(VALUE),
						sd = sd(VALUE),
						n = n(),
						se = sd/sqrt(n))

treat.vs.ref.date <- plant.lg |>
	group_by(REFERENCE, DATE, METRIC) |>
	summarize(Mean = mean(VALUE),
						sd = sd(VALUE),
						n = n(),
						se = sd/sqrt(n))

biomass <- plant.lg |>
	group_by(BIOMASS, METRIC) |>
	filter(Metric %in% c("N", "K", "P")) |>
	summarize(Mean = mean(VALUE),
						sd = sd(VALUE),
						n = n(),
						se = sd/sqrt(n))
# Nitrogen MME = 1.620667, single = 1.292500
(1.6206670-1.292500/1.292500)*100 # 62.0667% increase

# Potassium (K) MME = 1.549000, single = 1.145833
((1.549000-1.1458330)/1.145833)*100 # 35.18549% increase

# Phosphorous (P) MME = 0.201500, single = 0.126625
((0.201500-0.126625)/0.126625)*100 # 59.13129% increase

sum(62.0667, 35.18549, 59.13129) # 156.3835
156.3835/3
# 52.12783

biomass.fencing <- plant.lg |>
	group_by(BIOMASS, EXCLUSION, METRIC) |>
	filter(Metric %in% c("N", "K", "P")) |>
	summarize(Mean = mean(VALUE),
						sd = sd(VALUE),
						n = n(),
						se = sd/sqrt(n))

biomass.fencing.date <- plant.lg |>
	group_by(DATE, BIOMASS, EXCLUSION, METRIC) |>
	filter(Metric %in% c("N", "K", "P")) |>
	summarize(Mean = mean(VALUE),
						sd = sd(VALUE),
						n = n(),
						se = sd/sqrt(n))

ggplot(biomass.fencing.date |> filter(EXCLUSION != "Reference"), 
			 aes(x = BIOMASS, y = Mean, color = DATE))+
	geom_point(position=position_dodge(width=0.5), size = 1)+
	geom_linerange(aes(ymin = Mean-se, ymax = Mean+se),
								 position=position_dodge(width=0.5))+
	facet_wrap(~METRIC*EXCLUSION)

plant.lg$TREATMENT <- paste(plant.lg$BIOMASS, plant.lg$EXCLUSION)

Treatment.date <- plant.lg |>
	group_by(DATE, TREATMENT, METRIC) |>
	filter(Metric %in% c("N", "K", "P")) |>
	summarize(Mean = mean(VALUE),
						sd = sd(VALUE),
						n = n(),
						se = sd/sqrt(n))

treat <- ggplot(Treatment.date |> filter(DATE != "Reference Reference"), 
			 aes(x = METRIC, y = Mean, color = DATE))+
	geom_point(position=position_dodge(width=0.5), size = 1)+
	geom_linerange(aes(ymin = Mean-se, ymax = Mean+se),
								 position=position_dodge(width=0.5))+
	facet_wrap(~TREATMENT)

ref <- ggplot(Treatment.date |> filter(DATE == "Reference Reference"), 
			 aes(x = METRIC, y = Mean, color = DATE))+
		geom_point(position=position_dodge(width=0.5), size = 1)+
		geom_linerange(aes(ymin = Mean-se, ymax = Mean+se),
								 position=position_dodge(width=0.5))+
	scale_y_continuous(limits = c(0,2))

library(patchwork)
treat+ref

# Single carcass open calculations at 1 year since deployment
# K = 1.17075
# P = 0.12100

# Single carcass herbivore
# K = 1.58950
# P = 0.21225

# Single carcass scavenger
# K = 1.54100
# P = 0.21825

1.17075/1.58950 # 0.7365524 K
1.17075/1.54100 # 0.7597339

0.12100/0.21825 # 0.5544101
0.12100/0.21225 # 0.5700824

# MME open calculations at 1 year since deployment
# N = 2.11925

# Single carcass herbivore
# N = 1.79150

# Single carcass scavenger
# N = 1.76250

2.11925/1.79150
2.11925/1.76250

plant.lg |>
	filter(METRIC %in% c("N", "P", "K")) |>
	filter(REFERENCE == 'No') |>
	group_by(DATE, METRIC) |>
		summarize(Mean = mean(VALUE),
						sd = sd(VALUE),
						n = n(),
						se = sd/sqrt(n))
