## --------------- HEADER ------------------------------------------------------
## Script name: 4c_Plant-nutrients-analysis.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliaton: University of Florida
## Date Created: 2022-7-10
## Date Last Modified: 2022-7-10
## Copyright (c) David S. Mason, 2022
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
plant <- read.csv("Animals-plants-seeds/Raw-data/Plants/Plant-tissue-nutrients.csv")

# Fix the date
plant$Date <- mdy(plant$Date)

pred <- plant[,1:4]
resp <- plant[,c(5,7,9)]
resp.hell <- decostand(resp, method = "hell")

pred$Treatment <- paste(pred$Biomass, pred$Fencing)
## --------------- ADONIS ------------------------------------------------------

# Set the permutation to account for blocking design
perm <- how(nperm = 199)
setBlocks(perm) <- with(pred, Site)

# Run the model
adonis2(resp.hell ~ Treatment*Date,
				data = pred,
				permutations = perm
)

## --------------- MEANS -------------------------------------------------------

plant$Reference <- NA

for(i in 1:nrow(plant)){
	if(plant$Biomass[i] == 'Reference'){
		plant$Reference[i] <- 'Yes'
	} else {
		plant$Reference[i] <- 'No'
	}
}

plant.lg <- plant |>
	pivot_longer(cols = 5:15, names_to = "Metric", values_to = "Value")

treat.vs.ref <- plant.lg |>
	group_by(Reference, Metric) |>
	summarize(Mean = mean(Value),
						sd = sd(Value),
						n = n(),
						se = sd/sqrt(n))

treat.vs.ref.date <- plant.lg |>
	group_by(Reference, Date, Metric) |>
	summarize(Mean = mean(Value),
						sd = sd(Value),
						n = n(),
						se = sd/sqrt(n))

biomass <- plant.lg |>
	group_by(Biomass, Metric) |>
	filter(Metric %in% c("N", "K", "P")) |>
	summarize(Mean = mean(Value),
						sd = sd(Value),
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
	group_by(Biomass, Fencing, Metric) |>
	filter(Metric %in% c("N", "K", "P")) |>
	summarize(Mean = mean(Value),
						sd = sd(Value),
						n = n(),
						se = sd/sqrt(n))

biomass.fencing.date <- plant.lg |>
	group_by(Date, Biomass, Fencing, Metric) |>
	filter(Metric %in% c("N", "K", "P")) |>
	summarize(Mean = mean(Value),
						sd = sd(Value),
						n = n(),
						se = sd/sqrt(n))

ggplot(biomass.fencing.date |> filter(Fencing != "Reference"), 
			 aes(x = Biomass, y = Mean, color = Date))+
	geom_point(position=position_dodge(width=0.5), size = 1)+
	geom_linerange(aes(ymin = Mean-se, ymax = Mean+se),
								 position=position_dodge(width=0.5))+
	facet_wrap(~Metric*Fencing)

plant.lg$Treatment <- paste(plant.lg$Biomass, plant.lg$Fencing)

Treatment.date <- plant.lg |>
	group_by(Date, Treatment, Metric) |>
	filter(Metric %in% c("N", "K", "P")) |>
	summarize(Mean = mean(Value),
						sd = sd(Value),
						n = n(),
						se = sd/sqrt(n))

treat <- ggplot(Treatment.date |> filter(Treatment != "Reference Reference"), 
			 aes(x = Metric, y = Mean, color = Date))+
	geom_point(position=position_dodge(width=0.5), size = 1)+
	geom_linerange(aes(ymin = Mean-se, ymax = Mean+se),
								 position=position_dodge(width=0.5))+
	facet_wrap(~Treatment)

ref <- ggplot(Treatment.date |> filter(Treatment == "Reference Reference"), 
			 aes(x = Metric, y = Mean, color = Date))+
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
	filter(Metric %in% c("N", "P", "K")) |>
	filter(Reference == 'No') |>
	group_by(Date, Metric) |>
	summarize(Mean = mean(Value),
						Std.dev = sd(Value),
						n = n(),
						se = Std.dev/(sqrt(n)))
