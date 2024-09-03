## --------------- HEADER ------------------------------------------------------
## Script name: 1c_Traps-composition-permanova.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliaton: University of Florida
## Date Created: 2021-12-15
## Copyright (c) David S. Mason, 2021
## Contact: masond@ufl.edu, @EcoGraffito
## Purpose of script: This is a script for conducting community analysis on the 
## seed trap data.

## --------------- SET—UP WORKSPACE --------------------------------------------

# Clear the decks
rm(list=ls())

library(tidyverse)
library(tidylog)
library(lubridate)
library(styler)
library(mvabund)
library(vegan)

# Bring in the data
traps.lg <- read.csv("Animals-plants-seeds/Clean-data/Seeds/Traps-community-matrix-lg.csv")

# Pivot the data wider
traps.wd <- traps.lg %>% 
	dplyr::select(-DormancyClass, -Dispersal) %>% 
	pivot_wider(names_from = Genus.species, values_from = Seeds)
	
# Separate the data into species and predictors
spec <- as.matrix(traps.wd[,5:36])
pred <- traps.wd[,1:4]

## --------------- ROW STANDARDIZE ----------------------------------------------

spec.std <- decostand(spec, method="total")


## --------------- ADONIS ------------------------------------------------------

# Round dates 
pred$DATE <- mdy(pred$DATE)

pred <- pred %>% 
	mutate(Rounded.date = round_date(pred$DATE, unit = "week"))

# Set the permutation to account for blocking design
perm <- how(nperm = 199)
setBlocks(perm) <- with(pred, SITE)

# Calculate hellinger distance (euclidean distance on hellinger transformation)
spec.std.dist <- dist(spec.std)

# Run the model
adonis2(spec.std.dist ~ TREATMENT * Rounded.date,
				data = pred,
				permutations = perm
)

# Control differs from MME
# Time is a significant factor
