## --------------- HEADER ------------------------------------------------------
## Script name: 1f_Composition-survey-permanova.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliaton: University of Florida
## Date Created: 2022-05-04
## Date Last modified: 2022-05-04
## Copyright (c) David S. Mason, 2022
## Contact: masond@ufl.edu, @EcoGraffito
## Purpose of script: This is a script for creating an ordination based on the
## community composition data.

## --------------- SET—UP WORKSPACE --------------------------------------------

rm(list=ls())

library(tidyverse)
library(lubridate)
library(vegan)
library(fitdistrplus)

# Bring in the combined plant survey data
surv <- read.csv("Animals-plants-seeds/Clean-data/Plants/Community-matrix-lg.csv")

## --------------- TRANSFORM DATA  ---------------------------------------------

surv <- surv %>% 
	dplyr::select(-DormancyClass) %>% 
	pivot_wider(names_from = Genus.species, values_from = Cover)

# Parse the species matrix into the environmental (explanatory) components
# and the species matrix
spec <- surv[, 6:87]
env <- surv[, 1:5]

# Applying the hellinger transformation
spec.hell <- decostand(spec, "hell")

## --------------- DISTANCE MATRIX ---------------------------------------------

# Calculate hellinger distance (euclidean distance on hellinger transformation)
spec.hell.dist <- dist(spec.hell)

## --------------- ADONIS ------------------------------------------------------

# Set the permutation to account for blocking design
perm <- how(nperm = 199)
setBlocks(perm) <- with(env, Site)

# Run the model
adonis2(spec.hell.dist ~ Rounded.date * Treatment,
				data = env,
				permutations = perm
)

