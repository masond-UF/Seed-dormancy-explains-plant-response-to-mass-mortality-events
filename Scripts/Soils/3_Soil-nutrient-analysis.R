## --------------- HEADER ------------------------------------------------------
## Script name: 3_Soil-nutrients-analysis.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliaton: University of Florida
## Date Created: 2022-6-25
## Date Last Modified: 2022-6-25
## Copyright (c) David S. Mason, 2022
## Contact: masond@ufl.edu, @EcoGraffito
## Purpose of script: This script creates soil nutrient figures

rm(list=ls())

soil <- read.csv("Animals-plants-seeds/Raw-data/Soils/Soil-nutrients.csv")

pred <- soil[,1:5]
resp <- soil[,c(7,8,14)]
resp.hell <- decostand(resp, method = "hell")

## --------------- ADONIS ------------------------------------------------------

# Set the permutation to account for blocking design
perm <- how(nperm = 199)
setBlocks(perm) <- with(pred, Site)

# Run the model
adonis2(resp.hell ~ Treatment*Date,
				data = pred,
				permutations = perm
)


