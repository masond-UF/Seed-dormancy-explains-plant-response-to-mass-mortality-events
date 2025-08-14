## --------------- HEADER ------------------------------------------------------
## Script name: 3a_Plant-nutrients-clean.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliation: University of Florida
## Date Created: 2022-6-26
## Date Last Modified: 2025-8-13
## Copyright (c) David S. Mason, 2025
## Contact: masond@ufl.edu, @EcoGraffito
## Purpose of script: This script calculates effect sizes for the plant 
## nutrient data.

## --------------- SET—UP WORKSPACE --------------------------------------------
library(tidyverse)
library(tidylog)
library(lubridate)
library(styler)

# Clear the deck
rm(list = ls())

# Bring in the data
plant <- read.csv("Raw-data/Plants/Plant-tissue-nutrients.csv")

# Fix the date
plant$DATE <- mdy(plant$DATE)

## --------------- ARRANGE DATA WITH WIDE REFERENCE ----------------------------

# DF
df.1 <- plant %>% filter(SITE == "DF" & BIOMASS != "Reference" & DATE == "2020-08-01")
df.1.ref <- plant %>% filter(SITE == "DF" & BIOMASS == "Reference" & DATE == "2020-08-01")
df.1.ref <- df.1.ref[,5:15]
df.1 <- cbind(df.1, df.1.ref)

df.2 <- plant %>% filter(SITE == "DF" & BIOMASS != "Reference" & DATE == "2021-05-04")
df.2.ref <- plant %>% filter(SITE == "DF" & BIOMASS == "Reference" & DATE == "2021-05-04")
df.2.ref <- df.2.ref[,5:15]
df.2 <- cbind(df.2, df.2.ref)

# OS
os.1 <- plant %>% filter(SITE == "OS" & BIOMASS != "Reference" & DATE == "2020-08-01")
os.1.ref <- plant %>% filter(SITE == "OS" & BIOMASS == "Reference" & DATE == "2020-08-01")
os.1.ref <- os.1.ref[,5:15]
os.1 <- cbind(os.1, os.1.ref)

os.2 <- plant %>% filter(SITE == "OS" & BIOMASS != "Reference" & DATE == "2021-05-04")
os.2.ref <- plant %>% filter(SITE == "OS" & BIOMASS == "Reference" & DATE == "2021-05-04")
os.2.ref <- os.2.ref[,5:15]
os.2 <- cbind(os.2, os.2.ref)

# WP
wp.1 <- plant %>% filter(SITE == "WP" & BIOMASS != "Reference" & DATE == "2020-08-01")
wp.1.ref <- plant %>% filter(SITE == "WP" & BIOMASS == "Reference" & DATE == "2020-08-01")
wp.1.ref <- wp.1.ref[,5:15]
wp.1 <- cbind(wp.1, wp.1.ref)

wp.2 <- plant %>% filter(SITE == "WP" & BIOMASS != "Reference" & DATE == "2021-05-04")
wp.2.ref <- plant %>% filter(SITE == "WP" & BIOMASS == "Reference" & DATE == "2021-05-04")
wp.2.ref <- wp.2.ref[,5:15]
wp.2 <- cbind(wp.2, wp.2.ref)

# GG
gg.1 <- plant %>% filter(SITE == "GG" & BIOMASS != "Reference" & DATE == "2020-08-01")
gg.1.ref <- plant %>% filter(SITE == "GG" & BIOMASS == "Reference" & DATE == "2020-08-01")
gg.1.ref <- gg.1.ref[,5:15]
gg.1 <- cbind(gg.1, gg.1.ref)

gg.2 <- plant %>% filter(SITE == "GG" & BIOMASS != "Reference" & DATE == "2021-05-04")
gg.2.ref <- plant %>% filter(SITE == "GG" & BIOMASS == "Reference" & DATE == "2021-05-04")
gg.2.ref <- gg.2.ref[,5:15]
gg.2 <- cbind(gg.2, gg.2.ref)

plant.merge <- rbind(df.1, df.2, os.1, os.2, wp.1, wp.2, gg.1, gg.2)

## --------------- CALCULATE EFFECT SIZES --------------------------------------

# Calculate log odds ratio for each DATE compared to reference
effect <- data.frame(
	log(plant.merge[5] / plant.merge[16]),
	log(plant.merge[6] / plant.merge[17]),
	log(plant.merge[7] / plant.merge[18]),
	log(plant.merge[8] / plant.merge[19]),
	log(plant.merge[9] / plant.merge[20]),
	log(plant.merge[10] / plant.merge[21]),
	log(plant.merge[11] / plant.merge[22]),
	log(plant.merge[12] / plant.merge[23]),
	log(plant.merge[13] / plant.merge[24]),
	log(plant.merge[14] / plant.merge[25]),
	log(plant.merge[15] / plant.merge[26]))

# Grab the predictors from the original dataframe
pred <- plant.merge %>% 
	select(DATE, SITE, BIOMASS, EXCLUSION)

# Bring everything together
merged <- cbind(pred, effect)

# Convert long
plants.lg <- merged %>% 
	pivot_longer(cols = 5:15, names_to = "VARIABLE", values_to = "EFFECT.SIZE")

plants.lg <- round(plants.lg$EFFECT.SIZE, 2)

## --------------- SAVE DATAFRAME TO CSV ---------------------------------------

colnames(plants.lg)[4] <- "Exclusion"
colnames(plants.lg) <- toupper(colnames(plants.lg))

write.csv(plants.lg,
					"Clean-data/Plants/Plant-nutrients-effect-size.csv",
					row.names = FALSE)
