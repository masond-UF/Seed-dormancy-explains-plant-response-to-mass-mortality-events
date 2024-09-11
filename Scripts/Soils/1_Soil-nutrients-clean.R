## --------------- HEADER ------------------------------------------------------
## Script name: 1_Soil-nutrients-clean.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliation: University of Florida
## Date Created: 2022-6-25
## Date Last Modified: 2024-09-11
## Copyright (c) David S. Mason, 2022
## Contact: masond@ufl.edu, @EcoGraffito
## Purpose of script: This script prepares the soil nutrient data for analysis
## and figures.

## --------------- SET—UP WORKSPACE --------------------------------------------
library(tidyverse)
library(tidylog)
library(lubridate)
library(styler)

# Clear the deck
rm(list = ls())

# Bring in the data
soil <- read.csv("Raw-data/Soils/Soil-nutrients.csv")

# Convert to year
soil$Date <- mdy(soil$Date)

## --------------- ARRANGE DATA IN WIDE TIME-SERIES ----------------------------

# Filter data by event
soil.pre <- soil %>% filter(Date == "2019-04-12")
soil.post1 <- soil %>%
  filter(Date == "2020-05-19") %>%
  dplyr::select(!Date:Fencing)
soil.post2 <- soil %>%
  filter(Date == "2020-07-21") %>%
  dplyr::select(!Date:Fencing)
soil.post3 <- soil %>%
  filter(Date == "2021-05-05") %>%
  dplyr::select(!Date:Fencing)

# Bring together the data in wide time-series format
soil.merged <- cbind(soil.pre, soil.post1, soil.post2, soil.post3)

## --------------- CALCULATE EFFECT SIZES --------------------------------------

# Calculate log odds ratio for each time series
effect <- data.frame(
  log(soil.merged[23] / soil.merged[6]),
  log(soil.merged[40] / soil.merged[6]),
  log(soil.merged[57] / soil.merged[6]),
  log(soil.merged[24] / soil.merged[7]),
  log(soil.merged[41] / soil.merged[7]),
  log(soil.merged[58] / soil.merged[7]),
  log(soil.merged[25] / soil.merged[8]),
  log(soil.merged[42] / soil.merged[8]),
  log(soil.merged[59] / soil.merged[8]),
  log(soil.merged[30] / soil.merged[13]),
  log(soil.merged[47] / soil.merged[13]),
  log(soil.merged[64] / soil.merged[13]),
  log(soil.merged[31] / soil.merged[14]),
  log(soil.merged[48] / soil.merged[14]),
  log(soil.merged[65] / soil.merged[14])
)

## --------------- CONVERT TO LONG FORMAT AND ADD LABELS -----------------------

# Add names for the variables
names(effect) <- c(
  "OM.Yr1", "OM.Yr2a", "OM.Yr3",
  "P.Yr1", "P.Yr2a", "P.Yr3",
  "K.Yr1", "K.Yr2a", "K.Yr3",
  "pH.Yr1", "pH.Yr2a", "pH.Yr3",
  "N.Yr1", "N.Yr2a", "N.Yr3"
)

# Grab the predictors from the original dataframe
pred <- soil.pre %>% dplyr::select(Site, Treatment, Biomass, Fencing)

# Bring everything together
merged <- cbind(pred, effect)

# Convert long
soil.lg <- merged %>% 
	pivot_longer(cols = 5:19, names_to = "Variable.Measurement", values_to = "Effect.size")

# Add Measurement
measurement <- data.frame("Measurement" = rep(seq(from = 1, to = 3, by = 1), 140))
soil.lg <- cbind(soil.lg, measurement)

# Add Variable
soil.lg$Variable <- NA
for(i in 1:nrow(soil.lg)){
	if(isTRUE(soil.lg$Variable.Measurement[i] == "OM.Yr1" |
		 soil.lg$Variable.Measurement[i] == "OM.Yr2a" |
		 soil.lg$Variable.Measurement[i] == "OM.Yr3")) 
		{
		soil.lg$Variable[i] <- "Organic matter"
	} 
	if(isTRUE(soil.lg$Variable.Measurement[i] == "pH.Yr1" |
						soil.lg$Variable.Measurement[i] == "pH.Yr2a" |
						soil.lg$Variable.Measurement[i] == "pH.Yr3")) 
	{
		soil.lg$Variable[i] <- "Soil pH"
	}
	if(isTRUE(soil.lg$Variable.Measurement[i] == "P.Yr1" |
						soil.lg$Variable.Measurement[i] == "P.Yr2a" |
						soil.lg$Variable.Measurement[i] == "P.Yr3")) 
	{
		soil.lg$Variable[i] <- "Phosphorous"
	}
	if(isTRUE(soil.lg$Variable.Measurement[i] == "K.Yr1" |
						soil.lg$Variable.Measurement[i] == "K.Yr2a" |
						soil.lg$Variable.Measurement[i] == "K.Yr3")) 
	{
		soil.lg$Variable[i] <- "Potassium"
	}
	if(isTRUE(soil.lg$Variable.Measurement[i] == "N.Yr1" |
						soil.lg$Variable.Measurement[i] == "N.Yr2a" |
						soil.lg$Variable.Measurement[i] == "N.Yr3")) 
	{
		soil.lg$Variable[i] <- "Percent Nitrogen"
	}
} 

# Add date
soil.lg$Date <- NA
for(i in 1:nrow(soil.lg)){
	if(isTRUE(soil.lg$Measurement[i] == 1)){
		soil.lg$Date[i] <- "2020-05-19"
	}
	if(isTRUE(soil.lg$Measurement[i] == 2)){
		soil.lg$Date[i] <- "2020-07-21"
	}
	if(isTRUE(soil.lg$Measurement[i] == 3)){
		soil.lg$Date[i] <- "2021-05-05"
	}
}

## --------------- SELECT AND REORDER ------------------------------------------

soil.lg <- soil.lg %>% 
	dplyr::select(Site, Treatment, Biomass, Fencing, Variable, Date, Measurement, Effect.size)

## --------------- SAVE DATAFRAME TO CSV ---------------------------------------
write.csv(soil.lg,
					"Clean-data/Soils/Soil-nutrients-effect-size.csv",
					row.names = FALSE)
