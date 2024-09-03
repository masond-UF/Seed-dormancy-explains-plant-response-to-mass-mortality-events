## --------------- HEADER ------------------------------------------------------
## Script name: 2_Soil-nutrients-EDA.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliaton: University of Florida
## Date Created: 2022-6-25
## Date Last Modified: 2022-6-25
## Copyright (c) David S. Mason, 2022
## Contact: masond@ufl.edu, @EcoGraffito
## Purpose of script: This script explores the nutrient data

## --------------- SET—UP WORKSPACE --------------------------------------------
library(tidyverse)
library(tidylog)
library(lubridate)
library(styler)

rm(list = ls())

soil <- read.csv("Animals-plants-seeds/Clean-data/Soils/Soil-nutrients-effect-size.csv")

## --------------- DATA EXPLORATION --------------------------------------------

library(DataExplorer)

plot_str(soil)
plot_missing(soil) # nothing missing
plot_histogram(soil)

ggplot(soil, aes(x = Variable, y = Effect.size, color = Measurement))+
	geom_point()+
	facet_wrap(~Biomass*Fencing)

## --------------- DISTRIBUTIONS -----------------------------------------------

library(fitdistrplus)
descdist(soil$Effect.size, discrete = FALSE)
