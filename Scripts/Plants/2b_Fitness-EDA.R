## --------------- HEADER ------------------------------------------------------
## Script name: 2b_Plant-fitness-EDA.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliation: University of Florida
## Date Last modified: 2025-08-14
## Copyright (c) David S. Mason, 2025
## Contact: masond@ufl.edu, @EcoGraffito
## Purpose of script: This script performs data exploratory analysis on the 
## plant fitness data.

## --------------- SET—UP WORKSPACE --------------------------------------------
library(tidyverse)
library(tidylog)
library(lubridate)
library(styler)

d <- read.csv("Clean-data/Plants/Plant-fitness.csv")

## --------------- HEIGHT BOXPLOT ----------------------------------------------
d %>% ggplot(aes(x = MH, y = HEIGHT, col = SPECIES)) +
  geom_boxplot()

## --------------- INFLOR BOXPLOT ----------------------------------------------
d %>% ggplot(aes(x = MH, y = INFLOR, col = SPECIES)) +
  geom_boxplot()

## --------------- BASIC EDA ---------------------------------------------------
library(DataExplorer)

plot_str(d)
# 4 sites
# 2 dates
# 9 treatments
# Is the treatment simulating herbivore MME (y/n)
# 2 species
# 2 integers (height, inflor)

# height was measured to mm for one species but only to cm for the other

plot_missing(d)
# missing infloresence data for one species

plot_histogram(d)
plot_density(d)

plot_bar(d)
# most of the samples are from WP

## --------------- DISTRIBUTION ------------------------------------------------
library(fitdistrplus)

descdist(d$HEIGHT, discrete = FALSE) # Beta
descdist(log(d$HEIGHT), discrete = FALSE) # Normal 
descdist(d$INFLOR, discrete = TRUE) # Poisson?
