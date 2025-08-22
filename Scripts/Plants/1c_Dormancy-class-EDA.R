## --------------- HEADER ------------------------------------------------------
## Script name: 1c_Dormancy-class-EDA.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliation: University of Florida
## Date Created: 2022-05-01
## Date Last modified: 2025-08-13
## Copyright (c) David S. Mason, 2025
## Contact: david.mason@jonesctr.org
## Purpose of script: This is a script for exploring the binary colonization
## extinction data for plants belonging to seed dormancy classes.

## --------------- SET—UP WORKSPACE --------------------------------------------

# Load the packages
library(tidyverse)
library(tidylog)
library(lubridate)
library(styler)
library(broom)
library(DataExplorer)
library(psych)
library(vtable)

# Clear the decks
rm(list=ls())

# Bring in the data
dormancy.col <- read.csv("Clean-data/Plants/Dormancy-class-colonization.csv")
dormancy.ext <- read.csv("Clean-data/Plants/Dormancy-class-extirpation.csv")

## --------------- COUNT BINARY DATA -------------------------------------------

table(dormancy.col$COLONIZED.END)
table(dormancy.ext$EXTIRPATED.EVER)

plot_str(dormancy.col)

## --------------- VISUALIZE COLONIZATION AND EXTINCTION BY TREATMENT ----------


# Colonized by the end of sampling
ggplot(d = dormancy.col, aes(x = COLONIZED.END, color = DORMANCY.CLASS)) +
	geom_histogram() +
	facet_wrap(~ DORMANCY.CLASS * TREATMENT, nrow = 3)

# Extirpated ever
ggplot(d = dormancy.ext, aes(x = EXTIRPATED.EVER, color = DORMANCY.CLASS)) +
  geom_histogram() +
  facet_wrap(~ DORMANCY.CLASS * TREATMENT, nrow = 2)

## --------------- VISUALIZE COLONIZATION AND EXTINCTION BY BIOMASS ------------

# Colonized by the end of sampling
ggplot(d = dormancy.col, aes(x = COLONIZED.END, color = DORMANCY.CLASS)) +
	geom_histogram() +
	facet_wrap(~ DORMANCY.CLASS * BIOMASS, nrow = 3)

# Extirpated ever
ggplot(d = dormancy.ext, aes(x = EXTIRPATED.EVER, color = DORMANCY.CLASS)) +
	geom_histogram() +
	facet_wrap(~ DORMANCY.CLASS * BIOMASS, nrow = 2)


## --------------- VISUALIZE COLONIZATION AND EXTINCTION BY EXCLUSION ----------

# Colonized by the end of sampling
ggplot(d = dormancy.col, aes(x = COLONIZED.END, color = DORMANCY.CLASS)) +
	geom_histogram() +
	facet_wrap(~ DORMANCY.CLASS * EXCLUSION, nrow = 3)

# Extirpated ever
ggplot(d = dormancy.ext, aes(x = EXTIRPATED.EVER, color = DORMANCY.CLASS)) +
	geom_histogram() +
	facet_wrap(~ DORMANCY.CLASS * EXCLUSION, nrow = 2)

## --------------- CHECK THAT THERE ARE NO DUPLICATES --------------------------

dormancy.col |>
	dplyr::select(SITE, TREATMENT, BIOMASS, EXCLUSION) |>
	unique()

dormancy.col |>
	dplyr::select(SITE, TREATMENT, BIOMASS, EXCLUSION, TRANSECT) |>
	unique() |>
	nrow()
4*6*4

check <- dormancy.col |>
	dplyr::select(SITE, TREATMENT, BIOMASS, EXCLUSION, TRANSECT, DORMANCY.CLASS) |>
	group_by(SITE, TREATMENT, BIOMASS, EXCLUSION, TRANSECT, DORMANCY.CLASS) |>
	summarize(Count = n())

dormancy.ext |>
	dplyr::select(SITE, TREATMENT, BIOMASS, EXCLUSION) |>
	unique()

dormancy.ext |>
	dplyr::select(SITE, TREATMENT, BIOMASS, EXCLUSION, TRANSECT) |>
	unique() |>

## --------------- CHECK THAT THE ROW VALUES MAKE SENSE ------------------------

dormancy.col <- mutate(dormancy.col, Check = rowSums(across(10:16)))
# dormancy.col looks good

dormancy.ext <- mutate(dormancy.ext, Check = rowSums(across(11:16)))
# dormancy.ext looks good