## --------------- HEADER ------------------------------------------------------
## Script name: 1c_Dormancy-class-EDA.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliation: University of Florida
## Date Created: 2022-05-01
## Date Last modified: 2024-10-19
## Copyright (c) David S. Mason, 2022
## Contact: masond@ufl.edu, @EcoGraffito
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
dormancy.ext <- read.csv("Clean-data/Plants/Dormancy-class-extinction.csv")

## --------------- COUNT BINARY DATA -------------------------------------------

table(dormancy.col$Colonized.end)
table(dormancy.col$Colonized.ever)

table(dormancy.ext$Extirpated.end)
table(dormancy.ext$Extirpated.ever)

plot_str(dormancy.col)

## --------------- VISUALIZE COLONIZATION AND EXTINCTION BY TREATMENT ----------

# Colonized ever
ggplot(d = dormancy.col, aes(x = Colonized.ever, color = DormancyClass)) +
  geom_histogram() +
  facet_wrap(~ DormancyClass * Treatment, nrow = 3)

# Colonized by the end of sampling
ggplot(d = dormancy.col, aes(x = Colonized.end, color = DormancyClass)) +
	geom_histogram() +
	facet_wrap(~ DormancyClass * Treatment, nrow = 3)

# Extirpated ever
ggplot(d = dormancy.ext, aes(x = Extirpated.ever, color = DormancyClass)) +
  geom_histogram() +
  facet_wrap(~ DormancyClass * Treatment, nrow = 2)

# Extirpated by the end of sampling
ggplot(d = dormancy.ext, aes(x = Extirpated.end, color = DormancyClass)) +
  geom_histogram() +
  facet_wrap(~ DormancyClass * Treatment, nrow = 2)

## --------------- VISUALIZE COLONIZATION AND EXTINCTION BY BIOMASS ------------

# Colonized ever
ggplot(d = dormancy.col, aes(x = Colonized.ever, color = DormancyClass)) +
	geom_histogram() +
	facet_wrap(~ DormancyClass * Carrion, nrow = 3)

# Colonized by the end of sampling
ggplot(d = dormancy.col, aes(x = Colonized.end, color = DormancyClass)) +
	geom_histogram() +
	facet_wrap(~ DormancyClass * Carrion, nrow = 3)

# Extirpated ever
ggplot(d = dormancy.ext, aes(x = Extirpated.ever, color = DormancyClass)) +
	geom_histogram() +
	facet_wrap(~ DormancyClass * Carrion, nrow = 2)

# Extirpated by the end of sampling
ggplot(d = dormancy.ext, aes(x = Extirpated.end, color = DormancyClass)) +
	geom_histogram() +
	facet_wrap(~ DormancyClass * Carrion, nrow = 2)

## --------------- VISUALIZE COLONIZATION AND EXTINCTION BY EXCLUSION ----------

# Colonized ever
ggplot(d = dormancy.col, aes(x = Colonized.ever, color = DormancyClass)) +
	geom_histogram() +
	facet_wrap(~ DormancyClass * Exclusion, nrow = 3)

# Colonized by the end of sampling
ggplot(d = dormancy.col, aes(x = Colonized.end, color = DormancyClass)) +
	geom_histogram() +
	facet_wrap(~ DormancyClass * Exclusion, nrow = 3)

# Extirpated ever
ggplot(d = dormancy.ext, aes(x = Extirpated.ever, color = DormancyClass)) +
	geom_histogram() +
	facet_wrap(~ DormancyClass * Exclusion, nrow = 2)

# Extirpated by the end of sampling
ggplot(d = dormancy.ext, aes(x = Extirpated.end, color = DormancyClass)) +
	geom_histogram() +
	facet_wrap(~ DormancyClass * Exclusion, nrow = 2)

## --------------- CHECK THAT THERE ARE NO DUPLICATES --------------------------

dormancy.col |>
	dplyr::select(Site, Treatment, Carrion, Exclusion) |>
	unique()

dormancy.col |>
	dplyr::select(Site, Treatment, Carrion, Exclusion, Transect) |>
	unique() |>
	nrow()
4*6*4

check <- dormancy.col |>
	dplyr::select(Site, Treatment, Carrion, Exclusion, Transect, DormancyClass) |>
	group_by(Site, Treatment, Carrion, Exclusion, Transect, DormancyClass) |>
	summarize(Count = n())

dormancy.ext |>
	dplyr::select(Site, Treatment, Carrion, Exclusion) |>
	unique()

dormancy.ext |>
	dplyr::select(Site, Treatment, Carrion, Exclusion, Transect) |>
	unique() |>

## --------------- CHECK THAT THE ROW VALUES MAKE SENSE ------------------------

dormancy.col <- mutate(dormancy.col, Check = rowSums(across(10:16)))
# dormancy.col looks good

dormancy.ext <- mutate(dormancy.ext, Check = rowSums(across(11:16)))
# dormancy.ext looks good