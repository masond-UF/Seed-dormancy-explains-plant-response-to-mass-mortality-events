## --------------- HEADER ------------------------------------------------------
## Script name: 2c_Dormancy-class-EDA.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliaton: University of Florida
## Date Created: 2022-05-01
## Date Last modified: 2022-05-01
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

dormancy.col <- read.csv("Animals-plants-seeds/Clean-data/Plants/Dormancy-class-colonization.csv")
dormancy.ext <- read.csv("Animals-plants-seeds/Clean-data/Plants/Dormancy-class-extinction.csv")

# Sampling effects. Higher probability of adding a PD species
# Correct for abundance?
# Only PD species around during the initial sample. Use the first date or what?
# Confounding season with detection of PY and ND

## --------------- COUNT BINARY DATA -------------------------------------------

table(dormancy.col$Colonized.end)
table(dormancy.col$Colonized.ever)

table(dormancy.ext$Extirpated.end)
table(dormancy.ext$Extirpated.ever)

plot_str(dormancy.col)

## --------------- SUMMARY TABLE -----------------------------------------------

sumtable(dormancy.col)
sumtable(dormancy.ext)

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

