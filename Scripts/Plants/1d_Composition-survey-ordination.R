## --------------- HEADER ------------------------------------------------------
## Script name: 1d_Composition-survey-ordination.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliation: University of Florida
## Date Created: 2022-05-03
## Date Last modified: 2022-05-03
## Copyright (c) David S. Mason, 2022
## Contact: masond@ufl.edu, @EcoGraffito
## Purpose of script: This is a script for creating an ordination based on the
## community composition data.

## --------------- SET—UP WORKSPACE --------------------------------------------
library(tidyverse)
library(lubridate)
library(vegan)

rm(list=ls())

# Bring in the combined plant survey data
surv <- read.csv("Animals-plants-seeds/Clean-data/Plants/Community-matrix-lg.csv")

## --------------- CALCULATE DORMANCY CLASS GRADIENT ---------------------------

# Summarize by dormancy class
dorm.sum <- surv %>%
  group_by(Rounded.date, Site, Treatment, Carrion, Exclusion, DormancyClass) %>%
  summarize(Cover = sum(Cover))

dorm.sum <- dorm.sum %>%
  filter(DormancyClass != "PD") %>%
  drop_na(DormancyClass)

# Pivot wider
dorm.sum <- dorm.sum %>%
  pivot_wider(names_from = DormancyClass, values_from = Cover)

# Calculate the gradient representing our rare DormancyClasses
dorm.sum <- dorm.sum %>%
  mutate(Gradient = -ND + PY)

# Calculate z score of the gradient
m <- mean(dorm.sum$Gradient)
a <- sd(dorm.sum$Gradient)
dorm.sum <- dorm.sum %>%
  mutate(Gradient.scaled = (Gradient - m) / a)

# Filter for before after
dorm.sum <- dorm.sum %>%
  filter(Rounded.date == "2019-07-21" | Rounded.date == "2021-07-18") %>%
  filter(Treatment != "REF")

## --------------- PREPARE DATA FOR ORDINATION  --------------------------------

# Drop reference and genus.specis
surv <- surv %>%
  filter(Treatment != "REF")

# Create a vector of the dormancy class
Dormancy <- surv %>%
  dplyr::select(Genus.species, DormancyClass) %>%
  unique()

# Put the data in wide format
surv <- surv %>%
  dplyr::select(-DormancyClass) %>%
  pivot_wider(names_from = Genus.species, values_from = Cover)

# Parse the species matrix into the environmental (explanatory) components
# and the species matrix
spec <- surv[, 6:87]
env <- surv[, 1:5]

## --------------- CREATE ORDINATION DATAFRAMES --------------------------------

# Create ordination
ord <- metaMDS(spec, trymax = 2000)

# Using the scores function from vegan to extract
# the site scores and convert to a data.frame
site.scores <- as.data.frame(vegan::scores(ord, "sites"))

# create a column of site names, from the rownames of data.scores
site.scores <- cbind(env, site.scores)

# Using the scores function from vegan to extract
# the species scores and convert to a data.frame
species.scores <- as.data.frame(vegan::scores(ord, "species"))

# create a column of species, from the rownames of species.scores
species.scores$Species <- rownames(species.scores)

## --------------- ADD DORMANCY CLASS TO DATAFRAME -----------------------------

# Rename the species column in the species.score dataframe so that it matches
# the species label for dormancy class
names(species.scores)[3] <- "Genus.species"

# Merge the dormancy class with the species score dataframe
species.scores <- merge(species.scores, Dormancy, all = FALSE)

# Convert NA dormancyclass to unknown
levels(species.scores$DormancyClass) <- c(
  levels(species.scores$DormancyClass),
  "Unknown"
)
for (i in 1:nrow(species.scores)) {
  if (is.na(species.scores$DormancyClass[i])) {
    species.scores$DormancyClass[i] <- "Unknown"
  }
}

# Drop NAs / unknowns
species.scores.filt <- species.scores %>%
  filter(DormancyClass != "Unknown")

## --------------- VISUALIZE ORDINATION ----------------------------------------

# Filter out july year 1 and july year 2
site.scores.filt <- site.scores %>%
  filter(Rounded.date == "2019-07-21" | Rounded.date == "2021-07-18")

# Add the gradient score to the site.scores
site.scores.filt <- merge(site.scores.filt, dorm.sum)

# Create a dataframe for the background site scores
site.scores.bg <- site.scores.filt %>%
  dplyr::select(NMDS1, NMDS2, Site)
colnames(site.scores.bg)[3] <- "Site.bg"


p <- ggplot() +
  # Create a layer of points with no variables that will be faceted later
  geom_point(
    data = site.scores.bg, aes(x = NMDS1, y = NMDS2, shape = Site.bg),
    fill = "grey", color = "grey", alpha = 0.7, size = 4
  ) +
  # Create the "visible" layer of points
  geom_point(data = site.scores.filt, aes(
    x = NMDS1, y = NMDS2,
    shape = Site, color = Rounded.date, stroke = 1,
    fill = Gradient.scaled
  ), size = 4) +
  # Set the color for the outline of the shapes
  scale_color_manual(values = c("gray55", "black")) +
  # Set the shape
  scale_shape_manual(values = c(21, 22, 23, 24)) +
  # Set the fill gradient
  scale_fill_gradientn(
    colors = rainbow(5),
    name = "Dormancy class"
  ) +
  # Creae the ellipse
  stat_ellipse(data = site.scores.filt, aes(
    x = NMDS1, y = NMDS2,
    color = Rounded.date
  ), level = 0.95) +
  theme_bw() +
  facet_wrap(~Treatment)

dev.new()
p
