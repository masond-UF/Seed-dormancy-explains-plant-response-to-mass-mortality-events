## --------------- HEADER ------------------------------------------------------
## Script name: 2e_Dormancy-class-colonization-extinction-figures.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliaton: University of Florida
## Date Created: 2021-05-02
## Date Last modified: 2022-05-02
## Copyright (c) David S. Mason, 2021
## Contact: masond@ufl.edu, @EcoGraffito
## Purpose of script: This script will generate a scatter and line plot showing
## changes in the relative abundance (compared to the reference) in plants
## grouped by seed dormancy functional group in each treatment as a time series

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
rm(list = ls())

# Convert scientific notation
options(scipen = 999)

# Bring in the data
col.means <- read.csv("Animals-plants-seeds/Analysis/Plants/Colonization-means.csv")[1:18,1:6]
ext.means <- read.csv("Animals-plants-seeds/Analysis/Plants/Extinction-means.csv")[1:6,1:6]

## --------------- PREP COLONIZATION ------------------------------------------------

# Initialize column for MME identity / functional role loss
col.means["Functional"] <- NA

for (i in 1:nrow(col.means)) {
  if (col.means$Treatment[i] == "CH" | col.means$Treatment[i] == "MH") {
    col.means$Functional[i] <- "Herbivore exclusion"
  }
  if (col.means$Treatment[i] == "CO" | col.means$Treatment[i] == "MO") {
    col.means$Functional[i] <- "No exclusion"
  }
  if (col.means$Treatment[i] == "CS" | col.means$Treatment[i] == "MS") {
    col.means$Functional[i] <- "Scavenger exclusion"
  }
}

# Initialize column for carrion biomass
col.means["Mortality"] <- NA

for (i in 1:nrow(col.means)) {
  if (col.means$Treatment[i] == "CH" | col.means$Treatment[i] == "CO" | col.means$Treatment[i] == "CS") {
    col.means$Mortality[i] <- "Single carrion"
  }
  if (col.means$Treatment[i] == "MH" | col.means$Treatment[i] == "MO" | col.means$Treatment[i] == "MS") {
    col.means$Mortality[i] <- "Mass mortality"
  }
}

DormancyClass <- c(
  `ND` = "No dormancy",
  `PD` = "Physiological",
  `PY` = "Physical"
)

## --------------- ND COLONIZATION ------------------------------------------------

ND <- col.means %>%
  filter(DormancyClass == "ND")

# Initialize column for Nudge factor (to separate points)
ND["Nudge"] <- NA

for (i in 1:nrow(ND)) {
  if (ND$Mortality[i] == "Mass mortality") {
    ND$Nudge[i] <- 0.2
  } else {
    ND$Nudge[i] <- -0.2
  }
}

# Reorder factors
ND$Functional <- factor(ND$Functional,
  levels = c(
    "Herbivore exclusion",
    "Scavenger exclusion",
    "No exclusion"
  )
)

ND$prob <- as.numeric(ND$prob)
ND$SE <- as.numeric(ND$SE)

ND.p <- ggplot(ND, aes(x = prob, y = Functional)) +
  geom_errorbar(aes(xmin = prob - SE, xmax = prob + SE),
    size = 0.5, width = 0.25, position = position_nudge(0, ND$Nudge)
  ) +
  geom_point(aes(fill = Mortality),
    color = "black",
    pch = 21, size = 4, position = position_nudge(0, ND$Nudge)
  ) +
  scale_fill_manual(values = c("black", "white")) +
  coord_fixed(ratio = 0.05) +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    text = element_text(size = 22),
    axis.title = element_text(face = "bold"),
    legend.position = "none",
    legend.title = element_blank()
  ) +
  theme(axis.ticks.y = element_blank()) +
  scale_x_continuous(limits = c(0, 0.5)) +
  xlab("") +
  ylab("")
## --------------- PD COLONIZATION ------------------------------------------------

PD <- col.means %>%
  filter(DormancyClass == "PD")

# Initialize column for Nudge factor (to separate points)
PD["Nudge"] <- NA

for (i in 1:nrow(PD)) {
  if (PD$Mortality[i] == "Mass mortality") {
    PD$Nudge[i] <- 0.2
  } else {
    PD$Nudge[i] <- -0.2
  }
}

# Reorder factors
PD$Functional <- factor(PD$Functional,
  levels = c(
    "Herbivore exclusion",
    "Scavenger exclusion",
    "No exclusion"
  )
)

PD$prob <- as.numeric(PD$prob)
PD$SE <- as.numeric(PD$SE)

PD.p <- ggplot(PD, aes(x = prob, y = Functional)) +
  geom_errorbar(aes(xmin = prob - SE, xmax = prob + SE),
    size = 0.5, width = 0.25, position = position_nudge(0, PD$Nudge)
  ) +
  geom_point(aes(fill = Mortality),
    color = "black",
    pch = 21, size = 4, position = position_nudge(0, PD$Nudge)
  ) +
  scale_fill_manual(values = c("black", "white")) +
  coord_fixed(ratio = 0.05) +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    text = element_text(size = 22),
    axis.title = element_text(face = "bold"),
    legend.position = "none",
    legend.title = element_blank()
  ) +
  theme(axis.ticks.y = element_blank()) +
  scale_x_continuous(limits = c(0.5, 1)) +
  xlab("Probability of Colonization") +
  ylab("")

## --------------- PY COLONIZATION ------------------------------------------------

PY <- col.means %>%
  filter(DormancyClass == "PY")

# Initialize column for Nudge factor (to separate points)
PY["Nudge"] <- NA

for (i in 1:nrow(PY)) {
  if (PY$Mortality[i] == "Mass mortality") {
    PY$Nudge[i] <- 0.2
  } else {
    PY$Nudge[i] <- -0.2
  }
}

# Reorder factors
PY$Functional <- factor(PY$Functional,
  levels = c(
    "Herbivore exclusion",
    "Scavenger exclusion",
    "No exclusion"
  )
)

PY$prob <- as.numeric(PY$prob)
PY$SE <- as.numeric(PY$SE)

PY.p <- ggplot(PY, aes(x = prob, y = Functional)) +
  geom_errorbar(aes(xmin = prob - SE, xmax = prob + SE),
    size = 0.5, width = 0.25, position = position_nudge(0, PY$Nudge)
  ) +
  geom_point(aes(fill = Mortality),
    color = "black",
    pch = 21, size = 4, position = position_nudge(0, PY$Nudge)
  ) +
  scale_fill_manual(values = c("black", "white")) +
  coord_fixed(ratio = 0.05) +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    text = element_text(size = 22),
    axis.title = element_text(face = "bold"),
    legend.position = "none",
    legend.title = element_blank()
  ) +
  theme(axis.ticks.y = element_blank()) +
  scale_x_continuous(limits = c(0, 0.5)) +
  xlab("") +
  ylab("")

## --------------- EXTIRPATION -------------------------------------------------

# Initialize column for MME identity / functional role loss
ext.means["Functional"] <- NA

for (i in 1:nrow(ext.means)) {
  if (ext.means$Treatment[i] == "CH" | ext.means$Treatment[i] == "MH") {
    ext.means$Functional[i] <- "Herbivore exclusion"
  }
  if (ext.means$Treatment[i] == "CO" | ext.means$Treatment[i] == "MO") {
    ext.means$Functional[i] <- "No exclusion"
  }
  if (ext.means$Treatment[i] == "CS" | ext.means$Treatment[i] == "MS") {
    ext.means$Functional[i] <- "Scavenger exclusion"
  }
}

# Initialize column for mortality
ext.means["Mortality"] <- NA

for (i in 1:nrow(ext.means)) {
  if (ext.means$Treatment[i] == "CH" | ext.means$Treatment[i] == "CO" | ext.means$Treatment[i] == "CS") {
    ext.means$Mortality[i] <- "Single carrion"
  }
  if (ext.means$Treatment[i] == "MH" | ext.means$Treatment[i] == "MO" | ext.means$Treatment[i] == "MS") {
    ext.means$Mortality[i] <- "Mass mortality"
  }
}

# Initialize column for Nudge factor (to separate points)
ext.means$Nudge <- NA

for (i in 1:nrow(ext.means)) {
  if (isTRUE(ext.means$Mortality[i] == "Mass mortality")) {
    ext.means$Nudge[i] <- 0.2
  } else {
    ext.means$Nudge[i] <- -0.2
  }
}

ext.means$prob <- as.numeric(ext.means$prob)

ext.p <- ggplot(ext.means, aes(x = prob, y = Functional)) +
  geom_errorbar(aes(xmin = prob - SE, xmax = prob + SE),
    size = 0.5, width = 0.25, position = position_nudge(0, ext.means$Nudge)
  ) +
  geom_point(aes(fill = Mortality),
    color = "black",
    pch = 21, size = 4, position = position_nudge(0, ext.means$Nudge)
  ) +
  scale_fill_manual(values = c("black", "white")) +
  coord_fixed(ratio = 0.05) +
  scale_x_continuous(
    limits = c(0.25, 0.80),
    breaks = c(0.25, 0.5, 0.75)
  ) +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    text = element_text(size = 22),
    axis.title = element_text(face = "bold"),
    legend.position = "none",
    legend.title = element_blank()
  ) +
  theme(axis.ticks.y = element_blank()) +
  xlab("Probability of Extirpation") +
  ylab("")
## --------------- COMBINE FIGURES -------------------------------------------------

library(patchwork)
ext.p/ND.p/PY.p/PD.p
