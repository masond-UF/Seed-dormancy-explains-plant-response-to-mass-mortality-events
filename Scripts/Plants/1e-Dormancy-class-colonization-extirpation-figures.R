## --------------- HEADER ------------------------------------------------------
## Script name: 1e_Dormancy-class-colonization-extinction-figures.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliation: University of Florida
## Date Created: 2021-05-02
## Date Last modified: 2025-08-14
## Copyright (c) David S. Mason, 2025
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
col.means <- read.csv("Analysis/Plants/Colonization-means.csv")[1:18,1:6]
ext.means <- read.csv("Analysis/Plants/Extirpation-means.csv")[1:6,1:6]

## --------------- PREP COLONIZATION ------------------------------------------------

# Initialize column for MME identity / functional role loss
col.means["FUNCTIONAL"] <- NA

for (i in 1:nrow(col.means)) {
  if (col.means$TREATMENT[i] == "CH" | col.means$TREATMENT[i] == "MH") {
    col.means$EXCLUSION[i] <- "Herbivore"
  }
  if (col.means$TREATMENT[i] == "CO" | col.means$TREATMENT[i] == "MO") {
    col.means$EXCLUSION[i] <- "Open"
  }
  if (col.means$TREATMENT[i] == "CS" | col.means$TREATMENT[i] == "MS") {
    col.means$EXCLUSION[i] <- "Scavenger"
  }
}

# Initialize column for carrion biomass
col.means["Mortality"] <- NA

for (i in 1:nrow(col.means)) {
  if (col.means$TREATMENT[i] == "CH" | col.means$TREATMENT[i] == "CO" | col.means$TREATMENT[i] == "CS") {
    col.means$BIOMASS[i] <- "Single carrion"
  }
  if (col.means$TREATMENT[i] == "MH" | col.means$TREATMENT[i] == "MO" | col.means$TREATMENT[i] == "MS") {
    col.means$BIOMASS[i] <- "MME"
  }
}

DORMANCY.CLASS <- c(
  `ND` = "No dormancy",
  `PD` = "Physiological",
  `PY` = "Physical"
)

## --------------- ND COLONIZATION ------------------------------------------------

ND <- col.means %>%
  filter(DORMANCY.CLASS == "ND")

# Initialize column for Nudge factor (to separate points)
ND["Nudge"] <- NA

for (i in 1:nrow(ND)) {
  if (ND$BIOMASS[i] == "MME") {
    ND$Nudge[i] <- 0.2
  } else {
    ND$Nudge[i] <- -0.2
  }
}

# Reorder factors
ND$EXCLUSION <- factor(ND$EXCLUSION,
  levels = c(
    "Herbivore",
    "Scavenger",
    "Open"
  )
)

ND$prob <- as.numeric(ND$prob)
ND$SE <- as.numeric(ND$SE)

ND.p <- ggplot(ND, aes(x = prob, y = EXCLUSION)) +
  geom_errorbar(aes(xmin = prob - SE, xmax = prob + SE),
    size = 0.5, width = 0.25, position = position_nudge(0, ND$Nudge)
  ) +
  geom_point(aes(fill = BIOMASS),
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
  filter(DORMANCY.CLASS == "PD")

# Initialize column for Nudge factor (to separate points)
PD["Nudge"] <- NA

for (i in 1:nrow(PD)) {
  if (PD$BIOMASS[i] == "MME") {
    PD$Nudge[i] <- 0.2
  } else {
    PD$Nudge[i] <- -0.2
  }
}

# Reorder factors
PD$EXCLUSION <- factor(PD$EXCLUSION,
  levels = c(
    "Herbivore",
    "Scavenger",
    "Open"
  )
)

PD$prob <- as.numeric(PD$prob)
PD$SE <- as.numeric(PD$SE)

PD.p <- ggplot(PD, aes(x = prob, y = EXCLUSION)) +
  geom_errorbar(aes(xmin = prob - SE, xmax = prob + SE),
    size = 0.5, width = 0.25, position = position_nudge(0, PD$Nudge)
  ) +
  geom_point(aes(fill = BIOMASS),
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
  filter(DORMANCY.CLASS == "PY")

# Initialize column for Nudge factor (to separate points)
PY["Nudge"] <- NA

for (i in 1:nrow(PY)) {
  if (PY$BIOMASS[i] == "MME") {
    PY$Nudge[i] <- 0.2
  } else {
    PY$Nudge[i] <- -0.2
  }
}

# Reorder factors
PY$EXCLUSION <- factor(PY$EXCLUSION,
  levels = c(
    "Herbivore",
    "Scavenger",
    "Open"
  )
)

PY$prob <- as.numeric(PY$prob)
PY$SE <- as.numeric(PY$SE)

PY.p <- ggplot(PY, aes(x = prob, y = EXCLUSION)) +
  geom_errorbar(aes(xmin = prob - SE, xmax = prob + SE),
    size = 0.5, width = 0.25, position = position_nudge(0, PY$Nudge)
  ) +
  geom_point(aes(fill = BIOMASS),
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
ext.means["FUNCTIONAL"] <- NA

for (i in 1:nrow(ext.means)) {
  if (ext.means$TREATMENT[i] == "CH" | ext.means$TREATMENT[i] == "MH") {
    ext.means$EXCLUSION[i] <- "Herbivore exclusion"
  }
  if (ext.means$TREATMENT[i] == "CO" | ext.means$TREATMENT[i] == "MO") {
    ext.means$EXCLUSION[i] <- "No exclusion"
  }
  if (ext.means$TREATMENT[i] == "CS" | ext.means$TREATMENT[i] == "MS") {
    ext.means$EXCLUSION[i] <- "Scavenger exclusion"
  }
}

# Initialize column for mortality
ext.means["BIOMASS"] <- NA

for (i in 1:nrow(ext.means)) {
  if (ext.means$TREATMENT[i] == "CH" | ext.means$TREATMENT[i] == "CO" | ext.means$TREATMENT[i] == "CS") {
    ext.means$BIOMASS[i] <- "Single carrion"
  }
  if (ext.means$TREATMENT[i] == "MH" | ext.means$TREATMENT[i] == "MO" | ext.means$TREATMENT[i] == "MS") {
    ext.means$BIOMASS[i] <- "MME"
  }
}

# Initialize column for Nudge factor (to separate points)
ext.means$Nudge <- NA

for (i in 1:nrow(ext.means)) {
  if (isTRUE(ext.means$BIOMASS[i] == "MME")) {
    ext.means$Nudge[i] <- 0.2
  } else {
    ext.means$Nudge[i] <- -0.2
  }
}

ext.means$prob <- as.numeric(ext.means$prob)

ext.p <- ggplot(ext.means, aes(x = prob, y = EXCLUSION)) +
  geom_errorbar(aes(xmin = prob - SE, xmax = prob + SE),
    size = 0.5, width = 0.25, position = position_nudge(0, ext.means$Nudge)
  ) +
  geom_point(aes(fill = BIOMASS),
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
