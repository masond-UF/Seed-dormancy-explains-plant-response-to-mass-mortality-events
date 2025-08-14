## --------------- HEADER ------------------------------------------------------
## Script name: 4_Cam-traps-figure.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliation: University of Florida
## Date Created: 2021-11-23
## Date Last Modified: 2025-08-13
## Copyright (c) David S. Mason, 2025
## Contact: masond@ufl.edu, @EcoGraffito
## Purpose of script: This script will produce a figure for the camera trap data

## --------------- SET—UP WORKSPACE --------------------------------------------

library(tidyverse)
rm(list = ls())

decomp.means <- read.csv("Analysis/Animals/Decomp-means.csv")
herb.means <- read.csv("Analysis/Animals/Herb-means.csv")

## --------------- MANIPULATE THE DATA -----------------------------------------

decomp.means$FUNCTIONAL <- "Scavengers"
herb.means$FUNCTIONAL <- "Herbivores"

means <- rbind(decomp.means, herb.means)
rm(decomp.means, herb.means)

means$Biomass <- NA

for (i in 1:nrow(means)) {
  if (means$Treatment[i] %in% c("CO", "CS", "CH")) {
    means$Biomass[i] <- "Single carcass"
  } else {
    means$Biomass[i] <- "MMME"
  }
}

means$EXCLUSION <- NA

for (i in 1:nrow(means)) {
  if (means$Treatment[i] %in% c("CO", "MO")) {
    means$EXCLUSION[i] <- "Open"
  }
  if (means$Treatment[i] %in% c("CS", "MS")) {
    means$EXCLUSION[i] <- "Scavenger"
  }
  if (means$Treatment[i] %in% c("CH", "MH")) {
    means$EXCLUSION[i] <- "Herbivore"
  }
}

means$Nudge <- NA

for (i in 1:nrow(means)) {
  if (means$Biomass[i] == "MME") {
    means$Nudge[i] <- 0.2
  } else {
    means$Nudge[i] <- -0.2
  }
}

means$EXCLUSION <- factor(means$EXCLUSION,
  levels = c("Herbivore", "Scavenger", "Open")
)

means$Biomass <- factor(means$Biomass,
  levels = c("MME", "Single carcass")
)

means$FUNCTIONAL <- factor(means$FUNCTIONAL,
  levels = c(
    "Scavengers",
    "Herbivores"
  )
)

## --------------- CREATE A FIGURE ---------------------------------------------

p <- ggplot(means, aes(x = Mean, y = EXCLUSION)) +
  geom_errorbar(aes(xmin = Mean - se, xmax = Mean + se),
    size = 0.5, width = 0.05, position = position_nudge(0, means$Nudge)
  ) +
  geom_point(aes(fill = Biomass),
    color = "black",
    pch = 21, size = 6, position = position_nudge(0, means$Nudge)
  ) +
  scale_fill_manual(
    values = c("black", "white"),
    labels = c("Mass mortality", "Single carrion")
  ) +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    text = element_text(size = 20),
    axis.title = element_text(face = "bold"),
    legend.position = "top",
    legend.title = element_blank(),
    aspect.ratio = 2,
    strip.background = element_blank(),
    strip.text.x = element_blank()
  ) +
  theme(axis.ticks.y = element_blank()) +
  xlab("Detections") +
  ylab("EXCLUSION") +
  facet_wrap(~FUNCTIONAL, scales = "free_x")


p <- p + theme(
  panel.background = element_rect(
    fill = "transparent",
    colour = NA_character_
  ), # necessary to avoid drawing panel outline
  panel.grid.major = element_blank(), # get rid of major grid
  panel.grid.minor = element_blank(), # get rid of minor grid
  plot.background = element_rect(
    fill = "transparent",
    colour = NA_character_
  ), # necessary to avoid drawing plot outline
  legend.background = element_rect(fill = "transparent"),
  legend.box.background = element_rect(fill = "transparent"),
  legend.key = element_rect(fill = "transparent")
)

ggsave(
  plot = p,
  filename = "Figures/Animals/FUNCTIONAL-detections.png",
  bg = "transparent",
  width = 8,
  height = 13,
  units = "in"
)

