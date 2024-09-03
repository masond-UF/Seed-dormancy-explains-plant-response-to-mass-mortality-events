## --------------- HEADER ------------------------------------------------------
## Script name: 1a_Composition-survey-combine.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliaton: University of Florida
## Date Created: 2021-11-20
## Date Last modified: 2022-05-03
## Copyright (c) David S. Mason, 2022
## Contact: masond@ufl.edu, @EcoGraffito
## Purpose of script: This is a pre data munging script for the aggregated
## plant community data that combines surveys from 2019-2020 with 2021.

## --------------- SET—UP WORKSPACE --------------------------------------------
library(tidyverse)
library(tidylog)
library(lubridate)
library(styler)

# Spreadsheet with the sampling data from 2019 and 2020
surv.a <- read.csv("Animals-plants-seeds/Raw-data/Plants/2019-2020_plot-summarized-veg-survey.csv")

# Spreadsheet with the sampling data from 2021
surv.b <- read.csv("Animals-plants-seeds/Raw-data/Plants/transect-veg-survey/2021_July-raw-lg.csv")

## --------------- MUNGE DATA --------------------------------------------------

# Manipulate df so the surveys can be combined

# CA is an error, this should be CH
for (i in 1:nrow(surv.b)) {
  if (surv.b$Treatment[i] == "CA") {
    surv.b$Treatment[i] <- "CH"
  }
}

# The single carrion open treatment (CO) was transposed in the Gilgai site (GG)
for (i in 1:nrow(surv.b)) {
  if (surv.b$Treatment[i] == "OC") {
    surv.b$Treatment[i] <- "CO"
  }
}

# ALERT DF has two plots labeled as CH, but one should be CS
for (i in 1:nrow(surv.b)) {
  if (surv.b$Site[i] == "DF" & surv.b$Plot[i] == "5") {
    surv.b$Treatment[i] <- "CS"
  }
}

# Add columns for Carrion and Exclusion based on Treatment
for (i in 1:nrow(surv.b)) {
  if (surv.b$Treatment[i] == "MS") {
    surv.b$Carrion[i] <- "High"
    surv.b$Exclusion[i] <- "Scavenger"
  } else {
    if (surv.b$Treatment[i] == "CS") {
      surv.b$Carrion[i] <- "Low"
      surv.b$Exclusion[i] <- "Scavenger"
    } else {
      if (surv.b$Treatment[i] == "MO") {
        surv.b$Carrion[i] <- "High"
        surv.b$Exclusion[i] <- "Open"
      } else {
        if (surv.b$Treatment[i] == "CO") {
          surv.b$Carrion[i] <- "Low"
          surv.b$Exclusion[i] <- "Open"
        } else {
          if (surv.b$Treatment[i] == "CH") {
            surv.b$Carrion[i] <- "Low"
            surv.b$Exclusion[i] <- "Herbivore"
          } else {
            if (surv.b$Treatment[i] == "MH") {
              surv.b$Carrion[i] <- "High"
              surv.b$Exclusion[i] <- "Herbivore"
            } else {
              surv.b$Carrion[i] <- "Reference"
              surv.b$Exclusion[i] <- "Herbivore"
            }
          }
        }
      }
    }
  }
}

## --------------- CALCULATE COVER FOR SURV.B ----------------------------------

# Tally the number of observations at each plot and calculate relative cover
surv.b <- surv.b %>%
  group_by(Site, Treatment, Carrion, Exclusion, Month, Day, Year, Date, Species) %>%
  summarise(Detections = n()) %>%
  mutate(Cover = round((Detections / 16), digits = 2))

summary(surv.b)

## --------------- COMBINE THE LONG DATA -----------------------------

# Convert surv.a to long format
surv.a <- surv.a %>%
  pivot_longer(cols = 9:100, names_to = "Species", values_to = "Cover")


surv.b <- surv.b %>% select(-Detections)
surv.b$Carrion <- as.factor(surv.b$Carrion)
surv.b$Exclusion <- as.factor(surv.b$Exclusion)
surv.b <- as.data.frame(surv.b)

# Drop the detections column from the year 2 survey
surv.comb <- rbind(surv.a, surv.b)

## --------------- WRITE THE WIDE DATA -----------------------------
surv.comb.wd <- surv.comb %>%
  pivot_wider(names_from = Species, values_from = Cover)

surv.comb.wd[is.na(surv.comb.wd)] <- 0

write.csv(surv.comb.wd,
  "Animals-plants-seeds/Raw-data/Plants/Combined-plot-summarized-wd.csv",
  row.names = FALSE
)
