## --------------- HEADER ------------------------------------------------------
## Script name: 2a_Plant-fitness-clean.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliation: University of Florida
## Date Last modified: 2025-08-14
## Copyright (c) David S. Mason, 2025
## Contact: david.mason@jonesctr.org
## Purpose of script: This script combines and cleans fitness data from two 
## different years/species. The output of this script is a spreadsheet with both
## species and two different metrics (e.g., height and infloresences)

## --------------- SET—UP WORKSPACE --------------------------------------------
library(tidyverse)
library(tidylog)
library(lubridate)
library(styler)

fit <- read.csv("Animals-plants-seeds/Raw-data/Plants/LASE-height-inflor.csv")

## --------------- MUNGE SPEC2 DATA --------------------------------------------

# Drop the treatment in fit so we can add a new one that matches other
fit <- select(fit, -TREATMENT)

# Rename the site column because we are going to replace the value
names(fit)[names(fit) == "SITE"] <- "TMP"

# Fill site and treatment columns based on tmp value
for (i in 1:nrow(fit)) {
  if (fit$TMP[i] == "WP-MH") {
    fit$SITE[i] <- "WP"
    fit$TREATMENT[i] <- "MH"
  } else {
    if (fit$TMP[i] == "OS-MH") {
      fit$SITE[i] <- "OS"
      fit$TREATMENT[i] <- "MH"
    } else {
      if (fit$TMP[i] == "GG-MH") {
        fit$SITE[i] <- "GG"
        fit$TREATMENT[i] <- "MH"
      } else {
        if (fit$TMP[i] == "GG-CS") {
          fit$SITE[i] <- "GG"
          fit$TREATMENT[i] <- "CS"
        } else {
          if (fit$TMP[i] == "GG") {
            fit$SITE[i] <- "GG"
            fit$TREATMENT[i] <- "NONE"
          } else {
            if (fit$TMP[i] == "GG-MS") {
              fit$SITE[i] <- "GG"
              fit$TREATMENT[i] <- "MS"
            } else {
              fit$SITE[i] <- "GATE"
              fit$TREATMENT[i] <- "NONE"
            }
          }
        }
      }
    }
  }
}

# Add a value for species
fit$SPECIES <- "LASE"

# Add date information
fit$DATE <- mdy("07-22-2021")

fit <- select(fit, SITE, DATE, TREATMENT, MH, SPECIES, HEIGHT, INFLOR)

write.csv(fit, "Clean-data/Plants/Plant-fitness.csv",
  row.names = FALSE
)
