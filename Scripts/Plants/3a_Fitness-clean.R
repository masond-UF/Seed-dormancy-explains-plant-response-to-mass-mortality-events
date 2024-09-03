## --------------- HEADER ------------------------------------------------------
## Script name: 3a_Plant-fitness-clean.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliaton: University of Florida
## Date Created: 2021-11-22
## Copyright (c) David S. Mason, 2021
## Contact: masond@ufl.edu, @EcoGraffito
## Purpose of script: This script combines and cleans fitness data from two 
## different years/species. The output of this script is a spreadsheet with both
## species and two different metrics (e.g., height and infloresences)

## --------------- SET—UP WORKSPACE --------------------------------------------
library(tidyverse)
library(tidylog)
library(lubridate)
library(styler)

spec1 <- read.csv("Animals-plants-seeds/Raw-data/Plants/AMDR-height.csv")
spec2 <- read.csv("Animals-plants-seeds/Raw-data/Plants/LASE-height-inflor.csv")

## --------------- MUNGE SPEC2 DATA --------------------------------------------

# First, I want to get these two dataframes on the same page regarding the names
# and contents of the columns

# Drop the treatment in spec2 so we can add a new one that matches spec1
spec2 <- select(spec2, -TREATMENT)

# Rename the site column because we are going to replace the value
names(spec2)[names(spec2) == "SITE"] <- "TMP"

# Fill site and treatment columns based on tmp value
for (i in 1:nrow(spec2)) {
  if (spec2$TMP[i] == "WP-MH") {
    spec2$SITE[i] <- "WP"
    spec2$TREATMENT[i] <- "MH"
  } else {
    if (spec2$TMP[i] == "OS-MH") {
      spec2$SITE[i] <- "OS"
      spec2$TREATMENT[i] <- "MH"
    } else {
      if (spec2$TMP[i] == "GG-MH") {
        spec2$SITE[i] <- "GG"
        spec2$TREATMENT[i] <- "MH"
      } else {
        if (spec2$TMP[i] == "GG-CS") {
          spec2$SITE[i] <- "GG"
          spec2$TREATMENT[i] <- "CS"
        } else {
          if (spec2$TMP[i] == "GG") {
            spec2$SITE[i] <- "GG"
            spec2$TREATMENT[i] <- "NONE"
          } else {
            if (spec2$TMP[i] == "GG-MS") {
              spec2$SITE[i] <- "GG"
              spec2$TREATMENT[i] <- "MS"
            } else {
              spec2$SITE[i] <- "GATE"
              spec2$TREATMENT[i] <- "NONE"
            }
          }
        }
      }
    }
  }
}

# Add a value for species
spec2$SPECIES <- "LASE"

# Add date information
spec2$DATE <- mdy("07-22-2021")

# Rearrange the data
spec2 <- select(spec2, SITE, TREATMENT, DATE, SPECIES, HEIGHT, INFLOR)

## --------------- MUNGE SPEC1 DATA --------------------------------------------

# Add a value for species
spec1$SPECIES <- "AMDR"

# Add date information
spec1$DATE <- mdy(spec1$Date)

# Add value for inflorescences
spec1$INFLOR <- NA

# Rename the columns to match spec2
names(spec1)[names(spec1) == "Site"] <- "SITE"
names(spec1)[names(spec1) == "Treatment"] <- "TREATMENT"
names(spec1)[names(spec1) == "Height..cm."] <- "HEIGHT"

spec1 <- select(spec1, SITE, TREATMENT, DATE, SPECIES, HEIGHT, INFLOR)

## --------------- COLLATE AND WRITE DATA --------------------------------------

# Combine the data
fit <- rbind(spec1, spec2)

# Although there are combinations of carrion biomass and exclusion in the
# experimental design, we are mostly interested in whether the plants were in
# mass mortality herbivore exlucion plots for fitness comparison.
# We need to add an extra column.

for (i in 1:nrow(fit)) {
  if (fit$TREATMENT[i] == "MH") {
    fit$MH[i] <- "Y"
  } else {
    fit$MH[i] <- "N"
  }
}

fit <- select(fit, SITE, DATE, TREATMENT, MH, SPECIES, HEIGHT, INFLOR)

write.csv(fit, "Animals-plants-seeds/Clean-data/Plants/Plant-fitness.csv",
  row.names = FALSE
)
