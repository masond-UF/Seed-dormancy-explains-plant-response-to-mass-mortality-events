## --------------- HEADER ------------------------------------------------------
## Script name: 1_Cam-traps-clean.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliation: University of Florida
## Date Created: 2021-11-19
## Date Last Modified: 2025-08-13
## Copyright (c) David S. Mason, 2025
## Contact: masond@ufl.edu, @EcoGraffito
## Purpose of script: This script cleans the camera trap data, adds functional
## roles to each observation (e.g., herbivore, scavenger) and summarizes by 
## treatment and time intervals. The output of this script is a summarized 
## spreadsheet for further analysis.

## --------------- SET—UP WORKSPACE --------------------------------------------

# Clear the decks
rm(list=ls())

# Load packages
library(tidyverse)
library(tidylog)
library(lubridate)
library(styler)

# cam <- read.csv("Animals-plants-seeds/Raw-data/Animals/2022-06-4_Cam-traps.csv")
cam <- read.csv("Raw-data/Animals/2023-12-16_Cam-traps.csv")

## --------------- MUNGE DATA --------------------------------------------------

# Convert date into a format the tidyverse trucks with and round to nearest week
cam$DATE <- mdy(cam$DATE)

# Fix busted date
cam[8746,10] <- as_date('2019-07-09')

# Make species levels consistent
cam$COMMON.NAME <- dplyr::recode(cam$COMMON.NAME,
  "wild turkey" = "Wild turkey",
  "Wild Turkey" = "Wild turkey",
  "striped skunk" = "Striped skunk",
  "white-tailed deer" = "White-tailed deer",
  "Racoon" = "Raccoon",
  "Black Vulture" = "Black vulture",
  "armadillo" = "Nine-banded armadillo",
  "Armadillo" = "Nine-banded armadillo",
  "Unknown Mammal" = "Unknown mammal",
  "Unknown Bird" = "Unknown bird",
  "unknown bird" = "Unknown bird",
  "American Crow" = "American crow",
  "Crow" = "American crow",
  "Vulture" = "Unknown vulture",
  "Opossum" = "Virginia opossum",
  "Oppossum" = "Virginia opossum",
  "Turkey Vulture" = "Turkey vulture",
  "Turkey vulture " = "Turkey vulture",
  "Roadrunner" = "Greater roadrunner",
  "Feral hog" = "Wild pig",
  "Unknown Animal" = "Unknown animal",
  "Unknown Hummingbird" = "Unknown hummingbird",
  "Unknown Sparrow" = "Unknown sparrow",
  "Unknownj vulture" = "Unknown vulture"
)

# Ensure species levels are consistent
tmp <- data.frame(unique(cam$COMMON.NAME))

## --------------- ADD FUNCTIONAL ROLE DATA ------------------------------------

# Create the groups
Herbivore <- c("White-tailed deer", "Cattle", "Cottontail rabbit")
Obligate.scav <- c("Black vulture", "Unknown vulture", "Turkey vulture")
Facult.scav <- c(
  "Striped skunk", "Raccoon", "Nine-banded armadillo",
  "American crow", "Virginia opossum", "Coyote", "Dog",
  "Wild pig", "Unknown hawk", "Unknown raptor", "Greater roadrunner",
  "Bobcat"
)
Other.consum <- c(
  "Wild turkey", "Greater roadrunner", "Scissor-tailed flycatcher",
  "Unknown songbird", "Unknown sparrow", "Unknown hummingbird"
)
Undetermined <- c(
  "Unknown mammal", "Unknown bird", "Researcher", "Unknown animal",
	"Unknown rodent"
)

length(c(Herbivore, Obligate.scav, Facult.scav, Other.consum, Undetermined))

rm(tmp)

# Species to consider: cattle/dog (not supposed to be here), greater roadrunner/
# Wild turkey (likely scavenge if the carcass is small enough),

# Add value for functional column for each observation
for (i in 1:nrow(cam)) {
  if (cam$COMMON.NAME[i] %in% Herbivore) {
    cam$FUNCTIONAL[i] <- "Herbivore"
  } else {
    if (cam$COMMON.NAME[i] %in% Obligate.scav) {
      cam$FUNCTIONAL[i] <- "Scavenger"
    } else {
      if (cam$COMMON.NAME[i] %in% Facult.scav) {
        cam$FUNCTIONAL[i] <- "Scavenger"
      } else {
        if (cam$COMMON.NAME[i] %in% Other.consum) {
          cam$FUNCTIONAL[i] <- "Other"
        } else {
          if (cam$COMMON.NAME[i] %in% Undetermined) {
            cam$FUNCTIONAL[i] <- "Other"
          }
        }
      }
    }
  }
}
## --------------- SET ASIDE COYOTE --------------------------------------------

# For the special issue of food webs
coyote <- cam |>
	filter(COMMON.NAME == 'Coyote')

## --------------- FILTER DATA -------------------------------------------------

# Remove observations where the animals were outside the plot
cam <- cam %>% filter(NOTE != "outside of plot" &
  NOTE != "not in plot" &
  NOTE != "edge of plot" &
  NOTE != "edge if plot" &
  NOTE != "Not in Plot" &
  NOTE != "Not in plot (outside of cage)" &
  NOTE != "Not in plot (outside of cage), clip cut short" &
  NOTE != "edge of plot, walks out of view" & 
  NOTE != "edge of plot, walks out of view, clip cut short" &
  NOTE != "edge of plot, clip cut short" &
  NOTE != "edge of plot: trying to get in plot" &
  NOTE != "edge of plot, climbing fence, clip cut short" &
  NOTE != "edge of plot, investigating fence, clip cut short" &
  NOTE != "edge of plot, investigating exclusion fence" &
  NOTE != "edge of plot, runs out of view" &
  NOTE != "edge of plot, hard to see activity" &
  NOTE != "edge of plot messing with the camera" &
  NOTE != "Edge of plot and video cut short" &
  NOTE != "edge of plot and runs away" &
  NOTE != "too close to camera; edge of plot" &
  NOTE != "flies over edge of plot" &
  NOTE != "gripping edge of fence" &
  NOTE != "walks by edge of plot" &
  NOTE != "walks through edge of plot" &
  NOTE != "walks through edge of plot, investigating camera trap" &
  NOTE != "outside the plot" &
  NOTE != "really far outside the plot" &
  NOTE != "runs through plot, maybe outside" &
	NOTE != "outside the electric fence" &
	NOTE != "outside plot, perched on camera" &
	NOTE != "outside plot" &
	NOTE != "outside of plot, spreading wings" &
	NOTE != "outside of plot, could be eating" &
	NOTE != "outside of plot (near)" &
	NOTE != "outside of plot" &
	NOTE != "outside of lot (near)" &
	NOTE != "outside of electric fence" &
	NOTE != "outside of block" &
	NOTE != "outside  plot" &
	NOTE != "Not in plot (outside of cage), clip cut short" &
	NOTE != "Not in plot (outside of cage)" &
	NOTE != "flys outside of plot" &
	NOTE != "flies onto fence post from outside plot" &
  NOTE != "perched on fencing of other plot" &
  NOTE != "perched on camera trap" &
  NOTE != "flys over plot" &
  NOTE != "flys over plot" &
  NOTE != "flys over plot" &
  NOTE != "flies over plot" &
  NOTE != "flies onto exclusion fence post out of view"
)

# Remove observations without treatment information
cam <- cam %>%
  dplyr::select(DATE, TIME, SITE, TREATMENT,
  							GROUP.SIZE, INDIVIDUAL, COMMON.NAME, FUNCTIONAL, NOTE) %>% 
	filter(TREATMENT %in% c("CO", "MO", "CH", "MH", "CS", "MS"))

# Every row is associated with a treatment

# Remove observations without site information
cam <- cam %>%
  filter(SITE %in% c("WP", "DF", "GG", "OS"))

# One row was missing site

## --------------- ADD FACTORS -------------------------------------------------

for (i in 1:nrow(cam)) {
  if (cam$TREATMENT[i] == "CO") {
    cam$BIOMASS[i] <- "Single"
    cam$EXCLUSION[i] <- "Open"
  } else {
    if (cam$TREATMENT[i] == "CS") {
      cam$BIOMASS[i] <- "Single"
      cam$EXCLUSION[i] <- "Scavenger"
    } else {
      if (cam$TREATMENT[i] == "CH") {
        cam$BIOMASS[i] <- "Single"
        cam$EXCLUSION[i] <- "Herbivore"
      } else {
        if (cam$TREATMENT[i] == "MO") {
          cam$BIOMASS[i] <- "Mass"
          cam$EXCLUSION[i] <- "Open"
        } else {
          if (cam$TREATMENT[i] == "MS") {
            cam$BIOMASS[i] <- "Mass"
            cam$EXCLUSION[i] <- "Scavenger"
          } else {
            if (cam$TREATMENT[i] == "MH") {
              cam$BIOMASS[i] <- "Mass"
              cam$EXCLUSION[i] <- "Herbivore"
            }
          }
        }
      }
    }
  }
}

cam <- cam %>% dplyr::select(DATE, TIME, SITE, TREATMENT, GROUP.SIZE, 
														 INDIVIDUAL, BIOMASS, EXCLUSION, 
														 COMMON.NAME, FUNCTIONAL, NOTE)

## --------------- DROP MISSING DATES ------------------------------------------

cam <- cam |> 
	drop_na()

## --------------- DUPLICATES --------------------------------------------------


# Dupes that were caused by typos in the time, or individual observed were
# corrected in the raw data.

# The rest are potentially true duplicates and are therefore removed

cam <- cam[!duplicated(cam[-11]), ]

# Check for duplicates
test <- cam |> dplyr::select(-NOTE)
nrow(unique(test))

library(janitor)
dupes <- test |>
	get_dupes()

## --------------- WRITE TO CSV ------------------------------------------------

# Detections by treatment, functional group, and week for MME
write.csv(cam, "Clean-data/Animals/Camera-traps.csv",
  row.names = FALSE
)
