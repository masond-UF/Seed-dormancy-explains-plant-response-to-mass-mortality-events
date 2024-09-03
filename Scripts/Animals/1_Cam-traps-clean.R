## --------------- HEADER ------------------------------------------------------
## Script name: 1_Cam-traps-clean.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliation: University of Florida
## Date Created: 2021-11-19
## Date Last Modified: 2022-12-23
## Copyright (c) David S. Mason, 2022
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
cam <- read.csv("Animals-plants-seeds/Raw-data/Animals/2023-12-16_Cam-traps.csv")

## --------------- MUNGE DATA --------------------------------------------------

# Convert date into a format the tidyverse trucks with and round to nearest week
cam$Date <- mdy(cam$Date)

# Fix busted date
cam[8746,10] <- as_date('2019-07-09')

# Make species levels consistent
cam$Common.name <- dplyr::recode(cam$Common.name,
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
tmp <- data.frame(unique(cam$Common.name))

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
  if (cam$Common.name[i] %in% Herbivore) {
    cam$Functional[i] <- "Herbivore"
  } else {
    if (cam$Common.name[i] %in% Obligate.scav) {
      cam$Functional[i] <- "Scavenger"
    } else {
      if (cam$Common.name[i] %in% Facult.scav) {
        cam$Functional[i] <- "Scavenger"
      } else {
        if (cam$Common.name[i] %in% Other.consum) {
          cam$Functional[i] <- "Other"
        } else {
          if (cam$Common.name[i] %in% Undetermined) {
            cam$Functional[i] <- "Other"
          }
        }
      }
    }
  }
}
## --------------- SET ASIDE COYOTE --------------------------------------------

# For the special issue of food webs
coyote <- cam |>
	filter(Common.name == 'Coyote')

## --------------- FILTER DATA -------------------------------------------------

# Remove observations where the animals were outside the plot
cam <- cam %>% filter(Note != "outside of plot" &
  Note != "not in plot" &
  Note != "edge of plot" &
  Note != "edge if plot" &
  Note != "Not in Plot" &
  Note != "Not in plot (outside of cage)" &
  Note != "Not in plot (outside of cage), clip cut short" &
  Note != "edge of plot, walks out of view" & 
  Note != "edge of plot, walks out of view, clip cut short" &
  Note != "edge of plot, clip cut short" &
  Note != "edge of plot: trying to get in plot" &
  Note != "edge of plot, climbing fence, clip cut short" &
  Note != "edge of plot, investigating fence, clip cut short" &
  Note != "edge of plot, investigating exclusion fence" &
  Note != "edge of plot, runs out of view" &
  Note != "edge of plot, hard to see activity" &
  Note != "edge of plot messing with the camera" &
  Note != "Edge of plot and video cut short" &
  Note != "edge of plot and runs away" &
  Note != "too close to camera; edge of plot" &
  Note != "flies over edge of plot" &
  Note != "gripping edge of fence" &
  Note != "walks by edge of plot" &
  Note != "walks through edge of plot" &
  Note != "walks through edge of plot, investigating camera trap" &
  Note != "outside the plot" &
  Note != "really far outside the plot" &
  Note != "runs through plot, maybe outside" &
	Note != "outside the electric fence" &
	Note != "outside plot, perched on camera" &
	Note != "outside plot" &
	Note != "outside of plot, spreading wings" &
	Note != "outside of plot, could be eating" &
	Note != "outside of plot (near)" &
	Note != "outside of plot" &
	Note != "outside of lot (near)" &
	Note != "outside of electric fence" &
	Note != "outside of block" &
	Note != "outside  plot" &
	Note != "Not in plot (outside of cage), clip cut short" &
	Note != "Not in plot (outside of cage)" &
	Note != "flys outside of plot" &
	Note != "flies onto fence post from outside plot" &
  Note != "perched on fencing of other plot" &
  Note != "perched on camera trap" &
  Note != "flys over plot" &
  Note != "flys over plot" &
  Note != "flys over plot" &
  Note != "flies over plot" &
  Note != "flies onto exclusion fence post out of view"
)

# Remove observations without treatment information
cam <- cam %>%
  dplyr::select(Date, Time, Site, Treatment,
  							Group.Size, Individual.Observed, Common.name, Functional, Note) %>% 
	filter(Treatment %in% c("CO", "MO", "CH", "MH", "CS", "MS"))

# Every row is associated with a treatment

# Remove observations without site information
cam <- cam %>%
  filter(Site %in% c("WP", "DF", "GG", "OS"))

# One row was missing site

## --------------- ADD FACTORS -------------------------------------------------

for (i in 1:nrow(cam)) {
  if (cam$Treatment[i] == "CO") {
    cam$Carrion[i] <- "Single"
    cam$Exclusion[i] <- "Open"
  } else {
    if (cam$Treatment[i] == "CS") {
      cam$Carrion[i] <- "Single"
      cam$Exclusion[i] <- "Scavenger"
    } else {
      if (cam$Treatment[i] == "CH") {
        cam$Carrion[i] <- "Single"
        cam$Exclusion[i] <- "Herbivore"
      } else {
        if (cam$Treatment[i] == "MO") {
          cam$Carrion[i] <- "Mass"
          cam$Exclusion[i] <- "Open"
        } else {
          if (cam$Treatment[i] == "MS") {
            cam$Carrion[i] <- "Mass"
            cam$Exclusion[i] <- "Scavenger"
          } else {
            if (cam$Treatment[i] == "MH") {
              cam$Carrion[i] <- "Mass"
              cam$Exclusion[i] <- "Herbivore"
            }
          }
        }
      }
    }
  }
}

cam <- cam %>% dplyr::select(Date, Time, Site, Treatment, Group.Size, 
														 Individual.Observed, Carrion, Exclusion, 
														 Common.name, Functional, Note)

## --------------- DROP MISSING DATES ------------------------------------------

cam <- cam |> 
	drop_na()

## --------------- DROP MISSING DATES ------------------------------------------


# Dupes that were caused by typos in the time, or individual observed were
# corrected in the raw data.

# The rest are potentially true duplications and are therefore removed

cam <- cam[!duplicated(cam[-11]), ]

# Check for duplicates
test <- cam |> dplyr::select(-Note)
nrow(unique(test))

library(janitor)
dupes <- test |>
	get_dupes()

## --------------- SUMMARIZE BY WEEK -------------------------------------------
# cam$Date <- round_date(cam$Date, unit = "week")

# cam.trmt.wk <- cam %>%
  # group_by(Treatment, Date, Functional) %>%
  # summarise(Detections = n())

## --------------- ADD ZEROES TO DATA ------------------------------------------

# cam.trmt.wk.wd <- cam.trmt.wk %>%
  # pivot_wider(names_from = Functional, values_from = Detections)

# cam.trmt.wk.wd[is.na(cam.trmt.wk.wd)] <- 0

# cam.trmt.wk <- cam.trmt.wk.wd %>%
  # pivot_longer(3:7, names_to = "Functional", values_to = "Detections")

## --------------- WRITE TO CSV ------------------------------------------------

# Detections by treatment, functional group, and week for MME
write.csv(cam, "Animals-plants-seeds/Clean-data/Animals/Camera-traps.csv",
  row.names = FALSE
)

# For special issue
write.csv(coyote, "Animals-plants-seeds/Clean-data/Animals/Coyote.csv",
					row.names = FALSE
)

## --------------- SUMMARIZE TOTALS --------------------------------------------

# cam.totals <- cam %>%
  # group_by(Treatment, Carrion, Exclusion, Functional) %>%
  # summarise(Detections = n())

## --------------- ADD ZEROES TO DATA ------------------------------------------

# cam.totals.wd <- cam.totals %>%
  # pivot_wider(names_from = Functional, values_from = Detections)

# cam.totals.wd[is.na(cam.totals.wd)] <- 0

# cam.totals <- cam.totals.wd %>%
  # pivot_longer(4:8, names_to = "Functional", values_to = "Detections")

# cam.totals <- cam.totals %>%
  # filter(Treatment %in% c("CO", "MO", "CH", "MH", "CS", "MS"))

## --------------- WRITE TO CSV ------------------------------------------------

# Long total detections by treatment and functional group
# write.csv(cam.totals, "Animals-plants-seeds/Clean-data/Animals/Cam-trap-totals-lg.csv",
  # row.names = FALSE
# )

# Wide total detections by treatment and functional group
# write.csv(cam.totals.wd, "Animals-plants-seeds/Clean-data/Animals/Cam-trap-totals-wd.csv",
					# row.names = FALSE
# )
