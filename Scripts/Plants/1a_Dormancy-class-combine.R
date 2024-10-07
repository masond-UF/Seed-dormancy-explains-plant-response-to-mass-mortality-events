## --------------- HEADER ------------------------------------------------------
## Script name: 1a_Dormancy-class-combine.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliation: University of Florida
## Date Created: 2022-04-28
## Date Last modified: 2024-10-04
## Copyright (c) David S. Mason, 2022
## Contact: masond@ufl.edu, @EcoGraffito
## Purpose of script: This is a pre data munging script for the unsummarized
## transect data that combines surveys from 2019-2020 with 2021.

## --------------- SET—UP WORKSPACE --------------------------------------------
library(tidyverse)
library(tidylog)
library(lubridate)
library(styler)

# Bring in the wide raw data from 2019-2020
march2019 <- read.csv("Raw-data/Plants/transect-veg-survey/2019-March-raw-wd.csv")
april2019 <- read.csv("Raw-data/Plants/transect-veg-survey/2019-April-raw-wd.csv")
may2019 <- read.csv("Raw-data/Plants/transect-veg-survey/2019-May-raw-wd.csv")
june2019 <- read.csv("Raw-data/Plants/transect-veg-survey/2019-June-raw-wd.csv")
july2019 <- read.csv("Raw-data/Plants/transect-veg-survey/2019-July-raw-wd.csv")
july2020 <- read.csv("Raw-data/Plants/transect-veg-survey/2020-July-raw-wd.csv")

# Bring in the long raw data from 2021
july2021 <- read.csv("Raw-data/Plants/transect-veg-survey/2021_July-raw-lg.csv")

# This data is in wide format, with the presence of each species at each date
# and transect point indicated with a 1. That data is also summarized as a
# proportion at the end of each site (this proportion was used to construct the
# community matrix).

# This code will create a dataframe with the presence/absence of each dormancy
# class at each plot. The columns will be sampling events.

# The final dataframes for the logistic binomial regression will indicate
# whether or not the dormancy class was EVER added or lost to a particular
# transect point.

## --------------- CLEAN MARCH 2019 --------------------------------------------

# Rename Distance (m) column
march2019 <- march2019 %>%
  dplyr::rename(Distance = Dist..m., Carrion = Mortality)

# Set Distance (m) as a factor
march2019$Distance <- as.factor(march2019$Distance)

# Drop the summarized row at the end of each site
march2019 <- march2019 %>%
  filter(Distance != "NA")

# Replace NA values with 0
march2019[is.na(march2019)] <- 0

# Convert the wide data into long format
march2019 <- march2019 %>%
  pivot_longer(cols = 7:52, names_to = "Species", values_to = "Present")

# Initialize column for treatment
march2019["Treatment"] <- NA

# Add column for treatment
for (i in 1:nrow(march2019)) {
  if (march2019$Carrion[i] == "Control" & march2019$Exclusion[i] == "Open") {
    march2019$Treatment[i] <- "CO"
  } else {
    if (march2019$Carrion[i] == "Control" & march2019$Exclusion[i] == "Scavenger") {
      march2019$Treatment[i] <- "CS"
    } else {
      if (march2019$Carrion[i] == "Control" & march2019$Exclusion[i] == "Herbivore") {
        march2019$Treatment[i] <- "CH"
      } else {
        if (march2019$Carrion[i] == "Mass" & march2019$Exclusion[i] == "Open") {
          march2019$Treatment[i] <- "MO"
        } else {
          if (march2019$Carrion[i] == "Mass" & march2019$Exclusion[i] == "Scavenger") {
            march2019$Treatment[i] <- "MS"
          } else {
            if (march2019$Carrion[i] == "Mass" & march2019$Exclusion[i] == "Herbivore") {
              march2019$Treatment[i] <- "MH"
            }
          }
        }
      }
    }
  }
}

march2019 <- march2019 %>%
  select(Date, Site, Treatment, Carrion, Exclusion, Transect, Distance, Species, Present)

## --------------- CLEAN APRIL 2019 --------------------------------------------

# Rename Distance (m) column
april2019 <- april2019 %>%
  dplyr::rename(Distance = Dist..m., Carrion = Mortality)

# Set Distance (m) as a factor
april2019$Distance <- as.factor(april2019$Distance)

# Drop the summarized row at the end of each site
april2019 <- april2019 %>%
  filter(Distance != "NA")

# Replace NA values with 0
april2019[is.na(april2019)] <- 0

# Convert the wide data into long format
april2019 <- april2019 %>%
  pivot_longer(cols = 7:75, names_to = "Species", values_to = "Present")

# Initialize column for treatment
april2019["Treatment"] <- NA

# Add column for treatment
for (i in 1:nrow(april2019)) {
  if (april2019$Carrion[i] == "Control" & april2019$Exclusion[i] == "Open") {
    april2019$Treatment[i] <- "CO"
  } else {
    if (april2019$Carrion[i] == "Control" & april2019$Exclusion[i] == "Scavenger") {
      april2019$Treatment[i] <- "CS"
    } else {
      if (april2019$Carrion[i] == "Control" & april2019$Exclusion[i] == "Herbivore") {
        april2019$Treatment[i] <- "CH"
      } else {
        if (april2019$Carrion[i] == "Mass" & april2019$Exclusion[i] == "Open") {
          april2019$Treatment[i] <- "MO"
        } else {
          if (april2019$Carrion[i] == "Mass" & april2019$Exclusion[i] == "Scavenger") {
            april2019$Treatment[i] <- "MS"
          } else {
            if (april2019$Carrion[i] == "Mass" & april2019$Exclusion[i] == "Herbivore") {
              april2019$Treatment[i] <- "MH"
            }
          }
        }
      }
    }
  }
}

april2019 <- april2019 %>%
  select(Date, Site, Treatment, Carrion, Exclusion, Transect, Distance, Species, Present)

## --------------- CLEAN MAY 2019 --------------------------------------------

# Set Distance (m) as a factor
may2019$Distance <- as.factor(may2019$Distance)

# Drop the summarized row at the end of each site
may2019 <- may2019 %>%
  filter(Distance != "NA")

# Rename levels of factor
levels(may2019$Transect) <- list(
  North = "N", East = "E",
  South = "S", West = "W"
)

# Replace NA values with 0
may2019[is.na(may2019)] <- 0

# Convert the wide data into long format
may2019 <- may2019 %>%
  pivot_longer(cols = 6:87, names_to = "Species", values_to = "Present")

# Initialize column for treatment
may2019["Carrion"] <- NA
may2019["Exclusion"] <- NA

# Add columns for Carrion and Exclusion based on Treatment
for (i in 1:nrow(may2019)) {
  if (may2019$Treatment[i] == "MS") {
    may2019$Carrion[i] <- "High"
    may2019$Exclusion[i] <- "Scavenger"
  } else {
    if (may2019$Treatment[i] == "CS") {
      may2019$Carrion[i] <- "Low"
      may2019$Exclusion[i] <- "Scavenger"
    } else {
      if (may2019$Treatment[i] == "MO") {
        may2019$Carrion[i] <- "High"
        may2019$Exclusion[i] <- "Open"
      } else {
        if (may2019$Treatment[i] == "CO") {
          may2019$Carrion[i] <- "Low"
          may2019$Exclusion[i] <- "Open"
        } else {
          if (may2019$Treatment[i] == "CH") {
            may2019$Carrion[i] <- "Low"
            may2019$Exclusion[i] <- "Herbivore"
          } else {
            if (may2019$Treatment[i] == "MH") {
              may2019$Carrion[i] <- "High"
              may2019$Exclusion[i] <- "Herbivore"
            } else {
              may2019$Carrion[i] <- "Reference"
              may2019$Exclusion[i] <- "Herbivore"
            }
          }
        }
      }
    }
  }
}

may2019 <- may2019 %>%
  select(Date, Site, Treatment, Carrion, Exclusion, Transect, Distance, Species, Present)

# Rename levels of factor
may2019$Carrion <- as.factor(may2019$Carrion)

levels(may2019$Carrion) <- list(
  Control = "Low", Mass = "High"
)

## --------------- CLEAN JUNE 2019 ---------------------------------------------

# Rename Distance (m) column
june2019 <- june2019 %>%
  dplyr::rename(Distance = Point, Plot = Treatment)


# Drop the summarized row at the end of each site
june2019 <- june2019 %>%
  filter(Distance != "NA")

# Initialize column for treatment
june2019["Carrion"] <- NA
june2019["Exclusion"] <- NA
june2019["Treatment"] <- NA


# Add treatment information
for (i in 1:nrow(june2019)) {
  if (june2019$Site[i] == "Dixon" & june2019$Plot[i] == "1") {
    june2019$Carrion[i] <- "Mass"
    june2019$Exclusion[i] <- "Scavenger"
    june2019$Treatment[i] <- "MS"
  } else if (june2019$Site[i] == "Dixon" & june2019$Plot[i] == "2") {
    june2019$Carrion[i] <- "Control"
    june2019$Exclusion[i] <- "Scavenger"
    june2019$Treatment[i] <- "CS"
  } else if (june2019$Site[i] == "Dixon" & june2019$Plot[i] == "3") {
    june2019$Carrion[i] <- "Mass"
    june2019$Exclusion[i] <- "Herbivore"
    june2019$Treatment[i] <- "MH"
  } else if (june2019$Site[i] == "Dixon" & june2019$Plot[i] == "4") {
    june2019$Carrion[i] <- "Mass"
    june2019$Exclusion[i] <- "Open"
    june2019$Treatment[i] <- "MO"
  } else if (june2019$Site[i] == "Dixon" & june2019$Plot[i] == "5") {
    june2019$Carrion[i] <- "Control"
    june2019$Exclusion[i] <- "Herbivore"
    june2019$Treatment[i] <- "CH"
  } else if (june2019$Site[i] == "Dixon" & june2019$Plot[i] == "6") {
    june2019$Carrion[i] <- "Control"
    june2019$Exclusion[i] <- "Open"
    june2019$Treatment[i] <- "CO"
  } else if (june2019$Site[i] == "Wellpad" & june2019$Plot[i] == "1") {
    june2019$Carrion[i] <- "Mass"
    june2019$Exclusion[i] <- "Scavenger"
    june2019$Treatment[i] <- "MS"
  } else if (june2019$Site[i] == "Wellpad" & june2019$Plot[i] == "2") {
    june2019$Carrion[i] <- "Control"
    june2019$Exclusion[i] <- "Herbivore"
    june2019$Treatment[i] <- "CH"
  } else if (june2019$Site[i] == "Wellpad" & june2019$Plot[i] == "3") {
    june2019$Carrion[i] <- "Mass"
    june2019$Exclusion[i] <- "Herbivore"
    june2019$Treatment[i] <- "MH"
  } else if (june2019$Site[i] == "Wellpad" & june2019$Plot[i] == "4") {
    june2019$Carrion[i] <- "Control"
    june2019$Exclusion[i] <- "Open"
    june2019$Treatment[i] <- "CO"
  } else if (june2019$Site[i] == "Wellpad" & june2019$Plot[i] == "5") {
    june2019$Carrion[i] <- "Control"
    june2019$Exclusion[i] <- "Scavenger"
    june2019$Treatment[i] <- "CS"
  } else if (june2019$Site[i] == "Wellpad" & june2019$Plot[i] == "6") {
    june2019$Carrion[i] <- "Mass"
    june2019$Exclusion[i] <- "Open"
    june2019$Treatment[i] <- "MO"
  } else if (june2019$Site[i] == "Gilgai" & june2019$Plot[i] == "1") {
    june2019$Carrion[i] <- "Control"
    june2019$Exclusion[i] <- "Open"
    june2019$Treatment[i] <- "CO"
  } else if (june2019$Site[i] == "Gilgai" & june2019$Plot[i] == "2") {
    june2019$Carrion[i] <- "Control"
    june2019$Exclusion[i] <- "Herbivore"
    june2019$Treatment[i] <- "CH"
  } else if (june2019$Site[i] == "Gilgai" & june2019$Plot[i] == "3") {
    june2019$Carrion[i] <- "Control"
    june2019$Exclusion[i] <- "Scavenger"
    june2019$Treatment[i] <- "CS"
  } else if (june2019$Site[i] == "Gilgai" & june2019$Plot[i] == "4") {
    june2019$Carrion[i] <- "Mass"
    june2019$Exclusion[i] <- "Scavenger"
    june2019$Treatment[i] <- "MS"
  } else if (june2019$Site[i] == "Gilgai" & june2019$Plot[i] == "5") {
    june2019$Carrion[i] <- "Mass"
    june2019$Exclusion[i] <- "Herbivroe"
    june2019$Treatment[i] <- "MH"
  } else if (june2019$Site[i] == "Gilgai" & june2019$Plot[i] == "6") {
    june2019$Carrion[i] <- "Mass"
    june2019$Exclusion[i] <- "Open"
    june2019$Treatment[i] <- "MO"
  } else if (june2019$Site[i] == "Oswalt" & june2019$Plot[i] == "1") {
    june2019$Carrion[i] <- "Control"
    june2019$Exclusion[i] <- "Scavenger"
    june2019$Treatment[i] <- "CS"
  } else if (june2019$Site[i] == "Oswalt" & june2019$Plot[i] == "2") {
    june2019$Carrion[i] <- "Control"
    june2019$Exclusion[i] <- "Herbivore"
    june2019$Treatment[i] <- "CH"
  } else if (june2019$Site[i] == "Oswalt" & june2019$Plot[i] == "3") {
    june2019$Carrion[i] <- "Mass"
    june2019$Exclusion[i] <- "Scavenger"
    june2019$Treatment[i] <- "MS"
  } else if (june2019$Site[i] == "Oswalt" & june2019$Plot[i] == "4") {
    june2019$Carrion[i] <- "Control"
    june2019$Exclusion[i] <- "Open"
    june2019$Treatment[i] <- "CO"
  } else if (june2019$Site[i] == "Oswalt" & june2019$Plot[i] == "5") {
    june2019$Carrion[i] <- "Mass"
    june2019$Exclusion[i] <- "Open"
    june2019$Treatment[i] <- "MO"
  } else if (june2019$Site[i] == "Oswalt" & june2019$Plot[i] == "6") {
    june2019$Carrion[i] <- "Mass"
    june2019$Exclusion[i] <- "Herbivore"
    june2019$Treatment[i] <- "MH"
  }
}

# Convert the wide data into long format
june2019 <- june2019 %>%
  pivot_longer(cols = 6:89, names_to = "Species", values_to = "Present") %>%
  select(
    Date, Site, Treatment, Carrion, Exclusion, Transect, Distance,
    Species, Present, -Plot
  )

# Replace NA values with 0
june2019$Present <- replace_na(june2019$Present, 0)

## --------------- CLEAN JULY 2019 ---------------------------------------------

# Replace NA values with 0
july2019[is.na(july2019)] <- 0

# Drop the summarized row at the end of each site
july2019 <- july2019 %>%
  filter(Transect != "")

# Add Distance
july2019$Distance <- rep(seq(1, 4, 1), 112)

# Rename plot column
july2019 <- july2019 %>%
  dplyr::rename(Plot = Treatment)


# Initialize column for treatment
july2019["Carrion"] <- NA
july2019["Exclusion"] <- NA
july2019["Treatment"] <- NA


# Add treatment information
for (i in 1:nrow(july2019)) {
  if (july2019$Site[i] == "Dixon" & july2019$Plot[i] == "1") {
    july2019$Carrion[i] <- "Mass"
    july2019$Exclusion[i] <- "Scavenger"
    july2019$Treatment[i] <- "MS"
  } else if (july2019$Site[i] == "Dixon" & july2019$Plot[i] == "2") {
    july2019$Carrion[i] <- "Control"
    july2019$Exclusion[i] <- "Scavenger"
    july2019$Treatment[i] <- "CS"
  } else if (july2019$Site[i] == "Dixon" & july2019$Plot[i] == "3") {
    july2019$Carrion[i] <- "Mass"
    july2019$Exclusion[i] <- "Herbivore"
    july2019$Treatment[i] <- "MH"
  } else if (july2019$Site[i] == "Dixon" & july2019$Plot[i] == "4") {
    july2019$Carrion[i] <- "Mass"
    july2019$Exclusion[i] <- "Open"
    july2019$Treatment[i] <- "MO"
  } else if (july2019$Site[i] == "Dixon" & july2019$Plot[i] == "5") {
    july2019$Carrion[i] <- "Control"
    july2019$Exclusion[i] <- "Herbivore"
    july2019$Treatment[i] <- "CH"
  } else if (july2019$Site[i] == "Dixon" & july2019$Plot[i] == "6") {
    july2019$Carrion[i] <- "Control"
    july2019$Exclusion[i] <- "Open"
    july2019$Treatment[i] <- "CO"
  } else if (july2019$Site[i] == "Wellpad" & july2019$Plot[i] == "1") {
    july2019$Carrion[i] <- "Mass"
    july2019$Exclusion[i] <- "Scavenger"
    july2019$Treatment[i] <- "MS"
  } else if (july2019$Site[i] == "Wellpad" & july2019$Plot[i] == "2") {
    july2019$Carrion[i] <- "Control"
    july2019$Exclusion[i] <- "Herbivore"
    july2019$Treatment[i] <- "CH"
  } else if (july2019$Site[i] == "Wellpad" & july2019$Plot[i] == "3") {
    july2019$Carrion[i] <- "Mass"
    july2019$Exclusion[i] <- "Herbivore"
    july2019$Treatment[i] <- "MH"
  } else if (july2019$Site[i] == "Wellpad" & july2019$Plot[i] == "4") {
    july2019$Carrion[i] <- "Control"
    july2019$Exclusion[i] <- "Open"
    july2019$Treatment[i] <- "CO"
  } else if (july2019$Site[i] == "Wellpad" & july2019$Plot[i] == "5") {
    july2019$Carrion[i] <- "Control"
    july2019$Exclusion[i] <- "Scavenger"
    july2019$Treatment[i] <- "CS"
  } else if (july2019$Site[i] == "Wellpad" & july2019$Plot[i] == "6") {
    july2019$Carrion[i] <- "Mass"
    july2019$Exclusion[i] <- "Open"
    july2019$Treatment[i] <- "MO"
  } else if (july2019$Site[i] == "Gilgai" & july2019$Plot[i] == "1") {
    july2019$Carrion[i] <- "Control"
    july2019$Exclusion[i] <- "Open"
    july2019$Treatment[i] <- "CO"
  } else if (july2019$Site[i] == "Gilgai" & july2019$Plot[i] == "2") {
    july2019$Carrion[i] <- "Control"
    july2019$Exclusion[i] <- "Herbivore"
    july2019$Treatment[i] <- "CH"
  } else if (july2019$Site[i] == "Gilgai" & july2019$Plot[i] == "3") {
    july2019$Carrion[i] <- "Control"
    july2019$Exclusion[i] <- "Scavenger"
    july2019$Treatment[i] <- "CS"
  } else if (july2019$Site[i] == "Gilgai" & july2019$Plot[i] == "4") {
    july2019$Carrion[i] <- "Mass"
    july2019$Exclusion[i] <- "Scavenger"
    july2019$Treatment[i] <- "MS"
  } else if (july2019$Site[i] == "Gilgai" & july2019$Plot[i] == "5") {
    july2019$Carrion[i] <- "Mass"
    july2019$Exclusion[i] <- "Herbivroe"
    july2019$Treatment[i] <- "MH"
  } else if (july2019$Site[i] == "Gilgai" & july2019$Plot[i] == "6") {
    july2019$Carrion[i] <- "Mass"
    july2019$Exclusion[i] <- "Open"
    july2019$Treatment[i] <- "MO"
  } else if (july2019$Site[i] == "Oswalt" & july2019$Plot[i] == "1") {
    july2019$Carrion[i] <- "Control"
    july2019$Exclusion[i] <- "Scavenger"
    july2019$Treatment[i] <- "CS"
  } else if (july2019$Site[i] == "Oswalt" & july2019$Plot[i] == "2") {
    july2019$Carrion[i] <- "Control"
    july2019$Exclusion[i] <- "Herbivore"
    july2019$Treatment[i] <- "CH"
  } else if (july2019$Site[i] == "Oswalt" & july2019$Plot[i] == "3") {
    july2019$Carrion[i] <- "Mass"
    july2019$Exclusion[i] <- "Scavenger"
    july2019$Treatment[i] <- "MS"
  } else if (july2019$Site[i] == "Oswalt" & july2019$Plot[i] == "4") {
    july2019$Carrion[i] <- "Control"
    july2019$Exclusion[i] <- "Open"
    july2019$Treatment[i] <- "CO"
  } else if (july2019$Site[i] == "Oswalt" & july2019$Plot[i] == "5") {
    july2019$Carrion[i] <- "Mass"
    july2019$Exclusion[i] <- "Open"
    july2019$Treatment[i] <- "MO"
  } else if (july2019$Site[i] == "Oswalt" & july2019$Plot[i] == "6") {
    july2019$Carrion[i] <- "Mass"
    july2019$Exclusion[i] <- "Herbivore"
    july2019$Treatment[i] <- "MH"
  }
}

# Convert the wide data into long format
july2019 <- july2019 %>%
  select(
    Date, Site, Treatment, Carrion, Exclusion, Transect,
    Distance, everything()
  ) %>%
  select(-Plot)

july2019 <- july2019 %>%
  pivot_longer(cols = 8:101, names_to = "Species", values_to = "Present")

# Rename levels of factor
levels(july2019$Transect) <- list(
  North = "N", East = "E",
  South = "S", West = "W"
)

## --------------- CLEAN JULY 2020 ---------------------------------------------

# Replace NA values with 0
july2020[is.na(july2020)] <- 0

# Drop the summarized row at the end of each site
july2020 <- july2020 %>%
  filter(Transect != "")

# Rename distance and plot column
july2020 <- july2020 %>%
  dplyr::rename(
    Distance = "Point",
    Plot = "Treatment"
  )

# Initialize column for treatment
july2020["Carrion"] <- NA
july2020["Exclusion"] <- NA
july2020["Treatment"] <- NA

# Add treatment information
for (i in 1:nrow(july2020)) {
  if (july2020$Site[i] == "Dixon" & july2020$Plot[i] == "1") {
    july2020$Carrion[i] <- "Mass"
    july2020$Exclusion[i] <- "Scavenger"
    july2020$Treatment[i] <- "MS"
  } else if (july2020$Site[i] == "Dixon" & july2020$Plot[i] == "2") {
    july2020$Carrion[i] <- "Control"
    july2020$Exclusion[i] <- "Scavenger"
    july2020$Treatment[i] <- "CS"
  } else if (july2020$Site[i] == "Dixon" & july2020$Plot[i] == "3") {
    july2020$Carrion[i] <- "Mass"
    july2020$Exclusion[i] <- "Herbivore"
    july2020$Treatment[i] <- "MH"
  } else if (july2020$Site[i] == "Dixon" & july2020$Plot[i] == "4") {
    july2020$Carrion[i] <- "Mass"
    july2020$Exclusion[i] <- "Open"
    july2020$Treatment[i] <- "MO"
  } else if (july2020$Site[i] == "Dixon" & july2020$Plot[i] == "5") {
    july2020$Carrion[i] <- "Control"
    july2020$Exclusion[i] <- "Herbivore"
    july2020$Treatment[i] <- "CH"
  } else if (july2020$Site[i] == "Dixon" & july2020$Plot[i] == "6") {
    july2020$Carrion[i] <- "Control"
    july2020$Exclusion[i] <- "Open"
    july2020$Treatment[i] <- "CO"
  } else if (july2020$Site[i] == "Wellpad" & july2020$Plot[i] == "1") {
    july2020$Carrion[i] <- "Mass"
    july2020$Exclusion[i] <- "Scavenger"
    july2020$Treatment[i] <- "MS"
  } else if (july2020$Site[i] == "Wellpad" & july2020$Plot[i] == "2") {
    july2020$Carrion[i] <- "Control"
    july2020$Exclusion[i] <- "Herbivore"
    july2020$Treatment[i] <- "CH"
  } else if (july2020$Site[i] == "Wellpad" & july2020$Plot[i] == "3") {
    july2020$Carrion[i] <- "Mass"
    july2020$Exclusion[i] <- "Herbivore"
    july2020$Treatment[i] <- "MH"
  } else if (july2020$Site[i] == "Wellpad" & july2020$Plot[i] == "4") {
    july2020$Carrion[i] <- "Control"
    july2020$Exclusion[i] <- "Open"
    july2020$Treatment[i] <- "CO"
  } else if (july2020$Site[i] == "Wellpad" & july2020$Plot[i] == "5") {
    july2020$Carrion[i] <- "Control"
    july2020$Exclusion[i] <- "Scavenger"
    july2020$Treatment[i] <- "CS"
  } else if (july2020$Site[i] == "Wellpad" & july2020$Plot[i] == "6") {
    july2020$Carrion[i] <- "Mass"
    july2020$Exclusion[i] <- "Open"
    july2020$Treatment[i] <- "MO"
  } else if (july2020$Site[i] == "Gilgai" & july2020$Plot[i] == "1") {
    july2020$Carrion[i] <- "Control"
    july2020$Exclusion[i] <- "Open"
    july2020$Treatment[i] <- "CO"
  } else if (july2020$Site[i] == "Gilgai" & july2020$Plot[i] == "2") {
    july2020$Carrion[i] <- "Control"
    july2020$Exclusion[i] <- "Herbivore"
    july2020$Treatment[i] <- "CH"
  } else if (july2020$Site[i] == "Gilgai" & july2020$Plot[i] == "3") {
    july2020$Carrion[i] <- "Control"
    july2020$Exclusion[i] <- "Scavenger"
    july2020$Treatment[i] <- "CS"
  } else if (july2020$Site[i] == "Gilgai" & july2020$Plot[i] == "4") {
    july2020$Carrion[i] <- "Mass"
    july2020$Exclusion[i] <- "Scavenger"
    july2020$Treatment[i] <- "MS"
  } else if (july2020$Site[i] == "Gilgai" & july2020$Plot[i] == "5") {
    july2020$Carrion[i] <- "Mass"
    july2020$Exclusion[i] <- "Herbivroe"
    july2020$Treatment[i] <- "MH"
  } else if (july2020$Site[i] == "Gilgai" & july2020$Plot[i] == "6") {
    july2020$Carrion[i] <- "Mass"
    july2020$Exclusion[i] <- "Open"
    july2020$Treatment[i] <- "MO"
  } else if (july2020$Site[i] == "Oswalt" & july2020$Plot[i] == "1") {
    july2020$Carrion[i] <- "Control"
    july2020$Exclusion[i] <- "Scavenger"
    july2020$Treatment[i] <- "CS"
  } else if (july2020$Site[i] == "Oswalt" & july2020$Plot[i] == "2") {
    july2020$Carrion[i] <- "Control"
    july2020$Exclusion[i] <- "Herbivore"
    july2020$Treatment[i] <- "CH"
  } else if (july2020$Site[i] == "Oswalt" & july2020$Plot[i] == "3") {
    july2020$Carrion[i] <- "Mass"
    july2020$Exclusion[i] <- "Scavenger"
    july2020$Treatment[i] <- "MS"
  } else if (july2020$Site[i] == "Oswalt" & july2020$Plot[i] == "4") {
    july2020$Carrion[i] <- "Control"
    july2020$Exclusion[i] <- "Open"
    july2020$Treatment[i] <- "CO"
  } else if (july2020$Site[i] == "Oswalt" & july2020$Plot[i] == "5") {
    july2020$Carrion[i] <- "Mass"
    july2020$Exclusion[i] <- "Open"
    july2020$Treatment[i] <- "MO"
  } else if (july2020$Site[i] == "Oswalt" & july2020$Plot[i] == "6") {
    july2020$Carrion[i] <- "Mass"
    july2020$Exclusion[i] <- "Herbivore"
    july2020$Treatment[i] <- "MH"
  }
}

# Convert the wide data into long format
july2020 <- july2020 %>%
  select(
    Date, Site, Treatment, Carrion, Exclusion, Transect,
    Distance, everything()
  ) %>%
  select(-Plot)

july2020 <- july2020 %>%
  pivot_longer(cols = 8:99, names_to = "Species", values_to = "Present")

## --------------- CLEAN JULY 2021 ---------------------------------------------

# Manipulate df so the surveys can be combined

# CA is an error, this should be CH
for (i in 1:nrow(july2021)) {
  if (july2021$Treatment[i] == "CA") {
    july2021$Treatment[i] <- "CH"
  }
}

# The single carrion open treatment (CO) was transposed in the Gilgai site (GG)
for (i in 1:nrow(july2021)) {
  if (july2021$Treatment[i] == "OC") {
    july2021$Treatment[i] <- "CO"
  }
}

# Dixon has two plots labeled as CH, but one should be CS
for (i in 1:nrow(july2021)) {
  if (july2021$Site[i] == "DF" & july2021$Plot[i] == "5") {
    july2021$Treatment[i] <- "CS"
  }
}

# Rename distance and plot column
july2021 <- july2021 %>%
  dplyr::rename(Distance = "Point")

july2021$Present <- 1

# Initialize column for treatment
july2021["Carrion"] <- NA
july2021["Exclusion"] <- NA

# Add columns for Carrion and Exclusion based on Treatment
for (i in 1:nrow(july2021)) {
  if (july2021$Treatment[i] == "MS") {
    july2021$Carrion[i] <- "Mass"
    july2021$Exclusion[i] <- "Scavenger"
  } else {
    if (july2021$Treatment[i] == "CS") {
      july2021$Carrion[i] <- "Control"
      july2021$Exclusion[i] <- "Scavenger"
    } else {
      if (july2021$Treatment[i] == "MO") {
        july2021$Carrion[i] <- "Mass"
        july2021$Exclusion[i] <- "Open"
      } else {
        if (july2021$Treatment[i] == "CO") {
          july2021$Carrion[i] <- "Control"
          july2021$Exclusion[i] <- "Open"
        } else {
          if (july2021$Treatment[i] == "CH") {
            july2021$Carrion[i] <- "Control"
            july2021$Exclusion[i] <- "Herbivore"
          } else {
            if (july2021$Treatment[i] == "MH") {
              july2021$Carrion[i] <- "Mass"
              july2021$Exclusion[i] <- "Herbivore"
            } else {
              july2021$Carrion[i] <- "Reference"
              july2021$Exclusion[i] <- "Herbivore"
            }
          }
        }
      }
    }
  }
}

july2021 <- july2021 %>%
  select(
    Date, Site, Treatment, Carrion, Exclusion, Transect, Distance,
    Species, Present
  )

# Rename levels of factor
levels(july2021$Site) <- list(
  Dixon = "DF", Oswalt = "OS",
  Wellpad = "WP", Gilgai = "GG"
)

# Save as tibble
july2021 <- as_tibble(july2021)

# Remove duplicated entries
july2021 <- distinct(july2021)

## --------------- COMBINE AND FILTER ------------------------------------------

# Combine data
comb.transect.lg <- rbind(march2019, april2019, may2019, june2019, july2019, july2020, july2021)

# Drop reference
comb.transect.lg <- comb.transect.lg %>% filter(Treatment != "REF")

## --------------- SAVE DATA  --------------------------------------------------

# The dates for the final sampling event ate being changed during the save.
comb.transect.lg$Date <- mdy(comb.transect.lg$Date)

write.csv(comb.transect.lg,
  "Raw-data/Plants/Combined-transect-lg.csv",
  row.names = FALSE
)
