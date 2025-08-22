## --------------- HEADER ------------------------------------------------------
## Script name: 1_Clean-data.R
## Author: David Mason
## Date Created: 2025-8-22
## Date Last Modified: 2025-8-22
## Copyright (c) David Mason, 2025
## Contact: david.mason@jonesctr.org
## Purpose of script: This script cleans the raw carrion decomposition data

#Clear
rm(list=ls())

# Bring in the data
decay <- read.csv("Raw-data/Carrion-decomposition/Carrion-decay-rate.csv")

# Split treatment column into biomass and exclusion
decay <- decay %>%
  mutate(
    BIOMASS = case_when(
      grepl("Single Carcass", TREATMENT) ~ "Single Carcass",
      grepl("MME", TREATMENT) ~ "MME",
      TRUE ~ NA_character_
    ),
    EXCLUSION = case_when(
      grepl("Herbivore", TREATMENT) ~ "Herbivore",
      grepl("Scavenger", TREATMENT) ~ "Scavenger",
      grepl("Open", TREATMENT) ~ "Open",
      TRUE ~ NA_character_
    )
  )


# Add standard coding for treatment
decay <- decay %>%
  mutate(
    BIOMASS_CODE = case_when(
      grepl("Single Carcass", TREATMENT) ~ "C",
      grepl("MME", TREATMENT) ~ "M",
      TRUE ~ NA_character_
    ),
    EXCLUSION_CODE = case_when(
      grepl("Open", TREATMENT) ~ "O",
      grepl("Herbivore", TREATMENT) ~ "H",
      grepl("Scavenger", TREATMENT) ~ "S",
      TRUE ~ NA_character_
    ),
    TREATMENT_CODE = paste0(BIOMASS_CODE, EXCLUSION_CODE)
  )

# Remove unwanted columns
decay <- decay |> dplyr::select(-TREATMENT, -BIOMASS_CODE, -EXCLUSION_CODE)

# Rename coded column
colnames(decay)[7] <- "TREATMENT"

# Rearrange the columns
decay <- decay |> dplyr::select(SITE, TREATMENT, BIOMASS, EXCLUSION, 
																DECAY.STAGE, MEAN.TBS, DAY.REACHED)

# Export the data
write.csv(decay, "Clean-data/Carrion-decomposition/Carrion-decay-rate.csv",
					row.names = FALSE)

