## --------------- HEADER ------------------------------------------------------
## Script name: 4_Seed-survival-figures.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliation: University of Florida
## Date Created: 2022-06-01
## Date Last Modified: 2025-9-14
## Copyright (c) David S. Mason, 2025
## Contact: david.mason@jonesctr.org
## Purpose of script: This will be a script for visualizing the seed survival data

## --------------- SET—UP WORKSPACE --------------------------------------------

library(tidyverse)
library(tidylog)
library(lubridate)
library(styler)
library(emmeans)
library(broom)
library(car)
library(lme4)
library(broom.mixed)
library(MuMIn)
library(grid)
library(tidyr)

options(scipen=999)

rm(list=ls())

seed.surv <- read.csv("Clean-data/Seeds/Seed-survival.csv")

seed.surv$FINAL.STATUS[is.na(seed.surv$FINAL.STATUS)] <- 0

survival_probs <- seed.surv |>
  group_by(SITE, SPECIES, DORMANCY.CLASS, BIOMASS, EXCLUSION, PACKET) |>
  dplyr::summarize(
    survived = sum(FINAL.STATUS),
    total = n(),
    .groups = "drop"
  ) |>
  mutate(
    prob = (survived + 0.5) / (total + 1)
  )

# All non-reference conditions
treatments <- survival_probs |>
  filter(BIOMASS != "Reference") |>
  # Create the reference packet key for joining
  mutate(ref_packet = if_else(PACKET %in% c("BA", "BU"), "B", "A"))

# Only reference conditions
references <- survival_probs |>
	dplyr::filter(BIOMASS == "Reference") |>
  dplyr::rename(ref_prob = prob) |> # Rename for clarity after joining
  dplyr::mutate(ref_packet = PACKET) |> # Create the same key here (it's just the packet itself)
  dplyr::select(SITE, SPECIES, DORMANCY.CLASS, ref_packet, ref_prob) # Select only the columns needed for the join

# Join treatments to their corresponding reference values
block.specific.data <- left_join(
  treatments,
  references,
  by = c("SITE", "SPECIES", "DORMANCY.CLASS", "ref_packet")
)

block.specific.ors <- block.specific.data |>
  # Remove rows where a reference was not found
  filter(!is.na(ref_prob)) |>
  mutate(
    odds_treatment = prob / (1 - prob),
    odds_reference = ref_prob / (1 - ref_prob),
    odds_ratio = odds_treatment / odds_reference
  )

final.mean.or <- block.specific.ors |>
  mutate(log_or = log(odds_ratio)) |> # log transform, average, exponentiate
  group_by(BIOMASS, EXCLUSION, PACKET, DORMANCY.CLASS) |>
  dplyr::summarize(
    mean_log_or = mean(log_or, na.rm = TRUE),
    se_log_or   = sd(log_or, na.rm = TRUE) / sqrt(n()),
    n_sites     = n(),
    .groups     = "drop"
  ) |>
  mutate(
    mean_odds_ratio = exp(mean_log_or),
    lower_ci = exp(mean_log_or - 1.96 * se_log_or),
    upper_ci = exp(mean_log_or + 1.96 * se_log_or)
  )

# Rename and reorder factor levels 
final.mean.or <- final.mean.or |>
  mutate(BIOMASS = forcats::fct_relevel(BIOMASS, "MME", "Single carcass"))

# Reorder factor levels
final.mean.or$EXCLUSION <- factor(final.mean.or$EXCLUSION, 
														 levels = c("Open", "Scavenger",
														 					 "Herbivore"))

final.mean.or <- final.mean.or |>
  dplyr::rename(geometric_mean_or = mean_odds_ratio) |>
  mutate(
    Timing = case_when(
      substr(PACKET, 1, 1) == "A" ~ "Seed rain",
      substr(PACKET, 1, 1) == "B" ~ "Seed bank"
    ),
    Location = case_when(
      substr(PACKET, 2, 2) == "A" ~ "Adjacent",
      substr(PACKET, 2, 2) %in% c("T", "U") ~ "Contact"
    ),
    BIOMASS = dplyr::recode(BIOMASS,
      "MME" = "Mass mortality",
      "Single carcass" = "Single carcass"
    ),
    DORMANCY.CLASS = dplyr::recode(DORMANCY.CLASS,
      "ND" = "No dormancy",
      "PD" = "Physiological dormancy",
      "PY" = "Physical dormancy"
    )
  )

timing_order <- c("Seed rain", "Seed bank")
biomass_order <- c("Single carcass", "Mass mortality") # Set correct order
location_order <- c("Adjacent", "Contact")
exclusion_order <- c("Open", "Scavenger", "Herbivore")

final.mean.or <- final.mean.or |>
  mutate(
    Timing = factor(Timing, levels = timing_order),
    BIOMASS = factor(BIOMASS, levels = biomass_order),
    Location = factor(Location, levels = location_order),
    EXCLUSION = factor(EXCLUSION, levels = exclusion_order)
  )

final.mean.or <- final.mean.or |>
		pivot_wider(
    id_cols = c(Timing, BIOMASS, Location, EXCLUSION), # Columns to keep
    names_from = DORMANCY.CLASS, # Column to pivot into new column headers
    values_from = c(geometric_mean_or, mean_log_or, se_log_or), # Values for the new columns
    names_vary = "slowest" # Keeps names grouped like: mean_ND, mean_PD instead of ND_mean, PD_mean
  	) |>
		rename_with(~ gsub(" ", "_", .x), contains("dormancy"))
	
final.mean.or <- final.mean.or |>
	dplyr::select(
    Timing, BIOMASS, Location, EXCLUSION,
    # No dormancy columns
    geometric_mean_or_No_dormancy,
    mean_log_or_No_dormancy,
    se_log_or_No_dormancy,
    # Physical dormancy columns
    geometric_mean_or_Physical_dormancy,
    mean_log_or_Physical_dormancy,
    se_log_or_Physical_dormancy,
    # Physiological dormancy columns
    geometric_mean_or_Physiological_dormancy,
    mean_log_or_Physiological_dormancy,
    se_log_or_Physiological_dormancy
  ) |>
  arrange(Timing, BIOMASS, Location, EXCLUSION) |>
  dplyr::mutate(across(where(is.numeric), ~round(., 2))) |>
  mutate(across(starts_with("se_log_or"), ~paste0("'(", .x, ")")))


write.csv(final.mean.or, "species-block-or-means.csv", row.names = FALSE)


