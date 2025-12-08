## --------------- HEADER ------------------------------------------------------
## Script name: 4_Seed-survival-figures.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliation: University of Florida
## Date Created: 2022-06-01
## Date Last Modified: 2025-8-13
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

# Bring in the estimated mariginal means from the models
surv.means <- read.csv("Analysis/Seeds/Seed-survival-means.csv")

# Bring in the reference data
seed.surv <- read.csv("Clean-data/Seeds/Seed-survival.csv")

# Final status is 0 if not 1
seed.surv$FINAL.STATUS[is.na(seed.surv$FINAL.STATUS)] <- 0

## --------------- GET ESTIMATED MARGINAL MEANS FOR REF ------------------------

ref <- seed.surv |> 
	filter(EXCLUSION == "Reference")

ref <- ref |> 
	group_by(SITE, BIOMASS, EXCLUSION, PACKET, DORMANCY.CLASS, SPECIES) |> 
	dplyr::summarize(SURV = sum(FINAL.STATUS),
						TRIALS = n()) 

ref <- ref |> 
	mutate(DEAD = TRIALS-SURV)

ref.m <- glmer(cbind(SURV,DEAD) ~ DORMANCY.CLASS * PACKET + (1|SPECIES),
						data = ref, family = binomial,
						       control = glmerControl(optimizer = "bobyqa",
                                      optCtrl = list(maxfun = 2e5))
)

r.squaredGLMM(ref.m) # Fixed effects 28-29% Random effects 56-59%

ref.anova.df <-as.data.frame(Anova(ref.m))
ref.m.sum <- broom::tidy(ref.m, conf.int=TRUE)
ref.means <- tidy(emmeans(ref.m, ~ DORMANCY.CLASS * PACKET, type = "response"))

## --------------- CALCULATE THE ODDS RATIO ------------------------------------

colnames(surv.means)[4] <- "DORMANCY.CLASS"

ref.means <- ref.means |>
  mutate(DORMANCY.CLASS = fct_recode(DORMANCY.CLASS,
    "No dormancy"          = "ND",
    "Physiological dormancy" = "PD",
    "Physical dormancy"      = "PY"
  ))

ref.means <- ref.means |>
	dplyr::select(DORMANCY.CLASS, PACKET, prob)


# Make a helper column for merging
surv.means <- surv.means |>
  mutate(TMP.PACKET = case_when(
    grepl("Bank", PACKET, ignore.case = TRUE) ~ "B",
    grepl("Rain", PACKET, ignore.case = TRUE) ~ "A",
    TRUE ~ NA_character_
  ))

# Rename columns before merge
colnames(ref.means)[2] <- 'TMP.PACKET'
colnames(ref.means)[3] <- 'ref.prob'

surv.means <- merge(surv.means, ref.means)

# Calculate odds ratio
surv.means <- surv.means |>
  mutate(odds.ratio = (prob / (1 - prob)) / (ref.prob / (1 - ref.prob)))

# I created a list accidentally?
surv.means <- as.data.frame(lapply(surv.means, unlist))

## --------------- MEDIAN & GEOMETRIC MEANS ------------------------------------

surv.means$BIOMASS <- as_factor(surv.means$BIOMASS)
levels(surv.means$BIOMASS)
surv.means$PACKET <- as_factor(surv.means$PACKET)
levels(surv.means$PACKET)

# Reorder factor levels
colnames(surv.means)[4] <- 'EXCLUSION'
surv.means$EXCLUSION <- factor(surv.means$EXCLUSION, 
														 levels = c("Open", "Scavenger exclusion",
														 					 "Herbivore exclusion"))

# Filter the data for seed rain adjacent
surv.means |>
  filter(PACKET == 'Rain\nadjacent') |>
  group_by(DORMANCY.CLASS) |>
  summarise(
    median_odds_ratio = median(odds.ratio),
    mean_log_odds_ratio = mean(log(odds.ratio), na.rm = TRUE),
    geometric_mean_odds_ratio = exp(mean(log(odds.ratio), na.rm = TRUE))
 )

# Filter the data for seed rain proximal
surv.means |>
  filter(PACKET == 'Rain\nproximal') |>
  group_by(DORMANCY.CLASS) |>
  summarise(
    median_odds_ratio = median(odds.ratio),
    mean_log_odds_ratio = mean(log(odds.ratio), na.rm = TRUE),
    geometric_mean_odds_ratio = exp(mean(log(odds.ratio), na.rm = TRUE))
 )

# Filter the data for seed bank
surv.means |>
  filter(TMP.PACKET == 'B') |>
  group_by(DORMANCY.CLASS) |>
  summarise(
    median_odds_ratio = median(odds.ratio),
    mean_log_odds_ratio = mean(log(odds.ratio), na.rm = TRUE),
    geometric_mean_odds_ratio = exp(mean(log(odds.ratio), na.rm = TRUE))
 )

# Filter the data for scavenger exclusion
surv.means |>
  filter(EXCLUSION == "Scavenger exclusion") |>
  group_by(DORMANCY.CLASS) |>
  summarise(
    median_odds_ratio = median(odds.ratio),
    mean_log_odds_ratio = mean(log(odds.ratio), na.rm = TRUE),
    geometric_mean_odds_ratio = exp(mean(log(odds.ratio), na.rm = TRUE))
 )

# Scavenger interaction with dormancy class and biomass
surv.means |>
	filter(DORMANCY.CLASS %in% c("No dormancy", "Physical dormancy")) |>
  mutate(scavenger_activity = if_else(
    EXCLUSION == "Scavenger exclusion", 
    "Without Scavengers", 
    "With Scavengers"
  )) |>
    group_by(DORMANCY.CLASS, BIOMASS, scavenger_activity) |>
    summarise(
    median_odds_ratio = median(odds.ratio, na.rm = TRUE),
    geometric_mean_odds_ratio = exp(mean(log(odds.ratio), na.rm = TRUE)),
    .groups = "drop" # Drop grouping after summarising
  )

surv.means |>
	filter(DORMANCY.CLASS %in% c("No dormancy", "Physical dormancy")) |>
  mutate(scavenger_activity = if_else(
    EXCLUSION == "Scavenger exclusion", 
    "Without Scavengers", 
    "With Scavengers"
  )) |>
    group_by(DORMANCY.CLASS, scavenger_activity) |>
    summarise(
    median_odds_ratio = median(odds.ratio, na.rm = TRUE),
    geometric_mean_odds_ratio = exp(mean(log(odds.ratio), na.rm = TRUE)),
    .groups = "drop" # Drop grouping after summarising
  )


# Create comparisons
comparison_groups <- list(
  "Overall Seed Rain" = quote(TMP.PACKET == 'A'),
  "Overall Seed Bank" = quote(TMP.PACKET == 'B'),
  "Seed Rain - Adjacent" = quote(TMP.PACKET == 'A' & PACKET == 'Rain\nadjacent'),
  "Seed Rain - Adjacent with Scavengers" = quote(TMP.PACKET == 'A' & PACKET == 'Rain\nadjacent' & EXCLUSION %in% c('Open', 'Herbivore exclusion')),
  "Seed Rain - Contact" = quote(TMP.PACKET == 'A' & PACKET == 'Rain\nproximal'),
  "Seed Rain - Adjacent, Scavengers (PY only)" = quote(TMP.PACKET == 'A' & PACKET == 'Rain\nadjacent' & EXCLUSION %in% c('Open', 'Herbivore exclusion') & DORMANCY.CLASS == 'Physical dormancy'),
  "Seed Rain - Adjacent, Scavengers (ND only)" = quote(TMP.PACKET == 'A' & PACKET == 'Rain\nadjacent' & EXCLUSION %in% c('Open', 'Herbivore exclusion') & DORMANCY.CLASS == 'No dormancy'),
  "Seed Bank - Physiological Dormancy" = quote(TMP.PACKET == 'B' & DORMANCY.CLASS == 'Physiological dormancy')
)

# Function to calculate median OR and GM for a given filter condition
calculate_summary <- function(data, filter_condition) {
  data |>
    filter(!!filter_condition) |>
    summarise(median_or = median(odds.ratio, na.rm = TRUE),
  						mean_log_odds_ratio = mean(log(odds.ratio), na.rm = TRUE),
    					geometric_mean_odds_ratio = exp(mean(log(odds.ratio), na.rm = TRUE)))
}

# Apply the function to each group
results <- purrr::map_dfr(comparison_groups,
													~calculate_summary(surv.means, .x), .id = "Comparison")

# Check
results |>
	 dplyr::select(Comparison,median_or, GM_or = geometric_mean_odds_ratio) |>
	 mutate(across(where(is.numeric), ~round(., 2)))

# 83 % claim
seed.bank <- surv.means |>
  filter(TMP.PACKET == "B") |>
	filter(DORMANCY.CLASS == "No dormancy" | DORMANCY.CLASS == "Physical dormancy" )

rows <- nrow(seed.bank)

rows.less.1 <- seed.bank |>
  filter(odds.ratio < 1) |>
  nrow()

(rows.less.1 / rows) * 100

seed.bank <- surv.means |>
  filter(TMP.PACKET == "B") |>
	filter(DORMANCY.CLASS == "Physiological dormancy")

rows <- nrow(seed.bank)

rows.less.1 <- seed.bank |>
  filter(odds.ratio < 1) |>
  nrow()

(rows.less.1 / rows) * 100

surv.means |>
  filter(TMP.PACKET == "B") |>
	group_by(DORMANCY.CLASS) |>
	summarise(
    median_odds_ratio = median(odds.ratio, na.rm = TRUE),
    geometric_mean_odds_ratio = exp(mean(log(odds.ratio), na.rm = TRUE)),
    .groups = "drop" # Drop grouping after summarising
  )

# Dormancy class
surv.means |>
  group_by(DORMANCY.CLASS) |>
  summarise(
    median_or = median(odds.ratio, na.rm = TRUE),
    geometric_mean_or = exp(mean(log(odds.ratio), na.rm = TRUE))
  ) |>
  pivot_longer(
    cols = c(median_or, geometric_mean_or),
    names_to = "statistic",
    values_to = "value"
  ) |>
  ggplot(aes(x = DORMANCY.CLASS, y = value, color = statistic)) +
  geom_point(position = position_dodge(width = 0.3), size = 4, alpha = 0.8) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  labs(
    title = "Overall Seed Survival by Dormancy Class",
    x = "Dormancy Class",
    y = "Odds Ratio (OR)",
    color = "Statistic"
  ) +
  scale_color_manual(labels = c("Geometric Mean", "Median"), values = c("skyblue", "goldenrod")) +
  theme_minimal()

# Bank vs rain
timing_labels <- c("A" = "Seed Rain", "B" = "Seed Bank")

surv.means |>
  group_by(TMP.PACKET) |>
  summarise(
    median_or = median(odds.ratio, na.rm = TRUE),
    geometric_mean_or = exp(mean(log(odds.ratio), na.rm = TRUE))
  ) |>
  pivot_longer(
    cols = c(median_or, geometric_mean_or),
    names_to = "statistic",
    values_to = "value"
  ) |>
  ggplot(aes(x = TMP.PACKET, y = value, color = statistic)) +
  geom_point(position = position_dodge(width = 0.3), size = 4, alpha = 0.8) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  labs(
    title = "Overall Seed Survival: Seed Rain vs. Seed Bank",
    x = "Timing of Arrival",
    y = "Odds Ratio (OR)",
    color = "Statistic"
  ) +
  scale_x_discrete(labels = timing_labels) + 
  scale_color_manual(labels = c("Geometric Mean", "Median"), values = c("skyblue", "goldenrod")) +
  theme_minimal()

# Location
surv.means |>
  mutate(LOCATION = if_else(str_detect(PACKET, "adjacent"), "Adjacent", "Contact")) |>
  group_by(LOCATION) |>
  summarise(
    median_or = median(odds.ratio, na.rm = TRUE),
    geometric_mean_or = exp(mean(log(odds.ratio), na.rm = TRUE))
  ) |>
  pivot_longer(
    cols = c(median_or, geometric_mean_or),
    names_to = "statistic",
    values_to = "value"
  ) |>
  ggplot(aes(x = LOCATION, y = value, color = statistic)) +
  geom_point(position = position_dodge(width = 0.3), size = 4, alpha = 0.8) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  labs(
    title = "Overall Seed Survival: Adjacent vs. Contact",
    x = "Location Relative to Carrion",
    y = "Odds Ratio (OR)",
    color = "Statistic"
  ) +
  scale_color_manual(labels = c("Geometric Mean", "Median"), values = c("skyblue", "goldenrod")) +
  theme_minimal()

# Biomass
surv.means |>
  group_by(BIOMASS) |>
  summarise(
    median_or = median(odds.ratio, na.rm = TRUE),
    geometric_mean_or = exp(mean(log(odds.ratio), na.rm = TRUE))
  ) |>
  pivot_longer(
    cols = c(median_or, geometric_mean_or),
    names_to = "statistic",
    values_to = "value"
  ) |>
  ggplot(aes(x = BIOMASS, y = value, color = statistic)) +
  geom_point(position = position_dodge(width = 0.3), size = 4, alpha = 0.8) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  labs(
    title = "Overall Seed Survival: MME vs. Single Carrion",
    x = "Biomass Level",
    y = "Odds Ratio (OR)",
    color = "Statistic"
  ) +
  scale_color_manual(labels = c("Geometric Mean", "Median"), values = c("skyblue", "goldenrod")) +
  theme_minimal()

# Dormancy class, exclusion
surv.means |>
  group_by(DORMANCY.CLASS, EXCLUSION) |>
  summarise(
    median_or = median(odds.ratio, na.rm = TRUE),
    geometric_mean_or = exp(mean(log(odds.ratio), na.rm = TRUE)),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = c(median_or, geometric_mean_or),
    names_to = "statistic",
    values_to = "value"
  ) |>
  ggplot(aes(x = EXCLUSION, y = value, color = statistic)) +
  geom_point(position = position_dodge(width = 0.4), size = 4, alpha = 0.8) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  facet_wrap(~ DORMANCY.CLASS, scales = "free_y") +
  labs(
    title = "Seed Survival by Dormancy and Exclusion Treatment",
    x = "Exclusion Treatment",
    y = "Odds Ratio (OR)",
    color = "Statistic"
  ) +
  scale_color_manual(labels = c("Geometric Mean", "Median"), values = c("skyblue", "goldenrod")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Combine open and herbivore
surv.means |>
  mutate(scavenger_activity = if_else(
    EXCLUSION == "Scavenger exclusion", 
    "Without Scavengers", 
    "With Scavengers"
    )) |>
  group_by(DORMANCY.CLASS, scavenger_activity) |>
  summarise(
    median_or = median(odds.ratio, na.rm = TRUE),
    geometric_mean_or = exp(mean(log(odds.ratio), na.rm = TRUE)),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = c(median_or, geometric_mean_or),
    names_to = "statistic",
    values_to = "value"
  ) |>
  ggplot(aes(x = scavenger_activity, y = value, color = statistic)) +
  geom_point(position = position_dodge(width = 0.4), size = 4, alpha = 0.8) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  facet_wrap(~ DORMANCY.CLASS, scales = "free_y") +
  labs(
    title = "Effect of Scavenger Presence on Seed Survival by Dormancy Class",
    x = "Scavenger Activity",
    y = "Odds Ratio (OR)",
    color = "Statistic"
  ) +
  scale_color_manual(labels = c("Geometric Mean", "Median"), values = c("skyblue", "goldenrod")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Combine open and herbivore add biomass
surv.means |>
  mutate(scavenger_activity = if_else(
    EXCLUSION == "Scavenger exclusion", 
    "Without Scavengers", 
    "With Scavengers"
    )) |>
  group_by(DORMANCY.CLASS, scavenger_activity, BIOMASS) |>
  summarise(
    median_or = median(odds.ratio, na.rm = TRUE),
    geometric_mean_or = exp(mean(log(odds.ratio), na.rm = TRUE)),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = c(median_or, geometric_mean_or),
    names_to = "statistic",
    values_to = "value"
  ) |>
  ggplot(aes(x = scavenger_activity, y = value, color = statistic)) +
  geom_point(position = position_dodge(width = 0.4), size = 4, alpha = 0.8) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  facet_grid(rows = vars(DORMANCY.CLASS), cols = vars(BIOMASS), scales = "free_y") +
  labs(
    title = "Effect of Scavenger Presence by Dormancy and Biomass",
    x = "Scavenger Activity",
    y = "Odds Ratio (OR)",
    color = "Statistic"
  ) +
  scale_color_manual(labels = c("Geometric Mean", "Median"), values = c("skyblue", "goldenrod")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# All factors
surv.means |>
  mutate(scavenger_activity = if_else(
    EXCLUSION == "Scavenger exclusion", 
    "Without Scavengers", 
    "With Scavengers"
    )) |>
  group_by(DORMANCY.CLASS, scavenger_activity, BIOMASS, PACKET) |>
  summarise(
    median_or = median(odds.ratio, na.rm = TRUE),
    geometric_mean_or = exp(mean(log(odds.ratio), na.rm = TRUE)),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = c(median_or, geometric_mean_or),
    names_to = "statistic",
    values_to = "value"
  ) |>
  ggplot(aes(x = scavenger_activity, y = value, color = BIOMASS, shape = statistic)) +
  geom_point(position = position_dodge(width = 0.6), size = 4, alpha = 0.8) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  facet_grid(rows = vars(DORMANCY.CLASS), cols = vars(PACKET), scales = "free_y") +
  labs(
    title = "Seed Survival Across All Interacting Factors",
    x = "Scavenger Activity",
    y = "Odds Ratio (OR)",
    color = "Biomass",
    shape = "Statistic"
  ) +
  scale_color_manual(values = c("High biomass" = "black", "Low biomass" = "grey70")) +
  scale_shape_manual(labels = c("Geometric Mean", "Median"), values = c(16, 17)) + # Circle and Triangle
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 8)) # Adjust facet label size



## --------------- FILTER DATA AND CREATE HEAT MAPS ----------------------------
library(forcats)
library(dplyr)

# Define the absolute min and max
cap_limits <- c(0.1, 10)

# No Dormancy seed rain
surv.mean.nd.rain <- surv.means |>
  filter((DORMANCY.CLASS == "No dormancy" & PACKET == "Rain\nadjacent") |
           (DORMANCY.CLASS == "No dormancy" & PACKET == "Rain\nproximal")) |>
  as.data.frame() |>
  lapply(unlist) |>
  as.data.frame() |>
  mutate(
    or_label = if_else(
      odds.ratio < cap_limits[1] | odds.ratio > cap_limits[2],
      sprintf("%.2e", odds.ratio),
      NA_character_
    ),
    text_color = if_else(odds.ratio < 1, "white", "black")
  )

p1 <- ggplot(surv.mean.nd.rain, aes(x = EXCLUSION, y = BIOMASS)) +
  geom_tile(mapping = aes(fill = odds.ratio), color = "white", size = 0.5) +
  geom_text(aes(label = or_label, color = text_color), size = 3.5, fontface = "bold") +
  scale_color_identity() +
  scale_fill_gradientn(
    colors = c("#440154", "#3B528B", "#21908C", "#5DC863", "#FDE725"),
    values = scales::rescale(c(0.1, sqrt(0.1*1), 1.0, sqrt(1*10), 10)),
    trans = "log10",
    limits = cap_limits,
    oob = scales::squish
  ) +
  theme_classic() +
  facet_grid(PACKET ~ ., space = "free_x", scales = "free_y") +
  theme(
    strip.text = element_blank(),
    panel.spacing = unit(0, "cm"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    legend.position = "none",
    plot.margin = margin(1, 1, 1, 1, "pt")
  )

# No dormancy seed bank
surv.mean.nd.bank <- surv.means |>
  filter((DORMANCY.CLASS == "No dormancy" & PACKET == "Bank\nadjacent") |
           (DORMANCY.CLASS == "No dormancy" & PACKET == "Bank\nproximal")) |>
  as.data.frame() |>
  lapply(unlist) |>
  as.data.frame() |>
  mutate(
    or_label = if_else(
      odds.ratio < cap_limits[1] | odds.ratio > cap_limits[2],
      sprintf("%.2e", odds.ratio),
      NA_character_
    ),
    text_color = if_else(odds.ratio < 1, "white", "black")
  )

p2 <- ggplot(surv.mean.nd.bank, aes(x = EXCLUSION, y = BIOMASS)) +
  geom_tile(mapping = aes(fill = odds.ratio), color = "white", size = 0.5) +
  geom_text(aes(label = or_label, color = text_color), size = 3.5, fontface = "bold") +
  scale_color_identity() +
  scale_fill_gradientn(
    colors = c("#440154", "#3B528B", "#21908C", "#5DC863", "#FDE725"),
    values = scales::rescale(c(0.1, sqrt(0.1*1), 1.0, sqrt(1*10), 10)),
    trans = "log10",
    limits = cap_limits,
    oob = scales::squish
  ) +
  theme_classic() +
  facet_grid(PACKET ~ ., space = "free_x", scales = "free_y") +
  theme(
    strip.text = element_blank(),
    panel.spacing = unit(0, "cm"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    legend.position = "none",
    plot.margin = margin(1, 1, 1, 1, "pt")
  )

# Physiological dormancy seed rain
surv.mean.pd.rain <- surv.means |>
  filter((DORMANCY.CLASS == "Physiological dormancy" & PACKET == "Rain\nadjacent") |
           (DORMANCY.CLASS == "Physiological dormancy" & PACKET == "Rain\nproximal")) |>
  as.data.frame() |>
  lapply(unlist) |>
  as.data.frame() |>
  mutate(
    or_label = if_else(
      odds.ratio < cap_limits[1] | odds.ratio > cap_limits[2],
      sprintf("%.2e", odds.ratio),
      NA_character_
    ),
    text_color = if_else(odds.ratio < 1, "white", "black")
  )

p3 <- ggplot(surv.mean.pd.rain, aes(x = EXCLUSION, y = BIOMASS)) +
  geom_tile(mapping = aes(fill = odds.ratio), color = "white", size = 0.5) +
  geom_text(aes(label = or_label, color = text_color), size = 3.5, fontface = "bold") +
  scale_color_identity() +
  scale_fill_gradientn(
    colors = c("#440154", "#3B528B", "#21908C", "#5DC863", "#FDE725"),
    values = scales::rescale(c(0.1, sqrt(0.1*1), 1.0, sqrt(1*10), 10)),
    trans = "log10",
    limits = cap_limits,
    oob = scales::squish
  ) +
  theme_classic() +
  facet_grid(PACKET ~ ., space = "free_x", scales = "free_y") +
  theme(
    strip.text = element_blank(),
    panel.spacing = unit(0, "cm"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    legend.position = "none",
    plot.margin = margin(1, 1, 1, 1, "pt")
  )

# Physiological dormancy seed bank
surv.mean.pd.bank <- surv.means |>
  filter((DORMANCY.CLASS == "Physiological dormancy" & PACKET == "Bank\nadjacent") |
           (DORMANCY.CLASS == "Physiological dormancy" & PACKET == "Bank\nproximal")) |>
  as.data.frame() |>
  lapply(unlist) |>
  as.data.frame() |>
  mutate(
    or_label = if_else(
      odds.ratio < cap_limits[1] | odds.ratio > cap_limits[2],
      sprintf("%.2e", odds.ratio),
      NA_character_
    ),
    text_color = if_else(odds.ratio < 1, "white", "black")
  )

p4 <- ggplot(surv.mean.pd.bank, aes(x = EXCLUSION, y = BIOMASS)) +
  geom_tile(mapping = aes(fill = odds.ratio), color = "white", size = 0.5) +
  geom_text(aes(label = or_label, color = text_color), size = 3.5, fontface = "bold") +
  scale_color_identity() +
  scale_fill_gradientn(
    colors = c("#440154", "#3B528B", "#21908C", "#5DC863", "#FDE725"),
    values = scales::rescale(c(0.1, sqrt(0.1*1), 1.0, sqrt(1*10), 10)),
    trans = "log10",
    limits = cap_limits,
    oob = scales::squish
  ) +
  theme_classic() +
  facet_grid(PACKET ~ ., space = "free_x", scales = "free_y") +
  theme(
    strip.text = element_blank(),
    panel.spacing = unit(0, "cm"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    legend.position = "none",
    plot.margin = margin(1, 1, 1, 1, "pt")
  )

# Physical dormancy seed rain
surv.mean.py.rain <- surv.means |>
  filter((DORMANCY.CLASS == "Physical dormancy" & PACKET == "Rain\nadjacent") |
           (DORMANCY.CLASS == "Physical dormancy" & PACKET == "Rain\nproximal")) |>
  as.data.frame() |>
  lapply(unlist) |>
  as.data.frame() |>
  mutate(
    or_label = if_else(
      odds.ratio < cap_limits[1] | odds.ratio > cap_limits[2],
      sprintf("%.2e", odds.ratio),
      NA_character_
    ),
    text_color = if_else(odds.ratio < 1, "white", "black")
  )

p5 <- ggplot(surv.mean.py.rain, aes(x = EXCLUSION, y = BIOMASS)) +
  geom_tile(mapping = aes(fill = odds.ratio), color = "white", size = 0.5) +
  geom_text(aes(label = or_label, color = text_color), size = 3.5, fontface = "bold") +
  scale_color_identity() +
  scale_fill_gradientn(
    colors = c("#440154", "#3B528B", "#21908C", "#5DC863", "#FDE725"),
    values = scales::rescale(c(0.1, sqrt(0.1*1), 1.0, sqrt(1*10), 10)),
    trans = "log10",
    limits = cap_limits,
    oob = scales::squish
  ) +
  theme_classic() +
  facet_grid(PACKET ~ ., space = "free_x", scales = "free_y") +
  theme(
    strip.text = element_blank(),
    panel.spacing = unit(0, "cm"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    legend.position = "none",
    plot.margin = margin(1, 1, 1, 1, "pt")
  )

# Physical dormancy seed bank
surv.mean.py.bank <- surv.means |>
  filter((DORMANCY.CLASS == "Physical dormancy" & PACKET == "Bank\nadjacent") |
           (DORMANCY.CLASS == "Physical dormancy" & PACKET == "Bank\nproximal")) |>
  as.data.frame() |>
  lapply(unlist) |>
  as.data.frame() |>
  mutate(
    or_label = if_else(
      odds.ratio < cap_limits[1] | odds.ratio > cap_limits[2],
      sprintf("%.2e", odds.ratio),
      NA_character_
    ),
    text_color = if_else(odds.ratio < 1, "white", "black")
  )

p6 <- ggplot(surv.mean.py.bank, aes(x = EXCLUSION, y = BIOMASS)) +
  geom_tile(mapping = aes(fill = odds.ratio), color = "white", size = 0.5) +
  geom_text(aes(label = or_label, color = text_color), size = 3.5, fontface = "bold") +
  scale_color_identity() +
  scale_fill_gradientn(
    colors = c("#440154", "#3B528B", "#21908C", "#5DC863", "#FDE725"),
    values = scales::rescale(c(0.1, sqrt(0.1*1), 1.0, sqrt(1*10), 10)),
    trans = "log10",
    limits = cap_limits,
    oob = scales::squish
  ) +
  theme_classic() +
  facet_grid(PACKET ~ ., space = "free_x", scales = "free_y") +
  theme(
    strip.text = element_blank(),
    panel.spacing = unit(0, "cm"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    legend.position = "none",
    plot.margin = margin(1, 1, 1, 1, "pt")
  )

# Combine Plots
library(patchwork)
combined <- (p1 / p2) | (p5 / p6) | (p3 / p4)
combined
