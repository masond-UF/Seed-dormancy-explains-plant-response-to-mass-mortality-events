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

ref <- seed.surv %>% 
	filter(EXCLUSION == "Reference")

ref <- ref %>% 
	group_by(SITE, BIOMASS, EXCLUSION, PACKET, DORMANCY.CLASS, SPECIES) %>% 
	dplyr::summarize(SURV = sum(FINAL.STATUS),
						TRIALS = n()) 

ref <- ref %>% 
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

ref.means <- ref.means %>%
  mutate(DORMANCY.CLASS = fct_recode(DORMANCY.CLASS,
    "No dormancy"          = "ND",
    "Physiological dormancy" = "PD",
    "Physical dormancy"      = "PY"
  ))

ref.means <- ref.means |>
	dplyr::select(DORMANCY.CLASS, PACKET, prob)


# Make a helper column for merging
surv.means <- surv.means %>%
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

## --------------- MEDIAN VALUES -----------------------------------------------

surv.means$BIOMASS <- as_factor(surv.means$BIOMASS)
levels(surv.means$BIOMASS)
surv.means$PACKET <- as_factor(surv.means$PACKET)
levels(surv.means$PACKET)

# Reorder factor levels
colnames(surv.means)[4] <- 'EXCLUSION'
surv.means$EXCLUSION <- factor(surv.means$EXCLUSION, 
														 levels = c("Open", "Scavenger exclusion",
														 					 "Herbivore exclusion"))

geometric_mean <- function(x) {
  # Add a small constant to avoid log(0)
  x[x == 0] <- 1e-9 
  exp(mean(log(x)))
}

geometric_mean_alt <- function(x) {
  # Handle zero values to avoid product becoming zero
  x[x == 0] <- 1e-9 
  # Calculate the product of all numbers
  product_of_x <- prod(x)
  # Count the number of values
  n <- length(x)
  # Take the nth root of the product
  product_of_x^(1/n)
}

# Physical dormancy
surv.means |>
  filter(DORMANCY.CLASS == "Physical dormancy", EXCLUSION == "Scavenger exclusion") |>
  summarise(median_or = median(odds.ratio),
  					gm = geometric_mean(odds.ratio),
  					min = min(odds.ratio),
  					max = max(odds.ratio))
  
# No dormancy with scavenger exclusion
surv.means |>
  filter(DORMANCY.CLASS == "No dormancy", EXCLUSION == "Scavenger exclusion") |>
  summarise(median_or = median(odds.ratio),
  					gm = geometric_mean(odds.ratio),
  					min = min(odds.ratio),
  					max = max(odds.ratio))

# Physiological dormancy scavenger
surv.means |>
  filter(DORMANCY.CLASS == "Physiological dormancy", EXCLUSION == "Scavenger exclusion") |>
  summarise(median_or = median(odds.ratio),
  					gm = geometric_mean(odds.ratio),
  					min = min(odds.ratio),
  					max = max(odds.ratio))

# Overall seed rain and seed bank
surv.means |>
  filter(TMP.PACKET == "A") |>
  summarise(median_or = median(odds.ratio),
  					gm = geometric_mean(odds.ratio),
  					min = min(odds.ratio),
  					max = max(odds.ratio))
surv.means |>
  filter(TMP.PACKET == "B") |>
  summarise(median_or = median(odds.ratio),
  					gm = geometric_mean(odds.ratio),
  					min = min(odds.ratio),
  					max = max(odds.ratio))

# Seed rain adjacent
surv.means |>
  filter(TMP.PACKET == "A", PACKET == "Rain\nadjacent") |>
  summarise(median_or = median(odds.ratio),
  					gm = geometric_mean(odds.ratio),
  					min = min(odds.ratio),
  					max = max(odds.ratio))
surv.means |>
  filter(TMP.PACKET == "A", PACKET == "Rain\nadjacent", EXCLUSION %in% c("Open", "Herbivore exclusion")) |>
  summarise(median_or = median(odds.ratio),
  					gm = geometric_mean(odds.ratio),
  					min = min(odds.ratio),
  					max = max(odds.ratio))

# Seed rain contact
surv.means |>
  filter(TMP.PACKET == "A", PACKET == "Rain\nproximal") |>
  summarise(median_or = median(odds.ratio),
  					gm = geometric_mean(odds.ratio),
  					min = min(odds.ratio),
  					max = max(odds.ratio))
surv.means |>
  filter(TMP.PACKET == "A", PACKET == "Rain\nproximal", EXCLUSION %in% c("Open", "Herbivore exclusion")) |>
  summarise(median_or = median(odds.ratio),
  					gm = geometric_mean(odds.ratio),
  					min = min(odds.ratio),
  					max = max(odds.ratio))

# Scavenger access seed rain adjacent compared to bank
surv.means |>
  filter(TMP.PACKET == "A", DORMANCY.CLASS == "No dormancy", PACKET == "Rain\nadjacent", EXCLUSION %in% c("Open", "Herbivore exclusion")) |>
  summarise(median_or = median(odds.ratio),
  					gm = geometric_mean(odds.ratio),
  					min = min(odds.ratio),
  					max = max(odds.ratio))
surv.means |>
  filter(DORMANCY.CLASS == "No dormancy", TMP.PACKET == "A") |>
  summarise(median_or = median(odds.ratio),
  					gm = geometric_mean(odds.ratio),
  					min = min(odds.ratio),
  					max = max(odds.ratio))
surv.means |>
  filter(DORMANCY.CLASS == "No dormancy", TMP.PACKET == "B") |>
  summarise(median_or = median(odds.ratio),
  					gm = geometric_mean(odds.ratio),
  					min = min(odds.ratio),
  					max = max(odds.ratio))

surv.means |>
  filter(TMP.PACKET == "A", DORMANCY.CLASS == "Physical dormancy", PACKET == "Rain\nadjacent", EXCLUSION %in% c("Open", "Herbivore exclusion")) |>
  summarise(median_or = median(odds.ratio),
  					gm = geometric_mean(odds.ratio),
  					min = min(odds.ratio),
  					max = max(odds.ratio))
surv.means |>
  filter(DORMANCY.CLASS == "Physical dormancy", TMP.PACKET == "A") |>
  summarise(median_or = median(odds.ratio),
  					gm = geometric_mean(odds.ratio),
  					min = min(odds.ratio),
  					max = max(odds.ratio))
surv.means |>
  filter(DORMANCY.CLASS == "Physical dormancy", TMP.PACKET == "B") |>
  summarise(median_or = median(odds.ratio),
  					gm = geometric_mean(odds.ratio),
  					min = min(odds.ratio),
  					max = max(odds.ratio))

surv.means |>
  filter(TMP.PACKET == "A", DORMANCY.CLASS == "Physiological dormancy", PACKET == "Rain\nadjacent", EXCLUSION %in% c("Open", "Herbivore exclusion")) |>
  summarise(median_or = median(odds.ratio),
  					gm = geometric_mean(odds.ratio),
  					min = min(odds.ratio),
  					max = max(odds.ratio))
surv.means |>
  filter(DORMANCY.CLASS == "Physiological dormancy", TMP.PACKET == "A") |>
  summarise(median_or = median(odds.ratio),
  					gm = geometric_mean(odds.ratio),
  					min = min(odds.ratio),
  					max = max(odds.ratio))
surv.means |>
  filter(DORMANCY.CLASS == "Physiological dormancy", TMP.PACKET == "B") |>
  summarise(median_or = median(odds.ratio),
  					gm = geometric_mean(odds.ratio),
  					min = min(odds.ratio),
  					max = max(odds.ratio))

## --------------- FILTER DATA AND CREATE HEAT MAPS ----------------------------
library(forcats)
library(dplyr)

# Define the absolute min and max
cap_limits <- c(0.1, 10)

# No Dormancy seed rain
surv.mean.nd.rain <- surv.means %>%
  filter((DORMANCY.CLASS == "No dormancy" & PACKET == "Rain\nadjacent") |
           (DORMANCY.CLASS == "No dormancy" & PACKET == "Rain\nproximal")) %>%
  as.data.frame() %>%
  lapply(unlist) %>%
  as.data.frame() %>%
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
surv.mean.nd.bank <- surv.means %>%
  filter((DORMANCY.CLASS == "No dormancy" & PACKET == "Bank\nadjacent") |
           (DORMANCY.CLASS == "No dormancy" & PACKET == "Bank\nproximal")) %>%
  as.data.frame() %>%
  lapply(unlist) %>%
  as.data.frame() %>%
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
surv.mean.pd.rain <- surv.means %>%
  filter((DORMANCY.CLASS == "Physiological dormancy" & PACKET == "Rain\nadjacent") |
           (DORMANCY.CLASS == "Physiological dormancy" & PACKET == "Rain\nproximal")) %>%
  as.data.frame() %>%
  lapply(unlist) %>%
  as.data.frame() %>%
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
surv.mean.pd.bank <- surv.means %>%
  filter((DORMANCY.CLASS == "Physiological dormancy" & PACKET == "Bank\nadjacent") |
           (DORMANCY.CLASS == "Physiological dormancy" & PACKET == "Bank\nproximal")) %>%
  as.data.frame() %>%
  lapply(unlist) %>%
  as.data.frame() %>%
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
surv.mean.py.rain <- surv.means %>%
  filter((DORMANCY.CLASS == "Physical dormancy" & PACKET == "Rain\nadjacent") |
           (DORMANCY.CLASS == "Physical dormancy" & PACKET == "Rain\nproximal")) %>%
  as.data.frame() %>%
  lapply(unlist) %>%
  as.data.frame() %>%
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
surv.mean.py.bank <- surv.means %>%
  filter((DORMANCY.CLASS == "Physical dormancy" & PACKET == "Bank\nadjacent") |
           (DORMANCY.CLASS == "Physical dormancy" & PACKET == "Bank\nproximal")) %>%
  as.data.frame() %>%
  lapply(unlist) %>%
  as.data.frame() %>%
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

## --------------- MEDIAN COMPARISONS ------------------------------------------

# Filter the data for only the "Scavenger exclusion" treatments
surv.means |>
  filter(EXCLUSION == "Scavenger exclusion") |>
  group_by(DORMANCY.CLASS) %>%
  summarise(
    median_odds_ratio = median(odds.ratio)
  )

# Define the grouping variables for each comparison
comparison_groups <- list(
  "Overall Seed Rain" = quote(TMP.PACKET == 'A'),
  "Overall Seed Bank" = quote(TMP.PACKET == 'B'),
  "Seed Rain - Adjacent" = quote(TMP.PACKET == 'A' & PACKET == 'Rain\nadjacent'),
  "Seed Rain - Adjacent with Scavengers" = quote(TMP.PACKET == 'A' & PACKET == 'Rain\nadjacent' & EXCLUSION %in% c('Open', 'Herbivore exclusion')),
  "Seed Rain - Contact" = quote(TMP.PACKET == 'A' & PACKET == 'Rain\nproximal'),
  "Seed Rain - Contact with Scavengers" = quote(TMP.PACKET == 'A' & PACKET == 'Rain\nproximal' & EXCLUSION %in% c('Open', 'Herbivore exclusion')),
  "Seed Rain - Adjacent, Scavengers (ND only)" = quote(TMP.PACKET == 'A' & PACKET == 'Rain\nadjacent' & EXCLUSION %in% c('Open', 'Herbivore exclusion') & DORMANCY.CLASS == 'No dormancy'),
  "Seed Rain - Physical Dormancy" = quote(TMP.PACKET == 'A' & DORMANCY.CLASS == 'Physical dormancy'),
  "Seed Rain - Physiological Dormancy" = quote(TMP.PACKET == 'A' & DORMANCY.CLASS == 'Physiological dormancy'),
  "Seed Bank - Physical Dormancy" = quote(TMP.PACKET == 'B' & DORMANCY.CLASS == 'Physical dormancy'),
  "Seed Bank - Physiological Dormancy" = quote(TMP.PACKET == 'B' & DORMANCY.CLASS == 'Physiological dormancy')
)

# Function to calculate median OR for a given filter condition
calculate_median <- function(data, filter_condition) {
  data %>%
    filter(!!filter_condition) %>%
    summarise(median_or = median(odds.ratio, na.rm = TRUE)) %>%
    pull(median_or)
}

# Apply the function to each group and store the results
results <- purrr::map_dfr(comparison_groups, ~{
  tibble(median_or = calculate_median(surv.means, .x))
}, .id = "Comparison")

# Print the final table
print(results)


