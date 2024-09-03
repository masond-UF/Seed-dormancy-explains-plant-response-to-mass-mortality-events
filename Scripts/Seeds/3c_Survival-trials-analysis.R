## --------------- HEADER ------------------------------------------------------
## Script name: 3c_Seed-survival-analysis.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliation: University of Florida
## Date Created: 2022-05-13
## Date Last modified: 2022-12-21
## Copyright (c) David S. Mason, 2022
## Contact: masond@ufl.edu, @EcoGraffito
## Purpose of script: This will be a script for analyzing the seed survival data

## --------------- SET—UP WORKSPACE --------------------------------------------

# Clear the decks
rm(list = ls())

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

options(scipen = 999)

# Bring in the data
seed.surv <- read.csv("Animals-plants-seeds/Clean-data/Seeds/Seed-survival.csv")

## --------------- PREPARE THE DATA --------------------------------------------

seed.surv <- seed.surv %>% drop_na(FINAL.STATUS.NA)

# Set aside the reference plots
seed.surv.ref <- seed.surv %>%
  filter(PLOT == "REF")

# Filter out the reference
seed.surv <- seed.surv %>%
  filter(PLOT != "REF")

seed.surv <- seed.surv %>%
  group_by(SITE, PLOT, BIOMASS, TREATMENT, PACKET, TYPE, SPECIES) %>%
  summarize(
    SURV = sum(FINAL.STATUS),
    TRIALS = n()
  )

# Softening values
# for(i in 1:nrow(seed.surv)){
# if(seed.surv$SURV[i] == 10){
# seed.surv$SURV[i] <- sample(7:9, 1)
# }
# if(seed.surv$SURV[i] == 0){
# seed.surv$SURV[i] <- sample(0:2, 1)
# }
# }

# Add values for failed trials
seed.surv <- seed.surv %>%
  mutate(DEAD = TRIALS - SURV)

# Combine the carrion/exclusion treatments
# seed.surv <- seed.surv %>%
# tidyr::unite(COMB.TRMT, BIOMASS, TREATMENT, sep = " ", remove = FALSE)

# Separate packet
seed.surv <- seed.surv %>% separate(PACKET, c("TIMING", "LOCATION"))


## --------------- CONSTRUCT THE MAIN MODEL ------------------------------------

m1 <- glmer(cbind(SURV, DEAD) ~ BIOMASS * TREATMENT * TYPE + TYPE * TIMING + LOCATION + (1 | SITE),
  data = seed.surv, family = binomial, weights = TRIALS
)

r.squaredGLMM(m1) # Fixed effects 19-28%, 8-12%
# Answers are different than they were before??

anova.df <- as.data.frame(Anova(m1))
m1.sum <- broom::tidy(m1, conf.int = TRUE)

red.mof <- glmer(cbind(SURV, DEAD) ~ TYPE + TYPE * TIMING + LOCATION + (1 | SITE),
  data = seed.surv.ref, family = binomial, weights = TRIALS
)

## --------------- SUMMARY COMPARISONS -----------------------------------------

# Dormancy class means
surv.trials.comp <- emmeans(m1, pairwise ~ TYPE, type = "response")
surv.trials.comp <- as.data.frame(surv.trials.comp[["contrasts"]])

# Location means
surv.trials.comp <- emmeans(m1, pairwise ~ LOCATION, type = "response")
surv.trials.comp <- as.data.frame(surv.trials.comp[["contrasts"]])

# Dormancy class and location means
surv.trials.comp <- emmeans(m1, pairwise ~ TYPE * TIMING, type = "response")
surv.trials.comp <- as.data.frame(surv.trials.comp[["contrasts"]])

# Dormancy class and location means
surv.trials.comp <- emmeans(m1, pairwise ~ TYPE * TREATMENT, type = "response")
surv.trials.comp <- as.data.frame(surv.trials.comp[["emmeans"]])

# ND interaction term comparison
surv.trials.comp <- emmeans(m1, pairwise ~ BIOMASS * TREATMENT * TYPE + TYPE * TIMING + LOCATION, type = "response")
surv.trials.comp <- as.data.frame(surv.trials.comp[["emmeans"]])
surv.trials.means <- surv.trials.comp %>% filter(TYPE == "No dormancy")

# PY interaction term comparison
surv.trials.comp <- emmeans(m1, pairwise ~ BIOMASS * TREATMENT * TYPE + TYPE * TIMING + LOCATION, type = "response")
surv.trials.comp <- as.data.frame(surv.trials.comp[["emmeans"]])
surv.trials.means <- surv.trials.comp %>% filter(TYPE == "Physical dormancy")

emmeans(m1, pairwise ~ BIOMASS * TREATMENT * TYPE, type = "response")


## --------------- VISUALIZE THE MEANS -----------------------------------------

# Model means
surv.trials.comp <- emmeans(m1, pairwise ~ BIOMASS * TREATMENT * TYPE + TYPE * TIMING + LOCATION, type = "response")
surv.trials.means <- as.data.frame(surv.trials.comp[["emmeans"]])

surv.trials.means <- surv.trials.means %>% 
  mutate(LOCATION = fct_recode(as.factor(LOCATION),
                      Adjacent = "adjacent",
                      Proximal = "proximal")) |>
	mutate(TIMING = fct_recode(as.factor(TIMING),
                      'Seed bank' = "Bank",
                      'Seed rain' = "Rain")) |>
	mutate(BIOMASS = fct_recode(as.factor(BIOMASS),
                      MME = "High biomass",
                      Single = "Low biomass")) |>
	mutate(TREATMENT = factor(TREATMENT, levels = c("Open",
																								"Herbivore exclusion",
																								"Scavenger exclusion"))) |>
	mutate(BIOMASS = factor(BIOMASS, levels = c("Single", "MME")))


# yellow is open
# green herb
# purple scavenger

nd.means <- surv.trials.means %>% filter(TYPE == "No dormancy")
nd.plot <- ggplot(nd.means, aes(x = BIOMASS, y = prob, color = TREATMENT)) +
	scale_color_viridis_d(direction = -1)+
  geom_linerange(aes(ymin = prob - SE, ymax = prob + SE),
    position = position_dodge(0.5), linewidth = 2
  ) +
  geom_point(position = position_dodge(0.5)) +
  facet_wrap(~ TIMING * LOCATION) +
	theme_bw () +
	scale_y_continuous(limits = c(0,1)) +
	ylab("Probability of survival")+
	xlab("Carcass biomass")+
	theme(strip.background =element_rect(fill="black"),
				strip.text =element_text(color="white"),
				legend.position = 'none')+
  ggtitle("No dormancy")

pd.means <- surv.trials.means %>% filter(TYPE == "Physiological dormancy")
pd.plot <- ggplot(pd.means, aes(x = BIOMASS, y = prob, color = TREATMENT)) +
	scale_color_viridis_d(direction = -1)+
  geom_linerange(aes(ymin = prob - SE, ymax = prob + SE),
    position = position_dodge(0.5), linewidth = 2
  ) +
  geom_point(position = position_dodge(0.5)) +
  facet_wrap(~ TIMING * LOCATION) +
	theme_bw () +
	scale_y_continuous(limits = c(0,1)) +
	ylab("Probability of survival")+
	xlab("Carcass biomass")+
	theme(strip.background =element_rect(fill="black"),
				strip.text =element_text(color="white"),
				legend.position = 'none')+
  ggtitle("Physiological dormancy")

py.means <- surv.trials.means %>% filter(TYPE == "Physical dormancy")
py.plot <- ggplot(py.means, aes(x = BIOMASS, y = prob, color = TREATMENT)) +
	scale_color_viridis_d(direction = -1)+
  geom_linerange(aes(ymin = prob - SE, ymax = prob + SE),
    position = position_dodge(0.5), linewidth = 2
  ) +
  geom_point(position = position_dodge(0.5)) +
  facet_wrap(~ TIMING * LOCATION) +
	theme_bw () +
	scale_y_continuous(limits = c(0,1))+
	ylab("Probability of survival")+
	xlab("Carcass biomass")+
	theme(strip.background =element_rect(fill="black"),
				strip.text =element_text(color="white"),
				legend.position = 'none')+
  ggtitle("Physical dormancy")

library(patchwork)
nd.plot/pd.plot/py.plot

## --------------- p values for contrasts --------------------------------------

# Contrasts
surv.trials.comp <- as.data.frame(surv.trials.comp[["contrasts"]])

surv.trials.comp <- surv.trials.comp %>%
  separate(contrast, c("First", "Second"), sep = "/") %>%
  separate(First, c("First.biomass", "First.exclusion", "First.type", "First.timing", "First.location"),
    sep = ","
  ) %>%
  separate(Second, c("Second.biomass", "Second.exclusion", "Second.type", "Second.timing", "Second.location"),
    sep = ","
  )

ND.comp.bank <- surv.trials.comp %>%
  filter(First.type == "No dormancy" & Second.type == "No dormancy") %>%
  filter(First.timing == "Bank" & Second.timing == "Bank")

## --------------- SAVE MODEL RESULTS ------------------------------------------

# Model summary
write.csv(m1.sum, "Animals-plants-seeds/Analysis/Seeds/Seed-survival-model-summ.csv",
  row.names = FALSE
)

# Pairwise comparison
write.csv(surv.trials.comp, "Animals-plants-seeds/Analysis/Seeds/Seed-survival-pairwise.csv",
  row.names = FALSE
)

# Anova output
write.csv(anova.df, "Animals-plants-seeds/Analysis/Seeds/Seed-survival-pairwise.csv",
  row.names = FALSE
)

# Means
surv.trials.means$BIOMASS <- NA
surv.trials.means$TREATMENT <- NA

for (i in 1:nrow(surv.trials.means)) {
  if (surv.trials.means$COMB.TRMT[i] == "High biomass Herbivore exclusion") {
    surv.trials.means$BIOMASS[i] <- "Mass mortality"
    surv.trials.means$TREATMENT[i] <- "Herbivore exclusion"
  }
  if (surv.trials.means$COMB.TRMT[i] == "High biomass Open") {
    surv.trials.means$BIOMASS[i] <- "Mass mortality"
    surv.trials.means$TREATMENT[i] <- "No exclusion"
  }
  if (surv.trials.means$COMB.TRMT[i] == "High biomass Scavenger exclusion") {
    surv.trials.means$BIOMASS[i] <- "Mass mortality"
    surv.trials.means$TREATMENT[i] <- "Scavenger exclusion"
  }
  if (surv.trials.means$COMB.TRMT[i] == "Low biomass Herbivore exclusion") {
    surv.trials.means$BIOMASS[i] <- "Single carrion"
    surv.trials.means$TREATMENT[i] <- "Herbivore exclusion"
  }
  if (surv.trials.means$COMB.TRMT[i] == "Low biomass Open") {
    surv.trials.means$BIOMASS[i] <- "Single carrion"
    surv.trials.means$TREATMENT[i] <- "No exclusion"
  }
  if (surv.trials.means$COMB.TRMT[i] == "Low biomass Scavenger exclusion") {
    surv.trials.means$BIOMASS[i] <- "Single carrion"
    surv.trials.means$TREATMENT[i] <- "Scavenger exclusion"
  }
}

surv.trials.means$DIST <- NA
surv.trials.means$TIME <- NA

for (i in 1:nrow(surv.trials.means)) {
  if (surv.trials.means$PACKET[i] == "Bank\nadjacent" | surv.trials.means$PACKET[i] == "Bank\nproximal") {
    surv.trials.means$TIME[i] <- "Seed bank"
  }
  if (surv.trials.means$PACKET[i] == "Rain\nadjacent" | surv.trials.means$PACKET[i] == "Rain\nproximal") {
    surv.trials.means$TIME[i] <- "Seed rain"
  }
  if (surv.trials.means$PACKET[i] == "Rain\nadjacent" | surv.trials.means$PACKET[i] == "Bank\nadjacent") {
    surv.trials.means$DIST[i] <- "Adjacent"
  }
  if (surv.trials.means$PACKET[i] == "Bank\nproximal" | surv.trials.means$PACKET[i] == "Rain\nproximal") {
    surv.trials.means$DIST[i] <- "Proximal"
  }
}


surv.trials.means <- surv.trials.means %>%
  select(BIOMASS, TREATMENT, PACKET, TYPE, prob, std.error, p.value)

write.csv(surv.trials.means, "Animals-plants-seeds/Analysis/Seeds/Seed-survival-means.csv",
  row.names = FALSE
)
