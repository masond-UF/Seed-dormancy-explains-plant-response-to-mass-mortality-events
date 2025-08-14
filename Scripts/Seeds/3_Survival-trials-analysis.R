## --------------- HEADER ------------------------------------------------------
## Script name: 3_Seed-survival-analysis.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliation: University of Florida
## Date Created: 2022-05-13
## Date Last Modified: 2025-8-13
## Copyright (c) David S. Mason, 2025
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

seed.surv <- seed.surv %>%
  group_by(SITE, PLOT, BIOMASS, EXCLUSION, PACKET, DORMANCY.CLASS, SPECIES) %>%
  summarize(
    SURV = sum(FINAL.STATUS),
    TRIALS = n()
  )

# Add values for failed trials
seed.surv <- seed.surv %>%
  mutate(DEAD = TRIALS - SURV)

# Set aside the reference plots
seed.surv.ref <- seed.surv %>%
  filter(PLOT == "REF")

# Filter out the reference
seed.surv <- seed.surv %>%
  filter(PLOT != "REF")

# Separate packet
seed.surv <- seed.surv %>% separate(PACKET, c("TIMING", "LOCATION"))

## --------------- CONSTRUCT THE MAIN MODEL ------------------------------------

m1 <- glmer(cbind(SURV, DEAD) ~ BIOMASS * EXCLUSION * DORMANCY.CLASS + DORMANCY.CLASS * TIMING + LOCATION + (1 | SITE),
  data = seed.surv, family = binomial)

ncol(model.matrix(m1))

r.squaredGLMM(m1) # Fixed effects 48-58, Random = 13-15%
# Answers are different than they were before??

options(contrasts = c("contr.sum", "contr.poly"))
anova.df <- as.data.frame(Anova(m1, type = 3))
m1.sum <- broom::tidy(m1, conf.int = TRUE)

ref.mod <- glmer(cbind(SURV, DEAD) ~ DORMANCY.CLASS + DORMANCY.CLASS * PACKET + (1 | SITE),
  data = seed.surv.ref, family = binomial
)

library(performance)
check_model(m1)

library(DHARMa)
nrow(model.frame(m1))   # how many obs used in model
nrow(seed.surv)         # how many in original data

sim <- simulateResiduals(fittedModel = m1, plot = TRUE)
plot(sim)

## --------------- SUMMARY COMPARISONS -----------------------------------------

# Dormancy class means
surv.trials.comp <- emmeans(m1, pairwise ~ DORMANCY.CLASS, type = "response")
surv.trials.comp <- as.data.frame(surv.trials.comp[["contrasts"]])

# Biomass and exclusion
surv.trials.comp <- emmeans(m1, pairwise ~ BIOMASS*EXCLUSION, type = "response")
surv.trials.comp <- as.data.frame(surv.trials.comp[["contrasts"]])

# Exclusion
surv.trials.comp <- emmeans(m1, pairwise ~ BIOMASS, type = "response")
surv.trials.comp <- as.data.frame(surv.trials.comp[["contrasts"]])

# Timing means
surv.trials.comp <- emmeans(m1, pairwise ~ TIMING, type = "response")
surv.trials.comp <- as.data.frame(surv.trials.comp[["contrasts"]])

# Location means
surv.trials.comp <- emmeans(m1, pairwise ~ LOCATION, type = "response")
surv.trials.comp <- as.data.frame(surv.trials.comp[["contrasts"]])

# Dormancy class and timing means
surv.trials.comp <- emmeans(m1, pairwise ~ DORMANCY.CLASS * TIMING, type = "response")
surv.trials.comp <- as.data.frame(surv.trials.comp[["contrasts"]])

# Dormancy class and exclusion means
surv.trials.comp <- emmeans(m1, pairwise ~ DORMANCY.CLASS * TREATMENT, type = "response")
surv.trials.comp <- as.data.frame(surv.trials.comp[["emmeans"]])

# ND interaction term comparison
surv.trials.comp <- emmeans(m1, pairwise ~ BIOMASS * EXCLUSION * DORMANCY.CLASS + DORMANCY.CLASS * TIMING + LOCATION, type = "response")
surv.trials.comp <- as.data.frame(surv.trials.comp[["emmeans"]])
surv.trials.means <- surv.trials.comp %>% filter(DORMANCY.CLASS == "No dormancy")

# PY interaction term comparison
surv.trials.comp <- emmeans(m1, pairwise ~ BIOMASS * EXCLUSION * DORMANCY.CLASS + DORMANCY.CLASS * TIMING + LOCATION, type = "response")
surv.trials.comp <- as.data.frame(surv.trials.comp[["emmeans"]])
surv.trials.means <- surv.trials.comp %>% filter(DORMANCY.CLASS == "Physical dormancy")

surv.trials.comp <- emmeans(m1, pairwise ~ BIOMASS * TREATMENT * DORMANCY.CLASS, type = "response")


## --------------- VISUALIZE THE MEANS -----------------------------------------

# Model means
surv.trials.comp <- emmeans(m1, pairwise ~ BIOMASS * EXCLUSION * DORMANCY.CLASS + DORMANCY.CLASS * TIMING + LOCATION, type = "response")
surv.trials.means <- as.data.frame(surv.trials.comp[["emmeans"]])

surv.trials.means <- surv.trials.means %>% 
  mutate(LOCATION = fct_recode(as.factor(LOCATION),
                      Adjacent = "adjacent",
                      Proximal = "proximal")) |>
	mutate(TIMING = fct_recode(as.factor(TIMING),
                      'Seed bank' = "Bank",
                      'Seed rain' = "Rain")) |>
	mutate(BIOMASS = fct_recode(as.factor(BIOMASS),
                      MME = "MME",
                      Single = "Single carcass")) |>
	mutate(EXCLUSION = factor(EXCLUSION, levels = c("Open",
																								"Herbivore",
																								"Scavenger"))) |>
	mutate(BIOMASS = factor(BIOMASS, levels = c("Single", "MME")))


# yellow is open
# green herb
# purple scavenger

nd.means <- surv.trials.means %>% filter(DORMANCY.CLASS == "No dormancy")
nd.plot <- ggplot(nd.means, aes(x = BIOMASS, y = prob, color = EXCLUSION)) +
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

pd.means <- surv.trials.means %>% filter(DORMANCY.CLASS == "Physiological dormancy")
pd.plot <- ggplot(pd.means, aes(x = BIOMASS, y = prob, color = EXCLUSION)) +
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

py.means <- surv.trials.means %>% filter(DORMANCY.CLASS == "Physical dormancy")
py.plot <- ggplot(py.means, aes(x = BIOMASS, y = prob, color = EXCLUSION)) +
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
write.csv(m1.sum, "Analysis/Seeds/Seed-survival-model-summ.csv",
  row.names = FALSE
)

# Anova output
write.csv(anova.df, "Analysis/Seeds/Seed-survival-pairwise.csv",
  row.names = FALSE
)

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
  select(BIOMASS, EXCLUSION, PACKET, DORMANCY.CLASS, prob, std.error, p.value)

write.csv(surv.trials.means, "Analysis/Seeds/Seed-survival-means.csv",
  row.names = FALSE
)
