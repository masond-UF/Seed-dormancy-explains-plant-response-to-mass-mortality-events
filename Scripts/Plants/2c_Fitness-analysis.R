## --------------- HEADER ------------------------------------------------------
## Script name: 3c_Plant-fitness-analysis.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliation: University of Florida
## Date Created: 2021-11-22
## Date Last modified: 2022-05-02
## Copyright (c) David S. Mason, 2021
## Contact: masond@ufl.edu, @EcoGraffito
## Purpose of script: This script conducts simple linear models on the plant
## fitness data. The output of this script is a spreadsheet containing a summary
## of the model and the model coefficients.

################## SET—UP WORKSPACE --------------------------------------------
library(tidyverse)
library(tidylog)
library(lubridate)
library(styler)
library(broom)

rm(list = ls())
ht <- read.csv("Animals-plants-seeds/Clean-data/Plants/Plant-fitness.csv") |>
	filter(SPECIES == "LASE")
inflor <- ht %>% filter(SPECIES == "LASE")

################## HEIGHT MODEL ------------------------------------------------

# Write model
ht.mod <- lm(HEIGHT ~ MH, d = ht)

# Check assumptions
summary(ht.mod)
plot(ht.mod)
hist(ht.mod$residuals)
shapiro.test(ht.mod$residuals)
anova(ht.mod)

# Write output
ht.mod.tib <- tidy(ht.mod, conf.int = TRUE)
write.csv(ht.mod.tib, "Animals-plants-seeds/Analysis/Plants/Fitness-ht-coef.csv")

ht.mod.glance <- glance(ht.mod)
write.csv(ht.mod.glance, "Animals-plants-seeds/Analysis/Plants/Fitness-ht-summ.csv")

# Grab means
ht %>%
  group_by(MH, SPECIES) %>%
  summarize(
    mean = mean(HEIGHT),
    n = n(),
    se = sd(HEIGHT) / sqrt(n)
  )1

# New t-test 
MH <- ht |> filter(MH == "Y")
Control <- ht |> filter(MH == "N")

t.test(MH$HEIGHT, Control$HEIGHT)

################## INFLOR MODEL ------------------------------------------------

inflor.mod <- lm(INFLOR ~ MH, d = inflor)

# Check assumptions
summary(inflor.mod)
plot(inflor.mod)
hist(inflor.mod$residuals)
shapiro.test(inflor.mod$residuals)

anova(inflor.mod)

inflor %>%
  group_by(MH) %>%
  summarize(
    mean = mean(INFLOR),
    n = n(),
    se = sd(INFLOR) / sqrt(n)
  )

# Write output
inflor.mod.tib <- tidy(inflor.mod, conf.int = TRUE)
write.csv(
  inflor.mod.tib,
  "Animals-plants-seeds/Analysis/Plants/Fitness-inflor-coef.csv"
)

inflor.mod.glance <- glance(inflor.mod)
write.csv(
  inflor.mod.glance,
  "Animals-plants-seeds/Analysis/Plants/Fitness-inflor-summ.csv"
)

# New t-test 
MH <- inflor |> filter(MH == "Y")
Control <- inflor |> filter(MH == "N")

t.test(MH$INFLOR, Control$INFLOR)
