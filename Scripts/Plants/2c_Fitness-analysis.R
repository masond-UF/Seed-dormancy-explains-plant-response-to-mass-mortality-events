## --------------- HEADER ------------------------------------------------------
## Script name: 3c_Plant-fitness-analysis.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliation: University of Florida
## Date Created: 2021-11-22
## Date Last modified: 2025-08-14
## Copyright (c) David S. Mason, 2025
## Contact: david.mason@jonesctr.org
## Purpose of script: This script conducts simple linear models on the plant
## fitness data. The output of this script is a spreadsheet containing a summary
## of the model and the model coefficients.

# ---------------- SET—UP WORKSPACE --------------------------------------------
library(tidyverse)
library(tidylog)
library(lubridate)
library(styler)
library(broom)

rm(list = ls())
d <- read.csv("Clean-data/Plants/Plant-fitness.csv")

# ---------------- HEIGHT MODEL ------------------------------------------------

hist(d$HEIGHT)

# Write model
ht.mod <- lm(HEIGHT ~ MH, d = d)

# Check assumptions
summary(ht.mod)
plot(ht.mod)
hist(ht.mod$residuals)
shapiro.test(ht.mod$residuals)
anova(ht.mod)

# Write output
ht.mod.tib <- tidy(ht.mod, conf.int = TRUE)
write.csv(ht.mod.tib, "Analysis/Plants/Fitness-ht-coef.csv")

ht.mod.glance <- glance(ht.mod)
write.csv(ht.mod.glance, "Analysis/Plants/Fitness-ht-summ.csv")

# Grab means
d |>
  group_by(MH) |>
  summarize(
    mean = mean(HEIGHT),
    n = n(),
    se = sd(HEIGHT) / sqrt(n)
  )

# New t-test 
MH <- d |> filter(MH == "Y")
Control <- d |> filter(MH == "N")

t.test(MH$HEIGHT, Control$HEIGHT)

# ---------------- INFLOR MODEL ------------------------------------------------

inflor.mod <- lm(INFLOR ~ MH, d = d)

# Check assumptions
summary(inflor.mod)
plot(inflor.mod)
hist(inflor.mod$residuals)
shapiro.test(inflor.mod$residuals)

anova(inflor.mod)

d |>
  group_by(MH) |>
  dplyr::summarize(
    mean = mean(INFLOR),
    n = n(),
    se = sd(INFLOR) / sqrt(n)
  )

# Write output
inflor.mod.tib <- tidy(inflor.mod, conf.int = TRUE)
write.csv(
  inflor.mod.tib,
  "Analysis/Plants/Fitness-inflor-coef.csv"
)

inflor.mod.glance <- glance(inflor.mod)
write.csv(
  inflor.mod.glance,
  "Analysis/Plants/Fitness-inflor-summ.csv"
)

# New t-test 
MH <- d |> filter(MH == "Y")
Control <- d |> filter(MH == "N")

poisson.test(c(sum(MH$INFLOR), sum(Control$INFLOR)))
