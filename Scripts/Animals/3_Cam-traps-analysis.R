## --------------- HEADER ------------------------------------------------------
## Script name: 3_Cam-traps-analysis.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliation: University of Florida
## Date Created: 2021-11-23
## Date Last Modified: 2025-08-13
## Copyright (c) David S. Mason, 2025
## Contact: david.mason@jonesctr.org
## Purpose of script: This script conducts a basic general linear model on the
## camera trap data. The output of this script (should be) should be the model
## summary and coefficients.

## --------------- SET—UP WORKSPACE --------------------------------------------

library(tidyverse)
library(tidylog)
library(lubridate)
library(styler)
library(DataExplorer)

# Clear the decks
rm(list = ls())

# Bring in the data
cam <- read.csv("Clean-data/Animals/Camera-traps.csv")

cam$DATE <- as_date(cam$DATE)

## --------------- SUBSET THE DATA ---------------------------------------------

# Filter the scavenger exclusion period 21 days
decomp <- cam %>%
  filter(DATE >= "2019-04-15" & DATE <= "2019-05-05")

# Filter the herbivory period
herbivory <- cam %>%
  filter(DATE > "2019-05-05")

## --------------- SUMMARIZE DECOMP DATA ---------------------------------------

decomp.sum <- decomp %>%
  group_by(SITE, TREATMENT, BIOMASS, EXCLUSION, FUNCTIONAL) %>%
  summarize(Total.detections = n())

decomp.sum.wd <- decomp.sum %>%
  pivot_wider(names_from = FUNCTIONAL, values_from = Total.detections)

decomp.sum.wd[is.na(decomp.sum.wd)] <- 0

decomp.sum <- decomp.sum.wd %>%
  pivot_longer(5:7, names_to = "FUNCTIONAL", values_to = "Total.detections")

decomp.sum <- decomp.sum %>%
  filter(FUNCTIONAL == "Scavenger")

# Clear the decks
rm(decomp, decomp.sum.wd)

## --------------- SUMMARIZE HERBIVORY DATA ------------------------------------

herbivory$DATE <- as_date(herbivory$DATE)
herbivory$Week <- round_date(herbivory$DATE, "week")

herbivory.sum <- herbivory %>%
  group_by(SITE, TREATMENT, BIOMASS, EXCLUSION, FUNCTIONAL) %>%
  summarize(Total.detections = n())

herbivory.sum.wd <- herbivory.sum %>%
  pivot_wider(names_from = FUNCTIONAL, values_from = Total.detections)

herbivory.sum.wd[is.na(herbivory.sum.wd)] <- 0

herbivory.sum <- herbivory.sum.wd %>%
  pivot_longer(5:7, names_to = "FUNCTIONAL", values_to = "Total.detections")

herbivory.sum <- herbivory.sum %>%
  filter(FUNCTIONAL == "Herbivore")
herbivory.sum <- as.data.frame(herbivory.sum)

# Clear the decks
rm(herbivory, herbivory.sum.wd)

## --------------- INSEPCT THE SUMMARIZED DATA ---------------------------------

library(DataExplorer)

create_report(decomp.sum)
create_report(herbivory.sum)

library(fitdistrplus)
descdist(decomp.sum$Total.detections, discrete = TRUE) # Negative binomial
descdist(herbivory.sum$Total.detections, discrete = TRUE) # Poisson?

## --------------- MODEL DECOMP DATA -------------------------------------------

library(MASS)
m1 <- glm.nb(Total.detections ~ TREATMENT + SITE,
  control = glm.control(maxit = 1000),
  data = decomp.sum
)

summary(m1)

library(performance)
check_zeroinflation(m1)
dev.new()
check_model(m1)
check_singularity(m1, tolerance = 1e-05)

# Check for overdispersion
E2 <- resid(m1, type = "pearson")
N <- nrow(decomp.sum)
p <- length(coef(m1)) # '+1' is for variance parameter in NB
sum(E2^2) / (N - p)

# Check residuals
library(DHARMa)
sim_m1 <- simulateResiduals(fittedModel = m1, n = 250)
plot(sim_m1)

library(car)
Anova(m1)

# Means
library(emmeans)
emmeans(m1, pairwise ~ TREATMENT, type = "response")

decomp.means <- data.frame(
  TREATMENT = c("CH", "CO", "CS", "MH", "MO", "MS"),
  Mean = c(5.32, 151.42, 1.07, 180.52, 174.51, 2.37),
  se = c(4.05, 108.00, 0.96, 128.70, 124.42, 1.92),
  LCL = c(1.20, 37.42, 0.19, 44.64, 43.14, 0.48),
  UCL = c(23.62, 612.78, 6.15, 730.09, 705.86, 11.57)
)

mean(c(180.52, 174.51)) # 177.515
mean(c(5.32, 151.42)) # 78.37
177.515 / 78.37

write.csv(decomp.means, "Analysis/Animals/Decomp-means.csv",
  row.names = FALSE
)

options(scipen = 0)
emmeans <- emmeans(m1, pairwise ~ TREATMENT, type = "response")

# P values

decomp.p <- emmeans(m1, pairwise ~ TREATMENT,
  type = "response",
  adjust = "none"
)
decomp.p <- as.data.frame(decomp.p)
decomp.p <- decomp.p[7:21, c(-1, -5)]

colnames(decomp.p)[1] <- "Contrast"
colnames(decomp.p)[2] <- "Ratio"
colnames(decomp.p)[3] <- "se"
colnames(decomp.p)[4] <- "LCL"
colnames(decomp.p)[5] <- "UCL"

decomp.p$Ratio <- round(decomp.p$Ratio, digits = 2)
decomp.p$se <- round(decomp.p$se, digits = 2)
decomp.p$LCL <- round(decomp.p$LCL, digits = 2)
decomp.p$UCL <- round(decomp.p$UCL, digits = 2)

pvals <- c(
  "0.0013", "0.1705", "0.0007", "0.0008", "0.4653", "<.0001",
  "0.8616", "0.8881", "0.0001", "<.0001", "<.0001", "0.5084",
  "0.9732", "0.0001", "0.0001"
)

decomp.p$p.value <- pvals

write.csv(decomp.p, "Analysis/Animals/Decomp-contrasts.csv",
  row.names = FALSE
)


# Anova
decomp.aov <- as.data.frame(Anova(m1))
decomp.aov <- format(decomp.aov, scientific = FALSE)
write.csv(decomp.aov, "Analysis/Animals/Decomp-aov.csv",
  row.names = FALSE
)

# Explained variance
with(summary(m1), 1 - deviance / null.deviance)
# 67%

## --------------- MODEL HERB DATA ---------------------------------------------

m2 <- glm(Total.detections ~ TREATMENT + SITE,
  family = "poisson",
  data = herbivory.sum
)

library(performance)
check_zeroinflation(m2) # underfitting zeroes
dev.new()
check_model(m2)
check_singularity(m2, tolerance = 1e-05)

# Check for overdispersion
E2 <- resid(m2, type = "pearson")
N <- nrow(herbivory.sum)
p <- length(coef(m2)) # '+1' is for variance parameter in NB
sum(E2^2) / (N - p) # overdispersed

# Check residuals
sim_m2 <- simulateResiduals(fittedModel = m2, n = 250)
plot(sim_m2)
Anova(m2)

# Means
options(scipen = 999)
herb.means <- emmeans(m2, pairwise ~ TREATMENT, type = "response")
herb.means <- as.data.frame(herb.means)
herb.means <- herb.means[1:6, c(-2, -5)]

colnames(herb.means)[2] <- "Mean"
colnames(herb.means)[3] <- "se"
colnames(herb.means)[4] <- "LCL"
colnames(herb.means)[5] <- "UCL"

herb.means[6, 5] <- 0

herb.means$Mean <- round(herb.means$Mean, digits = 2)
herb.means$se <- round(herb.means$se, digits = 2)
herb.means$LCL <- round(herb.means$LCL, digits = 2)
herb.means$UCL <- round(herb.means$UCL, digits = 2)

write.csv(herb.means, "Analysis/Animals/Herb-means.csv",
  row.names = FALSE
)

# P values

herb.p <- emmeans(m2, pairwise ~ TREATMENT, type = "response", adjust = "none")
herb.p <- as.data.frame(herb.p)
herb.p <- herb.p[7:21, c(-1, -5)]

for (i in 1:nrow(herb.p)) {
  if (herb.p$asymp.UCL[i] == "Inf") {
    herb.p$asymp.UCL[i] <- 0
  }
}

colnames(herb.p)[1] <- "Contrast"
colnames(herb.p)[2] <- "Ratio"
colnames(herb.p)[3] <- "se"
colnames(herb.p)[4] <- "LCL"
colnames(herb.p)[5] <- "UCL"

herb.p$Ratio <- round(herb.p$Ratio, digits = 2)
herb.p$se <- round(herb.p$se, digits = 2)
herb.p$LCL <- round(herb.p$LCL, digits = 2)
herb.p$UCL <- round(herb.p$UCL, digits = 2)

pvals <- c(
  "<.0001", "0.0188", "0.2150", "0.0024", "0.9946", "<.0001",
  "<.0001", "<.0001", "0.9937", "0.0087", "0.3196", "0.9942",
  "0.0029", "0.9950", "0.9941"
)

herb.p$p.value <- pvals

write.csv(herb.p, "Analysis/Animals/Herb-contrasts.csv",
  row.names = FALSE
)


# Anova
herb.aov <- as.data.frame(Anova(m2))
write.csv(herb.aov, "Analysis/Animals/Herb-aov.csv",
  row.names = FALSE
)

# Explained variance
with(summary(m2), 1 - deviance / null.deviance)
# 81%
