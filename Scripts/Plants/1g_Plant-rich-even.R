## --------------- HEADER ------------------------------------------------------
## Script name: 1g_Plant-rich-even.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliaton: University of Florida
## Date Created: 2022-12-15
## Date Last modified: 2022-12-15
## Copyright (c) David S. Mason, 2022
## Contact: masond@ufl.edu, @EcoGraffito
## Purpose of script: This is a script for modeling the richness and evenness
## of plant communities

## --------------- SET—UP WORKSPACE --------------------------------------------
library(tidyverse) # data munging
library(lubridate) # dates
library(vegan) # calculating evenness
library(fitdistrplus) # descdist()
library(car) # Anova()
library(performance) # check_overdispersion()
library(boot) # diagnostic plots for GLM
library(MASS) # diagnostic plots for GLM
library(emmeans) # extract means from GLM
library(tidyr) # clean up the script

# Bring in the combined plant survey data
surv <- read.csv("Animals-plants-seeds/Clean-data/Plants/Community-matrix-lg.csv")

## --------------- CALCULATE RICHNESS ------------------------------------------

rich <- surv |>
	filter(Cover > 0.00) |>
	group_by(Rounded.date, Site, Treatment, Carrion, Exclusion) |>
	summarize(Richness = n())
	
## --------------- MODEL RICHNESS ----------------------------------------------

descdist(rich$Richness, discrete = TRUE)

rich.mod <- glm(Richness ~ Carrion * Exclusion + Rounded.date + Site,
									family = 'poisson', data = rich)

# The random effects model with site threw up the error: 'fixed-effect model 
# matrix is rank deficient so dropping 1 column / coefficient.' This revised
# approach accounts for site using site as a fixed effect.

summary(rich.mod)
Anova(rich.mod)

# Check model
E1 <- resid(rich.mod, type="pearson")
F1 <- fitted(rich.mod, type="response")

p1 <- scatter.smooth(F1, E1, cex.lab = 1.5, xlab="Fitted values", ylab="Pearson Residuals")
abline(h = 0, v=0, lty=2); text(F1, E1, labels = row.names(rich), pos = 4)

check_overdispersion(rich.mod) # good
# no zeroes in data

# Explore the patterns with time
rich |>
	mutate(Year = year(Rounded.date)) |>
	group_by(Year) |>
	dplyr::summarize(Mean = mean(Richness))

## --------------- CALCULATE DIVERSITY & EVENNESS ------------------------------
 
# Pivot wider
surv.wd <- surv |>
	dplyr::select(-DormancyClass) |>
	pivot_wider(names_from = Genus.species, values_from = Cover)

# Separate species matrix from predictor variables
spec <- surv.wd[, 6:87]
div.even <- surv.wd[,1:5]

# Calculate the Shannon index (H) and Pielou's evenness (J)
H <- diversity(spec)
J <- H/log(specnumber(spec))

# Initiate columns
div.even$H <- NA
div.even$J <- NA

# Add the values
for (i in 1:nrow(div.even)){
	div.even$H[i] <- H[i]
	div.even$J[i] <- J[i]
}

## --------------- MODEL DIVERSITY ---------------------------------------------
descdist(div.even$H, discrete = FALSE)

div.mod <- glm(H ~ Carrion * Exclusion + Rounded.date + Site,
								family = Gamma(link='identity'), data = even)
Anova(div.mod)

# Check model
E1 <- resid(div.mod, type="pearson")
F1 <- fitted(div.mod, type="response")

p2 <- scatter.smooth(F1, E1, cex.lab = 1.5, xlab="Fitted values", ylab="Pearson Residuals")
abline(h = 0, v=0, lty=2); text(F1, E1, labels = row.names(rich), pos = 4)

div.diag <- glm.diag(div.mod)

par(mar = c(1, 1, 1, 1))
glm.diag.plots(div.mod, div.diag)
hist(div.mod$residuals)

# Explore the patterns with carrion biomass
emmeans(div.mod, pairwise ~Carrion*Exclusion, type = 'response')

# Explore the patterns with time
div.even |>
	group_by(Rounded.date) |>
	dplyr::summarize(Mean = mean(H))

## --------------- MODEL RICHNESS ----------------------------------------------
descdist(div.even$J, discrete = FALSE)

even.mod <- glm(J ~ Carrion * Exclusion + Rounded.date + Site,
							 family = Gamma(link='identity'), data = div.even)
Anova(even.mod)

# Check model
E1 <- resid(even.mod, type="pearson")
F1 <- fitted(even.mod, type="response")

p3 <- scatter.smooth(F1, E1, cex.lab = 1.5, xlab="Fitted values", ylab="Pearson Residuals")
abline(h = 0, v=0, lty=2); text(F1, E1, labels = row.names(rich), pos = 4)

even.diag <- glm.diag(even.mod)

par(mar = c(1, 1, 1, 1))
glm.diag.plots(even.mod, even.diag) # Skewed residuals at tails
hist(even.mod$residuals)

# Explore the patterns with carrion biomass
emmeans(even.mod, pairwise ~Carrion*Exclusion, type = 'response')

# Explore the patterns with time
div.even |>
	mutate(Year = year(Rounded.date)) |>
	group_by(Year) |>
	dplyr::summarize(Mean = mean(J))
