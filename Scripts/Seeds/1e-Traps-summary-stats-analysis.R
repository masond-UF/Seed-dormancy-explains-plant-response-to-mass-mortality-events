## --------------- HEADER ------------------------------------------------------
## Script name: 1f_Traps-summary-stats-analysis.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliation: University of Florida
## Date Created: 2022-05-10
## Date Last Modified: 2022-05-10
## Copyright (c) David S. Mason, 2021
## Contact: masond@ufl.edu, @EcoGraffito
## Purpose of script: 

## --------------- SET—UP WORKSPACE --------------------------------------------

# Clear the decks
rm(list=ls())

library(betareg)
library(glmmTMB)
library(car)
library(emmeans)
library(effects)
library(multcomp)
library(MuMIn)
require(DHARMa, quietly = TRUE) ## may be missing ...
library(broom)
library(broom.mixed)
require(dotwhisker, quietly = TRUE)
library(ggplot2); theme_set(theme_bw())
library(texreg)
library(xtable)
if (huxtable_OK) library(huxtable)
## retrieve slow stuff
L <- gt_load("vignette_data/model_evaluation.rda")

library(tidyverse)
library(tidylog)
library(lubridate)
library(styler)
library(mvabund)
library(vegan)
library(fitdistrplus)
library(lme4)
library(lmerTest)
library(emmeans)
library(car)
library(RVAideMemoire)
library(DHARMa)
library(MuMIn)
library(glmmTMB) 
library(dplyr)
library(broom)
library(broom.mixed)
library(dotwhisker)

# Bring in the data
traps.lg <- read.csv("Animals-plants-seeds/Clean-data/Seeds/Traps-community-matrix-lg.csv")

# Drop the extreme outliers
traps.lg <- traps.lg %>% 
	filter(Genus.species != "Anemochory sp. 3")

# Round dates 
traps.lg$DATE <- mdy(traps.lg$DATE)

traps.lg <- traps.lg %>% 
	mutate(Rounded.date = round_date(traps.lg$DATE, unit = "week"))

## --------------- TREATMENT SUMMARY STATISTICS --------------------------------

totals.spec <- traps.lg %>% 
	group_by(TREATMENT, Genus.species) %>% 
	summarize(Total = sum(Seeds)) %>% 
	as_tibble()

totals.class <- traps.lg %>% 
	group_by(TREATMENT, DormancyClass) %>% 
	summarize(Total = sum(Seeds)) %>% 
	as_tibble()

totals.disp <- traps.lg %>% 
	group_by(TREATMENT, Dispersal) %>% 
	summarize(Total = sum(Seeds)) %>% 
	as_tibble()

## --------------- BLOCK SUMMARY STATISTICS ------------------------------------

totals.spec <- traps.lg %>% 
	group_by(TREATMENT, SITE) %>% 
	summarize(Total = sum(Seeds)) %>% 
	as_tibble()

totals.class <- traps.lg %>% 
	group_by(TREATMENT, SITE, DormancyClass) %>% 
	summarize(Total = sum(Seeds)) %>% 
	as_tibble()

totals.disp <- traps.lg %>% 
	group_by(SITE, TREATMENT, Dispersal) %>% 
	summarize(Total = sum(Seeds)) %>% 
	as_tibble()

## --------------- CALCULATE RICHNESS PER TRAP ---------------------------------

# Calculate richness by site and treatment
rich <- traps.lg %>% 
	filter(Seeds > 0) %>% # Drop the zeros from working with wide data
	group_by(SITE, TREATMENT, TRAP, Genus.species) %>% 
	summarize(Seeds = sum(Seeds)) %>% 
	mutate(Seeds = 1) %>% 
	summarize(Seeds = sum(Seeds)) 

# Check out the distribution of trap richness
hist(test$Seeds)
shapiro.test(test$Seeds)

control <- filter(rich, TREATMENT == "CONTROL")
treatment <- filter(rich, TREATMENT == "MME")
t.test(control$Seeds, treatment$Seeds, paired = TRUE)

# Richness doesn't differ

## --------------- CALCULATE DIVERSITY PER TRAP ---------------------------------

trap.div <- traps.lg |>
	dplyr::select(Rounded.date, SITE, TREATMENT, TRAP, Genus.species, Seeds) |>
	pivot_wider(names_from = Genus.species, values_from = Seeds)

div <- trap.div[,1:4]
spec <- trap.div[,5:35]
	
# Calculate the Shannon index (H) and Pielou's evenness (J)
H <- diversity(spec)

# Initiate columns
div$H <- NA

# Add the values
for (i in 1:nrow(div)){
	div$H[i] <- H[i]
}

## --------------- MODEL DIVERSITY ---------------------------------------------
descdist(div$H, discrete = FALSE) # beta

min <- min(div$H)
max <- max(div$H)

# Need to normalize to use beta
div <- div |>
	mutate(H.norm = (H-min)/(max-min))
descdist(div$H.norm, discrete = FALSE) # still beta

for(i in 1:nrow(div)){
	if(div$H.norm[i] == 0)
		div$H.norm[i] <- 0.0001
}

for(i in 1:nrow(div)){
	if(div$H.norm[i] == 1)
		div$H.norm[i] <- 0.99
}

div.mod <- betareg(H.norm ~ TREATMENT + SITE, data = div, link = "logit")
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


## --------------- CALCULATE DETECTIONS PER TRAP -------------------------------

# Calculate total detections for each trap
detect <- traps.lg %>% 
	group_by(SITE, TREATMENT, TRAP) %>% 
	summarize(Seeds = sum(Seeds))

# Check out the distribution of trap richness
hist(detect$Seeds)

descdist(detect$Seeds, discrete = TRUE)

mod <- glmer(Seeds ~ TREATMENT + (1|SITE), family = "poisson",
					 data = detect)

anova(mod) # Detections don't differ


## --------------- CALCULATE DORMANCY CLASS PER TRAP ---------------------------

# Calculate total detections for each trap
dorm <- traps.lg %>% 
	group_by(Rounded.date, SITE, TREATMENT, DormancyClass) %>% 
	summarize(Seeds = sum(Seeds))

ggplot(dorm, aes(x = DormancyClass, y = Seeds, fill = TREATMENT))+
	geom_bar(stat = 'identity', position = 'dodge')


# Check out the distribution of trap richness
hist(dorm$Seeds)
descdist(dorm$Seeds, discrete = TRUE)

dorm$obs <- NA
dorm$obs <- 1:length(dorm$obs)

mod <- glmmTMB(Seeds ~ TREATMENT * DormancyClass + (1|Rounded.date/SITE), 
							 family = poisson,
							 zi = ~1,
							 data = dorm)

library(car)
Anova(mod) 
fixef(mod)

sim_mod <- simulateResiduals(fittedModel = mod, n = 250)
plot(sim_mod)

VarCorr(mod)
total.SD <- sqrt(0.34406^2 + 0.54694^2)

means.test <- emmeans(mod, pairwise ~TREATMENT:DormancyClass, type='response', bias.adj=T,
				sigma = total.SD)
means.test <- test(means.test)
means.test <- means.test[["contrasts"]]
write.csv(means.test, "Animals-plants-seeds/Analysis/Seeds/Trap-dormancy-contrasts.csv",
					row.names = FALSE)


means <- emmeans(mod, pairwise ~TREATMENT*DormancyClass, type='response', bias.adj=T,
								 sigma = total.SD)
means <- as.data.frame(means[1:6,-3])
write.csv(means, "Animals-plants-seeds/Analysis/Seeds/Trap-dormancy-means.csv",
					row.names = FALSE)
write.csv(means, "Animals-plants-seeds/Clean-data/Seeds/Trap-dormancy-means.csv",
					row.names = FALSE)

trap.dorm.anova <- tidy(Anova(mod))
write.csv(trap.dorm.anova, "Animals-plants-seeds/Analysis/Seeds/Trap-dormancy-anova.csv",
					row.names = FALSE)

mod.sum <- summary(mod)

# Random effects variances
formatVC(mod.sum$varcor$cond)

# Conditional fixed effects
coef(mod.sum)$cond

# Fixed effects
fixef(mod)

# Zero-inflation effects
coef(mod.sum)$zi

# AIC
mod.sum$AICtab
