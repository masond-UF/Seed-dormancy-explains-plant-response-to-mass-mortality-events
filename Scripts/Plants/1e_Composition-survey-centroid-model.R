## --------------- HEADER ------------------------------------------------------
## Script name: 1e_Composition-survey-centroid-model.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliaton: University of Florida
## Date Created: 2022-05-04
## Date Last modified: 2022-05-04
## Copyright (c) David S. Mason, 2022
## Contact: masond@ufl.edu, @EcoGraffito
## Purpose of script: This is a script for creating an ordination based on the
## community composition data.

## --------------- SET—UP WORKSPACE --------------------------------------------
library(tidyverse)
library(lubridate)
library(vegan)
library(fitdistrplus)

# Bring in the combined plant survey data
surv <- read.csv("Animals-plants-seeds/Clean-data/Plants/Community-matrix-lg.csv")

## --------------- PREPARE THE DATA  -------------------------------------------

# Remove dormancy class
surv <- surv %>% 
	dplyr::select(-DormancyClass) %>% 
	filter(Treatment != "REF")

# Pivot the data into a site x species matrix
surv <- surv %>% 
	pivot_wider(names_from = Genus.species, values_from = Cover)

# Combine date and treatment for a unique identifier
surv$Date.Treatment <- paste(surv$Rounded.date, surv$Treatment)

# Reorganize the data
surv <- surv %>% 
	dplyr::select(Date.Treatment, Rounded.date, Treatment,
				 Carrion, Exclusion, everything())

# Separate the predictor variables from the community matrix
pred <- surv[, 1:6]
spec <- surv[, 7:88]

## --------------- CREATE DATAFRAME WITH CENTROID DISTANCES --------------------

# Generate dissimilarity matrix
betad <- betadiver(spec, "z", binary = FALSE)

# Calculate centriods and distances
mod <- with(pred, betadisper(betad, Date.Treatment))

# Extract centroid distances from model object
distances <- as.matrix(mod$distances)
colnames(distances)[1] <- "Distance"

# Create dataframe with predictor variables and centroid distances 
pred <- cbind(pred,distances) 
dist.df <- pred %>% 
	dplyr::select(-Date.Treatment)

## --------------- CALCULATE DISTANCE MEANS ------------------------------------

library(dplyr)
dist.df <- dist.df %>%
	group_by(Rounded.date, Treatment) %>%
	summarise(mean = mean(Distance),
						sd = sd(Distance),
						n = n()) %>%
	mutate(se = sd / sqrt(n),
				 lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
				 upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se)
## --------------- RUN MODEL ON CENTRIOD DISTANCE ------------------------------

# Visualize the distribution of the distances
library(fitdistrplus)
descdist(pred$Distance, discrete = FALSE) 
hist(pred$Distance) # Normal distribution

mod2 <- lm(Distance~Treatment*Time, d = pred)
hist(mod2$residuals)
anova(mod2options(scipen=999))
summary(mod2)

## --------------- VISUALIZE DISTANCE TIME-SERIES ------------------------------

# Convert date to date object
dist.df$Rounded.date <- as_date(dist.df$Rounded.date)

# Convert date to numeric value
start <- ymd("2019-03-17")
pred$Rounded.date <- as_date(pred$Rounded.date)
pred <- pred %>% 
	mutate(Time = lubridate::time_length(difftime(Rounded.date, start), "days"))

# Visualize the means
ggplot(data = dist.df, aes(x = as.factor(Rounded.date), y = mean))+
	geom_point()+
	geom_errorbar(aes(ymin = lower.ci, ymax = upper.ci))+
	facet_wrap(~Treatment)

# Visualize the linear model
ggplot(pred, aes(x = Rounded.date, y = Distance))+
	geom_point(shape = 21, size = 6, alpha = 0.5, fill = "palevioletred1",
						 stroke = 2, color = "black")+
	geom_smooth(method = "lm", se = TRUE, color = "royalblue2", fill = "lightblue3",
							size = 2)+
	facet_wrap(~Treatment)+
	theme_bw()+
	theme(panel.grid.minor = element_blank())
