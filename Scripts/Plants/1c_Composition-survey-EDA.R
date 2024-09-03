## --------------- HEADER ------------------------------------------------------
## Script name: 1c_Plant-survey-EDA.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliaton: University of Florida
## Date Created: 2021-11-19
## Date Last modified: 2022-04-28
## Copyright (c) David S. Mason, 2021
## Contact: masond@ufl.edu, @EcoGraffito
## Purpose of script: This script does exploratory data analysis for the plant
## survey community data.

## --------------- SET—UP WORKSPACE --------------------------------------------
library(tidyverse)
library(tidylog)
library(lubridate)
library(styler)

# Bring in the finalized plant species matrix (community matrix)
surv <- read.csv("Animals-plants-seeds/Clean-data/Plants/Community-matrix-lg.csv")

## --------------- CONDITIONAL BOXPLOTS ----------------------------------------
library(lattice)

bwplot(Cover ~ Genus.species | Treatment, data = surv,
			 scales = list(rot=45, cex = 0.5))
bwplot(Cover ~ Genus.species | factor(Year), data = surv,
			 scales = list(rot=45, cex = 0.5))

# lots of apparent outliers
# more eveness in 2019 samples

## --------------- CONDITIONAL HISTOGRAMS --------------------------------------
histogram(~ Cover|Genus.species, data = surv,
			 scales = list(rot=45, cex = 0.5))

histogram(~ Cover|Treatment, data = surv,
					scales = list(rot=45, cex = 0.5))

histogram(~ Cover|factor(Rounded.date), data = surv,
					scales = list(rot=45, cex = 0.5))

## --------------- OUTLIER TEST ------------------------------------------------
library(outliers)

# Grubb's test for a single outlier
# H0: No outliers in dataset
# Ha: At least one outlier in dataset

max(surv$Cover) # 0.94
grubbs.test(surv$Cover, type = 10) # 0.94 is outlier

min(surv$Cover) # 0
grubbs.test(surv$Cover, type = 10,
						opposite = TRUE ) # 0 is not an outlier

# Cleveland dotplot 
plot(x = surv$Cover,
		 y = 1:nrow(surv),
		 xlab = "Plant cover",
		 ylab = "Order of the data",
		 pch = 16)

# The outlier does not look particularly egregious

## --------------- COEFFICIENTS OF VARIATION -----------------------------------
spec.mat <- surv %>% 
	select(-DormancyClass) %>% 
	pivot_wider(names_from = Genus.species, values_from = Cover) %>% 
	select(-Rounded.date, -Site, -Treatment, -Carrion, -Exclusion)
	
# Mccune and Grace recommend considering row standardization (i.e., among plots)
# when CV is over 50

# Calculate row sums
row.sums <- rowSums(spec.mat)

# Calculate coefficient of variation
cv <- sd(row.sums) / mean(row.sums) * 100

# CV is 26.4

## --------------- PAIRPLOT ----------------------------------------------------

# Reorder columns by total
spec.mat <- spec.mat[, order(colSums(-spec.mat))]

# Extract the top 8 most commonly encountered species
spec.top <- spec.mat[,1:8]

# Create a vector of the column names
labs <- colnames(spec.top)

# Use the reduced species only matrix with the labels
pairs(spec.top[,labs])

# Most species have inverse relationships
# Except AECY and BOCU, THFI and BOHI2
# AMPS positive with BRJA, BOCU, AECY
# Strong tradeoff between AMPS and THIFI
 

## --------------- PCA CORRELATION BIPLOT --------------------------------------

# All species
labs <- colnames(spec.mat)

cor.PCA <- princomp(spec.mat[,labs],
										cor = TRUE)

biplot(cor.PCA, choices = 1:2,
			 scale = 0,
			 cex = c(0.5, 1.5),
			 col = 1,
			 arrow.len = 0)

# Top 8 species
# PCA of all species
labs <- colnames(spec.top)

cor.PCA <- princomp(spec.mat[,labs],
										cor = TRUE)

biplot(cor.PCA, choices = 1:2,
			 scale = 0,
			 cex = c(0.5, 1.5),
			 col = 1,
			 arrow.len = 0)

# Collinear species in data,
# but no dramatic collinearity among top species