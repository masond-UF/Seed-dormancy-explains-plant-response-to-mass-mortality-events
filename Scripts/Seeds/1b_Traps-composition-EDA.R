## --------------- HEADER ------------------------------------------------------
## Script name: 1b_Seed-traps-EDA.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliaton: University of Florida
## Date Created: 2021-11-23
## Date Last modified: 2022-05-06
## Copyright (c) David S. Mason, 2021
## Contact: masond@ufl.edu, @EcoGraffito
## Purpose of script: This is a script for performing exploratory data analysis
## on the seed trap data.

## --------------- SET—UP WORKSPACE --------------------------------------------
library(lubridate)
library(styler)
library(tidyverse)
library(tidylog)
library(lattice)


seed.trap <- read.csv("Animals-plants-seeds/Clean-data/Seeds/Traps-community-matrix-lg.csv")

## --------------- CONDITIONAL BOXPLOTS ----------------------------------------

bwplot(Seeds ~ DormancyClass | TREATMENT, data = seed.trap,
			 scales = list(rot=45, cex = 0.5))
bwplot(Seeds ~ Dispersal | TREATMENT, data = seed.trap,
			 scales = list(rot=45, cex = 0.5))

# lots of apparent outliers
# more eveness in 2019 samples

## --------------- CONDITIONAL HISTOGRAMS --------------------------------------
histogram(~ Seeds|DormancyClass, data = seed.trap,
					scales = list(rot=45, cex = 0.5))

histogram(~ Seeds|Dispersal, data = seed.trap,
					scales = list(rot=45, cex = 0.5))

histogram(~ Seeds|TREATMENT, data = seed.trap,
					scales = list(rot=45, cex = 0.5))

## --------------- OUTLIER TEST ------------------------------------------------
library(outliers)

# Grubb's test for a single outlier
# H0: No outliers in dataset
# Ha: At least one outlier in dataset

max(seed.trap$Seeds) # 74
grubbs.test(seed.trap$Seeds, type = 10) # 74 is an outlier

min(seed.trap$Seeds) # 0
grubbs.test(seed.trap$Seeds, type = 10,
						opposite = TRUE ) # 0 is not an outlier

# Cleveland dotplot 
plot(x = seed.trap$Seeds,
		 y = 1:nrow(seed.trap),
		 xlab = "Seed detections",
		 ylab = "Order of the data",
		 pch = 16)

# The outlier does not look particularly egregious

## --------------- COEFFICIENTS OF VARIATION -----------------------------------
spec.mat <- seed.trap %>% 
	select(-DormancyClass, -Dispersal) %>% 
	pivot_wider(names_from = Genus.species, values_from = Seeds) %>% 
	select(-DATE, -SITE, -TREATMENT, -TRAP)

# Mccune and Grace recommend considering row standardization (i.e., among plots)
# when CV is over 50

# Calculate row sums
row.sums <- rowSums(spec.mat)

# Calculate coefficient of variation
cv <- sd(row.sums) / mean(row.sums) * 100

# CV is 143 (date needs to be row standardized)

## --------------- PAIRPLOT ----------------------------------------------------

# Reorder columns by total
spec.mat <- spec.mat[, order(colSums(-spec.mat))]

# Extract the top 8 most commonly encountered species
spec.top <- spec.mat[,1:8]

# Create a vector of the column names
labs <- colnames(spec.top)

# Use the reduced species only matrix with the labels
pairs(spec.top[,labs])

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

# Collinear species in data

## --------------- TREATMENT SUMMARY STATISTICS --------------------------------

totals.spec <- seed.trap %>% 
	group_by(TREATMENT, Genus.species) %>% 
	summarize(Total = sum(Seeds)) %>% 
	as_tibble()

totals.class <- seed.trap %>% 
	group_by(TREATMENT, DormancyClass) %>% 
	summarize(Total = sum(Seeds)) %>% 
	as_tibble()

totals.disp <- seed.trap %>% 
	group_by(TREATMENT, Dispersal) %>% 
	summarize(Total = sum(Seeds)) %>% 
	as_tibble()



## --------------- BLOCK SUMMARY STATISTICS ------------------------------------

totals.spec <- seed.trap %>% 
	group_by(TREATMENT, SITE) %>% 
	summarize(Total = sum(Seeds)) %>% 
	as_tibble()

totals.class <- seed.trap %>% 
	group_by(TREATMENT, SITE, DormancyClass) %>% 
	summarize(Total = sum(Seeds)) %>% 
	as_tibble()

totals.class <- totals.class %>% 
	group_by(TREATMENT) %>% 
	summarize(Total = sum(Total))

totals.disp <- seed.trap %>% 
	group_by(SITE, TREATMENT, Dispersal) %>% 
	summarize(Total = sum(Seeds)) %>% 
	as_tibble()

ggplot(totals.disp, aes(x = SITE,y = Total, fill = Dispersal)) + 
	geom_bar(stat = 'identity', position = 'dodge') +
	facet_wrap(~TREATMENT, ncol = 4) + 
	theme_bw()+
	theme(panel.grid.major.x = element_blank(),
				panel.grid.minor.x = element_blank())

# ant wind
# others animal 

# probability of colonizing 
# seed rain 

ggplot(totals.class, aes(x = DormancyClass,y = Total, fill = DormancyClass)) + 
	geom_bar(stat = 'identity', position = 'dodge') +
	facet_wrap(~TREATMENT, ncol = 4) + 
	theme_bw()+
	theme(panel.grid.major.x = element_blank(),
				panel.grid.minor.x = element_blank())
