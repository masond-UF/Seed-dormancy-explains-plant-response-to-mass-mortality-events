## --------------- HEADER ------------------------------------------------------
## Script name: 1d_Seed-traps-ord.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliaton: University of Florida
## Date Created:
## Copyright (c) David S. Mason, 2021
## Contact: masond@ufl.edu, @EcoGraffito
## Purpose of script: This is a script for generating ordinations to visualize
## community changes in the seed trap data across treatments and sampling
## intervals.

## --------------- SET—UP WORKSPACE --------------------------------------------
rm(list = ls()) # Clear environment
cat("\014") # Clear log

library(tidyverse)
library(tidylog)
library(lubridate)
library(styler)
library(mvabund)
library(vegan)

# Bring in the data
seed.wd <- read.csv("Animals-plants-seeds/Clean-data/Seeds/Seed-traps-reduc-wide.csv")

# Separate the data into species and predictors
spec <- as.matrix(seed.wd[, 5:15])
pred <- seed.wd[, 1:4]

## --------------- ADD DUMMY VALUES FOR EMPTY ROWS -----------------------------
dummy <- rep(1, 64)
spec <- cbind(spec, dummy)

## --------------- RUN ORDINATION ----------------------------------------------
ord <- metaMDS(spec) # run the ordination, stress failure

## --------------- GET ORDINATION SCORES ---------------------------------------
# Using the scores function from vegan to extract the site scores
# and convert to a data.frame
site.scores <- as.data.frame(scores(ord))

# Create a column of site names, from the rownames of site.scores
site.scores$ID <- rownames(site.scores)

# Add back the predictor variables set aside earlier
site.scores <- cbind(pred, site.scores)

# Rearrange the data
site.scores <- site.scores %>%
  select(ID, SITE, TREATMENT, DATE, TRAP, NMDS1, NMDS2)

# Using the scores function from vegan to extract
# the species scores and convert to a data.frame
spec.scores <- as.data.frame(scores(ord, "species"))

# Create a column of species, from the rownames of species.scores
spec.scores$SPECIES <- rownames(spec.scores)

# Remove the row labels
rownames(spec.scores) <- NULL

# Rearrange the data
spec.scores <- spec.scores %>%
  select(SPECIES, NMDS1, NMDS2)

## --------------- CREATE HULLS ------------------------------------------------
# Create the treatment hulls for all sites collectively
MME <- site.scores[site.scores$TREATMENT == "MME", ][chull(site.scores[site.scores$TREATMENT ==
  "MME", c("NMDS1", "NMDS2")]), ] # hull values for MME
CONTROL <- site.scores[site.scores$TREATMENT == "CONTROL", ][chull(site.scores[site.scores$TREATMENT ==
  "CONTROL", c("NMDS1", "NMDS2")]), ] # hull values for Control

hull.data <- rbind(MME, CONTROL) # Combine groups

# Create the treatment hulls by date

# Filter by date
may.site.scores <- site.scores %>% filter(DATE == "5/6/19")
june.site.scores <- site.scores %>% filter(DATE == "6/1/19")
july.site.scores <- site.scores %>% filter(DATE == "7/21/19")
oct.site.scores <- site.scores %>% filter(DATE == "10/21/19")

# Generate hulls for may
may.MME <- may.site.scores[may.site.scores$TREATMENT == "MME", ][chull(may.site.scores[may.site.scores$TREATMENT ==
																																			 	"MME", c("NMDS1", "NMDS2")]), ] 
may.CONTROL <- may.site.scores[may.site.scores$TREATMENT == "CONTROL", ][chull(may.site.scores[may.site.scores$TREATMENT ==
																																							 	"CONTROL", c("NMDS1", "NMDS2")]), ] 
# Generate hulls for june
june.MME <- june.site.scores[june.site.scores$TREATMENT == "MME", ][chull(june.site.scores[june.site.scores$TREATMENT ==
																																											 	"MME", c("NMDS1", "NMDS2")]), ] 
june.CONTROL <- june.site.scores[june.site.scores$TREATMENT == "CONTROL", ][chull(june.site.scores[june.site.scores$TREATMENT ==
																																															"CONTROL", c("NMDS1", "NMDS2")]), ] 
# Generate hulls for july
july.MME <- july.site.scores[july.site.scores$TREATMENT == "MME", ][chull(july.site.scores[july.site.scores$TREATMENT ==
																																													 	"MME", c("NMDS1", "NMDS2")]), ] 
july.CONTROL <- july.site.scores[july.site.scores$TREATMENT == "CONTROL", ][chull(july.site.scores[july.site.scores$TREATMENT ==
																																																	 	"CONTROL", c("NMDS1", "NMDS2")]), ]
# Generate hulls for october
oct.MME <- oct.site.scores[oct.site.scores$TREATMENT == "MME", ][chull(oct.site.scores[oct.site.scores$TREATMENT ==
																																													 	"MME", c("NMDS1", "NMDS2")]), ] 
oct.CONTROL <- oct.site.scores[oct.site.scores$TREATMENT == "CONTROL", ][chull(oct.site.scores[oct.site.scores$TREATMENT ==
																																																	 	"CONTROL", c("NMDS1", "NMDS2")]), ] 
hull.data.date <- rbind(may.MME, may.CONTROL,
												june.MME, june.CONTROL,
												july.MME, july.CONTROL,
												oct.MME, oct.CONTROL) # Combine groups

## --------------- VISUALIZE ORDINATION ----------------------------------------
site.scores$SITE <- as.factor(site.scores$SITE)

# Collective ordination with species labels
ord.spec <- ggplot() +
  ggrepel::geom_text_repel(
    data = spec.scores,
    aes(x = NMDS1, y = NMDS2, label = SPECIES)
  ) +
  geom_point(
    data = site.scores,
    aes(x = NMDS1, y = NMDS2, colour = TREATMENT), size = 2
  ) +
  geom_polygon(data = hull.data, aes(
    x = NMDS1, y = NMDS2,
    fill = TREATMENT, group = TREATMENT
  ), alpha = 0.30) +
  coord_equal(xlim = c(-0.5, 1.5), ylim = c(-1, 1)) +
  theme_classic() +
  theme(text = element_text(size = 22)) +
  theme(plot.margin = unit(c(0, -2, 0, 0), "in")) +
  scale_color_manual(values = c("#1F9E89FF", "orange")) +
  scale_fill_manual(values = c("#1F9E89FF", "orange"))

# Ordination by sampling date with species labels
ord.spec.date <- ggplot() +
	ggrepel::geom_text_repel(
		data = spec.scores,
		aes(x = NMDS1, y = NMDS2, label = SPECIES)
	) +
	geom_point(
		data = site.scores,
		aes(x = NMDS1, y = NMDS2, colour = TREATMENT), size = 2
	) +
	geom_polygon(data = hull.data.date, aes(
		x = NMDS1, y = NMDS2,
		fill = TREATMENT, group = TREATMENT
	), alpha = 0.30) +
	coord_equal(xlim = c(-0.5, 1.5), ylim = c(-1, 1)) +
	theme_classic() +
	theme(text = element_text(size = 22)) +
	theme(plot.margin = unit(c(0, -2, 0, 0), "in")) +
	scale_color_manual(values = c("#1F9E89FF", "orange")) +
	scale_fill_manual(values = c("#1F9E89FF", "orange")) +
	facet_wrap(~DATE)

# Collective ordination with site labels
ord.site.date <- ggplot() +
	ggrepel::geom_text_repel(
		data = site.scores,
		aes(x = NMDS1, y = NMDS2, label = SITE)
	) +
	geom_point(
		data = site.scores,
		aes(x = NMDS1, y = NMDS2, colour = TREATMENT), size = 2
	) +
	geom_polygon(data = hull.data, aes(
		x = NMDS1, y = NMDS2,
		fill = TREATMENT, group = TREATMENT
	), alpha = 0.30) +
	coord_equal(xlim = c(-0.5, 1.5), ylim = c(-1, 1)) +
	theme_classic() +
	theme(text = element_text(size = 22)) +
	theme(plot.margin = unit(c(0, -2, 0, 0), "in")) +
	scale_color_manual(values = c("#1F9E89FF", "orange")) +
	scale_fill_manual(values = c("#1F9E89FF", "orange")) 

# Ordination by sampling date with site labels
ord.site.date <- ggplot() +
  ggrepel::geom_text_repel(
    data = site.scores,
    aes(x = NMDS1, y = NMDS2, label = SITE)
  ) +
  geom_point(
    data = site.scores,
    aes(x = NMDS1, y = NMDS2, colour = TREATMENT), size = 2
  ) +
  geom_polygon(data = hull.data.date, aes(
    x = NMDS1, y = NMDS2,
    fill = TREATMENT, group = TREATMENT
  ), alpha = 0.30) +
  coord_equal(xlim = c(-0.5, 1.5), ylim = c(-1, 1)) +
  theme_classic() +
  theme(text = element_text(size = 22)) +
  theme(plot.margin = unit(c(0, -2, 0, 0), "in")) +
  scale_color_manual(values = c("#1F9E89FF", "orange")) +
  scale_fill_manual(values = c("#1F9E89FF", "orange")) +
	facet_wrap(~DATE)
