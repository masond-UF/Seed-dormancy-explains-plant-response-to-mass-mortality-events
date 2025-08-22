## --------------- HEADER ------------------------------------------------------
## Script name: 2c_Plant-fitness-analysis.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliation: University of Florida
## Date Created: 2021-11-30
## Date Last modified: 2025-08-14
## Copyright (c) David S. Mason, 2025
## Contact: david.mason@jonesctr.org
## Purpose of script: This script generates boxplots and sina graphs to compare
## the moments and distributions of the fitness metrics for plants in simulated
## herbivore mass mortality events and other plants.

## --------------- SET—UP WORKSPACE --------------------------------------------
library(tidyverse)
library(tidylog)
library(lubridate)
library(styler)
source('Functions.R')

# Clear the decks
rm(list=ls())

# Bring in the data
d <- read.csv("Clean-data/Plants/Plant-fitness.csv")

# Keep everything except for the infloresence data
ht <- d %>% select(SITE, DATE, TREATMENT, MH, SPECIES, HEIGHT)

inf <- d %>% select(SITE, DATE, TREATMENT, MH, SPECIES, INFLOR)

source("Functions.R") # Load packages

## --------------- CALCULATE SUM STATS -----------------------------------------
ht.sum <- ht %>%
  group_by(MH) %>%
  summarise(
    smean = mean(HEIGHT, na.rm = TRUE),
    ssd = sd(HEIGHT, na.rm = TRUE),
    count = n()
  ) %>%
  mutate(
    se = ssd / sqrt(count),
    lower_ci = lower_ci(smean, se, count),
    upper_ci = upper_ci(smean, se, count)
  )

inf.sum <- inf %>%
  group_by(MH) %>%
  summarise(
    smean = mean(INFLOR, na.rm = TRUE),
    ssd = sd(INFLOR, na.rm = TRUE),
    count = n()
  ) %>%
  mutate(
    se = ssd / sqrt(count),
    lower_ci = lower_ci(smean, se, count),
    upper_ci = upper_ci(smean, se, count)
  )

## --------------- CHANGE LABELS -----------------------------------------------

levels(ht.sum$MH)[levels(ht.sum$MH) == "N"] <- "Other"
levels(ht.sum$MH)[levels(ht.sum$MH) == "Y"] <- "Herbivore MME"

levels(ht.sum$SPECIES)[levels(ht.sum$SPECIES) == "LASE"] <- "*L. serriola*"

levels(ht$MH)[levels(ht$MH) == "N"] <- "Other"
levels(ht$MH)[levels(ht$MH) == "Y"] <- "Herbivore MME"

levels(inf.sum$MH)[levels(inf.sum$MH) == "N"] <- "Other"
levels(inf.sum$MH)[levels(inf.sum$MH) == "Y"] <- "Herbivore MME"

levels(inf$MH)[levels(inf$MH) == "N"] <- "Other"
levels(inf$MH)[levels(inf$MH) == "Y"] <- "Herbivore MME"


## --------------- SCATTERPLOTS WITH CIs ---------------------------------------
library(ggforce)
library(ggtext)

ht.mean <- ggplot(ht.sum, aes(x = MH, y = smean)) +
  geom_point(size = 8, position = position_dodge(width = 0.5), color = "#56B4E9") +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci),
  							width = 0, size = 1.25,
  							position = position_dodge(width = 0.5)) +
	ylab("Height (cm)") +
  theme_classic() +
	theme(axis.title.x = element_blank(),
				axis.text.x = element_blank(),
				axis.title.y = element_text(face = "bold"),
				legend.title = element_blank(),
				legend.text = ggtext::element_markdown(),
				text = element_text(size = 20)) 

ht.dat <- ggplot(ht, aes(x = MH, y = HEIGHT)) +
	geom_sina(size = 3, alpha = 0.7, color = "#56B4E9") +
	scale_y_continuous(breaks = c(25, 50, 75, 100, 125, 150, 175)) +
	ylab("Height (cm)") +
	theme_classic() +
	theme(axis.title.x = element_blank(),
				axis.text.x = element_text(face = "bold"),
				axis.title.y = element_text(face = "bold"),
				text = element_text(size = 20),
				legend.position = "none")

inf.mean <- ggplot(inf.sum, aes(x = MH, y = smean)) +
  geom_point(size = 8, color = "#56B4E9") +
  theme(axis.title.x = element_blank()) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), 
  							width = 0, size = 1.25, color = "#56B4E9") +
	ylab("Inflorescences") +
  theme_classic() +
	theme(axis.title.x = element_blank(),
				axis.text.x = element_blank(),
				axis.title.y = element_text(face = "bold"),
				text = element_text(size = 20))

inf.dat <- ggplot(inf, aes(x = MH, y = INFLOR)) +
	geom_sina(size = 3, alpha = 0.7, color = "#56B4E9") +
	scale_y_continuous(breaks = c(100, 200, 300, 400, 500, 600, 700)) +
	ylab("Inflorescences") +
	theme_classic() +
	theme(axis.title.x = element_blank(),
				axis.text.x = element_text(face = "bold"),
				axis.title.y = element_text(face = "bold"),
				text = element_text(size = 20),
				legend.position = "none")

theme(axis.title.y=element_text(face="italic"))

## --------------- FINAL FIGURE ------------------------------------------------

# Clear the decks
rm(list=ls())

source('Functions.R')

# Bring in the data
d <- read.csv("Clean-data/Plants/Plant-fitness.csv") |>
	pivot_longer(cols = 6:7, names_to = 'METRIC', values_to = 'VALUE')

means <- d %>%
  group_by(METRIC, MH) %>%
  summarise(
    smean = mean(VALUE, na.rm = TRUE),
    ssd = sd(VALUE, na.rm = TRUE),
    count = n()
  ) %>%
  mutate(
    se = ssd / sqrt(count),
    lower_ci = lower_ci(smean, se, count),
    upper_ci = upper_ci(smean, se, count)
  )

d <- merge(d, means)

ggplot(d, aes(x = MH, y = VALUE, fill = MH))+
	geom_point(shape = 21, color = 'black', alpha = 0.3, stroke = 0.8, size = 4,
						 position=position_jitterdodge(jitter.width = 0.2, jitter.height = 0.5))+
	scale_fill_manual(values=c("#E69F00", "#56B4E9"))+
	geom_errorbar(aes(ymin=lower_ci, ymax=upper_ci), width=.2,
             position=position_dodge(width = 0.7))+
	theme_bw()+
	theme(legend.position = 'none',
				panel.grid.major.x =  element_blank(),
				panel.grid.minor.y =  element_blank())+
	facet_wrap(~METRIC, scales = 'free_y')

ggplot(d, aes(x = MH, y = VALUE, fill = MH))+
	geom_jitter(shape = 21, color = 'black', alpha = 0.3, stroke = 0.8, size = 4)+
	scale_fill_manual(values=c("#E69F00", "#56B4E9"))+
	geom_errorbar(aes(ymin=lower_ci, ymax=upper_ci), width=.2)+
	theme_bw()+
	xlab('')+
	ylab('')+
	theme(legend.position = 'none',
				panel.grid.major.x =  element_blank(),
				panel.grid.minor.y =  element_blank(),
				axis.text.x = element_blank(),
				axis.ticks.x = element_blank(),
				strip.background = element_blank(),
  strip.text.x = element_blank())+
	facet_wrap(~METRIC, scales = 'free_y')

ht <- d |> filter(METRIC == 'HEIGHT')
inf <- d |> filter(METRIC == 'INFLOR')

p1 <- ggplot(ht, aes(x = MH, y = VALUE, fill = MH))+
	geom_jitter(shape = 21, color = 'black', alpha = 0.3, stroke = 0.8, size = 4)+
	scale_fill_manual(values=c("#E69F00", "#56B4E9"))+
	geom_errorbar(aes(ymin=lower_ci, ymax=upper_ci), width= 0, linewidth = 1.5)+
	geom_point(aes(x = MH, y = smean, size = 5))+
	theme_bw()+
	xlab('')+
	ylab('Height (cm)')+
	theme(legend.position = 'none',
				panel.grid.major.x =  element_blank(),
				panel.grid.minor.y =  element_blank(),
				axis.text.x = element_blank(),
				axis.ticks.x = element_blank(),
				axis.title = element_text(face = 'bold'),
				text=element_text(size=20))

p2 <- ggplot(inf, aes(x = MH, y = VALUE, fill = MH))+
	geom_jitter(shape = 21, color = 'black', alpha = 0.3, stroke = 0.8, size = 4)+
	scale_fill_manual(values=c("#E69F00", "#56B4E9"))+
	geom_errorbar(aes(ymin=lower_ci, ymax=upper_ci), width=0, linewidth = 1.5)+
	scale_y_continuous(limits = c(0,725),
										 breaks = seq(0,700,100))+
	geom_point(aes(x = MH, y = smean, size = 5))+
	theme_bw()+
	xlab('')+
	ylab('Inflorescences')+
	theme(legend.position = 'none',
				panel.grid.major.x =  element_blank(),
				panel.grid.minor.y =  element_blank(),
				axis.text.x = element_blank(),
				axis.ticks.x = element_blank(),
				axis.title = element_text(face = 'bold'),
				text=element_text(size=20))

library(patchwork)
p1+p2

ggsave(filename = "Figures/Plants/Plantfitness.jpeg",
			 width = 20, height = 15, units = "cm", dpi = 300)
