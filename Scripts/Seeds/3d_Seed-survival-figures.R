## --------------- HEADER ------------------------------------------------------
## Script name: 3d_Seed-survival-figures.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliaton: University of Florida
## Date Created: 2022-06-01
## Date Last modified: 2022-06-01
## Copyright (c) David S. Mason, 2022
## Contact: masond@ufl.edu, @EcoGraffito
## Purpose of script: This will be a script for analyzing the seed survival data

## --------------- SET—UP WORKSPACE --------------------------------------------
library(tidyverse)
library(tidylog)
library(lubridate)
library(styler)
library(emmeans)
library(broom)
library(car)
library(lme4)
library(broom.mixed)
library(MuMIn)
library(grid)
library(tidyr)
options(scipen=999)

rm(list=ls())

# Bring in the estimated mariginal means from the models
surv.means <- read.csv("Animals-plants-seeds/Analysis/Seeds/Seed-survival-means.csv")

# Bring in the reference data
seed.surv <- read.csv("Animals-plants-seeds/Clean-data/Seeds/Seed-survival.csv")

## --------------- GET ESTIMATED MARGINAL MEANS FOR REF ------------------------

ref <- seed.surv %>% 
	filter(TREATMENT == "Reference")

ref <- ref %>% drop_na(FINAL.STATUS.NA)

ref <- ref %>% 
	group_by(SITE, PLOT, BIOMASS, TREATMENT, PACKET, TYPE, SPECIES) %>% 
	summarize(SURV = sum(FINAL.STATUS),
						TRIALS = n()) 

ref <- ref %>% 
	mutate(DEAD = TRIALS-SURV)

ref.m <- glmer(cbind(SURV,DEAD) ~ TYPE * PACKET + (1|SPECIES),
						data = ref, family = binomial, weights = TRIALS
)

r.squaredGLMM(ref.m) # Fixed effects 16% Random effects 58%
# Answers are different than they were before??

ref.anova.df <-as.data.frame(Anova(ref.m))
ref.m.sum <- broom::tidy(ref.m, conf.int=TRUE)
ref.means <- tidy(emmeans(ref.m, ~ TYPE * PACKET, type = "response"))

## --------------- CALCULATE THE EFFECT SIZE -----------------------------------

surv.means$odds.ratio <- NA

for (i in 1:nrow(surv.means)){
	if(surv.means$TYPE[i] == "No dormancy" & surv.means$PACKET[i] == "Bank\nadjacent"){
		surv.means$odds.ratio[i] <- (surv.means$prob[i]/(1-surv.means$prob[i]))/
																			 (ref.means[1,3]/(1-ref.means[1,3]))
	}
	if(surv.means$TYPE[i] == "No dormancy" & surv.means$PACKET[i] == "Bank\nproximal"){
		surv.means$odds.ratio[i] <- (surv.means$prob[i]/(1-surv.means$prob[i]))/
			(ref.means[1,3]/(1-ref.means[1,3]))
	}
	if(surv.means$TYPE[i] == "No dormancy" & surv.means$PACKET[i] == "Rain\nadjacent"){
		surv.means$odds.ratio[i] <- (surv.means$prob[i]/(1-surv.means$prob[i]))/
			(ref.means[4,3]/(1-ref.means[4,3]))
	}
	if(surv.means$TYPE[i] == "No dormancy" & surv.means$PACKET[i] == "Rain\nproximal"){
		surv.means$odds.ratio[i] <- (surv.means$prob[i]/(1-surv.means$prob[i]))/
			(ref.means[4,3]/(1-ref.means[4,3]))
	}
	if(surv.means$TYPE[i] == "Physiological dormancy" & surv.means$PACKET[i] == "Bank\nadjacent"){
		surv.means$odds.ratio[i] <- (surv.means$prob[i]/(1-surv.means$prob[i]))/
			(ref.means[3,3]/(1-ref.means[3,3]))
	}
	if(surv.means$TYPE[i] == "Physiological dormancy" & surv.means$PACKET[i] == "Bank\nproximal"){
		surv.means$odds.ratio[i] <- (surv.means$prob[i]/(1-surv.means$prob[i]))/
			(ref.means[3,3]/(1-ref.means[3,3]))
	}
	if(surv.means$TYPE[i] == "Physiological dormancy" & surv.means$PACKET[i] == "Rain\nadjacent"){
		surv.means$odds.ratio[i] <- (surv.means$prob[i]/(1-surv.means$prob[i]))/
			(ref.means[6,3]/(1-ref.means[6,3]))
	}
	if(surv.means$TYPE[i] == "Physiological dormancy" & surv.means$PACKET[i] == "Rain\nproximal"){
		surv.means$odds.ratio[i] <- (surv.means$prob[i]/(1-surv.means$prob[i]))/
			(ref.means[6,3]/(1-ref.means[6,3]))
	}
	if(surv.means$TYPE[i] == "Physical dormancy" & surv.means$PACKET[i] == "Bank\nadjacent"){
		surv.means$odds.ratio[i] <- (surv.means$prob[i]/(1-surv.means$prob[i]))/
			(ref.means[2,3]/(1-ref.means[2,3]))
	}
	if(surv.means$TYPE[i] == "Physical dormancy" & surv.means$PACKET[i] == "Bank\nproximal"){
		surv.means$odds.ratio[i] <- (surv.means$prob[i]/(1-surv.means$prob[i]))/
			(ref.means[2,3]/(1-ref.means[2,3]))
	}
	if(surv.means$TYPE[i] == "Physical dormancy" & surv.means$PACKET[i] == "Rain\nadjacent"){
		surv.means$odds.ratio[i] <- (surv.means$prob[i]/(1-surv.means$prob[i]))/
			(ref.means[5,3]/(1-ref.means[5,3]))
	}
	if(surv.means$TYPE[i] == "Physical dormancy" & surv.means$PACKET[i] == "Rain\nproximal"){
		surv.means$odds.ratio[i] <- (surv.means$prob[i]/(1-surv.means$prob[i]))/
			(ref.means[5,3]/(1-ref.means[5,3]))
	}
}

# I created a list accidentally?
surv.means <- as.data.frame(lapply(surv.means, unlist))

# Calculate means

# No dormancy adjacent seed rain
no.adj.sr <- c(25.02352446, 11.55928621, 24.65441111, 30.96714611, 10.22572651, 14.65900771)
mean(no.adj.sr) # 19.51485
sd(no.adj.sr) # 8.497925
rm(no.adj.sr)

# Physical dormancy adjacent seed rain
physical.adj.sr <- c(9.08933025888222, 15.54913252011193, 1.46905391214297,
										 7.79667766888951, 3.48874983345884, 6.84072017107113)
mean(physical.adj.sr) # 7.372277
sd(physical.adj.sr) # 4.903096
rm(physical.adj.sr)

# Physiological dormancy adjacent seed rain
physio.adj.sr <- c(0.00000004957374, 1.01785862634997, 1.43272224293065,
									 3.66333215010267, 1.49825467015096, 3.12432952020917)
mean(physio.adj.sr) # 1.789416
sd(physio.adj.sr) # 1.364029
rm(physio.adj.sr)

# Physical dormancy proximal seed rain
physical.prox.sr <- c(2.68154321275626, 0.66075096144833, 0.00000009499801,
											2.22983793321620, 2.58442033329212, 2.46808951940911)
mean(physical.prox.sr) # 1.770774
sd(physical.prox.sr) # 1.145103
rm(physical.prox.sr)

# No dormancy proximal seed rain
no.prox.sr <- c(0.443977901195735, 0.089941209157076, 0.114193970009462,
								19.634372952718888, 2.516258567472764, 0.475802799098533)
mean(no.prox.sr) # 3.879091
sd(no.prox.sr) # 3.879091
rm(no.prox.sr)

# Physiological dormancy proximal seed rain
physio.prox.sr <- c(0.00000004322688, 0.42989882424764, 0.00000005552670,
										1.28346106965481, 0.11240600295322, 0.00000006451293)
mean(physio.prox.sr)
sd(physio.prox.sr)
rm(physio.prox.sr)

tmp.means <- surv.means |>
	filter(PACKET == "Bank\nproximal" | PACKET == "Bank\nadjacent") |>
	group_by(TYPE) |>
	summarize(Mean = mean(odds.ratio),
						std.dev = sd(odds.ratio))
rm(tmp.means)	

for(i in 1:nrow(surv.means)){
	if(surv.means$odds.ratio[i] >= 10){
		surv.means$odds.ratio[i] <- 10
	}
}

## --------------- FILTER DATA AND CREATE HEAT MAPS ----------------------------
library(forcats)

# Reorder factor levels
surv.means$BIOMASS <- factor(surv.means$BIOMASS, 
														 levels = c("Low biomass", "High biomass"))

# Reorder factor levels
surv.means$TREATMENT <- factor(surv.means$TREATMENT, 
														 levels = c("Open", "Scavenger exclusion",
														 					 "Herbivore exclusion"))
# ND
surv.mean.nd.rain <- surv.means %>% 
	filter((TYPE == "No dormancy" & PACKET == "Rain\nadjacent") |
				 	(TYPE == "No dormancy" & PACKET == "Rain\nproximal"))

surv.mean.nd.rain <- as.data.frame(lapply(surv.mean.nd.rain, unlist))

p1 <- ggplot(surv.mean.nd.rain, aes(x = TREATMENT, y = BIOMASS))+
	geom_tile(mapping = aes(fill = odds.ratio, width = 0.9, height = 0.9))+
	scale_fill_viridis_c(limits = c(0, 10))+
	theme_classic() + 
	facet_grid(PACKET ~., space = "free_x", scales = "free_y", switch = "y")+
	theme(strip.placement = "outside",
				strip.background = element_rect(fill=NA,colour=NA),
				panel.spacing=unit(0,"cm"), axis.title.y = element_blank()) +
	annotation_custom(grob = linesGrob(), xmin = -0.75, xmax = -0.75, ymin = -3.25, ymax = -0.75) +
	coord_cartesian(clip="off") +
	ggtitle("No dormancy seed rain")

surv.mean.nd.bank <- surv.means %>% 
	filter((TYPE == "No dormancy" & PACKET == "Bank\nadjacent") |
				 	(TYPE == "No dormancy" & PACKET == "Bank\nproximal"))

surv.mean.nd.bank <- as.data.frame(lapply(surv.mean.nd.bank, unlist))

p2 <- ggplot(surv.mean.nd.bank, aes(x = TREATMENT, y = BIOMASS))+
	geom_tile(mapping = aes(fill = odds.ratio, width = 0.9, height = 0.9))+
	scale_fill_viridis_c(limits = c(0, 10))+
	theme_classic() + 
	facet_grid(PACKET ~., space = "free_x", scales = "free_y", switch = "y")+
	theme(strip.placement = "outside",
				strip.background = element_rect(fill=NA,colour=NA),
				panel.spacing=unit(0,"cm"), axis.title.y = element_blank()) +
	annotation_custom(grob = linesGrob(), xmin = -0.75, xmax = -0.75, ymin = -3.25, ymax = -0.75) +
	coord_cartesian(clip="off") +
	ggtitle("No dormancy seed bank")

# PD

surv.mean.pd.rain <- surv.means %>% 
	filter((TYPE == "Physiological dormancy" & PACKET == "Rain\nadjacent") |
				 	(TYPE == "Physiological dormancy" & PACKET == "Rain\nproximal"))

surv.mean.pd.rain <- as.data.frame(lapply(surv.mean.pd.rain, unlist))

p3 <- ggplot(surv.mean.pd.rain, aes(x = TREATMENT, y = BIOMASS))+
	geom_tile(mapping = aes(fill = odds.ratio, width = 0.9, height = 0.9))+
	scale_fill_viridis_c(limits = c(0, 10))+
	theme_classic() + 
	facet_grid(PACKET ~., space = "free_x", scales = "free_y", switch = "y")+
	theme(strip.placement = "outside",
				strip.background = element_rect(fill=NA,colour=NA),
				panel.spacing=unit(0,"cm"), axis.title.y = element_blank()) +
	annotation_custom(grob = linesGrob(), xmin = -0.75, xmax = -0.75, ymin = -3.25, ymax = -0.75) +
	coord_cartesian(clip="off") +
	ggtitle("Physiological dormancy seed rain")

surv.mean.pd.bank <- surv.means %>% 
	filter((TYPE == "Physiological dormancy" & PACKET == "Bank\nadjacent") |
				 	(TYPE == "Physiological dormancy" & PACKET == "Bank\nproximal"))

surv.mean.pd.bank <- as.data.frame(lapply(surv.mean.pd.bank, unlist))

p4 <- ggplot(surv.mean.pd.bank, aes(x = TREATMENT, y = BIOMASS))+
	geom_tile(mapping = aes(fill = odds.ratio, width = 0.9, height = 0.9))+
	scale_fill_viridis_c(limits = c(0, 10))+
	theme_classic() + 
	facet_grid(PACKET ~., space = "free_x", scales = "free_y", switch = "y")+
	theme(strip.placement = "outside",
				strip.background = element_rect(fill=NA,colour=NA),
				panel.spacing=unit(0,"cm"), axis.title.y = element_blank()) +
	annotation_custom(grob = linesGrob(), xmin = -0.75, xmax = -0.75, ymin = -3.25, ymax = -0.75) +
	coord_cartesian(clip="off") +
	ggtitle("Physiological dormancy seed bank")

# PY

surv.mean.py.rain <- surv.means %>% 
	filter((TYPE == "Physical dormancy" & PACKET == "Rain\nadjacent") |
				 	(TYPE == "Physical dormancy" & PACKET == "Rain\nproximal"))

surv.mean.py.rain <- as.data.frame(lapply(surv.mean.py.rain, unlist))

p5 <- ggplot(surv.mean.py.rain, aes(x = TREATMENT, y = BIOMASS))+
	geom_tile(mapping = aes(fill = odds.ratio, width = 0.9, height = 0.9))+
	scale_fill_viridis_c(limits = c(0, 10))+
	theme_classic() + 
	facet_grid(PACKET ~., space = "free_x", scales = "free_y", switch = "y")+
	theme(strip.placement = "outside",
				strip.background = element_rect(fill=NA,colour=NA),
				panel.spacing=unit(0,"cm"), axis.title.y = element_blank()) +
	annotation_custom(grob = linesGrob(), xmin = -0.75, xmax = -0.75, ymin = -3.25, ymax = -0.75) +
	coord_cartesian(clip="off") +
	ggtitle("Physical dormancy seed rain")

surv.mean.py.bank <- surv.means %>% 
	filter((TYPE == "Physical dormancy" & PACKET == "Bank\nadjacent") |
				 	(TYPE == "Physical dormancy" & PACKET == "Bank\nproximal"))

surv.mean.py.bank <- as.data.frame(lapply(surv.mean.py.bank, unlist))

p6 <- ggplot(surv.mean.py.bank, aes(x = TREATMENT, y = BIOMASS))+
	geom_tile(mapping = aes(fill = odds.ratio, width = 0.9, height = 0.9))+
	scale_fill_viridis_c(limits = c(0, 10))+
	theme_classic() + 
	facet_grid(PACKET ~., space = "free_x", scales = "free_y", switch = "y")+
	theme(strip.placement = "outside",
				strip.background = element_rect(fill=NA,colour=NA),
				panel.spacing=unit(0,"cm"), axis.title.y = element_blank()) +
	annotation_custom(grob = linesGrob(), xmin = -0.75, xmax = -0.75, ymin = -3.25, ymax = -0.75) +
	coord_cartesian(clip="off") +
	ggtitle("Physical dormancy seed bank")


library(patchwork)
combined <- (p1 + p2 + p3 + p4 + p5 + p6)
combined + plot_layout(guides = "collect")


# Calculating means

