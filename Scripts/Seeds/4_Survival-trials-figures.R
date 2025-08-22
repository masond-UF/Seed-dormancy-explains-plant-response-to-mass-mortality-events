## --------------- HEADER ------------------------------------------------------
## Script name: 4_Seed-survival-figures.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliation: University of Florida
## Date Created: 2022-06-01
## Date Last Modified: 2025-8-13
## Copyright (c) David S. Mason, 2025
## Contact: david.mason@jonesctr.org
## Purpose of script: This will be a script for visualizing the seed survival data

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
surv.means <- read.csv("Analysis/Seeds/Seed-survival-means.csv")

# Bring in the reference data
seed.surv <- read.csv("Clean-data/Seeds/Seed-survival.csv")

## --------------- GET ESTIMATED MARGINAL MEANS FOR REF ------------------------

ref <- seed.surv %>% 
	filter(EXCLUSION == "Reference")

ref <- ref %>% 
	group_by(SITE, PLOT, BIOMASS, EXCLUSION, PACKET, DORMANCY.CLASS, SPECIES) %>% 
	summarize(SURV = sum(FINAL.STATUS),
						TRIALS = n()) 

ref <- ref %>% 
	mutate(DEAD = TRIALS-SURV)

ref.m <- glmer(cbind(SURV,DEAD) ~ DORMANCY.CLASS * PACKET + (1|SPECIES),
						data = ref, family = binomial
)

r.squaredGLMM(ref.m) # Fixed effects 28-29% Random effects 56-59%

ref.anova.df <-as.data.frame(Anova(ref.m))
ref.m.sum <- broom::tidy(ref.m, conf.int=TRUE)
ref.means <- tidy(emmeans(ref.m, ~ DORMANCY.CLASS * PACKET, type = "response"))

## --------------- CALCULATE THE EFFECT SIZE -----------------------------------

surv.means$odds.ratio <- NA

for (i in 1:nrow(surv.means)){
	if(surv.means$DORMANCY.CLASS[i] == "No dormancy" & surv.means$PACKET[i] == "Bank\nadjacent"){
		surv.means$odds.ratio[i] <- (surv.means$prob[i]/(1-surv.means$prob[i]))/
																			 (ref.means[1,3]/(1-ref.means[1,3]))
	}
	if(surv.means$DORMANCY.CLASS[i] == "No dormancy" & surv.means$PACKET[i] == "Bank\nproximal"){
		surv.means$odds.ratio[i] <- (surv.means$prob[i]/(1-surv.means$prob[i]))/
			(ref.means[1,3]/(1-ref.means[1,3]))
	}
	if(surv.means$DORMANCY.CLASS[i] == "No dormancy" & surv.means$PACKET[i] == "Rain\nadjacent"){
		surv.means$odds.ratio[i] <- (surv.means$prob[i]/(1-surv.means$prob[i]))/
			(ref.means[4,3]/(1-ref.means[4,3]))
	}
	if(surv.means$DORMANCY.CLASS[i] == "No dormancy" & surv.means$PACKET[i] == "Rain\nproximal"){
		surv.means$odds.ratio[i] <- (surv.means$prob[i]/(1-surv.means$prob[i]))/
			(ref.means[4,3]/(1-ref.means[4,3]))
	}
	if(surv.means$DORMANCY.CLASS[i] == "Physiological dormancy" & surv.means$PACKET[i] == "Bank\nadjacent"){
		surv.means$odds.ratio[i] <- (surv.means$prob[i]/(1-surv.means$prob[i]))/
			(ref.means[3,3]/(1-ref.means[3,3]))
	}
	if(surv.means$DORMANCY.CLASS[i] == "Physiological dormancy" & surv.means$PACKET[i] == "Bank\nproximal"){
		surv.means$odds.ratio[i] <- (surv.means$prob[i]/(1-surv.means$prob[i]))/
			(ref.means[3,3]/(1-ref.means[3,3]))
	}
	if(surv.means$DORMANCY.CLASS[i] == "Physiological dormancy" & surv.means$PACKET[i] == "Rain\nadjacent"){
		surv.means$odds.ratio[i] <- (surv.means$prob[i]/(1-surv.means$prob[i]))/
			(ref.means[6,3]/(1-ref.means[6,3]))
	}
	if(surv.means$DORMANCY.CLASS[i] == "Physiological dormancy" & surv.means$PACKET[i] == "Rain\nproximal"){
		surv.means$odds.ratio[i] <- (surv.means$prob[i]/(1-surv.means$prob[i]))/
			(ref.means[6,3]/(1-ref.means[6,3]))
	}
	if(surv.means$DORMANCY.CLASS[i] == "Physical dormancy" & surv.means$PACKET[i] == "Bank\nadjacent"){
		surv.means$odds.ratio[i] <- (surv.means$prob[i]/(1-surv.means$prob[i]))/
			(ref.means[2,3]/(1-ref.means[2,3]))
	}
	if(surv.means$DORMANCY.CLASS[i] == "Physical dormancy" & surv.means$PACKET[i] == "Bank\nproximal"){
		surv.means$odds.ratio[i] <- (surv.means$prob[i]/(1-surv.means$prob[i]))/
			(ref.means[2,3]/(1-ref.means[2,3]))
	}
	if(surv.means$DORMANCY.CLASS[i] == "Physical dormancy" & surv.means$PACKET[i] == "Rain\nadjacent"){
		surv.means$odds.ratio[i] <- (surv.means$prob[i]/(1-surv.means$prob[i]))/
			(ref.means[5,3]/(1-ref.means[5,3]))
	}
	if(surv.means$DORMANCY.CLASS[i] == "Physical dormancy" & surv.means$PACKET[i] == "Rain\nproximal"){
		surv.means$odds.ratio[i] <- (surv.means$prob[i]/(1-surv.means$prob[i]))/
			(ref.means[5,3]/(1-ref.means[5,3]))
	}
}

# I created a list accidentally?
surv.means <- as.data.frame(lapply(surv.means, unlist))

# Calculate means

# No dormancy adjacent seed rain
no.adj.sr <- c(25.02352446, 11.55928621, 24.65441111, 30.96714611, 10.22572651, 14.65900771)
no.adj.sr <- c(7.200174, 3.326025, 7.093967, 8.910369, 2.942312, 4.217927)
mean(no.adj.sr) # 5.615129
sd(no.adj.sr) # 2.445161
rm(no.adj.sr)

# Physical dormancy adjacent seed rain
physical.adj.sr <- c(9.08933025888222, 15.54913252011193, 1.46905391214297,
										 7.79667766888951, 3.48874983345884, 6.84072017107113)
physical.adj.sr <- c(7.205503, 12.326466, 1.164582, 6.180761, 2.765682, 5.422933)
mean(physical.adj.sr) # 5.844321
sd(physical.adj.sr) # 3.886895
rm(physical.adj.sr)

# Physiological dormancy adjacent seed rain
physio.adj.sr <- c(0.00000004957374, 1.01785862634997, 1.43272224293065,
									 3.66333215010267, 1.49825467015096, 3.12432952020917)
physio.adj.sr <- c(0.00000004100855, 0.84199641818905, 1.18518128704508,
									 3.03039387707926, 1.23939124073247, 2.58451831831697)
mean(physio.adj.sr) # 1.480247
sd(physio.adj.sr) # 1.128356
rm(physio.adj.sr)

# Physical dormancy proximal seed rain
physical.prox.sr <- c(2.68154321275626, 0.66075096144833, 0.00000009499801,
											2.22983793321620, 2.58442033329212, 2.46808951940911)
physical.prox.sr <- c(2.12577455715587, 0.52380568613675, 0.00000007530901,
											1.76768836782600, 2.04878107627499, 1.95656064022558)
mean(physical.prox.sr) # 1.403768
sd(physical.prox.sr) # 0.9077721
rm(physical.prox.sr)

# No dormancy proximal seed rain
no.prox.sr <- c(0.443977901195735, 0.089941209157076, 0.114193970009462,
								19.634372952718888, 2.516258567472764, 0.475802799098533)
no.prox.sr <- c(0.12774852, 0.02587934, 0.03285774,
								5.64952014, 0.72401871, 0.13690570)
mean(no.prox.sr) # 1.116155
sd(no.prox.sr) # 2.236215
rm(no.prox.sr)

# Physiological dormancy proximal seed rain
physio.prox.sr <- c(0.00000004322688, 0.42989882424764, 0.00000005552670,
										1.28346106965481, 0.11240600295322, 0.00000006451293)
physio.prox.sr <- c(0.00000003575828, 0.00000004593298, 0.09298486982319,
										0.35562234364337, 1.06170895992669, 0.00000005336660)
mean(physio.prox.sr) # 0.2517194
sd(physio.prox.sr) # 0.4200389
rm(physio.prox.sr)

surv.means |>
  filter((PACKET == "Bank\nadjacent" | PACKET == "Bank\nproximal")
  			 & DORMANCY.CLASS == "Physical dormancy") |>
  summarise(mean_odds_ratio = mean(odds.ratio),
  					std.dev = sd(odds.ratio))


surv.means$BIOMASS <- as_factor(surv.means$BIOMASS)
levels(surv.means$BIOMASS)
surv.means$PACKET <- as_factor(surv.means$PACKET)
levels(surv.means$PACKET)
tmp <- surv.means |>
  filter(BIOMASS == "Low biomass" & PACKET == "Rain\nadjacent") |>
	group_by(DORMANCY.CLASS)
  summarise(mean_odds_ratio = mean(odds.ratio),
  					std.dev = sd(odds.ratio))

  
  
tmp.means <- surv.means |>
	filter(PACKET == "Bank\nproximal" | PACKET == "Bank\nadjacent") |>
	group_by(DORMANCY.CLASS) |>
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
														 levels = c("Single carcass", "MMME"))

# Reorder factor levels
surv.means$EXCLUSION <- factor(surv.means$EXCLUSION, 
														 levels = c("Open", "Scavenger",
														 					 "Herbivore"))
# ND
surv.mean.nd.rain <- surv.means %>% 
	filter((DORMANCY.CLASS == "No dormancy" & PACKET == "Rain\nadjacent") |
				 	(DORMANCY.CLASS == "No dormancy" & PACKET == "Rain\nproximal"))

surv.mean.nd.rain <- as.data.frame(lapply(surv.mean.nd.rain, unlist))

library(grid)
p1 <- ggplot(surv.mean.nd.rain, aes(x = EXCLUSION, y = BIOMASS))+
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
	filter((DORMANCY.CLASS == "No dormancy" & PACKET == "Bank\nadjacent") |
				 	(DORMANCY.CLASS == "No dormancy" & PACKET == "Bank\nproximal"))

surv.mean.nd.bank <- as.data.frame(lapply(surv.mean.nd.bank, unlist))

p2 <- ggplot(surv.mean.nd.bank, aes(x = EXCLUSION, y = BIOMASS))+
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
	filter((DORMANCY.CLASS == "Physiological dormancy" & PACKET == "Rain\nadjacent") |
				 	(DORMANCY.CLASS == "Physiological dormancy" & PACKET == "Rain\nproximal"))

surv.mean.pd.rain <- as.data.frame(lapply(surv.mean.pd.rain, unlist))

p3 <- ggplot(surv.mean.pd.rain, aes(x = EXCLUSION, y = BIOMASS))+
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
	filter((DORMANCY.CLASS == "Physiological dormancy" & PACKET == "Bank\nadjacent") |
				 	(DORMANCY.CLASS == "Physiological dormancy" & PACKET == "Bank\nproximal"))

surv.mean.pd.bank <- as.data.frame(lapply(surv.mean.pd.bank, unlist))

p4 <- ggplot(surv.mean.pd.bank, aes(x = EXCLUSION, y = BIOMASS))+
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
	filter((DORMANCY.CLASS == "Physical dormancy" & PACKET == "Rain\nadjacent") |
				 	(DORMANCY.CLASS == "Physical dormancy" & PACKET == "Rain\nproximal"))

surv.mean.py.rain <- as.data.frame(lapply(surv.mean.py.rain, unlist))

p5 <- ggplot(surv.mean.py.rain, aes(x = EXCLUSION, y = BIOMASS))+
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
	filter((DORMANCY.CLASS == "Physical dormancy" & PACKET == "Bank\nadjacent") |
				 	(DORMANCY.CLASS == "Physical dormancy" & PACKET == "Bank\nproximal"))

surv.mean.py.bank <- as.data.frame(lapply(surv.mean.py.bank, unlist))

p6 <- ggplot(surv.mean.py.bank, aes(x = EXCLUSION, y = BIOMASS))+
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

ggsave("Output/Seed-survival.png",
			 width = 14, height = 8, dpi = 300, units = 'in')
