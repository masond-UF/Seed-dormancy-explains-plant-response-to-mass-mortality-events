## --------------- HEADER ------------------------------------------------------
## Script name: 3e_Survival-trials-ref-boot.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliaton: University of Florida
## Date Created: 2022-05-14
## Date Last modified: 2022-05-14
## Copyright (c) David S. Mason, 2022
## Contact: masond@ufl.edu, @EcoGraffito
## Purpose of script: This script generates the sampling distribution for
## reference p-hats. Then, we will plot the estimated p-hat for the treatments
## against these distributions to see if they differ from reference expectations

## --------------- SET—UP WORKSPACE --------------------------------------------
library(tidyverse)
library(tidylog)
library(lubridate)
library(styler)
options(scipen = 999)

# Bring in the data
seed.surv <- read.csv("Animals-plants-seeds/Clean-data/Seeds/Seed-survival.csv")

# Bring in the estimated mariginal means from the models
surv.means <- read.csv("Animals-plants-seeds/Analysis/Seeds/Seed-survival-means.csv")


## --------------- PREPARE THE DATA --------------------------------------------

# Set aside the reference plots
seed.surv.ref <- seed.surv %>%
  filter(PLOT == "REF")

## --------------- OLD BOOTSTRAPPING -------------------------------------------

ref.rain.PY <- seed.surv.ref %>%
  filter(PACKET == "Rain" & TYPE == "Physical dormancy") %>%
  group_by(SITE) %>%
  summarise(SURVIVAL = list(FINAL.STATUS))

PY.pooled.p.hat <- vector()

for (i in 1:10000) {
  # Resample df
  boot.df <- R.utils::resample(unlist(ref.rain.PY[1, 2]), replace = TRUE)
  p.hat.df <- sum(boot.df) / length(boot.df)

  # Resample gg
  boot.gg <- R.utils::resample(unlist(ref.rain.PY[2, 2]), replace = TRUE)
  p.hat.gg <- sum(boot.gg) / length(boot.gg)

  # Resample os
  boot.os <- R.utils::resample(unlist(ref.rain.PY[3, 2]), replace = TRUE)
  p.hat.os <- sum(boot.os) / length(boot.os)

  # Resample wp
  boot.wp <- R.utils::resample(unlist(ref.rain.PY[4, 2]), replace = TRUE)
  p.hat.wp <- sum(boot.wp) / length(boot.wp)

  # Pooled p-hat
  PY.pooled.p.hat[i] <- as.numeric((p.hat.df + p.hat.gg + p.hat.os + p.hat.wp) /
    (length(boot.df) + length(boot.gg) + length(boot.os) + length(boot.wp)))
}

PY.pooled.p.hat <- as.data.frame(PY.pooled.p.hat)
names(PY.pooled.p.hat)[names(PY.pooled.p.hat) == "PY.pooled.p.hat"] <- "p.hat"

names(PY.pooled.p.hat)[1] <- 'p.hat'
PY.pooled.p.hat$Dormancy <- "Physical"
PY.pooled.p.hat$Packet <- "Rain"

## --------------- PHYSICAL DORMANCY RAIN --------------------------------------

# HERBIVORE 
PY.pooled.p.hat <- as.data.frame(rnorm(1000, 0.06058345	, 0.07983835))
names(PY.pooled.p.hat)[1] <- 'p.hat'
quantile(PY.pooled.p.hat$p.hat, 0.975)
quantile(PY.pooled.p.hat$p.hat, 0.025)

# Indigo = high biomass, yellow = low biomass
# Big dash = proximal, little dash = adjacent

ggplot(PY.pooled.p.hat, aes(x = p.hat))+
  geom_density(adjust = 3)+
  geom_vline(xintercept = 0.2352134, color = "red")+
  annotate("text", label = "Critcal value", x = 0.1964873+0.01, 
           y = 4, angle = -90)+
  geom_vline(xintercept = 0.147436114590544, linetype = "longdash", color = "#0000a7")+
  annotate("text", label = "Proximal", x = 0.147436114590544+0.01, 
           y = 4, angle = -90)+ # HERB MME PROX
  geom_vline(xintercept = 0.369550789417684, linetype = "dashed", color = "#0000a7")+
  annotate("text", label = "Adjacent", x = 0.369550789417684+0.01, 
           y = 4, angle = -90)+ # HERB MME ADJ
  geom_vline(xintercept = 0.04087025, linetype = "longdash", color = "#EECC16")+
  annotate("text", label = "Proximal", x = 0.04087025+0.01, 
           y = 4, angle = -90)+ # HERB CON PROX
  geom_vline(xintercept = 0.50068980, linetype = "dashed", color = "#EECC16")+
  annotate("text", label = "Adjacent", x = 0.50068980+0.01, 
           y = 4, angle = -90)+ # HERB CON ADJ
  scale_y_continuous(expand = c(0,0), limits = c(0,5))+
  theme_classic()+
  ggtitle("Physical dormancy seed rain herbivore exclusion")

## --------------- PHYSICAL DORMANCY RAIN --------------------------------------

# Indigo = herbivore, yellow = scavenger, green = open
# Big dash = proximal, little dash = adjacent
PY.pooled.p.hat <- as.data.frame(rnorm(1000, 0.06058345	, 0.07983835))
names(PY.pooled.p.hat)[1] <- 'p.hat'
quantile(PY.pooled.p.hat$p.hat, 0.975)
quantile(PY.pooled.p.hat$p.hat, 0.025)


# MME
ggplot(PY.pooled.p.hat, aes(x = p.hat))+
  geom_density(adjust = 3)+
  geom_vline(xintercept = -0.09266255, color = "black", size = 1)+
  annotate("text", label = "Critcal", x = -0.09266255+0.02, 
           y = 4, angle = -90, size = 5)+
  geom_vline(xintercept = 0.2352134, color = "black", size = 1)+
  annotate("text", label = "Critcal", x = 0.2352134+0.02, 
           y = 4, angle = -90, size = 5)+
  geom_vline(xintercept = 0.16, linetype = "longdash", 
             color = "#0000a7", size = 1)+
  # annotate("text", label = "Proximal", x = 0.147436114590544+0.01, 
           # y = 4, angle = -90)+ # HERB PROX
  geom_vline(xintercept = 0.369550789417684, linetype = "dashed", 
             color = "#0000a7", size = 1)+
  annotate("text", label = "Herbivore", x = 0.369550789417684+0.02, 
           y = 4, angle = -90, size = 5)+ # HERB ADJ
  geom_vline(xintercept = 0.1428590, linetype = "longdash", 
             color = "#EECC16", size = 1)+
  # annotate("text", label = "Proximal", x = 0.1428590+0.01, 
           # y = 4, angle = -90)+ # SCAV PROX
  geom_vline(xintercept = 0.1836664, linetype = "dashed", 
             color = "#EECC16", size = 1)+
  # annotate("text", label = "Adjacent", x = 0.1836664+0.01, 
           # y = 4, angle = -90)+ # SCAV ADJ
  geom_vline(xintercept = 0.000000006126418, linetype = "longdash", 
             color = "#008176", size = 1)+
  # annotate("text", label = "Proximal", x = 0.000000006126418+0.01, 
           # y = 4, angle = -90)+ # OPEN PROX
  geom_vline(xintercept = 0.086540459261039	, linetype = "dashed", 
             color = "#008176", size = 1)+
  # annotate("text", label = "Adjacent", x = 0.086540459261039+0.01, 
           # y = 4, angle = -90)+ # OPEN ADJ
  scale_y_continuous(expand = c(0,0), limits = c(0,5))+
  theme_classic()+
  theme(axis.text.x=element_text(size=20),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y = element_text(size=20),
        axis.title.x = element_text(size=20))+
  ylab("Density")+
  xlab("Survival probability")+
  ggtitle("Physical dormancy MME seed rain")
  

# control
ggplot(PY.pooled.p.hat, aes(x = p.hat))+
  geom_density(adjust = 3)+
  geom_vline(xintercept = -0.09266255, color = "black", size = 1)+
  annotate("text", label = "Critcal", x = -0.09266255+0.02, y = 4, angle = -90, size = 5)+
  geom_vline(xintercept = 0.2352134, color = "black", size = 1)+
  annotate("text", label = "Critcal", x = 0.2352134+0.02, y = 4, angle = -90, size = 5)+
  geom_vline(xintercept = 0.04087025, linetype = "longdash", color = "#0000a7", size = 1)+
  # annotate("text", label = "Proximal", x = 0.04087025+0.01, 
           # y = 4, angle = -90)+ # HERB PROX
  geom_vline(xintercept = 0.50068980, linetype = "dashed", color = "#0000a7", size = 1)+
  annotate("text", label = "Herbivore exclusion", x = 0.50068980+0.02, 
           y = 4, angle = -90, size = 5)+ # HERB ADJ
  geom_vline(xintercept = 0.1373115, linetype = "longdash", color = "#EECC16", size = 1)+
  # annotate("text", label = "Proximal", x = 0.1373115+0.01, 
           # y = 4, angle = -90)+ # SCAV PROX
  geom_vline(xintercept = 0.3061135, linetype = "dashed", color = "#EECC16", size = 1)+
  annotate("text", label = "Scavenger exclusion", x = 0.3061135+0.02, 
           y = 4, angle = -90, size = 5)+ # SCAV ADJ
  geom_vline(xintercept = 0.1257229, linetype = "longdash", color = "#008176", size = 1)+
  # annotate("text", label = "Proximal", x = 0.1257229+0.01, 
           # y = 4, angle = -90)+ # OPEN PROX
  geom_vline(xintercept = 0.3345788, linetype = "dashed", color = "#008176", size = 1)+
  annotate("text", label = "No exclusion", x = 0.3345788+0.02, 
           y = 4, angle = -90, size = 5)+ # OPEN ADJ
  scale_y_continuous(expand = c(0,0), limits = c(0,5))+
  theme_classic()+
  theme(axis.text.x=element_text(size=20),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y = element_text(size=20),
        axis.title.x = element_text(size=20))+
  ylab("Density")+
  xlab("Survival probability")+
  ggtitle("Physical dormancy control seed rain")

## --------------- PHYSICAL DORMANCY BANK --------------------------------------

# Indigo = herbivore, yellow = scavenger, green = open
# Big dash = proximal, little dash = adjacent
PY.pooled.p.hat <- as.data.frame(rnorm(1000, 0.1153526, 0.1424981))
names(PY.pooled.p.hat)[1] <- 'p.hat'
quantile(PY.pooled.p.hat$p.hat, 0.975)
quantile(PY.pooled.p.hat$p.hat, 0.025)

# MME
ggplot(PY.pooled.p.hat, aes(x = p.hat))+
  geom_density(adjust = 3)+
  geom_vline(xintercept = -0.1545373, color = "black", size = 1)+
  annotate("text", label = "Critcal", x = -0.1545373+0.02, 
           y = 4, angle = -90, size = 5)+
  geom_vline(xintercept = 0.4062858, color = "black", size = 1)+
  annotate("text", label = "Critcal", x = 0.4062858+0.02, 
           y = 4, angle = -90, size = 5)+
  geom_vline(xintercept = 0.01, linetype = "longdash", color = "#0000a7", size = 1)+ #adjusted value for overlap
  # annotate("text", label = "Proximal", x = 0.000000007059442+0.01, 
           # y = 4, angle = -90)+ # HERB PROX
  geom_vline(xintercept = 0.249685201581460, linetype = "dashed", color = "#0000a7", size = 1)+
  # annotate("text", label = "Herbivore exclusion", x = 0.249685201581460+0.01, 
           # y = 4, angle = -90)+ # HERB ADJ
  geom_vline(xintercept = 0.1385766, linetype = "longdash", color = "#EECC16", size = 1)+
  # annotate("text", label = "Proximal", x = 0.1385766+0.01, 
           # y = 4, angle = -90)+ # SCAV PROX
  geom_vline(xintercept = 0.1609238, linetype = "dashed", color = "#EECC16", size = 1)+
  # annotate("text", label = "Adjacent", x = 0.1609238+0.01, 
           # y = 4, angle = -90)+ # SCAV ADJ
  geom_vline(xintercept = 0.000000007560445, linetype = "longdash", color = "#008176", size = 1)+
  # annotate("text", label = "Proximal", x = 0.000000007560445+0.01, 
           # y = 4, angle = -90)+ # OPEN PROX
  geom_vline(xintercept = 0.147411354853796, linetype = "dashed", color = "#008176", size = 1)+
  # annotate("text", label = "Adjacent", x = 0.147411354853796+0.01, 
           # y = 4, angle = -90)+ # OPEN ADJ
  scale_y_continuous(expand = c(0,0), limits = c(0,5))+
  theme_classic()+
  theme(axis.text.x=element_text(size=20),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y = element_text(size=20),
        axis.title.x = element_text(size=20))+
  ylab("Density")+
  xlab("Survival probability")+
  ggtitle("Physical dormancy MME seed BANK")

# control
ggplot(PY.pooled.p.hat, aes(x = p.hat))+
  geom_density(adjust = 3)+
  geom_vline(xintercept = -0.1545373, color = "black", size = 1)+
  annotate("text", label = "Critcal", x = -0.1545373+0.02, 
           y = 4, angle = -90, size = 5)+
  geom_vline(xintercept = 0.4062858, color = "black", size = 1)+
  annotate("text", label = "Critcal", x = 0.4062858+0.02, 
           y = 4, angle = -90, size = 5)+
  geom_vline(xintercept = 0.1196824	, linetype = "longdash", color = "#0000a7", size = 1)+
  # annotate("text", label = "Proximal", x = 0.1196824+0.01, 
  # y = 4, angle = -90)+ # HERB PROX
  geom_vline(xintercept = 0.2274024, linetype = "dashed", color = "#0000a7", size = 1)+
  # annotate("text", label = "Herbivore exclusion", x = 0.2274024+0.01, 
           # y = 4, angle = -90)+ # HERB ADJ
  geom_vline(xintercept = 0.0000000151076, linetype = "longdash", color = "#EECC16", size = 1)+
  # annotate("text", label = "Proximal", x = 0.0000000151076+0.01, 
  # y = 4, angle = -90)+ # SCAV PROX
  geom_vline(xintercept = 0.2074070605165, linetype = "dashed", color = "#EECC16", size = 1)+
  # annotate("text", label = "Scavenger exclusion", x = 0.2074070605165+0.01, 
           # y = 4, angle = -90)+ # SCAV ADJ
  geom_vline(xintercept = 0.0819895, linetype = "longdash", color = "#008176", size = 1)+
  # annotate("text", label = "Proximal", x = 0.0819895+0.01, 
  # y = 4, angle = -90)+ # OPEN PROX
  geom_vline(xintercept = 0.1191278, linetype = "dashed", color = "#008176", size = 1)+
  # annotate("text", label = "No exclusion", x = 0.1191278+0.01, 
           # y = 4, angle = -90)+ # OPEN ADJ
  scale_y_continuous(expand = c(0,0), limits = c(0,5))+
  theme_classic()+
  theme(axis.text.x=element_text(size=20),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y = element_text(size=20),
        axis.title.x = element_text(size=20))+
  ylab("Density")+
  xlab("Survival probability")+
  ggtitle("Physical dormancy control seed bank")

## --------------- PHYSIOLOGICAL DORMANCY RAIN --------------------------------------

# Indigo = herbivore, yellow = scavenger, green = open
# Big dash = proximal, little dash = adjacent
PD.pooled.p.hat <- as.data.frame(rnorm(1000, 0.1781665, 0.2002127))
names(PD.pooled.p.hat)[1] <- 'p.hat'
quantile(PD.pooled.p.hat$p.hat, 0.975)
quantile(PD.pooled.p.hat$p.hat, 0.025)

# MME
ggplot(PD.pooled.p.hat, aes(x = p.hat))+
  geom_density(adjust = 3)+
  geom_vline(xintercept = -0.2365043, color = "black", size = 1)+
  annotate("text", label = "Critcal", x = -0.2365043+0.03, 
           y = 2, angle = -90, size = 5)+
  geom_vline(xintercept = 0.587684, color = "black", size = 1)+
  annotate("text", label = "Critcal", x = 0.587684+0.03, 
           y = 2, angle = -90, size = 5)+
  geom_vline(xintercept = 0.000000009371363, linetype = "longdash", color = "#0000a7", size = 1)+ #adjusted value for overlap
  # annotate("text", label = "Proximal", x = 0.000000007059442+0.01, 
  # y = 4, angle = -90)+ # HERB PROX
  geom_vline(xintercept = -0.01, linetype = "dashed", color = "#0000a7", size = 1)+
  # annotate("text", label = "Herbivore exclusion", x = 0.249685201581460+0.01, 
  # y = 4, angle = -90)+ # HERB ADJ
  geom_vline(xintercept = 0.02378932, linetype = "longdash", color = "#EECC16", size = 1)+
  # annotate("text", label = "Proximal", x = 0.1385766+0.01, 
  # y = 4, angle = -90)+ # SCAV PROX
  geom_vline(xintercept = 0.24517698, linetype = "dashed", color = "#EECC16", size = 1)+
  # annotate("text", label = "Adjacent", x = 0.1609238+0.01, 
  # y = 4, angle = -90)+ # SCAV ADJ
  geom_vline(xintercept = 0.01, linetype = "longdash", color = "#008176", size = 1)+ # adjusted for overlap
  # annotate("text", label = "Proximal", x = 0.000000007560445+0.01, 
  # y = 4, angle = -90)+ # OPEN PROX
  geom_vline(xintercept = 0.2369946369093, linetype = "dashed", color = "#008176", size = 1)+
  # annotate("text", label = "Adjacent", x = 0.147411354853796+0.01, 
  # y = 4, angle = -90)+ # OPEN ADJ
  theme_classic()+
  theme(axis.text.x=element_text(size=20),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y = element_text(size=20),
        axis.title.x = element_text(size=20))+
  ylab("Density")+
  xlab("Survival probability")+
  ggtitle("Physiological dormancy MME seed rain")

# control
ggplot(PD.pooled.p.hat, aes(x = p.hat))+
  geom_density(adjust = 3)+
  geom_vline(xintercept = -0.2365043, color = "black", size = 1)+
  annotate("text", label = "Critcal", x = -0.2365043+0.03, 
           y = 2, angle = -90, size = 5)+
  geom_vline(xintercept = 0.587684, color = "black", size = 1)+
  annotate("text", label = "Critcal", x = 0.587684+0.03, 
           y = 2, angle = -90, size = 5)+
  geom_vline(xintercept = 0.08525417, linetype = "longdash", color = "#0000a7", size = 1)+
  # annotate("text", label = "Proximal", x = 0.1196824+0.01, 
  # y = 4, angle = -90)+ # HERB PROX
  geom_vline(xintercept = 0.18077541, linetype = "dashed", color = "#0000a7", size = 1)+
  # annotate("text", label = "Herbivore exclusion", x = 0.2274024+0.01, 
  # y = 4, angle = -90)+ # HERB ADJ
  geom_vline(xintercept = 0.00000001398607, linetype = "longdash", color = "#EECC16", size = 1)+
  # annotate("text", label = "Proximal", x = 0.0000000151076+0.01, 
  # y = 4, angle = -90)+ # SCAV PROX
  geom_vline(xintercept = 0.40381741387648, linetype = "dashed", color = "#EECC16", size = 1)+
  # annotate("text", label = "Scavenger exclusion", x = 0.2074070605165+0.01, 
  # y = 4, angle = -90)+ # SCAV ADJ
  geom_vline(xintercept = 0.2176790, linetype = "longdash", color = "#008176", size = 1)+
  # annotate("text", label = "Proximal", x = 0.0819895+0.01, 
  # y = 4, angle = -90)+ # OPEN PROX
  geom_vline(xintercept = 0.4426459, linetype = "dashed", color = "#008176", size = 1)+
  # annotate("text", label = "No exclusion", x = 0.1191278+0.01, 
  # y = 4, angle = -90)+ # OPEN ADJ
  theme_classic()+
  theme(axis.text.x=element_text(size=20),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y = element_text(size=20),
        axis.title.x = element_text(size=20))+
  ylab("Density")+
  xlab("Survival probability")+
  ggtitle("")

## --------------- PHYSIOLOGICAL DORMANCY BANK ---------------------------------

# Indigo = herbivore, yellow = scavenger, green = open
# Big dash = proximal, little dash = adjacent
PD.pooled.p.hat <- as.data.frame(rnorm(1000, 0.4793626, 0.3414873))
names(PD.pooled.p.hat)[1] <- 'p.hat'
quantile(PD.pooled.p.hat$p.hat, 0.975)
quantile(PD.pooled.p.hat$p.hat, 0.025)

# MME
ggplot(PD.pooled.p.hat, aes(x = p.hat))+
  geom_density(adjust = 3)+
  geom_vline(xintercept = -0.1733264, color = "black", size = 1)+
  annotate("text", label = "Critcal", x = -0.1733264+0.03, 
           y = 2, angle = -90, size = 5)+
  geom_vline(xintercept = 1.111526, color = "black", size = 1)+
  annotate("text", label = "Critcal", x = 1.111526+0.03, 
           y = 2, angle = -90, size = 5)+
  geom_vline(xintercept = 0.1233074, linetype = "longdash", color = "#0000a7", size = 1)+ #adjusted value for overlap
  # annotate("text", label = "Proximal", x = 0.000000007059442+0.01, 
  # y = 4, angle = -90)+ # HERB PROX
  geom_vline(xintercept = 0.3528661, linetype = "dashed", color = "#0000a7", size = 1)+
  # annotate("text", label = "Herbivore exclusion", x = 0.249685201581460+0.01, 
  # y = 4, angle = -90)+ # HERB ADJ
  geom_vline(xintercept = 0.34385948186138, linetype = "longdash", color = "#EECC16", size = 1)+
  # annotate("text", label = "Proximal", x = 0.1385766+0.01, 
  # y = 4, angle = -90)+ # SCAV PROX
  geom_vline(xintercept = 0.00000001093547, linetype = "dashed", color = "#EECC16", size = 1)+
  # annotate("text", label = "Adjacent", x = 0.1609238+0.01, 
  # y = 4, angle = -90)+ # SCAV ADJ
  geom_vline(xintercept = 0.2682495, linetype = "longdash", color = "#008176", size = 1)+ # adjusted for overlap
  # annotate("text", label = "Proximal", x = 0.000000007560445+0.01, 
  # y = 4, angle = -90)+ # OPEN PROX
  geom_vline(xintercept = 0.3412678, linetype = "dashed", color = "#008176", size = 1)+
  # annotate("text", label = "Adjacent", x = 0.147411354853796+0.01, 
  # y = 4, angle = -90)+ # OPEN ADJ
  theme_classic()+
  theme(axis.text.x=element_text(size=20),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y = element_text(size=20),
        axis.title.x = element_text(size=20))+
  ylab("Density")+
  xlab("Survival probability")+
  ggtitle("Physiological dormancy MME seed bank")

# control
ggplot(PD.pooled.p.hat, aes(x = p.hat))+
  geom_density(adjust = 3)+
  geom_vline(xintercept = -0.1733264, color = "black", size = 1)+
  annotate("text", label = "Critcal", x = -0.1733264+0.03, 
           y = 2, angle = -90, size = 5)+
  geom_vline(xintercept = 1.111526, color = "black", size = 1)+
  annotate("text", label = "Critcal", x = 1.111526+0.03, 
           y = 2, angle = -90, size = 5)+
  geom_vline(xintercept = 0.07343207, linetype = "longdash", color = "#0000a7", size = 1)+
  # annotate("text", label = "Proximal", x = 0.1196824+0.01, 
  # y = 4, angle = -90)+ # HERB PROX
  geom_vline(xintercept = 0.27262424, linetype = "dashed", color = "#0000a7", size = 1)+
  # annotate("text", label = "Herbivore exclusion", x = 0.2274024+0.01, 
  # y = 4, angle = -90)+ # HERB ADJ
  geom_vline(xintercept = 0.3150102, linetype = "longdash", color = "#EECC16", size = 1)+
  # annotate("text", label = "Proximal", x = 0.0000000151076+0.01, 
  # y = 4, angle = -90)+ # SCAV PROX
  geom_vline(xintercept = 0.2815593, linetype = "dashed", color = "#EECC16", size = 1)+
  # annotate("text", label = "Scavenger exclusion", x = 0.2074070605165+0.01, 
  # y = 4, angle = -90)+ # SCAV ADJ
  geom_vline(xintercept = 0.05643967, linetype = "longdash", color = "#008176", size = 1)+
  # annotate("text", label = "Proximal", x = 0.0819895+0.01, 
  # y = 4, angle = -90)+ # OPEN PROX
  geom_vline(xintercept = 0.35395587, linetype = "dashed", color = "#008176", size = 1)+
  # annotate("text", label = "No exclusion", x = 0.1191278+0.01, 
  # y = 4, angle = -90)+ # OPEN ADJ
  theme_classic()+
  theme(axis.text.x=element_text(size=20),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y = element_text(size=20),
        axis.title.x = element_text(size=20))+
  ylab("Density")+
  xlab("Survival probability")+
  ggtitle("Physiological dormancy control seed bank")

## --------------- NO DORMANCY DORMANCY RAIN --------------------------------------

# Indigo = herbivore, yellow = scavenger, green = open
# Big dash = proximal, little dash = adjacent
ND.pooled.p.hat <- as.data.frame(rnorm(1000, 0.08821526, 0.1122884))
names(ND.pooled.p.hat)[1] <- 'p.hat'
quantile(ND.pooled.p.hat$p.hat, 0.975)
quantile(ND.pooled.p.hat$p.hat, 0.025)

# MME
ggplot(ND.pooled.p.hat, aes(x = p.hat))+
  geom_density(adjust = 3)+
  geom_vline(xintercept = -0.158547, color = "black", size = 1)+
  annotate("text", label = "Critcal", x = -0.158547+0.02, 
           y = 3.4, angle = -90, size = 5)+
  geom_vline(xintercept = 0.3063958, color = "black", size = 1)+
  annotate("text", label = "Critcal", x = 0.3063958+0.02, 
           y = 3.4, angle = -90, size = 5)+
  geom_vline(xintercept = 0.04118587, linetype = "longdash", color = "#0000a7", size = 1)+ #adjusted value for overlap
  # annotate("text", label = "Proximal", x = 0.000000007059442+0.01, 
  # y = 4, angle = -90)+ # HERB PROX
  geom_vline(xintercept = 0.70769072, linetype = "dashed", color = "#0000a7", size = 1)+
  annotate("text", label = "Herbivore", x = 0.70769072+0.02, 
           y = 3.4, angle = -90, size = 5)+ # HERB ADJ
  geom_vline(xintercept = 0.1957852	, linetype = "longdash", color = "#EECC16", size = 1)+
  # annotate("text", label = "Proximal", x = 0.1385766+0.01, 
  # y = 4, angle = -90)+ # SCAV PROX
  geom_vline(xintercept = 0.4973213, linetype = "dashed", color = "#EECC16", size = 1)+
  annotate("text", label = "Scavenger", x = 0.4973213+0.02, 
        y = 3.4, angle = -90, size = 5)+ # SCAV ADJ
  geom_vline(xintercept = 0.01092, linetype = "longdash", color = "#008176", size = 1)+ 
  # annotate("text", label = "Proximal", x = 0.000000007560445+0.01, 
  # y = 4, angle = -90)+ # OPEN PROX
  geom_vline(xintercept = 0.67, linetype = "dashed", color = "#008176", size = 1)+ # adjusted for overlap
  annotate("text", label = "No exclusion", x = 0.67+0.02, 
        y = 3.4, angle = -90, size = 5)+ # OPEN ADJ
  scale_y_continuous(expand = c(0,0), limits = c(0,4))+
  theme_classic()+
  theme(axis.text.x=element_text(size=20),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y = element_text(size=20),
        axis.title.x = element_text(size=20))+
  ylab("Density")+
  xlab("Survival probability")+
  ggtitle("No dormancy MME seed rain")

# control
ggplot(ND.pooled.p.hat, aes(x = p.hat))+
  geom_density(adjust = 3)+
  geom_vline(xintercept = -0.158547, color = "black", size = 1)+
  annotate("text", label = "Critcal", x = -0.158547+0.02, 
           y = 3.4, angle = -90, size = 5)+
  geom_vline(xintercept = 0.3063958, color = "black", size = 1)+
  annotate("text", label = "Critcal", x = 0.3063958+0.02, 
           y = 3.4, angle = -90, size = 5)+
  geom_vline(xintercept = 0.008626772, linetype = "longdash", color = "#0000a7", size = 1)+
  # annotate("text", label = "Proximal", x = 0.1196824+0.01, 
  # y = 4, angle = -90)+ # HERB PROX
  geom_vline(xintercept = 0.527937746, linetype = "dashed", color = "#0000a7", size = 1)+
  annotate("text", label = "Herbivore", x = 0.527937746+0.02, 
           y = 3.4, angle = -90 ,size = 5)+ # HERB ADJ
  geom_vline(xintercept = 0.0440082, linetype = "longdash", color = "#EECC16", size = 1)+
  # annotate("text", label = "Proximal", x = 0.0000000151076+0.01, 
  # y = 4, angle = -90)+ # SCAV PROX
  geom_vline(xintercept = 0.5864802, linetype = "dashed", color = "#EECC16", size = 1)+
  annotate("text", label = "Scavenger", x = 0.5864802+0.02, 
           y = 3.4, angle = -90, size = 5)+ # SCAV ADJ
  geom_vline(xintercept = 0.6551286, linetype = "longdash", color = "#008176", size = 1)+
  annotate("text", label = "No exclusion", x = 0.6551286+0.02, 
           y = 3.4, angle = -90, size = 5)+ # OPEN PROX
  geom_vline(xintercept = 0.7497548, linetype = "dashed", color = "#008176", size = 1)+
  annotate("text", label = "No exclusion", x = 0.7497548+0.02, 
            y = 3.4, angle = -90, size = 5)+ # OPEN ADJ
  scale_y_continuous(expand = c(0,0), limits = c(0,4))+
  theme_classic()+
  theme(axis.text.x=element_text(size=20),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y = element_text(size=20),
        axis.title.x = element_text(size=20))+
  ylab("Density")+
  xlab("Survival probability")+
  ggtitle("No dormancy control seed rain")


## --------------- NO DORMANCY DORMANCY BANK --------------------------------------

# Indigo = herbivore, yellow = scavenger, green = open
# Big dash = proximal, little dash = adjacent
ND.pooled.p.hat <- as.data.frame(rnorm(1000, 0.7673963, 0.2500464))
names(ND.pooled.p.hat)[1] <- 'p.hat'
quantile(ND.pooled.p.hat$p.hat, 0.975)
quantile(ND.pooled.p.hat$p.hat, 0.025)

# MME
ggplot(ND.pooled.p.hat, aes(x = p.hat))+
  geom_density(adjust = 3)+
  geom_vline(xintercept = 0.2933585, color = "black", size = 1)+
  annotate("text", label = "Critcal", x = 0.2933585+0.02, 
           y = 2.4, angle = -90, size = 5)+
  geom_vline(xintercept = 1.230848, color = "black", size = 1)+
  annotate("text", label = "Critcal", x = 1.230848+0.02, 
           y = 2.4, angle = -90, size = 5)+
  geom_vline(xintercept = 0.000000009648104, linetype = "longdash", color = "#0000a7", size = 1)+ #adjusted value for overlap
  annotate("text", label = "Herbivore", x = 0.000000009648104+0.02, 
             y = 2.4, angle = -90, size = 5)+ # HERB PROX
  geom_vline(xintercept = 0.571706624723850, linetype = "dashed", color = "#0000a7", size = 1)+
  # annotate("text", label = "Herbivore exclusion", x = 0.571706624723850+0.01, 
           # y = 4, angle = -90)+ # HERB ADJ
  geom_vline(xintercept = 0.04089182, linetype = "longdash", color = "#EECC16", size = 1)+
  annotate("text", label = "Scavenger", x = 0.04089182+0.02, 
           y = 2.4, angle = -90, size = 5)+ # SCAV PROX
  geom_vline(xintercept = 0.25, linetype = "dashed", color = "#EECC16", size = 1)+ # adjusted for overlap
  annotate("text", label = "Scavenger", x = 0.25+0.02, 
           y = 2.4, angle = -90, size = 5)+ # SCAV ADJ
  geom_vline(xintercept = 0.1176174, linetype = "longdash", color = "#008176", size = 1)+ 
  annotate("text", label = "No exclusion", x = 0.1176174+0.02, 
           y = 2.4, angle = -90, size = 5)+ # OPEN PROX
  geom_vline(xintercept = 0.2003487, linetype = "dashed", color = "#008176", size = 1)+ # adjusted for overlap
  annotate("text", label = "No exclusion", x = 0.2003487+0.02, 
           y = 2.4, angle = -90, size = 5)+ # OPEN ADJ
  theme_classic()+
  scale_y_continuous(expand = c(0,0), limits = c(0,3))+
  theme(axis.text.x=element_text(size=20),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y = element_text(size=20),
        axis.title.x = element_text(size=20))+
  ylab("Density")+
  xlab("Survival probability")+
  ggtitle("No dormancy MME seed bank")

# control
ggplot(ND.pooled.p.hat, aes(x = p.hat))+
  geom_density(adjust = 3)+
  geom_vline(xintercept = 0.2933585, color = "black", size = 1)+
  annotate("text", label = "", x = 0.2933585+0.03, 
           y = 2.4, angle = -90, size = 5)+
  geom_vline(xintercept = 1.230848, color = "black", size = 1)+
  annotate("text", label = "Critcal", x = 1.230848+0.03, 
           y = 2.4, angle = -90, size = 5)+
  geom_vline(xintercept = 0.0595894, linetype = "longdash", color = "#0000a7", size = 1)+
  annotate("text", label = "Herbivore", x = 0.0595894+0.03, 
           y = 2.4, angle = -90, size = 5)+ # HERB PROX
  geom_vline(xintercept = 0.2079180, linetype = "dashed", color = "#0000a7", size = 1)+
  annotate("text", label = "Herbivore", x = 0.16 + 0.03, 
           y = 2.4, angle = -90, size = 5)+ # HERB ADJ
  geom_vline(xintercept = 0.000000007398312, linetype = "longdash", color = "#EECC16", size = 1)+
  annotate("text", label = "Scavenger", x = 0.0000000151076+0.03, 
           y = 2.4, angle = -90, size = 5)+ # SCAV PROX
  geom_vline(xintercept = 0.323711600708688, linetype = "dashed", color = "#EECC16", size = 1)+
  # annotate("text", label = "Scavenger exclusion", x = 0.323711600708688+0.01, 
           # y = 4, angle = -90)+ # SCAV ADJ
  geom_vline(xintercept = 0.2182150, linetype = "longdash", color = "#008176", size = 1)+
  annotate("text", label = "No exclusion", x = 0.2182150+0.02, 
           y = 2.4, angle = -90, size = 5)+ # OPEN PROX
  # geom_vline(xintercept = 0.5814075, linetype = "dashed", color = "#008176")+
  # annotate("text", label = "No exclusion", x = 0.5814075+0.01, 
           # y = 4, angle = -90)+ # OPEN ADJ
  theme_classic()+
  scale_y_continuous(expand = c(0,0), limits = c(0,3))+
  theme(axis.text.x=element_text(size=20),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y = element_text(size=20),
        axis.title.x = element_text(size=20))+
  ylab("Density")+
  xlab("Survival probability")+
  ggtitle("No dormancy control seed bank")

