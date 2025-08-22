## --------------- HEADER ------------------------------------------------------
## Script name: 3_Decay-rate-visualization.R
## Author: Abby Jones and David Mason
## Date Created: 2019-6-6
## Date Last Modified: 2025-8-22
## Copyright (c) Abby Jones, 2025
## Contact: david.mason@jonesctr.org
## Purpose of script: This script measures the rate of decay of different 
## MMEs and single carcasses over time

#Clear
rm(list=ls())

#Library downloads
library(ggplot2)
library(dplyr)
library(plyr)
library(ggfortify)
library(lme4)
library(lattice)

# Bring in the data
decay <- read.csv("Clean-data/Carrion-decomposition/Carrion-decay-rate.csv")

# Prepare the treatment labels
decay <- decay %>%
  mutate(
    TREATMENT = case_when(
      TREATMENT == "CO" ~ "Open Single Carcass",
      TREATMENT == "CH" ~ "Herbivore Single Carcass",
      TREATMENT == "CS" ~ "Scavenger Single Carcass",
      TREATMENT == "MO" ~ "Open MME",
      TREATMENT == "MH" ~ "Herbivore MME",
      TREATMENT == "MS" ~ "Scavenger MME",
      TRUE ~ NA_character_
    )
  )

## --------------- Visualizations ----------------------------------------------

#Open Themes
theme_aj <- function (base_size=16, font=NA) { 
  theme(axis.title.x = element_text(face="bold", size=16),
        axis.text.x  = element_text(size=16, colour = "black"),
        plot.title=element_text(face="bold", size = 16,hjust=0.01),
        axis.title.y = element_text(face="bold",angle=90,size=16),
        axis.text.y  = element_text(size=16,colour = "black"),
        plot.background = element_rect(fill = NA ,colour = NA),
        plot.margin = unit(c(1, 1, 0.5, 0.5), "lines"),
        panel.background =   element_rect(fill = NA , colour = NA ,size=0.7),
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        legend.position = c(1,1),
        legend.justification=c(1,1),
        legend.background =element_blank(),
        legend.key = element_blank(),
        legend.text =   element_text(size = rel(1.2)),
        legend.title =  element_text(size = rel(1.2), face = "bold", hjust = 0)) 
}

#Theme for angling text on axises - different from previous theme_aj2
theme_aj2 <- function (base_size=16, font=NA) { 
  theme(axis.title.x = element_text(face="bold", size=16),
        axis.text.x  = element_text(angle = 45, hjust = 1,size=12, colour = "black"),
        plot.title=element_text(face="bold", size = 16,hjust=0.01),
        axis.title.y = element_text(face="bold",angle=90,size=16),
        axis.text.y  = element_text(size=12,colour = "black"),
        plot.background = element_rect(fill = NA ,colour = NA),
        plot.margin = unit(c(1, 1, 0.5, 0.5), "lines"),
        panel.background =   element_rect(fill = NA , colour = NA ,size=0.7),
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        legend.position = c(0,1),
        legend.justification=c(0,1),
        legend.background =element_blank(),
        legend.key = element_blank(),
        legend.text =   element_text(size = rel(1.2)),
        legend.title =  element_text(size = rel(1.2), face = "bold", hjust = 0)) 
}

ggplot(decay, aes(x=TREATMENT, y=DAY.REACHED, color = DECAY.STAGE)) +
  geom_boxplot(stat = "boxplot", position = "dodge2")+
  ylab("Days to Reach Each Decay Stage") + xlab("Treatment") +
  scale_x_discrete(limits = c("Open Single Carcass", "Herbivore Single Carcass",
                              "Scavenger Single Carcass", "Open MME", "Herbivore MME", "Scavenger MME"))+
  scale_color_manual(breaks = c("Early", "Advanced", "Skeletal", "Missing"),
                     values = c("red","blue","black", "gray"))+
  theme_aj2()

ggsave()


