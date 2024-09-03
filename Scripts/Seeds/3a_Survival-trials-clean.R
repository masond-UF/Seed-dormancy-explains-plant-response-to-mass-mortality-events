## --------------- HEADER ------------------------------------------------------
## Script name: 3a_Seed-survival-clean.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliaton: University of Florida
## Date Created: 2021-11-22
## Date Last modified: 2021-06-01
## Copyright (c) David S. Mason, 2021
## Contact: masond@ufl.edu, @EcoGraffito
## Purpose of script: This will be a script for cleaning the seed survival data.
## This script permutates missing data, and calculates the effect size (survival
## compared to the reference). The output of this script is a cleaned, complete
## spreadsheet with the effect size to be used in further analysis.

## --------------- SET—UP WORKSPACE --------------------------------------------
library(tidyverse)
library(tidylog)
library(lubridate)
library(styler)

rm(list = ls())

seed.surv <- read.csv("Animals-plants-seeds/Raw-data/Seeds/Seed-survival-raw.csv")

head(seed.surv) 
tail(seed.surv)

## --------------- CHECK FOR NAs -----------------------------------------------

n <- 0

for(i in 1:nrow(seed.surv)){
	if(is.na(seed.surv$FINAL.STATUS[i]) == TRUE){
	n <- n + 1
	}
}

n <- 0

for(i in 1:nrow(seed.surv)){
	if(is.na(seed.surv$FINAL.STATUS.NA[i]) == TRUE){
		n <- n + 1
	}
}

n <- 0

for(i in 1:nrow(seed.surv)){
	if(is.na(seed.surv$FINAL.STATUS.NO.NA[i]) == TRUE){
		n <- n + 1
	}
}


## --------------- PERMUTATE MISSING DATA --------------------------------------

# library(mice)
# imputed <- mice(data = seed.surv)
# seed.surv <- complete(imputed)

## --------------- RENAME FACTOR LEVELS ----------------------------------------

# rename levels of factor
levels(seed.surv$PACKET)[levels(seed.surv$PACKET)=="AA"] <- "Rain adjacent"
levels(seed.surv$PACKET)[levels(seed.surv$PACKET)=="AT"] <- "Rain proximal"
levels(seed.surv$PACKET)[levels(seed.surv$PACKET)=="BA"] <- "Bank adjacent"
levels(seed.surv$PACKET)[levels(seed.surv$PACKET)=="BU"] <- "Bank proximal"
levels(seed.surv$PACKET)[levels(seed.surv$PACKET)=="A"] <- "Rain"
levels(seed.surv$PACKET)[levels(seed.surv$PACKET)=="B"] <- "Bank"


levels(seed.surv$TYPE)[levels(seed.surv$TYPE)=="ND"] <- "No dormancy"
levels(seed.surv$TYPE)[levels(seed.surv$TYPE)=="PD"] <- "Physiological dormancy"
levels(seed.surv$TYPE)[levels(seed.surv$TYPE)=="PY"] <- "Physical dormancy"

levels(seed.surv$TREATMENT)[levels(seed.surv$TREATMENT)=="S"] <- "Scavenger exclusion"
levels(seed.surv$TREATMENT)[levels(seed.surv$TREATMENT)=="H"] <- "Herbivore exclusion"
levels(seed.surv$TREATMENT)[levels(seed.surv$TREATMENT)=="O"] <- "Open"

seed.surv$BIOMASS <- as.factor(seed.surv$BIOMASS)
levels(seed.surv$BIOMASS)[levels(seed.surv$BIOMASS)=="C"] <- "Low biomass"
levels(seed.surv$BIOMASS)[levels(seed.surv$BIOMASS)=="M"] <- "High biomass"

# reorder levels of factor
seed.surv$PACKET <- factor(seed.surv$PACKET, levels = c("Bank", "Bank proximal",
																											"Bank adjacent", "Rain",
																											"Rain proximal", "Rain adjacent"))

# make the label take up two lines
levels(seed.surv$PACKET) <- gsub(" ", "\n", levels(seed.surv$PACKET))

seed.surv$BIOMASS <- factor(seed.surv$BIOMASS,
														levels = c(levels(seed.surv$BIOMASS), "No carrion"))
seed.surv$TREATMENT <- factor(seed.surv$TREATMENT,
															levels = c(levels(seed.surv$TREATMENT), "Reference"))

for(i in 1:nrow(seed.surv)){
	if (seed.surv$PLOT[i] == "REF"){
		seed.surv$BIOMASS[i] <- "No carrion"
		seed.surv$TREATMENT[i] <- "Reference"
	}
}

## --------------- SELECT USABLE COLUMNS ---------------------------------------

seed.surv <- seed.surv %>% 
	select(SITE:ID, FINAL.STATUS:FINAL.STATUS.NO.NA)

## --------------- FIX GG BLOCK P4 ---------------------------------------------

# This plot should be high biomass scavenger exclusion (MS). However, it
# is labelled in the data set as MO, which is the same as p6

for ( i in 1:nrow(seed.surv)){if (seed.surv$SITE[i] == "GG" & seed.surv$PLOT[i] == "4"){seed.surv$TREATMENT[i] <- "Scavenger exclusion"}}


## --------------- WRITE CLEAN DATA --------------------------------------------

write.csv(seed.surv,
					"Animals-plants-seeds/Clean-data/Seeds/Seed-survival.csv",
					row.names = FALSE)

## --------------- OLD STUFF ---------------------------------------------------

## --------------- CALCULATE EFFECT SIZE ---------------------------------------
seed.surv.ref <-  seed.surv %>% 
	filter(PLOT == "REF")

seed.surv <-  seed.surv %>% 
	filter(PLOT != "REF") # separate out the reference plots

# Create effect size dataframe
seed.surv.ref <- seed.surv.ref %>% 
	group_by(SITE, PACKET, TYPE) %>% 
	summarize(counts=n(), sums=sum(FINAL.STATUS)) %>% 
	mutate(prop = sums/counts) %>% 
	select(-counts, -sums)

seed.surv <- seed.surv %>% 
	unite("TREATMENT", BIOMASS:TREATMENT, sep = "_") %>% 
	group_by(SITE, TREATMENT, PACKET, TYPE) %>% 
	summarize(counts=n(), sums=sum(FINAL.STATUS)) %>% 
	mutate(SURV = sums/counts) %>% 
	select(-counts, -sums) %>% 
	mutate(REF = ifelse((SITE == "DF" & TYPE == "ND") & (PACKET == "AA" | PACKET == "AT"), seed.surv.ref[1,4],
											ifelse((SITE == "DF" & TYPE == "PD") & (PACKET == "AA" | PACKET == "AT"), seed.surv.ref[2,4],
														 ifelse((SITE == "DF" & TYPE == "PY") & (PACKET == "AA" | PACKET == "AT"), seed.surv.ref[3,4],
														 			 ifelse((SITE == "DF" & TYPE == "ND") & (PACKET == "BA" | PACKET == "BU"), seed.surv.ref[4,4],
														 			 			 ifelse((SITE == "DF" & TYPE == "PD") & (PACKET == "BA" | PACKET == "BU"), seed.surv.ref[5,4],
														 			 			 			 ifelse((SITE == "DF" & TYPE == "PY") & (PACKET == "BA" | PACKET == "BU"), seed.surv.ref[6,4],
														 			 			 			 			 ifelse((SITE == "GG" & TYPE == "ND") & (PACKET == "AA" | PACKET == "AT"), seed.surv.ref[7,4],
														 			 			 			 			 			 ifelse((SITE == "GG" & TYPE == "PD") & (PACKET == "AA" | PACKET == "AT"), seed.surv.ref[8,4],
														 			 			 			 			 			 			 ifelse((SITE == "GG" & TYPE == "PY") & (PACKET == "AA" | PACKET == "AT"), seed.surv.ref[9,4],
														 			 			 			 			 			 			 			 ifelse((SITE == "GG" & TYPE == "ND") & (PACKET == "BA" | PACKET == "BU"), seed.surv.ref[10,4],
														 			 			 			 			 			 			 			 			 ifelse((SITE == "GG" & TYPE == "PD") & (PACKET == "BA" | PACKET == "BU"), seed.surv.ref[11,4],
														 			 			 			 			 			 			 			 			 			 ifelse((SITE == "GG" & TYPE == "PY") & (PACKET == "BA" | PACKET == "BU"), seed.surv.ref[12,4],	
														 			 			 			 			 			 			 			 			 			 			 ifelse((SITE == "OS" & TYPE == "ND") & (PACKET == "AA" | PACKET == "AT"), seed.surv.ref[13,4],
														 			 			 			 			 			 			 			 			 			 			 			 ifelse((SITE == "OS" & TYPE == "PD") & (PACKET == "AA" | PACKET == "AT"), seed.surv.ref[14,4],
														 			 			 			 			 			 			 			 			 			 			 			 			 ifelse((SITE == "OS" & TYPE == "PY") & (PACKET == "AA" | PACKET == "AT"), seed.surv.ref[15,4],
														 			 			 			 			 			 			 			 			 			 			 			 			 			 ifelse((SITE == "OS" & TYPE == "ND") & (PACKET == "BA" | PACKET == "BU"), seed.surv.ref[16,4],
														 			 			 			 			 			 			 			 			 			 			 			 			 			 			 ifelse((SITE == "OS" & TYPE == "PD") & (PACKET == "BA" | PACKET == "BU"), seed.surv.ref[17,4],
														 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 ifelse((SITE == "OS" & TYPE == "PY") & (PACKET == "BA" | PACKET == "BU"), seed.surv.ref[18,4],
														 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 ifelse((SITE == "WP" & TYPE == "ND") & (PACKET == "AA" | PACKET == "AT"), seed.surv.ref[19,4],
														 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 ifelse((SITE == "WP" & TYPE == "PD") & (PACKET == "AA" | PACKET == "AT"), seed.surv.ref[20,4],
														 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 ifelse((SITE == "WP" & TYPE == "PY") & (PACKET == "AA" | PACKET == "AT"), seed.surv.ref[21,4],
														 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 ifelse((SITE == "WP" & TYPE == "ND") & (PACKET == "BA" | PACKET == "BU"), seed.surv.ref[22,4],
														 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 ifelse((SITE == "WP" & TYPE == "PD") & (PACKET == "BA" | PACKET == "BU"), seed.surv.ref[23,4],
														 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 ifelse((SITE == "WP" & TYPE == "PY") & (PACKET == "BA" | PACKET == "BU"), seed.surv.ref[24,4],
														 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 			 NA)))))))))))))))))))))))))
# Get the difference
seed.surv$diff <- seed.surv$SURV - as.numeric(seed.surv$REF)
seed.surv <- seed.surv %>% 
	separate(TREATMENT, c("BIOMASS", "EXCL"), sep = "_")


## --------------- WRITE EFFECT SIZE DATA --------------------------------------
seed.surv$REF <- as.numeric(seed.surv$REF)

write.csv(seed.surv, "Animals-plants-seeds/Clean-data/Seeds/Seed-survival-ES.csv",
					row.names = FALSE)
