## --------------- HEADER ------------------------------------------------------
## Script name: 2c_Removal-trials-survival-analysis.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliation: University of Florida
## Date Created: 2022-05-11
## Date Last Modified: 2022-12-20
## Copyright (c) David S. Mason, 2021
## Contact: masond@ufl.edu, @EcoGraffito
## Purpose of script: This will be a script for analyzing the seed removal data

## --------------- SET—UP WORKSPACE --------------------------------------------

# Clear the decks
rm(list=ls())

# Load libraries
library(tidyverse)
library(tidylog)
library(lubridate)
library(styler)
library(fitdistrplus)
library(survminer)
library(survival)
library(broom)
library(broom.mixed)
library(coxme)

# Bring in the data
surv.tbl <- read.csv("Animals-plants-seeds/Clean-data/Seeds/Seed-removal-survival-table.csv")

## --------------- PREPARE TRIAL 1 ---------------------------------------------

# Pivot the table wide
surv.tbl.T1 <- surv.tbl %>% 
	filter(TRIAL == 1) %>% 
	dplyr::select(ID, TREATMENT, SITE, DISH, STATUS, DAY) %>% 
	pivot_wider(names_from = DAY, values_from = STATUS)

# Convet ID to factor before summing columns
surv.tbl.T1$ID <- as.factor(surv.tbl.T1$ID)

# Sum binary measurements and drop late samples
surv.tbl.T1 <- surv.tbl.T1 %>% 
	dplyr::select(-"94", -"95") %>% 
	mutate(TOTAL = rowSums(across(where(is.numeric))))

# Initiate the column
surv.tbl.T1$SURV.T <- NA	

# Convert total binary observations into survival days
for(i in 1:nrow(surv.tbl.T1)){
	if(surv.tbl.T1$TOTAL[i] == "1"){
		surv.tbl.T1$SURV.T[i] <- 1
	}
	if(surv.tbl.T1$TOTAL[i] == "2"){
		surv.tbl.T1$SURV.T[i] <- 2
	}
	if(surv.tbl.T1$TOTAL[i] == "3"){
		surv.tbl.T1$SURV.T[i] <- 3
	}
	if(surv.tbl.T1$TOTAL[i] == 4){
		surv.tbl.T1$SURV.T[i] <- 4
	}
	if(surv.tbl.T1$TOTAL[i] == 5){
		surv.tbl.T1$SURV.T[i] <- 5
	}
	if(surv.tbl.T1$TOTAL[i] == 6){
		surv.tbl.T1$SURV.T[i] <- 13
	}
	if(surv.tbl.T1$TOTAL[i] == 7){
		surv.tbl.T1$SURV.T[i] <- 14
	}
	if(surv.tbl.T1$TOTAL[i] == 8){
		surv.tbl.T1$SURV.T[i] <- 15
	}
}

# Add a column indicating censored observatons
surv.tbl.T1 <- surv.tbl.T1 %>% 
	dplyr::select(ID, TREATMENT, SITE, DISH, SURV.T) %>% 
	mutate(STATUS = ifelse(SURV.T == 15,1,2))

## --------------- CREATE THE MODELS -------------------------------------------

# Survival model for treatments
m1 <- survfit(Surv(SURV.T,STATUS) ~ TREATMENT, data = surv.tbl.T1)
survdiff(Surv(SURV.T,STATUS) ~ TREATMENT, data = surv.tbl.T1)

# Cox proportional hazard model
m2 <- coxph(Surv(SURV.T,STATUS) ~ TREATMENT*SITE, data = surv.tbl.T1)
gtsummary::tbl_regression(m2, exp = TRUE)

# Mixed effects cox proportional hazard model
m3 <- coxme(Surv(SURV.T,STATUS) ~ TREATMENT + (1|SITE/DISH), data = surv.tbl.T1)

m3$coefficients # -0.52 log hazard (directionality), 0.61 hazard ratio
m3$vcoef # SITE/DISH = 0.16 variance, site = 0.07 variance

## --------------- PLOT TRIAL 1 SURVIVAL PLOTS ---------------------------------

# Treatment
ggsurvplot(m1,  size = 1,  # change line size
					 conf.int = TRUE, # Add confidence interval
					 pval = TRUE, # Add p-value
					 xlab = "Days since carrion deployment",
					 font.main = c(16, "bold"),
					 font.x = c(14, "bold"),
					 font.y = c(14, "bold"),
					 legend = c(0.8, 0.8),
					 palette = c("#E7B800", "#2E9FDF"),
					 break.time.by = 3,
					 risk.table = TRUE,
					 legend.title = "",
					 legend.labs = c("Control", "Carrion"),
)

ggsurvplot_facet(m1, surv.tbl.T1, facet.by = 'SITE',
								 font.main = c(16, "bold"),
								 font.x = c(14, "bold"),
								 font.y = c(14, "bold"),
								 palette = c("#E7B800", "#2E9FDF"),
								 break.time.by = 3 )

## --------------- PREPARE TRIAL 2 ---------------------------------------------

# Pivot the table wide
surv.tbl.T2 <- surv.tbl %>% 
	filter(TRIAL == 2) %>% 
	dplyr::select(ID, TREATMENT, SITE, DISH, STATUS, DAY) %>% 
	pivot_wider(names_from = DAY, values_from = STATUS)

# Convet ID to factor before summing columns
surv.tbl.T2$ID <- as.factor(surv.tbl.T2$ID)

# Sum binary measurements for survival time 
surv.tbl.T2 <- surv.tbl.T2 %>% 
	mutate(SURV.T = rowSums(across(where(is.numeric))))

# Add a column indicating censored observatons
surv.tbl.T2 <- surv.tbl.T2 %>% 
	dplyr::select(ID, TREATMENT, SITE, DISH, SURV.T) %>% 
	mutate(STATUS = ifelse(SURV.T == 4,1,2))


## --------------- CREATE THE MODELS -------------------------------------------

# Survival model for treatments
m4 <- survfit(Surv(SURV.T,STATUS) ~ TREATMENT, data = surv.tbl.T2)
survdiff(Surv(SURV.T,STATUS) ~ TREATMENT, data = surv.tbl.T2)

# Cox proportional hazard model
m5 <- coxph(Surv(SURV.T,STATUS) ~ TREATMENT*SITE, data = surv.tbl.T2)
gtsummary::tbl_regression(m5, exp = TRUE)

# Mixed effects cox proportional hazard model
m6 <- coxme(Surv(SURV.T,STATUS) ~ TREATMENT + (1|SITE/DISH), data = surv.tbl.T2)

m6$coefficients # -0.52 log hazard (directionality), 0.61 hazard ratio
m6$vcoef # SITE/DISH = 0.16 variance, site = 0.07 variance


## --------------- PLOT TRIAL 2 SURVIVAL PLOTS ---------------------------------

# Treatment
ggsurvplot(m4,  size = 1,  # change line size
					 conf.int = TRUE, # Add confidence interval
					 pval = FALSE, # Add p-value
					 xlab = "Days since carrion deployment",
					 legend = "none",
					 font.main = c(16, "bold"),
					 font.x = c(14, "bold"),
					 font.y = c(14, "bold"),
					 palette = c("#E7B800", "#2E9FDF"),
					 break.time.by = 1,
					 risk.table = TRUE,
					 legend.labs = c("Control", "Carrion")
)

ggsurvplot_facet(m4, surv.tbl.T2, facet.by = 'SITE',
								 font.main = c(16, "bold"),
								 font.x = c(14, "bold"),
								 font.y = c(14, "bold"),
								 palette = c("#E7B800", "#2E9FDF"),
								 break.time.by = 1)


anova.coxph(m1)

