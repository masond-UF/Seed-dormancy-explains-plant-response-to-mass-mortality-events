## --------------- HEADER ------------------------------------------------------
## Script name: 3_Cam-traps-analysis.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliaton: University of Florida
## Date Created: 2021-11-23
## Date Last Modified: 2021-06-04
## Copyright (c) David S. Mason, 2022
## Contact: masond@ufl.edu, @EcoGraffito
## Purpose of script: This script conducts a basic general linear model on the
## camera trap data. The output of this script (should be) should be the model
## summary and coefficients.

## --------------- SET—UP WORKSPACE --------------------------------------------
library(tidyverse)
library(tidylog)
library(lubridate)
library(styler)
library(DataExplorer)
library(lme4)
library(lmerTest)
library(emmeans)
library(car)
library(RVAideMemoire)
library(DHARMa)
library(MuMIn)
library(glmmTMB)
library(broom)

# Clear the decks
rm(list = ls())

# Bring in the data
cam <- read.csv('Animals-plants-seeds/Clean-data/Animals/Camera-traps.csv')

cam$Date <- as_date(cam$Date)

## --------------- SUBSET THE DATA ---------------------------------------------

# Filter the scavenger exclusion period 21 days
decomp <- cam %>% 
	filter(Date >= "2019-04-15" & Date <= "2019-05-05")

# Filter the herbivory period
herbivory <- cam %>% 
	filter(Date > "2019-05-05")

## --------------- SUMMARIZE DECOMP DATA ---------------------------------------

# decomp$Date <- as_date(decomp$Date)
# decomp$Week <- round_date(decomp$Date, "week")

decomp.sum <- decomp %>% 
							group_by(Site, Treatment, Carrion, Exclusion, Functional) %>% 
							summarize(Total.detections = n())

decomp.sum.wd <- decomp.sum %>%
 pivot_wider(names_from = Functional, values_from = Total.detections)

decomp.sum.wd[is.na(decomp.sum.wd)] <- 0

decomp.sum <- decomp.sum.wd %>%
 pivot_longer(5:7, names_to = "Functional", values_to = "Total.detections")

decomp.sum <- decomp.sum %>% 
	filter(Functional == "Scavenger")

# # Add numeric value for date
# decomp.sum$WeekNum <- NA
# 
# for (i in 1:nrow(decomp.sum)){
# 	if (decomp.sum$Week[i] == "2019-04-14"){
# 		decomp.sum$WeekNum[i] <- 1
# 	}
# 	if (decomp.sum$Week[i] == "2019-04-21"){
# 		decomp.sum$WeekNum[i] <- 2
# 	}
# 	if (decomp.sum$Week[i] == "2019-04-28"){
# 		decomp.sum$WeekNum[i] <- 3
# 	}
# 	if (decomp.sum$Week[i] == "2019-05-05"){
# 		decomp.sum$WeekNum[i] <- 4
# 	}
# }
# 
# # Check data frame
# 6*4 # There should be 24 plots
# 24*4 # There should be 4 rows for each plot
# nrow(decomp.sum) - (24*4)
# # We are missing 14 rows
# 
# # Complete the dataframe 
# decomp.sum <- decomp.sum |>
# 	dplyr::select(-Week)
# 
# # Create missing rows
# decomp.sum <- complete(decomp.sum, Site, Treatment, WeekNum)
# 
# # Replace NA detection values with 0
# for(i in 1:nrow(decomp.sum)){
# 	if(is.na(decomp.sum$Total.detections[i])){
# 		decomp.sum$Total.detections[i] <- 0
# 	}
# }
# 
# # Fix functional
# decomp.sum$Functional <- "Scavenger"
# 
# # Fix carrion and exclusion
# for (i in 1:nrow(decomp.sum)) {
#   if (decomp.sum$Treatment[i] == "CO") {
#     decomp.sum$Carrion[i] <- "Single"
#     decomp.sum$Exclusion[i] <- "Open"
#   } else {
#     if (decomp.sum$Treatment[i] == "CS") {
#       decomp.sum$Carrion[i] <- "Single"
#       decomp.sum$Exclusion[i] <- "Scavenger"
#     } else {
#       if (decomp.sum$Treatment[i] == "CH") {
#         decomp.sum$Carrion[i] <- "Single"
#         decomp.sum$Exclusion[i] <- "Herbivore"
#       } else {
#         if (decomp.sum$Treatment[i] == "MO") {
#           decomp.sum$Carrion[i] <- "Mass"
#           decomp.sum$Exclusion[i] <- "Open"
#         } else {
#           if (decomp.sum$Treatment[i] == "MS") {
#             decomp.sum$Carrion[i] <- "Mass"
#             decomp.sum$Exclusion[i] <- "Scavenger"
#           } else {
#             if (decomp.sum$Treatment[i] == "MH") {
#               decomp.sum$Carrion[i] <- "Mass"
#               decomp.sum$Exclusion[i] <- "Herbivore"
#             }
#           }
#         }
#       }
#     }
#   }
# }

# Clear the decks
rm(decomp, decomp.sum.wd)

## --------------- SUMMARIZE HERBIVORY DATA ------------------------------------

herbivory$Date <- as_date(herbivory$Date)
herbivory$Week <- round_date(herbivory$Date, "week")

herbivory.sum <- herbivory %>% 
	group_by(Site, Treatment, Carrion, Exclusion, Functional) %>% 
	summarize(Total.detections = n())

herbivory.sum.wd <- herbivory.sum %>%
	pivot_wider(names_from = Functional, values_from = Total.detections)

herbivory.sum.wd[is.na(herbivory.sum.wd)] <- 0

herbivory.sum <- herbivory.sum.wd %>%
	pivot_longer(5:7, names_to = "Functional", values_to = "Total.detections")

herbivory.sum <- herbivory.sum %>% 
	filter(Functional == "Herbivore")
herbivory.sum <- as.data.frame(herbivory.sum)

# herbivory.sum$Total.detections <- as.numeric(herbivory.sum$Total.detections)
# herbivory.sum$Functional <- as.character(herbivory.sum$Functional)

# d <- data.frame("Site" = c("DF", "DF", "DF", "OS", "OS", "WP", "WP", "WP"),
								# "Treatment" = c("CO", "CH", "MO", "CO", "CH", "CO", "CH", "MO"), 
								# "Carrion" = c("Single", "Single", "Mass", "Single", "Single",
															# "Single", "Single", "Mass"),
								# "Exclusion" = c("Open", "Herbivore", "Open", "Open", "Herbivore",
																# "Open", "Herbivore", "Open"), 
								# "Functional" = "Herbivore",
								# "Total.detections" = 0)

# herbivory.sum <- rbind(herbivory.sum, d)

# herbivory.sum$WeekNum <- NA
# 
# for (i in 1:nrow(herbivory.sum)){
# 	if (herbivory.sum$Week[i] == "2019-05-05"){
# 		herbivory.sum$WeekNum[i] <- 1
# 	}
# 	if (herbivory.sum$Week[i] == "2019-05-12"){
# 		herbivory.sum$WeekNum[i] <- 2
# 	}
# 	if (herbivory.sum$Week[i] == "2019-05-19"){
# 		herbivory.sum$WeekNum[i] <- 3
# 	}
# 	if (herbivory.sum$Week[i] == "2019-05-26"){
# 		herbivory.sum$WeekNum[i] <- 4
# 	}
# 	if (herbivory.sum$Week[i] == "2019-06-02"){
# 		herbivory.sum$WeekNum[i] <- 5
# 	}
# 	if (herbivory.sum$Week[i] == "2019-06-09"){
# 		herbivory.sum$WeekNum[i] <- 6
# 	}
# 	if (herbivory.sum$Week[i] == "2019-06-16"){
# 		herbivory.sum$WeekNum[i] <- 7
# 	}
# 	if (herbivory.sum$Week[i] == "2019-06-23"){
# 		herbivory.sum$WeekNum[i] <- 8
# 	}
# 	if (herbivory.sum$Week[i] == "2019-06-30"){
# 		herbivory.sum$WeekNum[i] <- 9
# 	}
# 	if (herbivory.sum$Week[i] == "2019-07-07"){
# 		herbivory.sum$WeekNum[i] <- 10
# 	}
# 	if (herbivory.sum$Week[i] == "2019-07-14"){
# 		herbivory.sum$WeekNum[i] <- 11
# 	}
# 	if (herbivory.sum$Week[i] == "2019-07-21"){
# 		herbivory.sum$WeekNum[i] <- 12
# 	}
# 	if (herbivory.sum$Week[i] == "2019-07-28"){
# 		herbivory.sum$WeekNum[i] <- 13
# 	}
# 	if (herbivory.sum$Week[i] == "2019-08-04"){
# 		herbivory.sum$WeekNum[i] <- 14
# 	}
# 	if (herbivory.sum$Week[i] == "2019-08-11"){
# 		herbivory.sum$WeekNum[i] <- 15
# 	}
# 	if (herbivory.sum$Week[i] == "2019-08-25"){ # Missing dates
# 		herbivory.sum$WeekNum[i] <- 16
# 	}
# 	if (herbivory.sum$Week[i] == "2019-09-01"){
# 		herbivory.sum$WeekNum[i] <- 17
# 	}
# 	if (herbivory.sum$Week[i] == "2019-09-22"){
# 		herbivory.sum$WeekNum[i] <- 18
# 	}
# 	if (herbivory.sum$Week[i] == "2019-10-06"){
# 		herbivory.sum$WeekNum[i] <- 19
# 	}
# }
# 
# # Drop everything over week 15 because there is not complete data
# 
# herbivory.sum$WeekNum <- as.numeric(herbivory.sum$WeekNum)
# herbivory.sum <- herbivory.sum %>%
# 	filter(WeekNum < 16)
# 
# length(unique(herbivory.sum$WeekNum))
# (4*6)*15 # Missing rows again
# 
# # Complete the dataframe 
# herbivory.sum <- herbivory.sum |>
# 	dplyr::select(-Week)
# 
# # Create missing rows
# herbivory.sum <- complete(herbivory.sum, Site, Treatment, WeekNum)
# 
# # Replace NA detection values with 0
# for(i in 1:nrow(herbivory.sum)){
# 	if(is.na(herbivory.sum$Total.detections[i])){
# 		herbivory.sum$Total.detections[i] <- 0
# 	}
# }
# 
# # Fix functional
# herbivory.sum$Functional <- "Herbivore"
# 
# # Fix carrion and exclusion
# for (i in 1:nrow(herbivory.sum)) {
#   if (herbivory.sum$Treatment[i] == "CO") {
#     herbivory.sum$Carrion[i] <- "Single"
#     herbivory.sum$Exclusion[i] <- "Open"
#   } else {
#     if (herbivory.sum$Treatment[i] == "CS") {
#       herbivory.sum$Carrion[i] <- "Single"
#       herbivory.sum$Exclusion[i] <- "Scavenger"
#     } else {
#       if (herbivory.sum$Treatment[i] == "CH") {
#         herbivory.sum$Carrion[i] <- "Single"
#         herbivory.sum$Exclusion[i] <- "Herbivore"
#       } else {
#         if (herbivory.sum$Treatment[i] == "MO") {
#           herbivory.sum$Carrion[i] <- "Mass"
#           herbivory.sum$Exclusion[i] <- "Open"
#         } else {
#           if (herbivory.sum$Treatment[i] == "MS") {
#             herbivory.sum$Carrion[i] <- "Mass"
#             herbivory.sum$Exclusion[i] <- "Scavenger"
#           } else {
#             if (herbivory.sum$Treatment[i] == "MH") {
#               herbivory.sum$Carrion[i] <- "Mass"
#               herbivory.sum$Exclusion[i] <- "Herbivore"
#             }
#           }
#         }
#       }
#     }
#   }
# }

# Clear the decks
rm(herbivory, herbivory.sum.wd)

## --------------- INSEPCT THE SUMMARIZED DATA ---------------------------------

library(DataExplorer)

create_report(decomp.sum)
create_report(herbivory.sum)

library(fitdistrplus)
descdist(decomp.sum$Total.detections, discrete = TRUE) # Negative binomial
descdist(herbivory.sum$Total.detections, discrete = TRUE) # Poisson?

## --------------- MODEL DECOMP DATA -------------------------------------------

library(MASS)
m1 <- glm.nb(Total.detections ~ Treatment + Site, control = glm.control(maxit = 1000),
						 data = decomp.sum
)

summary(m1)

library(performance)
check_zeroinflation(m1)
dev.new()
check_model(m1)
check_singularity(m1, tolerance = 1e-05)

# Check for overdispersion
E2 <- resid(m1, type = "pearson")
N  <- nrow(decomp.sum)
p  <- length(coef(m1)) # '+1' is for variance parameter in NB
sum(E2^2) / (N - p)

# Check residuals
sim_m1 <- simulateResiduals(fittedModel = m1, n = 250)
plot(sim_m1)

Anova(m1)

# Means
emmeans(m1, pairwise~Treatment, type='response')

decomp.means <- data.frame(
	Treatment = c('CH', 'CO', 'CS', 'MH', 'MO', 'MS'),
	Mean = c(5.32, 151.42, 1.07, 180.52, 174.51, 2.37),
	se = c(4.05, 108.00, 0.96, 128.70, 124.42, 1.92),
	LCL = c(1.20, 37.42, 0.19, 44.64, 43.14, 0.48),
	UCL = c(23.62, 612.78, 6.15, 730.09, 705.86,11.57)
)

mean(c(180.52, 174.51)) 177.515
mean(c(5.32, 151.42)) 78.37
177.515/78.37

write.csv(decomp.means, "Animals-plants-seeds/Analysis/Animals/Decomp-means.csv",
					row.names = FALSE
)

options(scipen = 0)
emmeans <- emmeans(m1, pairwise~Treatment, type='response')

# P values

decomp.p <- emmeans(m1, pairwise~Treatment, type='response',
										adjust = 'none')
decomp.p <- as.data.frame(decomp.p)
decomp.p <- decomp.p[7:21, c(-1,-5)]

colnames(decomp.p)[1] <- 'Contrast'
colnames(decomp.p)[2] <- 'Ratio'
colnames(decomp.p)[3] <- 'se'
colnames(decomp.p)[4] <- 'LCL'
colnames(decomp.p)[5] <- 'UCL'

decomp.p$Ratio <- round(decomp.p$Ratio, digits = 2)
decomp.p$se <- round(decomp.p$se, digits = 2)
decomp.p$LCL <- round(decomp.p$LCL, digits = 2)
decomp.p$UCL <- round(decomp.p$UCL, digits = 2)

pvals <- c('0.0013', '0.1705', '0.0007', '0.0008', '0.4653', '<.0001',
					 '0.8616', '0.8881', '0.0001', '<.0001', '<.0001', '0.5084',
					 '0.9732', '0.0001', '0.0001')

decomp.p$p.value <- pvals

write.csv(decomp.p, "Animals-plants-seeds/Analysis/Animals/Decomp-contrasts.csv",
					row.names = FALSE
)


# Anova
decomp.aov <- as.data.frame(Anova(m1))
decomp.aov <- format(decomp.aov, scientific = FALSE)
write.csv(decomp.aov, "Animals-plants-seeds/Analysis/Animals/Decomp-aov.csv",
					row.names = FALSE
)

# Explained variance
with(summary(m1), 1 - deviance/null.deviance)
# 67%

## --------------- MODEL HERB DATA ---------------------------------------------

m2 <- glm(Total.detections ~ Treatment + Site, family = 'poisson',
						 data = herbivory.sum
)

library(performance)
check_zeroinflation(m2) # underfitting zeroes
check_model(m2)
check_singularity(m2, tolerance = 1e-05)

# Check for overdispersion
E2 <- resid(m2, type = "pearson")
N  <- nrow(herbivory.sum)
p  <- length(coef(m2)) # '+1' is for variance parameter in NB
sum(E2^2) / (N - p) # overdispersed

# Check residuals
sim_m2 <- simulateResiduals(fittedModel = m2, n = 250)
plot(sim_m2)
Anova(m)

# Means
options(scipen=999)
herb.means <- emmeans(m2, pairwise~Treatment, type='response')
herb.means <- as.data.frame(herb.means)
herb.means <- herb.means[1:6, c(-2,-5)]

colnames(herb.means)[2] <- 'Mean'
colnames(herb.means)[3] <- 'se'
colnames(herb.means)[4] <- 'LCL'
colnames(herb.means)[5] <- 'UCL'

herb.means[6,5] <- 0

herb.means$Mean <- round(herb.means$Mean, digits = 2)
herb.means$se <- round(herb.means$se, digits = 2)
herb.means$LCL <- round(herb.means$LCL, digits = 2)
herb.means$UCL <- round(herb.means$UCL, digits = 2)

write.csv(herb.means, "Animals-plants-seeds/Analysis/Animals/Herb-means.csv",
					row.names = FALSE
)

# P values

herb.p <- emmeans(m2, pairwise~Treatment, type='response', adjust = 'none')
herb.p <- as.data.frame(herb.p)
herb.p <- herb.p[7:21, c(-1,-5)]

for(i in 1:nrow(herb.p)){
	if(herb.p$asymp.UCL[i] == 'Inf'){
		 herb.p$asymp.UCL[i] <- 0
	}
}

colnames(herb.p)[1] <- 'Contrast'
colnames(herb.p)[2] <- 'Ratio'
colnames(herb.p)[3] <- 'se'
colnames(herb.p)[4] <- 'LCL'
colnames(herb.p)[5] <- 'UCL'

herb.p$Ratio <- round(herb.p$Ratio, digits = 2)
herb.p$se <- round(herb.p$se, digits = 2)
herb.p$LCL <- round(herb.p$LCL, digits = 2)
herb.p$UCL <- round(herb.p$UCL, digits = 2)

pvals <- c('<.0001', '0.0188', '0.2150', '0.0024', '0.9946', '<.0001',
					 '<.0001', '<.0001', '0.9937', '0.0087', '0.3196', '0.9942',
					 '0.0029', '0.9950', '0.9941')

herb.p$p.value <- pvals

write.csv(herb.p, "Animals-plants-seeds/Analysis/Animals/Herb-contrasts.csv",
					row.names = FALSE
)


# Anova
herb.aov <- as.data.frame(Anova(m2))
write.csv(herb.aov, "Animals-plants-seeds/Analysis/Animals/Herb-aov.csv",
					row.names = FALSE
)

# Explained variance
with(summary(m2), 1 - deviance/null.deviance)
# 81%
