## --------------- HEADER ------------------------------------------------------
## Script name: 5_Food-web-SI.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliation: University of Florida
## Date Created: 2021-12-23
## Date Last Modified: 2022-12-23
## Copyright (c) David S. Mason, 2022
## Contact: masond@ufl.edu, @EcoGraffito
## Purpose of script: This script cleans analyzes coyote feeding habits for the
## special issue of food webs. 

## --------------- SET—UP WORK SPACE -------------------------------------------

# Clear the deck
rm(list = ls())

# Load packages
library(tidyverse) 
library(tidylog)
library(lubridate)
library(styler)
library(car) # Anova
library(fitdistrplus) # check distribution descdist() 
library(patchwork) # combining figures
library(DHARMa)
library(emmeans)

# Bring in the data
coyote <- read.csv('Animals-plants-seeds/Clean-data/Animals/Coyote.csv')
coyote <- coyote[-c(377:385),] # remove double's from Ashley observations
coyote <- coyote |> dplyr::select(-Functional)
coyote$Date <- as_date(coyote$Date)

# Bring in the lost data
missing <- read.csv('Animals-plants-seeds/Clean-data/Animals/Missing-coyotes.csv')
missing$Date <- as_date(mdy(missing$Date))

coyote <- rbind(coyote, missing)

## --------------- CREATE DATAFRAME --------------------------------------------

# Check the notes 
unique(coyote$Note) 
# flies and bugs are keywords for fly eating behavior
# carrion, pig & carcasses are keywords for scavenging

# Add column for eating type
coyote$Diet <- NA

# Subset and characterize detections with 'flies' in Note column
flies <- dplyr::filter(coyote, grepl("flies", Note))
flies$Diet <- "Flies"

# Subset and characterize detections with 'flies' in Note column
Flies <- dplyr::filter(coyote, grepl("Flies", Note))
Flies$Diet <- "Flies"

# Subset and characterize detections with 'bugs' in Note column
bugs <- dplyr::filter(coyote, grepl("bugs", Note))
bugs$Diet <- "Flies"

# Subset and characterize feeding detections of carrion (i.e., not flies) 
carcass <- coyote |>
	dplyr::filter(Feeding > 0) |>
	dplyr::filter(grepl("carcass", Note))
carcass$Diet <- "Carrion"

carrion <- coyote |>
	dplyr::filter(Feeding > 0) |>
	dplyr::filter(grepl("carrion", Note))
carrion$Diet <- "Carrion" # Not consumption or movement (drop)

pig <- coyote |>
	dplyr::filter(Feeding > 0) |>
	dplyr::filter(grepl("pig", Note))
pig$Diet <- "Carrion" # Doesn't actually get carrion (drop)

Carcass <- coyote |>
	dplyr::filter(Feeding > 0) |>
	dplyr::filter(grepl("Carrion", Note))
Carcass$Diet <- "Carrion" # Doesn't actually get carrion (drop)

# Bring the data together
coyote <- rbind(flies, Flies, bugs, carcass, Carcass)

# Select important variables
coyote <- coyote |>
	dplyr::select(Site, Date, Treatment, Age, Sex, Diet)

# Remove transitional vectors or dataframes
rm(bugs, flies, carcass, carrion, pig, Carcass, Flies, missing)

## --------------- FEEDING PREFERENCES ANALYSIS --------------------------------

# Summarize detections by 
tot.tst <- coyote |>
	group_by(Site, Diet) |>
	summarize(Detections = n())

# Introduce true zeroes
miss <- tibble(Site = c('DF', 'GG', 'WP'),
							 Diet = c('Flies', 'Carrion', 'Flies'),
							 Detections = c(0, 0, 0))

tot.tst <- rbind(tot.tst,miss)
rm(miss)

means <- tot.tst |>
	group_by(Diet) |>
	summarize(Mean = mean(Detections))
				 
flies.lambda <- ceiling(means[2,2]) 
carrion.lambda <- ceiling(means[1,2])
# Used ceiling because round wasn't rounding up at

# Compare fly eating detections to a null 
# http://www.medicine.mcgill.ca/epidemiology/hanley/bios601/Intensity-Rate/2Intensities.pdf

poisson.test(x = c(flies.lambda$Mean, 0), alternative = c('greater'))
# population time is equal among groups, exact comparison appropriate
# p value 0.03125

# Compare fly eating detections to carrion 
poisson.test(x = c(flies.lambda$Mean, carrion.lambda$Mean))
# population time is equal among groups, exact comparison appropriate
# http://www.medicine.mcgill.ca/epidemiology/hanley/bios601/Intensity-Rate/2Intensities.pdf
# p value 0.2266

# Fly eating is greater than 0
# No significant difference between fly eating and carrion consumption

# Add the confidence intervals 
means$LCL <- c(poisson.test(carrion.lambda$Mean, conf.level = 0.95 )$conf.int[1],
	poisson.test(flies.lambda$Mean, conf.level = 0.95 )$conf.int[1])
means$UCL <- c(poisson.test(carrion.lambda$Mean, conf.level = 0.95 )$conf.int[2],
							 poisson.test(flies.lambda$Mean, conf.level = 0.95 )$conf.int[2])

## --------------- FEEDING PREFERENCES FIG -------------------------------------

# Reorder dataframe
tot.tst$Diet  <- as_factor(tot.tst$Diet)
tot.tst$Diet <- fct_relevel(tot.tst$Diet,
											c('Flies', 'Carrion'))

tot.tst <- merge(tot.tst, means)

p1 <- ggplot(tot.tst, aes(x = Diet, y = Mean)) +
	geom_jitter(aes(x = Diet, y = Detections, fill = Diet),
							size = 3.5, color = "black", stroke = 0.75, shape = 21,
							alpha = 0.4, height = 0, width = 0.15, set.seed(3))+
	geom_linerange(aes(ymin = LCL, ymax = UCL), size = 0.75) +
	geom_point(aes(fill = Diet),
						 size = 5, color = "black", pch = 21, stroke = 1)+
	scale_fill_manual(values = c("#7ad151", "#440154")) +
	scale_y_continuous(limits = c(0, 20),
										 breaks = c(0, 5, 10, 15, 20),
										 expand = c(0,0.5))+
	xlab("Food item") +
	ylab("Total detections") +
	theme_bw() +
	theme(
		text = element_text(size = 20),
		legend.position = "none",
		axis.title.y = element_text(face = "bold"),
		axis.text.x = element_text(face = "bold"),
		panel.grid.minor.y = element_blank()
	)+
	theme(aspect.ratio = 2.5)+
	annotate('text',
					 x = 1, y = 19.5,
					 label = 'Null (p=0.031)\n   Carrion (p=0.227)',
					 size = 4)

# Supplemental figure to show blocks
SI.1 <- ggplot(tot.tst, aes(x = Diet, y = Mean)) +
	geom_jitter(aes(x = Diet, y = Detections, fill = Diet, shape = Site),
							size = 3.5, color = "black", stroke = 0.75,
							alpha = 0.4, height = 0, width = 0.15, set.seed(3))+
	scale_shape_manual(values = c(21, 22, 23, 24))+
	scale_fill_manual(values = c("#7ad151", "#440154")) +
	scale_y_continuous(limits = c(0, 20),
										 breaks = c(0, 5, 10, 15, 20),
										 expand = c(0,0.5))+
	xlab("Food item") +
	ylab("Total detections") +
	theme_bw() +
	theme(
		text = element_text(size = 20),
		legend.position = "none",
		axis.title.y = element_text(face = "bold"),
		axis.text.x = element_text(face = "bold"),
		panel.grid.minor.y = element_blank()
	)+
	theme(aspect.ratio = 2.5)+
	annotate('text',
					 x = 1, y = 19.5,
					 label = 'Null (p=0.031)\n   Carrion (p=0.227)',
					 size = 4)

## --------------- TEMPORAL PATTERNS -------------------------------------------

coyote$Date <- as_date(coyote$Date)

time.tot <- coyote |>
	mutate(Week = week(Date))  |>
	group_by(Site, Week, Diet) |>
	summarize(Detections = n())

# Adding true zeroes for missing weeks
missing.16 <- tibble(Site = c('DF', 'GG', 'GG', 'OS', 'OS', 'WP', 'WP'),
									Week = c(16, 16, 16, 16, 16, 16, 16),
									Diet = c('Flies', 'Carrion', 'Flies', 'Carrion', 'Flies',
													 'Carrion', 'Flies'),
									Detections = c(0, 0, 0, 0, 0, 0, 0))

missing.17 <- tibble(Site = c('DF', 'DF', 'GG', 'GG', 'OS', 'OS', 'WP', 'WP'),
										 Week = c(17, 17, 17, 17, 17, 17, 17, 17),
										 Diet = c('Flies', 'Carrion', 'Flies', 'Carrion', 
										 				 'Flies','Carrion', 'Flies','Carrion'),
										 Detections = c(0, 0, 0, 0, 0, 0, 0, 0))

missing.18 <- tibble(Site = c('GG', 'OS', 'DF', 'DF', 'WP', 'WP'),
										 Week = c(18, 18, 18, 18, 18, 18),
										 Diet = c('Carrion', 'Carrion', 'Flies', 'Carrion',
										 				 'Flies', 'Carrion'),
										 Detections = c(0, 0, 0, 0, 0, 0))

missing.19 <- tibble(Site = c('DF', 'WP', 'GG', 'GG'),
										 Week = c(19, 19, 19, 19),
										 Diet = c('Flies', 'Flies', 'Carrion', 'Flies'),
										 Detections = c(0, 0, 0, 0))

missing.20 <- tibble(Site = c('DF', 'WP', 'GG', 'GG', 'OS'),
										 Week = c(20, 20, 20, 20, 20),
										 Diet = c('Flies', 'Carrion', 'Carrion', 'Flies',
										 				  'Flies'),
										 Detections = c(0, 0, 0, 0, 0))

missing.21 <- tibble(Site = c('DF', 'DF', 'WP', 'WP', 'GG', 'GG','OS'),
										 Week = c(21, 21, 21, 21, 21, 21, 21),
										 Diet = c('Flies', 'Carrion', 'Flies', 'Carrion',
										 				 'Flies', 'Carrion', 'Flies'),
										 Detections = c(0, 0, 0, 0, 0, 0, 0))


time.tot <- rbind(time.tot, missing.16, missing.17, missing.18, missing.19,
									missing.20, missing.21)

# Convert week to weeks since experiment
deploy.date <- as_date('2019-04-15')
deploy.week <- week(deploy.date) # Website approach yielded week 16**

# Add column for weeks since carrion deployment
time.tot <- time.tot |>
	mutate(Weeks.since = Week-deploy.week) |>
	ungroup() |>
	dplyr::select(Weeks.since, Site, Diet, Detections)

# Remove transitional vectors or dataframes
rm(missing.16, missing.17, missing.18, missing.19,
	 missing.20, deploy.date, deploy.week)

time.tot$Weeks.since <- as_factor(time.tot$Weeks.since)
time.tot$Site <- as_factor(time.tot$Site)
time.tot$Diet <- as_factor(time.tot$Diet)

descdist(time.tot$Detections, discrete = TRUE)

m1 <- glm.nb(Detections ~ Weeks.since * Diet, data = time.tot) 
# No convergence with random site effects

Anova(m1, type = 3)
summary(m1) # Standard error higher than estimate

# Interaction term p=0.0128
sim.m1 <- simulateResiduals(m1)
plot(sim.m1)

# Dharma package
testDispersion(sim.m1)

plotResiduals(m1, form = time.tot$Site)
plotResiduals(m1, form = time.tot$Diet)
plotResiduals(m1, form = time.tot$Weeks.since)

testOutliers(m1)
testQuantiles(m1)

testCategorical(m1, catPred = time.tot$Site)
testCategorical(m1, catPred = time.tot$Diet)
testCategorical(m1, catPred = time.tot$Weeks.since)

testUniformity(m1, alternative = c('two.sided'))

plot(cooks.distance(m1))

## --------------- TEMPORAL PATTERNS FIG ---------------------------------------

time.tot$Site <- as_factor(time.tot$Site)

# CI for temporal
time.tot.mean <- time.tot |>
	group_by(Weeks.since, Diet) |>
	summarize(Mean = mean(Detections))

poisson.test(as.numeric(time.tot.mean[1,3]), conf.level = 0.95 )$conf.int[1]

exactPoiCI <- function (X, conf.level=0.95) {
	alpha = 1 - conf.level
	upper <- 0.5 * qchisq(1-alpha/2, 2*X+2)
	lower <- 0.5 * qchisq(alpha/2, 2*X)
	return(c(lower, upper))
}

time.tot.mean$LCL <- NA
time.tot.mean$UCL <- NA

for(i in 1:nrow(time.tot.mean)){
	time.tot.mean$LCL[i] <- exactPoiCI(as.numeric(time.tot.mean[i,3]))[1]
	time.tot.mean$UCL[i] <-	exactPoiCI(as.numeric(time.tot.mean[i,3]))[2]	
}

time.tot <- merge(time.tot, time.tot.mean)


# Figure with just points
ggplot(time.tot, aes(x = Weeks.since, y = Detections, fill = Diet))+
	geom_jitter(size = 4, color = "black", stroke = 1, alpha = 0.4,
							shape = 21, height = 0, width = 0.35, set.seed(10))+
	scale_fill_manual(values = c("#7ad151", "#440154")) +
	xlab("Weeks since deployment") +
	ylab("Weekly detections") +
	scale_y_continuous(limits = c(0, 20),
										 breaks = c(0, 5, 10, 15, 20),
										 expand = c(0,0.5))+
	theme_bw() +
	theme(
		text = element_text(size = 20),
		legend.position = "none",
		axis.title.y = element_text(face = "bold"),
		axis.text.x = element_text(face = "bold"),
		panel.grid.minor.y = element_blank()
	)+
	theme(aspect.ratio = 1.5)+
	annotate('text',
					 x = 1.53, y = 19.75,
					 label = 'Diet*Week (p=0.013)',
					 size = 4)

# Figure with means and CI
p2 <- ggplot(time.tot, aes(x = Weeks.since, y = Detections, fill = Diet))+
	geom_linerange(aes(ymin = LCL, ymax = UCL), size = 0.35,
								 position=position_dodge(width=0.55))+
	geom_point(aes(fill = Diet, y = Mean),
						 size = 3, color = "black", pch = 21, stroke = 0.7,
						 position=position_dodge(width=0.55))+
	geom_jitter(size = 2, color = "black", stroke = 0.25,
							shape = 21, height = 0.1, width = 0.65, set.seed(13),
							alpha = 0.6)+
	scale_fill_manual(values = c("#440154","#7ad151")) +
	xlab("Weeks since deployment") +
	ylab("Weekly detections") +
	scale_y_continuous(limits = c(-0.1, 20),
										 breaks = c(0, 5, 10, 15, 20),
										 expand = c(0,0.5))+
	theme_bw() +
	theme(
		text = element_text(size = 20),
		legend.position = "none",
		axis.title.y = element_text(face = "bold"),
		axis.text.x = element_text(face = "bold"),
		panel.grid.minor.y = element_blank()
	)+
	theme(aspect.ratio = 1.5)+
	annotate('text',
					 x = 1.53, y = 19.75,
					 label = 'Diet*Week (p=0.013)',
					 size = 4)

# Supplemental figure to show blocks
SI.2 <- ggplot(time.tot, aes(x = Weeks.since, y = Detections, fill = Diet, shape = Site))+
	geom_jitter(size = 3, color = "black", stroke = 1, alpha = 0.6,
							height = 0.1, width = 0.55, set.seed(13))+
	scale_shape_manual(values = c(21, 22, 23, 24))+
	scale_fill_manual(values = c("#440154","#7ad151")) +
	xlab("Weeks since deployment") +
	ylab("Weekly detections") +
	scale_y_continuous(limits = c(-0.1, 20),
										 breaks = c(0, 5, 10, 15, 20),
										 expand = c(0,0.5))+
	theme_bw() +
	theme(
		text = element_text(size = 20),
		legend.position = "none",
		axis.title.y = element_text(face = "bold"),
		axis.text.x = element_text(face = "bold"),
		panel.grid.minor.y = element_blank()
	)+
	theme(aspect.ratio = 1.5)+
	annotate('text',
					 x = 1.53, y = 19.75,
					 label = 'Diet*Week (p=0.013)',
					 size = 4)

## --------------- Combine figures ---------------------------------------------

comb <- p1 + p2
ggsave('Animals-plants-seeds/Figures/Animals/Food-web-SI.png',
			 plot = comb,
			 dpi = 300,
			 units = 'cm',
			 width = 20,
			 height = 17)

SI.comb <- SI.1 + SI.2
ggsave('Animals-plants-seeds/Figures/Animals/Food-web-SI-supp.png',
			 plot = SI.comb,
			 dpi = 300,
			 units = 'cm',
			 width = 20,
			 height = 17)

## --------------- SUMMARY TABLES ----------------------------------------------

rm(list = ls())
coyote <- read.csv('Animals-plants-seeds/Clean-data/Animals/Coyote.csv')

coyote <- coyote[-c(377:385),] # 386
coyote <- coyote |> dplyr::select(-Functional)
coyote$Date <- as_date(coyote$Date)
class(coyote$Date)

dup <- coyote[duplicated(coyote),]
# 103/104  missing group size

# Bring in the lost data
missing <- read.csv('Animals-plants-seeds/Clean-data/Animals/Missing-coyotes.csv')
missing$Date <- mdy(missing$Date)
class(missing$Date)
dup <- missing[duplicated(missing),]

# 67/68 missing group size
# 18/19 missing group size
# 45/46 ok day/night
# 47/48 ok day/night

# Combine
coyote <- rbind(coyote, missing)

# Rows 239/240, 671/681, 811/821 missing group information or duplicated.
# Not likely duplicated because same observers.
# True duplicates would cross investigators,
# and there would be many more

# In any case, less than <1% of data if all duplicated. 


class(coyote$Date)

# coyote <- unique(coyote)
# Time of day
# 27 afternoon
# 32 morning 
# 5 day
# 65 total daytime / 507 observations
# 13%/12 day, 87%/88 night

feed <- coyote |>
	filter(Feeding > 0)
# 105 feeding events
# 22 flies = 21% 

feed.wk <- feed |>
	mutate(Week = week(Date))  |>
	group_by(Site, Week) |>
	summarize(Detections = n())

# Convert week to weeks since experiment
deploy.date <- as_date('2019-04-15')
deploy.week <- week(deploy.date) # Website approach yielded week 16**

feed.wk$Week <- feed.wk$Week-deploy.week
feed.wk[1,1] <- 'WP'
feed.wk[2,1] <- 'WP'

for(i in 1:nrow(coyote)){
	if(coyote$Site[i] == ''){
		coyote$Site[i] <- 'WP'
	}
}

all.wk <- coyote |>
	mutate(Week = week(Date))  |>
	group_by(Site, Week) |>
	summarize(Detections = n())

all.wk$Week <- all.wk$Week-deploy.week

# Checking for duplicates
nrow(unique(coyote))

no.author <- coyote |>
	dplyr::select(-Observer)
nrow(unique(no.author))

card.behavior <- coyote |>
	dplyr::select(-Site,-Plot,-Treatment,-Camera,-Cage, -Netting,
								-Carrion, -Observer)

card.only <- coyote |>
	dplyr::select(Card, Date, Time)

nrow(card.only)

card.only[duplicated(card.only) | duplicated(card.only, fromLast=TRUE)]

# Adding missing WP-4
missing <- read.csv('Animals-plants-seeds/Clean-data/Animals/WP-4-coyotes.csv')

missing$Week <- week(mdy(missing$Date))

deploy.date <- as_date('2019-04-15')
deploy.week <- week(deploy.date) # Website approach yielded week 16**

missing$Week <- missing$Week-deploy.week
