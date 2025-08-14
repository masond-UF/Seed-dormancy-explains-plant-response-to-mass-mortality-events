## --------------- HEADER ------------------------------------------------------
## Script name: 2_Cam-traps-EDA.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliation: University of Florida
## Date Created: 2021-11-19
## Date Last Modified: 2025-08-13
## Copyright (c) David S. Mason, 2025
## Contact: masond@ufl.edu, @EcoGraffito
## Purpose of script: This script does basic data exploratory analysis on the 
## camera trap data. 

## --------------- SET—UP WORKSPACE --------------------------------------------
library(tidyverse)
library(tidylog)
library(lubridate)
library(styler)

# Clear the decks
rm(list=ls())

# Bring in the data
cam <- read.csv('Clean-data/Animals/Camera-traps.csv')

cam$DATE <- as_date(cam$DATE)

## --------------- BASIC EDA ---------------------------------------------------
library(DataExplorer)

plot_str(cam)
# 6 Treatments
# 5 FUNCTIONAL groups

plot_missing(cam)
# No missing data

## --------------- CHECK COVERAGE ----------------------------------------------

# Summarize total detections (including blanks and researchers, etc.)
# for each trap day
daily <- cam |>
	group_by(DATE, SITE, TREATMENT) |>
	summarize(Count = n())

daily$DATE <- as_date(daily$DATE)
sites <- as_vector(unique(cam$SITE))

library(scales)
for(i in 1:length(sites)){
	site.i <- sites[i]
	tmp.d <- daily |>
		filter(SITE == site.i)
	
	p <- ggplot(tmp.d, aes(x = DATE, y = Count))+
					geom_point()+
					facet_wrap(~TREATMENT)+
					scale_x_date(labels = date_format("%b"),
											 date_breaks = 'month')+
					ggtitle(paste(site.i))
	
	print(p)
}

rm(daily, sites)

# Monthly totals
monthly <- cam |>
	mutate(Month = month(DATE)) |>
	group_by(Month) |>
	summarize(Total = n ())

ggplot(monthly, aes(x = as_factor(Month), y = Total))+
	geom_bar(stat = 'identity')

# Monthly by site
monthly <- cam |>
	mutate(Month = month(DATE)) |>
	group_by(SITE, Month) |>
	summarize(Total = n ())

ggplot(monthly, aes(x = as_factor(Month), y = Total))+
	geom_bar(stat = 'identity')+
	facet_wrap(~SITE)
# 1 site is really driving the patterns here

rm(monthly)

## --------------- CHECK SPECIES/FUNCTIONAL ROLE -------------------------------

# Monthly by functional role
func <- cam |>
	mutate(Month = month(DATE)) |>
	group_by(Month, FUNCTIONAL) |>
	summarize(Total = n ())

ggplot(func, aes(x = as_factor(Month), y = Total))+
	geom_bar(stat = 'identity')+
	facet_wrap(~FUNCTIONAL)

# Monthly by functional role by site
func <- cam |>
	mutate(Month = month(DATE)) |>
	group_by(SITE, Month, FUNCTIONAL) |>
	summarize(Total = n ())

ggplot(func, aes(x = as_factor(Month), y = Total, fill = SITE))+
	geom_bar(position = 'dodge', stat = 'identity')+
	facet_wrap(~FUNCTIONAL, scales = "free_y")

rm(func)

## --------------- CHECK SPECIES ----------------------------------------------

# Totals by species
species <- cam |>
	group_by(COMMON.NAME) |>
	summarize(Total = n ())

# Rank in descending order
species$COMMON.NAME <- reorder(species$COMMON.NAME, species$Total, decreasing = TRUE)

ggplot(species, aes(x = as_factor(COMMON.NAME), y = Total))+
	scale_x_discrete(guide = guide_axis(angle = 90)) +
	geom_bar(stat = 'identity')

rm(species)

## --------------- CHECK TIME --------------------------------------------------

library(data.table)
cam$TIME <- as.ITime(cam$TIME)
class(cam$TIME)

# Round time
cam <- cam |>
	mutate(TIME = as.ITime(round_date(as.POSIXct(TIME, format="%H:%M:%S"), unit = "hour"))
)

# Summarize
TIME <- cam |>
	group_by(TIME) |>
	summarize(Total = n ())

# Plot
ggplot(TIME, aes(x = TIME, y = Total))+
	scale_x_time()+
	geom_bar(stat = 'identity')

rm(TIME, hour)

library(data.table)
cam$TIME <- as.ITime(cam$TIME)

cam$TIME <- round(as.POSIXct(cam$TIME, format="%H:%M:%S", tz="UTC"), units="hours")

# Group by hour
TIME <- cam |>
	group_by(TIME) |>
	summarize(Total = n ())

TIME$Hour <- NA
TIME$Hour <- seq(0,23,1)

ggplot(TIME, aes(x = Hour, y = Total))+
	scale_x_continuous(breaks = seq(0,24,1))+
	geom_line()+
	theme_classic()
















































