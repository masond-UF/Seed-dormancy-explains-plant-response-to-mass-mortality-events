## --------------- HEADER ------------------------------------------------------
## Script name: 2_Cam-traps-EDA.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliaton: University of Florida
## Date Created: 2021-11-19
## Date Last Modified: 2021-06-04
## Copyright (c) David S. Mason, 2022
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
cam <- read.csv('Animals-plants-seeds/Clean-data/Animals/Camera-traps.csv')

cam$Date <- as_date(cam$Date)

## --------------- BASIC EDA ---------------------------------------------------
library(DataExplorer)

plot_str(cam)
# 6 Treatments
# 5 Functional groups

plot_missing(cam)
# No missing data

## --------------- CHECK COVERAGE ----------------------------------------------

# Summarize total detections (including blanks and researchers, etc.)
# for each trap day
daily <- cam |>
	group_by(Date, Site, Treatment) |>
	summarize(Count = n())

daily$Date <- as_date(daily$Date)
sites <- as_vector(unique(cam$Site))

for(i in 1:length(sites)){
	site.i <- sites[i]
	tmp.d <- daily |>
		filter(Site == site.i)
	
	p <- ggplot(tmp.d, aes(x = Date, y = Count))+
					geom_point()+
					facet_wrap(~Treatment)+
					scale_x_date(labels = date_format("%b"),
											 date_breaks = 'month')+
					ggtitle(paste(site.i))
	
	print(p)
}

# Investigate cut off in August?
# OS MH June and July missing? MS July?
rm(daily, sites)

# Monthly totals
monthly <- cam |>
	mutate(Month = month(Date)) |>
	group_by(Month) |>
	summarize(Total = n ())

ggplot(monthly, aes(x = as_factor(Month), y = Total))+
	geom_bar(stat = 'identity')

# Monthly by site
monthly <- cam |>
	mutate(Month = month(Date)) |>
	group_by(Site, Month) |>
	summarize(Total = n ())

ggplot(monthly, aes(x = as_factor(Month), y = Total))+
	geom_bar(stat = 'identity')+
	facet_wrap(~Site)
# 1 site is really driving the patterns here

rm(monthly)

## --------------- CHECK SPECIES/FUNCTIONAL ROLE -------------------------------

# Monthly by functional role
func <- cam |>
	mutate(Month = month(Date)) |>
	group_by(Month, Functional) |>
	summarize(Total = n ())

ggplot(func, aes(x = as_factor(Month), y = Total))+
	geom_bar(stat = 'identity')+
	facet_wrap(~Functional)

# Monthly by functional role by site
func <- cam |>
	mutate(Month = month(Date)) |>
	group_by(Site, Month, Functional) |>
	summarize(Total = n ())

ggplot(func, aes(x = as_factor(Month), y = Total, fill = Site))+
	geom_bar(position = 'dodge', stat = 'identity')+
	facet_wrap(~Functional, scales = "free_y")

rm(func)

## --------------- CHECK SPECIES ----------------------------------------------

# Totals by species
species <- cam |>
	group_by(Common.name) |>
	summarize(Total = n ())

# Rank in descending order
species$Common.name <- reorder(species$Common.name, species$Total, decreasing = TRUE)

ggplot(species, aes(x = as_factor(Common.name), y = Total))+
	scale_x_discrete(guide = guide_axis(angle = 90)) +
	geom_bar(stat = 'identity')

rm(species)

## --------------- CHECK TIME --------------------------------------------------

library(data.table)
cam$Time <- as.ITime(cam$Time)
cam$Time <- hms(cam$Time)

class(cam$Time)

install.packages("remotes")
remotes::install_github("ellisvalentiner/lubridateExtras")

library(lubridateExtras)

# Group by hour
Time <- cam |>
	mutate(Hour = lubridateExtras::round_hms(Time, 'hour')) |>
	group_by(Hour) |>
	summarize(Total = n ())

ggplot(Time, aes(x = Hour, y = Total))+
	geom_bar(stat = 'identity')


rm(Time, hour)




library(data.table)
cam$Time <- as.ITime(cam$Time)

cam$Time <- round(as.POSIXct(cam$Time, format="%H:%M:%S", tz="UTC"), units="hours")

# Group by hour
Time <- cam |>
	group_by(Time) |>
	summarize(Total = n ())

Time$Hour <- NA
Time$Hour <- seq(0,24,1)

ggplot(Time, aes(x = Hour, y = Total))+
	scale_x_continuous(breaks = seq(0,24,1))+
	geom_line()+
	theme_classic()
















































