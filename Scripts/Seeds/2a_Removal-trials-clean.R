## --------------- HEADER ------------------------------------------------------
## Script name: 2a_Seed-removal-clean.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliaton: University of Florida
## Date Created: 2021-11-22
## Date Last Modified: 2022-05-10
## Copyright (c) David S. Mason, 2021
## Contact: masond@ufl.edu, @EcoGraffito
## Purpose of script: This is a script for cleaning the seed removal data and 
## calculating the number of seeds lost. The output of this script will be a 
## cleaned spreadsheet for further analysis.

## --------------- SET—UP WORKSPACE --------------------------------------------
library(tidyverse)
library(tidylog)
library(lubridate)
library(styler)

# Seed removal table (not in tidy format)
seed.rm <- read.csv("Animals-plants-seeds/Raw-data/Seeds/Seed-removal-raw.csv")
str(seed.rm)

# Seed removal survival table (tidy format)
surv.tbl <- read.csv("Animals-plants-seeds/Raw-data/Seeds/Seed-removal-survival-table.csv")


## --------------- WRITE DATA -------------------------------------------------
write.csv(seed.rm,
					"Animals-plants-seeds/Clean-data/Seeds/Seed-removal.csv",
					row.names = FALSE)

write.csv(surv.tbl,
					"Animals-plants-seeds/Clean-data/Seeds/Seed-removal-survival-table.csv",
					row.names = FALSE)
