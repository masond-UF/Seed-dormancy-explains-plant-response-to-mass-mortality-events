## --------------- HEADER ------------------------------------------------------
## Script name: 1a_Traps-composition-clean.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliaton: University of Florida
## Date Created: 2021-11-22
## Date Last Modified: 2022-05-06
## Copyright (c) David S. Mason, 2022
## Contact: masond@ufl.edu, @EcoGraffito
## Purpose of script: This is a script for cleaning the seed trap data. The 
## output of this script will be a cleaned spreadsheet.

## --------------- SET—UP WORKSPACE --------------------------------------------
library(tidyverse)
library(tidylog)
library(lubridate)
library(styler)

seed.trap <- read.csv("Animals-plants-seeds/Raw-data/Seeds/Seed-traps-raw.csv")

# Set aside trap detections (e.g., bird and scat) 
trap.scat <- seed.trap %>% dplyr::select(SITE, TREATMENT, DATE, TRAP, SCAT, BIRD)

# Drop the scat detections (e.g., bird and scat) and notes
seed.trap <- seed.trap %>% select(-SCAT, -BIRD, -NOTES)

## --------------- FIX SPECIES NAMES -------------------------------------------

# NEW NAME = OLD NAME

# ANGE / ANDRO = UNK 25 
# ANEMO2 = UNK21
# VIVI = VICIA
# VISA = UNK14
# SISY = UNK20 *CHECK*
# CODR = UNK 23
# SCLERIA1 = UNK 22 + UNK 15***
# APIAC = UNK17..BIG.APIAC.	
# EUPHORB = UNK19
# SCLERIA2 = UNK24
# PHAM4 = UNK18
# AMTR (Ambrosia trifida) = UNK7
# UNK.PY = UNK3
# FOPU (FORESTIERA PUBE) = UNK11 + UNK 12
# RHUS = UNK10
# GALIUM = APIAC/DAUCUS
# AMPS = UNK6
# VICIA3 = UNK4 
# CROTON = UNK14
# DROP UNK8 & UNK 9 & UNK2
# ARPU = NALE
# GAURA = LASE
# LINUM? = UNK1
# ANEMO3 = UNK13
# SALVIA = CLVI3
# DIGITARIA = UNK16

seed.trap <- seed.trap %>% 
	mutate(ANDRO = UNK25, ANEMO2 = UNK21, VIVI = VICIA, VISA = UNK14,
				 SISY = UNK20, CODR = UNK23, SCLERIA1 = UNK15 + UNK22,
				 APIAC = UNK17..BIG.APIAC., EUPHORB = UNK19, SCLERIA2 = UNK24,
				 PHAM4 = UNK18, ANEMO1 = ANEMO, AMTR = UNK7, UNK.PY = UNK3,
				 FOPU = UNK11 + UNK12, RHUS = UNK10..TRIFO., AMPS = UNK6, 
				 VICIA3 = UNK4, CROTON = UNK14, ARPU = NALE + ARIST, GAURA = LASE, 
				 LINUM = UNK1, ANEMO3 = UNK13, SALVIA = CLVI3, DIGITARIA = UNK16,
				 UNK = UNK5
	) %>% 
	dplyr::select(-UNK25, -UNK21, -VICIA, -UNK14, -UNK20, -UNK23, -UNK15,
								-UNK22, -UNK17..BIG.APIAC., -UNK19, -UNK24, -UNK18,
								-ANEMO, -UNK3, -UNK10..TRIFO., -UNK6, -UNK4, -UNK14, -UNK8,
								-UNK9, -UNK2, -NALE, -LASE, -UNK1, -UNK13, -CLVI3, -UNK16,
								-UNK11, -UNK12, -UNK7, -UNK5, -ARIST
	)

## --------------- FIX SCLERIA1 ------------------------------------------------

# UNK15 (Now SCLERIA1) had 3 detections at WP MME A 10/21/19, but it should
# be 2 SCLERIA1 and 1 CAREX.

seed.trap <- seed.trap %>% 
	pivot_longer(5:35, names_to = "Species", values_to = "Seeds")

# Remove the extra observation for UNK15
for(i in 1:nrow(seed.trap)){
	if(seed.trap$SITE[i] == "WP" & seed.trap$TREATMENT[i] == "MME" &
		 seed.trap$DATE[i] == "10/21/19" & seed.trap$TRAP[i] == "A" &
		 seed.trap$Species[i] == "SCLERIA1"){
		seed.trap[i,6] <- 2
	}
}

seed.trap <- seed.trap %>% 
	pivot_wider(names_from = Species, values_from = Seeds)


# Add in the value for UNK carex
seed.trap$CAREX <- 0

seed.trap <- seed.trap %>% 
	pivot_longer(5:36, names_to ="Species", values_to = "Seeds")

for(i in 1:nrow(seed.trap)){
	if(seed.trap$SITE[i] == "WP" & seed.trap$TREATMENT[i] == "MME" &
		 seed.trap$DATE[i] == "10/21/19" & seed.trap$TRAP[i] == "A" &
		 seed.trap$Species[i] == "CAREX"){
		seed.trap[i, 6] <- 1
	}
}

## --------------- ADD GENUS SPECIES -------------------------------------------

list <- as.data.frame((unique(seed.trap$Species)))
names(list)[1] <- "Species"

genus.species <- rbind(
	data.frame(Species = c("DICO"), Genus.species = c("Dichanthelium sp.")),
	data.frame(Species = c("HEHI"), Genus.species = c("Helianthus hirsutus")),
	data.frame(Species = c("ASAS"), Genus.species = c("Asclepias asperula")),
	data.frame(Species = c("RUBUS"), Genus.species = c("Rubus sp.")),
	data.frame(Species = c("ASH"), Genus.species = c("Fraxinus sp.")),
	data.frame(Species = c("ANDRO"), Genus.species = c("Andropogon sp.")),
	data.frame(Species = c("ANEMO2"), Genus.species = c("Anemochory sp. 2")),
	data.frame(Species = c("VIVI"), Genus.species = c("Vicia villosa")),
	data.frame(Species = c("VISA"), Genus.species = c("Vicia sativa")),
	data.frame(Species = c("SISY"), Genus.species = c("Sisyrinchium sp.")),
	data.frame(Species = c("CODR"), Genus.species = c("Cornus drummondii")),
	data.frame(Species = c("SCLERIA1"), Genus.species = c("Scleria sp. 1")),
	data.frame(Species = c("APIAC"), Genus.species = c("Apiaceae sp. 1")),
	data.frame(Species = c("EUPHORB"), Genus.species = c("Euphorbiaceae sp.")),
	data.frame(Species = c("SCLERIA2"), Genus.species = c("Scleria sp. 2")),
	data.frame(Species = c("PHAM4"), Genus.species = c("Phytolacca americana")),
	data.frame(Species = c("ANEMO1"), Genus.species = c("Anemochory sp. 1")),
	data.frame(Species = c("AMTR"), Genus.species = c("Ambrosia trifida")),
	data.frame(Species = c("UNK.PY"), Genus.species = c("Unknown hard seed")),
	data.frame(Species = c("FOPU"), Genus.species = c("Forestiera pubescens")),
	data.frame(Species = c("RHUS"), Genus.species = c("Rhus sp.")),
	data.frame(Species = c("AMPS"), Genus.species = c("Ambrosia psilostachya")),
	data.frame(Species = c("VICIA3"), Genus.species = c("Vicia sp.")),
	data.frame(Species = c("CROTON"), Genus.species = c("Croton sp.")),
	data.frame(Species = c("ARPU"), Genus.species = c("Aristida purpurea_Aristida purpurascens")),
	data.frame(Species = c("GAURA"), Genus.species = c("Gaura sp.")),
	data.frame(Species = c("LINUM"), Genus.species = c("Linum sp.")),
	data.frame(Species = c("ANEMO3"), Genus.species = c("Anemochory sp. 3")),
	data.frame(Species = c("SALVIA"), Genus.species = c("Salvia sp.")),
	data.frame(Species = c("DIGITARIA"), Genus.species = c("Digitaria ciliaris")),
	data.frame(Species = c("UNK"), Genus.species = c("Unknown sp.")),
	data.frame(Species = c("CAREX"), Genus.species = c("Carex sp."))
)

# Join the genus species dataframe to the plant survey dataframe
seed.trap <- full_join(seed.trap, genus.species,
									keep = FALSE
)

## --------------- ADD DORMANCY CLASS ------------------------------------------

# Bring in the Baskins and Baskins seed dormancy database
seed.dorm.db <- read.csv("Animals-plants-seeds/Clean-data/Seeds/Seed-dormancy-db.csv")

# Drop everything that isn't dormany class and species
seed.dorm.db <- seed.dorm.db %>% 
	dplyr::select(DormancyClass, Genus.species) %>% 
	distinct()

# Filter for species with more than 1 entry in the database
entry.num <- seed.dorm.db %>%
 group_by(Genus.species) %>% 
 summarise(Observations = n()) %>% 
 filter(Observations > 1)

# Save the summarized data as a vector
entry.num <- as.vector(entry.num)

# Pull out the species name 
entry.num$Genus.species <- as.character(entry.num$Genus.species)
entry.num.names <- dplyr::pull(entry.num, "Genus.species")
seed.trap$Genus.species <- as.character(seed.trap$Genus.species)

# Initiate a column
seed.trap['Entries'] <- NA

# Add a value distinguishing whether each species has a single or multiple 
# entries in the seed dormancy database

for (i in 1:nrow(seed.trap)) {
 if (seed.trap$Genus.species[i] %in% entry.num.names) {
 	seed.trap$Entries[i] <- "Multiple"
  } else {
  seed.trap$Entries[i] <- "Single"
 }
}

# Remove PYPD observation for Vicia sativa  
seed.dorm.db <- seed.dorm.db[-12805,]

# Remove PD observation for Digitaria ciliaris 
seed.dorm.db <- seed.dorm.db[-4084, ]

# Add seed dormancy class to the plant survey dataframe
seed.trap <- merge(seed.trap, seed.dorm.db,
							all.x = TRUE, sort = FALSE
)

# Reorder the columns
seed.trap <- seed.trap %>% 
	select(DATE, SITE, TREATMENT, TRAP, Genus.species, DormancyClass, Seeds)

## --------------- ADD SEED DORMANCY FUNCTIONAL GROUP FOR NAs ------------------

# Filter for species without a seed dormancy class listed
seed.trap.unk <- seed.trap %>% 
	filter(is.na(DormancyClass) == TRUE
	)

# Create a vector of the plant names that do not have a seed dormancy class
unk.species <- as.data.frame(unique(seed.trap.unk$Genus.species))
colnames(unk.species)[1] <- "Genus.species"

# For loop to add dormancy class to unlisted species from the dormany database

for (i in 1:nrow(seed.trap)) {
	if (isTRUE(seed.trap$Genus.species[i] == "Aristida purpurea_Aristida purpurascens")) {
		seed.trap$DormancyClass[i] <- NA
	}
	if (isTRUE(seed.trap$Genus.species[i] == "Ambrosia psilostachya")) {
		seed.trap$DormancyClass[i] <- "PD"
	}
	if (isTRUE(seed.trap$Genus.species[i] == "Apiaceae sp. 1")) {
		seed.trap$DormancyClass[i] <- "PD"
	}
	if (isTRUE(seed.trap$Genus.species[i] == "Anemochory sp. 1")) {
		seed.trap$DormancyClass[i] <- "ND" # Justify
	}
	if (isTRUE(seed.trap$Genus.species[i] == "Anemochory sp. 2")) {
		seed.trap$DormancyClass[i] <- "ND" # Justify
	}
	if (isTRUE(seed.trap$Genus.species[i] == "Anemochory sp. 3")) {
		seed.trap$DormancyClass[i] <- "ND" # Justify
	}
	if (isTRUE(seed.trap$Genus.species[i] == "Asclepias asperula")) {
		seed.trap$DormancyClass[i] <- "ND"
	}
	if (isTRUE(seed.trap$Genus.species[i] == "Carex sp.")) {
		seed.trap$DormancyClass[i] <- NA # Both present
	}
	if (isTRUE(seed.trap$Genus.species[i] == "Cornus drummondii")) {
		seed.trap$DormancyClass[i] <- "PD" # Most Cornus species present at NRI are PD
	}
	if (isTRUE(seed.trap$Genus.species[i] == "Croton sp")) {
		seed.trap$DormancyClass[i] <- "PD"
	}
	if (isTRUE(seed.trap$Genus.species[i] == "Dichanthelium sp.")) {
		seed.trap$DormancyClass[i] <- "PD"
	}
	if (isTRUE(seed.trap$Genus.species[i] == "Digitaria ciliaris")) {
		seed.trap$DormancyClass[i] <- "ND" # Could fix as ciliaris but need to adjust dormancy database
	}
	if (isTRUE(seed.trap$Genus.species[i] == "Euphorbiaceae sp.")) {
		seed.trap$DormancyClass[i] <- NA # Could fix as ciliaris but need to adjust dormancy database
	}
	if (isTRUE(seed.trap$Genus.species[i] == "Forestiera pubescens")) {
		seed.trap$DormancyClass[i] <- "PD" # Only genus entry is PD 
	}
	if (isTRUE(seed.trap$Genus.species[i] == "Fraxinus sp.")) {
		seed.trap$DormancyClass[i] <- "PD" # Most PD
	}
	if (isTRUE(seed.trap$Genus.species[i] == "Gaura sp.")) {
		seed.trap$DormancyClass[i] <- "PD" # Most PD
	}
	if (isTRUE(seed.trap$Genus.species[i] == "Helianthus hirsutus")) {
		seed.trap$DormancyClass[i] <- "PD"
	}
	if (isTRUE(seed.trap$Genus.species[i] == "Vicia sp.")) {
		seed.trap$DormancyClass[i] <- "PY"
	}
	if (isTRUE(seed.trap$Genus.species[i] == "Linum sp.")) {
		seed.trap$DormancyClass[i] <- "PD"
	}
	if (isTRUE(seed.trap$Genus.species[i] == "Rhus sp.")) {
		seed.trap$DormancyClass[i] <- "PY"
	}
	if (isTRUE(seed.trap$Genus.species[i] == "Rubus sp.")) {
		seed.trap$DormancyClass[i] <- "PD" # Most PD
	}
	if (isTRUE(seed.trap$Genus.species[i] == "Salvia sp.")) {
		seed.trap$DormancyClass[i] <- "PD" # Most temperate PD
	}
	if (isTRUE(seed.trap$Genus.species[i] == "Scleria sp. 1")) {
		seed.trap$DormancyClass[i] <- "PD" # Leck and Schütz 2005, may be mechanical
	} # but not PY
	if (isTRUE(seed.trap$Genus.species[i] == "Scleria sp. 2")) {
		seed.trap$DormancyClass[i] <- "PD" # Leck and Schütz 2005, may be mechanical
	} # but not PY
	if (isTRUE(seed.trap$Genus.species[i] == "Sisyrinchium sp")) {
		seed.trap$DormancyClass[i] <- "PD"
	}
	if (isTRUE(seed.trap$Genus.species[i] == "Andropogon sp")) {
		seed.trap$DormancyClass[i] <- "PD"
	}
	if (isTRUE(seed.trap$Genus.species[i] == "Gaura sp")) {
		seed.trap$DormancyClass[i] <- "PD"
	}
	if (isTRUE(seed.trap$Genus.species[i] == "Unknown sp.")) {
		seed.trap$DormancyClass[i] <- NA
	}
	if (isTRUE(seed.trap$Genus.species[i] == "Unknown hard seed")) {
		seed.trap$DormancyClass[i] <- "PY"
	}
}

# Calculate how many unknown seed dormancy class observations remain
(sum(is.na(seed.trap$DormancyClass)))/(nrow(seed.trap))
# 17% of rows are NAs

# Check to the levels of the seed dormancy classes
unique(seed.trap$DormancyClass)

# Fix seed dormancy classes
for (i in 1:nrow(seed.trap)){
	if(isTRUE(seed.trap$DormancyClass[i] == "nd")){
		seed.trap$DormancyClass[i] <- "ND"
	}
	if(isTRUE(seed.trap$DormancyClass[i] == "pd")){
		seed.trap$DormancyClass[i] <- "PD"
	}
	if(isTRUE(seed.trap$DormancyClass[i] == "PYPD")){
		seed.trap$DormancyClass[i] <- "PY"
	}
	if(isTRUE(seed.trap$DormancyClass[i] == "MPD")){
		seed.trap$DormancyClass[i] <- "PD"
	}
}

## --------------- ADD DISPERSAL MODE ------------------------------------------

dispersal <- rbind(
	data.frame(Genus.species = c("Dichanthelium sp."), Dispersal = c("Dyszoochory")), # Also gravity
	data.frame(Genus.species = c("Helianthus hirsutus"), Dispersal = c("Anemochory")),
	data.frame(Genus.species = c("Asclepias asperula"), Dispersal = c("Anemochory")),
	data.frame(Genus.species = c("Rubus sp."), Dispersal = c("Endozoochory")),
	data.frame(Genus.species = c("Fraxinus sp."), Dispersal = c("Anemochory")),
	data.frame(Genus.species = c("Andropogon sp."), Dispersal = c("Anemochory")),
	data.frame(Genus.species = c("Anemochory sp. 2"), Dispersal = c("Anemochory")),
	data.frame(Genus.species = c("Vicia villosa"), Dispersal = c("Endozoochory")), # Also gravity or ballistic
	data.frame(Genus.species = c("Vicia sativa"), Dispersal = c("Endozoochory")), # Also gravity or ballistic
	data.frame(Genus.species = c("Sisyrinchium sp."), Dispersal = c("Dyszoochory")),
	data.frame(Genus.species = c("Cornus drummondii"), Dispersal = c("Endozoochory")),
	data.frame(Genus.species = c("Scleria sp. 1"), Dispersal = c("Endozoochory")),
	data.frame(Genus.species = c("Apiaceae sp. 1"), Dispersal = c("Epizoochory")),
	data.frame(Genus.species = c("Euphorbiaceae sp."), Dispersal = NA),
	data.frame(Genus.species = c("Scleria sp. 2"), Dispersal = c("Endozoochory")),
	data.frame(Genus.species = c("Phytolacca americana"), Dispersal = c("Endozoochory")),
	data.frame(Genus.species = c("Anemochory sp. 1"), Dispersal = c("Anemochory")),
	data.frame(Genus.species = c("Ambrosia trifida"), Dispersal = c("Dyszoochory")), # Endo, dys, regurgitate
	data.frame(Genus.species = c("Unknown hard seed"), Dispersal = NA),
	data.frame(Genus.species = c("Forestiera pubescens"), Dispersal = c("Endozoochory")),
	data.frame(Genus.species = c("Rhus sp."), Dispersal = c("Endozoochory")),
	data.frame(Genus.species = c("Ambrosia psilostachya"), Dispersal = c("Dyszoochory")),
	data.frame(Genus.species = c("Vicia sp."), Dispersal = c("Endozoochory")),
	data.frame(Genus.species = c("Croton sp."), Dispersal = c("Myrmecochory")), # Ballistochory, # Myrmechory
	data.frame(Genus.species = c("Aristida purpurea_Aristida purpurascens"), Dispersal = c("Anemochory")),
	data.frame(Genus.species = c("Gaura sp."), Dispersal = c("Dyszoochory")), # CHECK
	data.frame(Genus.species = c("Linum sp."), Dispersal = c("Endozoochory")), # mucilage Kreitschitz, haase, gorb 2021
	data.frame(Genus.species = c("Anemochory sp. 3"), Dispersal = c("Anemochory")),
	data.frame(Genus.species = c("Salvia sp."), Dispersal = c("Dyszoochory")), # CHECK mucilage again
	data.frame(Genus.species = c("Digitaria ciliaris"), Dispersal = c("Endozoochory")),
	data.frame(Genus.species = c("Unknown sp."), Dispersal = NA),
	data.frame(Genus.species = c("Carex sp."), Dispersal = c("Endozoochory"))
)

# Add seed dormancy class to the plant survey dataframe
seed.trap <- merge(seed.trap, dispersal,
									 all.x = TRUE, sort = FALSE
)

# Reorder the columns
seed.trap <- seed.trap %>% 
	select(DATE, SITE, TREATMENT, TRAP, Genus.species, DormancyClass, Dispersal, Seeds)

## --------------- WRITE THE WIDE DATA -----------------------------------------

write.csv(seed.trap, 
					"Animals-plants-seeds/Clean-data/Seeds/Traps-community-matrix-lg.csv",
					row.names = FALSE)

write.csv(trap.scat,
					"Animals-plants-seeds/Clean-data/Seeds/Traps-scat.csv",
					row.names = FALSE)
