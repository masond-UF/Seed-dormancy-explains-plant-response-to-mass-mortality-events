## --------------- HEADER ------------------------------------------------------
## Script name: 1b_Dormancy-class-clean.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliation: University of Florida
## DATE Created: 2022-04-28
## Date Last modified: 2025-08-13
## Copyright (c) David S. Mason, 2025
## Contact: david.mason@jonesctr.org
## Purpose of script: This is a script for munging the unsummarized transect
 
## --------------- SET—UP WORKSPACE --------------------------------------------

# Load the packages
library(tidyverse)
library(tidylog)
library(lubridate)
library(styler)

# Clear the decks
rm(list=ls())

# Bring in the data
comb.transect <- read.csv("Raw-data/Plants/Combined-transect-lg.csv")

## --------------- FIX HERBIVORE LABEL -----------------------------------------

for(i in 1:nrow(comb.transect)){
	if(comb.transect$EXCLUSION[i] == "Herbivroe"){
		comb.transect$EXCLUSION[i] <- "Herbivore"
	}
}

## --------------- COMBINE LIKE SPECIES ----------------------------------------

# The 2021 survey was entered in long format (not a community matrix),
# so there are not values for every species at every sampling point. Thus,
# pivot wider introduces NA for those values.

comb.transect <- comb.transect %>%
  pivot_wider(names_from = SPECIES, values_from = PRESENT)

# Convert NAs to 0
comb.transect[is.na(comb.transect)] <- 0

# We have combined surveys and some of the species are listed multiple times
# under different names. Here, we will combine those before moving forward.

# Get a list of the species names
list <- as.data.frame(colnames(comb.transect[1, 8:168]))
names(list)[1] <- "SPECIES"

# Combine like columns and drop the extra column

comb.transect <- comb.transect %>%
  mutate(
    AMDR = AMDR + AMOR, # Combine columns
    BOHI2 = BOHI2 + BOHI,
    BOIS = BOIS + BOYS,
    ENPE4 = ENPE4 + ENP + ENPI,
    THFI = GREENT + GREENT2 + GREENTHREAD + WIREASTER + THIFI + PINE,
    LASE = LASE + LACE + LASE.1,
    LEVI = LEVI + LEPIDIUM,
    APIAC = APIAC + CHAER,
    AGHE4 = AGHE4 + AGALI,
    ASAS = ASAS + MILKWEED,
    PAJA = PAJA + PARON,
    PANIC = PANIC + PANICLE + DICHA2 + DICO2 + DILO + DIOL + DICO,
    NALE3 = NALE3 + NEEDLE,
    RIBS = RIBS + RIB,
    SPCOC2 = SGSC + SPCOC2,
    ERCU = ERCU + LOVE,
    ARPU = ARPU + ERPU,
    LINUM = LIRI + LIPR + LINUM,
    MELU = MELU + MED,
    HEDR = HEDR + HEDEO,
    GAURA = GAVI + GABI + GASUS + GASU + GUARA
  ) %>%
  dplyr::select( # Drop the old column
    -AMOR, -BOHI, -BOYS, -DICHA2, -DICO2, -DILO,
    -DIOL, -DICO, -ENP, -ENPI, -GASUS, -GASU, -GREENT, -GREENT, -GREENT2,
    -GREENTHREAD, -THIFI, -LACE, -LASE.1, -LEPIDIUM, -CHAER,
    -GABI, -MILKWEED, -PARON, -PANICLE, -NEEDLE, -RIB, -SGSC, -LOVE,
    -WIREASTER, -AGALI, -ERPU, -LIRI, -LIPR, -MED, -PINE, -HEDEO, -GAVI, -GUARA
  )

## --------------- COMBINE UNKNOWNS  -------------------------------------------

# Pivot the data longer
comb.transect <- comb.transect %>%
  pivot_longer(8:131, names_to = "SPECIES", values_to = "PRESENT")

# Create a vector of the unknown species
unknowns <- c(
  "RIBS", "RAGGED", "UNKF10", "UNK28", "UNKF7", "UNK24", "UNKF5",
  "UNKF8", "UNK.G6", "UNK23", "UNKF3", "UNKF3", "UNK21", "UNK26",
  "UNK27", "UNK29", "TB", "BASALASTER", "BINP", "THINAMPR", "UNKF9",
  "UNK6", "UNKC1", "UNKF", "UNKF10", "UNKF11", "UNKF12", "UNKF13",
  "UNKF14", "UNKF2", "UNKF4", "UNKF5", "UNKF9", "LF", "RM", "SG",
  "UNK.G5", "UNK2___", "UNK20"
)

# Change the name of all species in the unknowns vector
comb.transect$SPECIES[comb.transect$SPECIES %in% unknowns] <- "Unknown_plant"

# Summarize the unknowns data
comb.transect <- comb.transect %>%
  group_by(
    DATE, SITE, TREATMENT, BIOMASS, EXCLUSION, TRANSECT, DISTANCE,
    SPECIES
  ) %>%
  summarize(PRESENT = sum(PRESENT))

# Summarizing unknowns caused some double values to pop up (multiple unknowns
# at the same point). However, there were already some double values for PANIC.
# I don't know where they came from (yet).
comb.transect$PRESENT[comb.transect$PRESENT > 0] <- 1

## --------------- DROP NONPLANTS  ---------------------------------------------

# Create a vector of non-plants
non.plants <- c(
  "DEAD", "BG", "LIT", "PIG", "COW.PATTIE", "WOODSURFACE",
  "COW PATTIE"
)

# Change the name of all species in the non.plants vector
comb.transect$SPECIES[comb.transect$SPECIES %in% non.plants] <- "Non_plant"

# Remove the non-plants from the plant survey data
comb.transect <- comb.transect %>%
  filter(!SPECIES == "Non_plant")

## --------------- ADD GENUS SPECIES  ------------------------------------------

# Create a list of species to reference
list <- as.data.frame(unique(comb.transect$SPECIES))
names(list)[1] <- "SPECIES"

# Create a dataframe matching the symbols/labels in the plant survey data
# with full genus.species names.
spec.func.list <- rbind(
  data.frame(SPECIES = c("BRJA"), GENUS.SPECIES = c("Bromus japonicus")),
  data.frame(SPECIES = c("BOCU"), GENUS.SPECIES = c("Bouteloua curtipendula")),
  data.frame(SPECIES = c("AECY"), GENUS.SPECIES = c("Aegilops cylindrica")),
  data.frame(SPECIES = c("AMPS"), GENUS.SPECIES = c("Ambrosia psilostachya")),
  data.frame(SPECIES = c("ARPU"), GENUS.SPECIES = c("Aristida purpurea_Aristida purpurascens")),
  data.frame(SPECIES = c("OPUNT"), GENUS.SPECIES = c("Opuntia sp")),
  data.frame(SPECIES = c("TRRA5"), GENUS.SPECIES = c("Tragia ramosa")),
  data.frame(SPECIES = c("APIAC"), GENUS.SPECIES = c("Apiaceae sp")),
  data.frame(SPECIES = c("BOHI2"), GENUS.SPECIES = c("Bouteloua hirsuta")),
  data.frame(SPECIES = c("GAPU"), GENUS.SPECIES = c("Gaillardia pulchella")),
  data.frame(SPECIES = c("VARA"), GENUS.SPECIES = c("Valerianella radiata")),
  data.frame(SPECIES = c("NALE3"), GENUS.SPECIES = c("Nassella leucotricha")),
  data.frame(SPECIES = c("AMDR"), GENUS.SPECIES = c("Amphiachyris dracunculoides")),
  data.frame(SPECIES = c("LOPE"), GENUS.SPECIES = c("Lolium perenne")),
  data.frame(SPECIES = c("CASTI2"), GENUS.SPECIES = c("Castilleja sp")),
  data.frame(SPECIES = c("ERLO5"), GENUS.SPECIES = c("Eriogonum longifolium")),
  data.frame(SPECIES = c("PYCA2"), GENUS.SPECIES = c("Pyrrhopappus carolinianus")),
  data.frame(SPECIES = c("ASAS"), GENUS.SPECIES = c("Asclepias asperula")),
  data.frame(SPECIES = c("SIDA"), GENUS.SPECIES = c("Sida sp")),
  data.frame(SPECIES = c("CROTON"), GENUS.SPECIES = c("Croton sp")),
  data.frame(SPECIES = c("PAJA"), GENUS.SPECIES = c("Paronychia jamesii")),
  data.frame(SPECIES = c("HEHI2"), GENUS.SPECIES = c("Helianthus hirsutus")),
  data.frame(SPECIES = c("ENPE4"), GENUS.SPECIES = c("Engelmannia peristenia")),
  data.frame(SPECIES = c("EVVE"), GENUS.SPECIES = c("Evax verna")),
  data.frame(SPECIES = c("CLOVER"), GENUS.SPECIES = c("Trifolium sp")),
  data.frame(SPECIES = c("ALLIUM"), GENUS.SPECIES = c("Allium sp")),
  data.frame(SPECIES = c("VICIA"), GENUS.SPECIES = c("Vicia sp")),
  data.frame(SPECIES = c("MOSS"), GENUS.SPECIES = c("Bryophyta sp")),
  data.frame(SPECIES = c("SPCOC2"), GENUS.SPECIES = c("Sporobolus compositus")),
  data.frame(SPECIES = c("NOBI2"), GENUS.SPECIES = c("Nothoscordum bivalve")),
  data.frame(SPECIES = c("SOLID"), GENUS.SPECIES = c("Solidago sp")),
  data.frame(SPECIES = c("LASE"), GENUS.SPECIES = c("Lactuca serriola")),
  data.frame(SPECIES = c("SCOV"), GENUS.SPECIES = c("Scutellaria ovata")),
  data.frame(SPECIES = c("BUTTON"), GENUS.SPECIES = c("Diodea teres")),
  data.frame(SPECIES = c("COEQ"), GENUS.SPECIES = c("Convolvulus equitans")),
  data.frame(SPECIES = c("PLVI"), GENUS.SPECIES = c("Plantago virginica")),
  data.frame(SPECIES = c("HECR9"), GENUS.SPECIES = c("Houstonia pusilla")),
  data.frame(SPECIES = c("LITE3"), GENUS.SPECIES = c("Lindheimera texana")),
  data.frame(SPECIES = c("ASVE"), GENUS.SPECIES = c("Asclepias verticillata")),
  data.frame(SPECIES = c("PHPO3"), GENUS.SPECIES = c("Phyllanthus polygonoides")),
  data.frame(SPECIES = c("PLAR"), GENUS.SPECIES = c("Plantago aristata")),
  data.frame(SPECIES = c("HEDR"), GENUS.SPECIES = c("Hedeoma drummondii")),
  data.frame(SPECIES = c("CITE2"), GENUS.SPECIES = c("Cirsium texanum")),
  data.frame(SPECIES = c("DICI"), GENUS.SPECIES = c("Digitaria ciliaris")),
  data.frame(SPECIES = c("SISYR"), GENUS.SPECIES = c("Sisyrinchium sp")),
  data.frame(SPECIES = c("DAPU"), GENUS.SPECIES = c("Dalea purpurea")),
  data.frame(SPECIES = c("KRLA"), GENUS.SPECIES = c("Krameria lanceolata")),
  data.frame(SPECIES = c("TROH"), GENUS.SPECIES = c("Tradescantia ohiensis")),
  data.frame(SPECIES = c("AGHE4"), GENUS.SPECIES = c("Agalinis heterophylla")),
  data.frame(SPECIES = c("ANDRO2"), GENUS.SPECIES = c("Andropogon sp")),
  data.frame(SPECIES = c("LEVI"), GENUS.SPECIES = c("Lepidium virginicum")),
  data.frame(SPECIES = c("LOBEL"), GENUS.SPECIES = c("Lobelia sp")),
  data.frame(SPECIES = c("NIP"), GENUS.SPECIES = c("Lapsana communis")),
  data.frame(SPECIES = c("OENOT"), GENUS.SPECIES = c("Oenothera biennis")),
  data.frame(SPECIES = c("SACA"), GENUS.SPECIES = c("Sabatia campestris")),
  data.frame(SPECIES = c("SETAR"), GENUS.SPECIES = c("Setaria sp")),
  data.frame(SPECIES = c("TRIOD"), GENUS.SPECIES = c("Triodanis perfoliata")),
  data.frame(SPECIES = c("HYTE2"), GENUS.SPECIES = c("Hymenopappus tenuifolius")),
  data.frame(SPECIES = c("VEHA"), GENUS.SPECIES = c("Verbena halei")),
  data.frame(SPECIES = c("ERCU"), GENUS.SPECIES = c("Eragrostis curtipedicellata")),
  data.frame(SPECIES = c("ERLE11"), GENUS.SPECIES = c("Eryngium leavenworthii")),
  data.frame(SPECIES = c("LESQU"), GENUS.SPECIES = c("Lesquerella sp")),
  data.frame(SPECIES = c("ERPI5"), GENUS.SPECIES = c("Erioneuron pilosum")),
  data.frame(SPECIES = c("VEPE2"), GENUS.SPECIES = c("Veronica peregrina")),
  data.frame(SPECIES = c("BOIS"), GENUS.SPECIES = c("Bothriochloa ischaemum")),
  data.frame(SPECIES = c("BOLA"), GENUS.SPECIES = c("Bothriochloa laguroides")),
  data.frame(SPECIES = c("GLBI"), GENUS.SPECIES = c("Glandularia bipinnatifida")),
  data.frame(SPECIES = c("PANIC"), GENUS.SPECIES = c("Panicum sp")),
  data.frame(SPECIES = c("STYLO5"), GENUS.SPECIES = c("Stylosanthes biflora")),
  data.frame(SPECIES = c("EUPE"), GENUS.SPECIES = c("Chamaesyce sp")),
  data.frame(SPECIES = c("WOODSORREL"), GENUS.SPECIES = c("Oxalis sp")),
  data.frame(SPECIES = c("SERO"), GENUS.SPECIES = c("Packera obovata")),
  data.frame(SPECIES = c("BOSA"), GENUS.SPECIES = c("Bothriochloa saccharoides")),
  data.frame(SPECIES = c("ERLO"), GENUS.SPECIES = c("Erigeron sp")),
  data.frame(SPECIES = c("DYLI"), GENUS.SPECIES = c("Dyschoriste linearis")),
  data.frame(SPECIES = c("SENO"), GENUS.SPECIES = c("Cassia marilandica")),
  data.frame(SPECIES = c("MINT"), GENUS.SPECIES = c("Lamiaceae sp")),
  data.frame(SPECIES = c("THFI"), GENUS.SPECIES = c("Thelesperma filifolium")),
  data.frame(SPECIES = c("CUD"), GENUS.SPECIES = c("Gnaphalium obtusifolium")),
  data.frame(SPECIES = c("MELU"), GENUS.SPECIES = c("Medicago lupulina")),
  data.frame(SPECIES = c("RUHI2"), GENUS.SPECIES = c("Rudbeckia hirta")),
  data.frame(SPECIES = c("LINUM"), GENUS.SPECIES = c("Linum sp")),
  data.frame(SPECIES = c("GAURA"), GENUS.SPECIES = c("Gaura sp")),
  data.frame(SPECIES = c("Unknown_plant"), GENUS.SPECIES = c("Unknown plant"))
)

# Join the genus species dataframe to the plant survey dataframe
comb.transect <- full_join(comb.transect, spec.func.list,
  keep = FALSE
)

# Rearrange the columns
comb.transect <- comb.transect %>%
  dplyr::select(DATE, SITE, TREATMENT, BIOMASS, EXCLUSION, TRANSECT, DISTANCE, GENUS.SPECIES, PRESENT)

## --------------- MERGE WITH DORMANCY CLASS DATABASE --------------------------

# Bring in the Baskins and Baskins seed dormancy database
seed.dorm.db <- read.csv("Clean-data/Seeds/Seed-dormancy-db.csv")

# Drop everything that isn't dormany class and species
seed.dorm.db <- seed.dorm.db %>% 
	dplyr::select(DORMANCY.CLASS, GENUS.SPECIES) %>% 
	distinct()

# Filter for species with more than 1 entry in the database
entry.num <- seed.dorm.db %>%
	group_by(GENUS.SPECIES) %>% 
	summarise(Observations = n()) %>% 
	filter(Observations > 1)

test <- comb.transect |>
	ungroup() |>
	dplyr::select(GENUS.SPECIES) |>
	unique()

# Initiate a column
test['Entries'] <- NA

# Add a value distinguishing whether each species has a single or multiple 
# entries in the seed dormancy database

for (i in 1:nrow(test)) {
  if (test$GENUS.SPECIES[i] %in% entry.num$GENUS.SPECIES) {
  	 test$Entries[i] <- "Multiple"
   } else {
  	 test$Entries[i] <- "Single"
   }
 }

# Remove PD dormancy observation for Digitaria ciliaris 
seed.dorm.db <- seed.dorm.db[-4084, ]

# Add seed dormancy class to the plant survey dataframe
comb.transect <- merge(comb.transect, seed.dorm.db,
  all.x = TRUE, sort = FALSE
)

# Reorder the columns
comb.transect <- comb.transect %>% 
	dplyr::select(DATE, SITE, TREATMENT, BIOMASS, EXCLUSION, TRANSECT, DISTANCE,
				 DORMANCY.CLASS, GENUS.SPECIES, PRESENT
)

## --------------- ADD SEED DORMANCY FUNCTIONAL GROUP FOR NAs ------------------

# Filter for species without a seed dormancy class listed
comb.transect.unk <- comb.transect %>% 
	filter(is.na(DORMANCY.CLASS) == TRUE
)

# Create a vector of the plant names that do not have a seed dormancy class
unk.species <- as.data.frame(unique(comb.transect.unk$GENUS.SPECIES))
colnames(unk.species)[1] <- "GENUS.SPECIES"

# For loop to add dormancy class to unlisted species from the dormany database
for (i in 1:nrow(comb.transect)) {
	if (comb.transect$GENUS.SPECIES[i] == "Aristida purpurea_Aristida purpurascens") {
		comb.transect$DORMANCY.CLASS[i] <- NA
	}
	if (comb.transect$GENUS.SPECIES[i] == "Opuntia sp") {
		comb.transect$DORMANCY.CLASS[i] <- "PD"
	}
	if (comb.transect$GENUS.SPECIES[i] == "Tragia ramosa") {
		comb.transect$DORMANCY.CLASS[i] <- NA
	}
	if (comb.transect$GENUS.SPECIES[i] == "Ambrosia psilostachya") {
		comb.transect$DORMANCY.CLASS[i] <- "PD"
	}
	if (comb.transect$GENUS.SPECIES[i] == "Apiaceae sp") {
		comb.transect$DORMANCY.CLASS[i] <- "PD"
	}
	if (comb.transect$GENUS.SPECIES[i] == "Nassella leucotricha") {
		comb.transect$DORMANCY.CLASS[i] <- "PD"
	}
	if (comb.transect$GENUS.SPECIES[i] == "Eriogonum longifolium") {
		comb.transect$DORMANCY.CLASS[i] <- "PD"
	}
	if (comb.transect$GENUS.SPECIES[i] == "Lolium perenne") {
		comb.transect$DORMANCY.CLASS[i] <- "PD"
	}
	if (comb.transect$GENUS.SPECIES[i] == "Castilleja sp") {
		comb.transect$DORMANCY.CLASS[i] <- "PD"
	}
	if (comb.transect$GENUS.SPECIES[i] == "Pyrrhopappus carolinianus") {
		comb.transect$DORMANCY.CLASS[i] <- "PD"
	}
	if (comb.transect$GENUS.SPECIES[i] == "Asclepias asperula") {
		comb.transect$DORMANCY.CLASS[i] <- "ND"
	}
	if (comb.transect$GENUS.SPECIES[i] == "Sida sp") {
		comb.transect$DORMANCY.CLASS[i] <- "PY"
	}
	if (comb.transect$GENUS.SPECIES[i] == "Croton sp") {
		comb.transect$DORMANCY.CLASS[i] <- "PD"
	}
	if (comb.transect$GENUS.SPECIES[i] == "Paronychia jamesii") {
		comb.transect$DORMANCY.CLASS[i] <- NA
	}
	if (comb.transect$GENUS.SPECIES[i] == "Helianthus hirsutus") {
		comb.transect$DORMANCY.CLASS[i] <- "PD"
	}
	if (comb.transect$GENUS.SPECIES[i] == "Engelmannia peristenia") {
		comb.transect$DORMANCY.CLASS[i] <- "PD"
	}
	if (comb.transect$GENUS.SPECIES[i] == "Evax verna") {
		comb.transect$DORMANCY.CLASS[i] <- "PD"
	}
	if (comb.transect$GENUS.SPECIES[i] == "Trifolium sp") {
		comb.transect$DORMANCY.CLASS[i] <- "PY"
	}
	if (comb.transect$GENUS.SPECIES[i] == "Allium sp") {
		comb.transect$DORMANCY.CLASS[i] <- "PD"
	}
	if (comb.transect$GENUS.SPECIES[i] == "Vicia sp") {
		comb.transect$DORMANCY.CLASS[i] <- "PY"
	}
	if (comb.transect$GENUS.SPECIES[i] == "Bryophyta sp") {
		comb.transect$DORMANCY.CLASS[i] <- NA
	}
	if (comb.transect$GENUS.SPECIES[i] == "Sporobolus compositus") {
		comb.transect$DORMANCY.CLASS[i] <- "PD"
	}
	if (comb.transect$GENUS.SPECIES[i] == "Scutellaria ovata") {
		comb.transect$DORMANCY.CLASS[i] <- "PD"
	}
	if (comb.transect$GENUS.SPECIES[i] == "Solidago sp") {
		comb.transect$DORMANCY.CLASS[i] <- "PD"
	}
	if (comb.transect$GENUS.SPECIES[i] == "Convolvulus equitans") {
		comb.transect$DORMANCY.CLASS[i] <- "PY"
	}
	if (comb.transect$GENUS.SPECIES[i] == "Linum sp") {
		comb.transect$DORMANCY.CLASS[i] <- "PD"
	}
	if (comb.transect$GENUS.SPECIES[i] == "Diodea teres") {
		comb.transect$DORMANCY.CLASS[i] <- "PD"
	}
	if (comb.transect$GENUS.SPECIES[i] == "Phyllanthus polygonoides") {
		comb.transect$DORMANCY.CLASS[i] <- NA
	}
	if (comb.transect$GENUS.SPECIES[i] == "Houstonia pusilla") {
		comb.transect$DORMANCY.CLASS[i] <- "PD"
	}
	if (comb.transect$GENUS.SPECIES[i] == "Hedeoma drummondii") {
		comb.transect$DORMANCY.CLASS[i] <- NA
	}
	if (comb.transect$GENUS.SPECIES[i] == "Asclepias verticillata") {
		comb.transect$DORMANCY.CLASS[i] <- "PD"
	}
	if (comb.transect$GENUS.SPECIES[i] == "Sisyrinchium sp") {
		comb.transect$DORMANCY.CLASS[i] <- "PD"
	}
	if (comb.transect$GENUS.SPECIES[i] == "Cirsium texanum") {
		comb.transect$DORMANCY.CLASS[i] <- "PD"
	}
	if (comb.transect$GENUS.SPECIES[i] == "Lobelia sp") {
		comb.transect$DORMANCY.CLASS[i] <- "PD"
	}
	if (comb.transect$GENUS.SPECIES[i] == "Dalea purpurea") {
		comb.transect$DORMANCY.CLASS[i] <- "PY"
	}
	if (comb.transect$GENUS.SPECIES[i] == "Krameria lanceolata") {
		comb.transect$DORMANCY.CLASS[i] <- "PD"
	}
	if (comb.transect$GENUS.SPECIES[i] == "Agalinis heterophylla") {
		comb.transect$DORMANCY.CLASS[i] <- "PD"
	}
	if (comb.transect$GENUS.SPECIES[i] == "Andropogon sp") {
		comb.transect$DORMANCY.CLASS[i] <- "PD"
	}
	if (comb.transect$GENUS.SPECIES[i] == "Sabatia campestris") {
		comb.transect$DORMANCY.CLASS[i] <- "PD"
	}
	if (comb.transect$GENUS.SPECIES[i] == "Eryngium leavenworthii") {
		comb.transect$DORMANCY.CLASS[i] <- "PD"
	}
	if (comb.transect$GENUS.SPECIES[i] == "Hymenopappus tenuifolius") {
		comb.transect$DORMANCY.CLASS[i] <- NA
	}
	if (comb.transect$GENUS.SPECIES[i] == "Setaria sp") {
		comb.transect$DORMANCY.CLASS[i] <- "PD"
	}
	if (comb.transect$GENUS.SPECIES[i] == "Verbena halei") {
		comb.transect$DORMANCY.CLASS[i] <- "PD"
	}
	if (comb.transect$GENUS.SPECIES[i] == "Eragrostis curtipedicellata") {
		comb.transect$DORMANCY.CLASS[i] <- "PD"
	}
	if (comb.transect$GENUS.SPECIES[i] == "Lesquerella sp") {
		comb.transect$DORMANCY.CLASS[i] <- "PD"
	}
	if (comb.transect$GENUS.SPECIES[i] == "Erioneuron pilosum") {
		comb.transect$DORMANCY.CLASS[i] <- NA
	}
	if (comb.transect$GENUS.SPECIES[i] == "Panicum sp") {
		comb.transect$DORMANCY.CLASS[i] <- "PD"
	}
	if (comb.transect$GENUS.SPECIES[i] == "Bothriochloa ischaemum") {
		comb.transect$DORMANCY.CLASS[i] <- "PD"
	}
	if (comb.transect$GENUS.SPECIES[i] == "Bothriochloa laguroides") {
		comb.transect$DORMANCY.CLASS[i] <- "PD"
	}
	if (comb.transect$GENUS.SPECIES[i] == "Glandularia bipinnatifida") {
		comb.transect$DORMANCY.CLASS[i] <- "PD"
	}
	if (comb.transect$GENUS.SPECIES[i] == "Stylosanthes biflora") {
		comb.transect$DORMANCY.CLASS[i] <- "PY"
	}
	if (comb.transect$GENUS.SPECIES[i] == "Chamaesyce sp") {
		comb.transect$DORMANCY.CLASS[i] <- NA
	}
	if (comb.transect$GENUS.SPECIES[i] == "Oxalis sp") {
		comb.transect$DORMANCY.CLASS[i] <- "ND"
	}
	if (comb.transect$GENUS.SPECIES[i] == "Packera obovata") {
		comb.transect$DORMANCY.CLASS[i] <- "ND"
	}
	if (comb.transect$GENUS.SPECIES[i] == "Bothriochloa saccharoides") {
		comb.transect$DORMANCY.CLASS[i] <- "PD"
	}
	if (comb.transect$GENUS.SPECIES[i] == "Erigeron sp") {
		comb.transect$DORMANCY.CLASS[i] <- "ND"
	}
	if (comb.transect$GENUS.SPECIES[i] == "Dyschoriste linearis") {
		comb.transect$DORMANCY.CLASS[i] <- NA
	}
	if (comb.transect$GENUS.SPECIES[i] == "Cassia marilandica") {
		comb.transect$DORMANCY.CLASS[i] <- "PY"
	}
	if (comb.transect$GENUS.SPECIES[i] == "Lamiaceae sp") {
		comb.transect$DORMANCY.CLASS[i] <- NA
	}
	if (comb.transect$GENUS.SPECIES[i] == "Gaura sp") {
		comb.transect$DORMANCY.CLASS[i] <- "PD"
	}
	if (comb.transect$GENUS.SPECIES[i] == "Thelesperma filifolium") {
		comb.transect$DORMANCY.CLASS[i] <- NA
	}
	if (comb.transect$GENUS.SPECIES[i] == "Unknown plant") {
		comb.transect$DORMANCY.CLASS[i] <- NA
	}
}

# Calculate how many unknown seed dormancy class observations remain
(sum(is.na(comb.transect$DORMANCY.CLASS)))/(nrow(comb.transect))
# 15% of rows are NAs

# Check to the levels of the seed dormancy classes
unique(comb.transect$DORMANCY.CLASS)

# Fix seed dormancy classes
for (i in 1:nrow(comb.transect)){
	if(isTRUE(comb.transect$DORMANCY.CLASS[i] == "nd")){
		comb.transect$DORMANCY.CLASS[i] <- "ND"
	}
	if(isTRUE(comb.transect$DORMANCY.CLASS[i] == "pd")){
		comb.transect$DORMANCY.CLASS[i] <- "PD"
	}
	if(isTRUE(comb.transect$DORMANCY.CLASS[i] == "PYPD")){
		comb.transect$DORMANCY.CLASS[i] <- "PY"
	}
	if(isTRUE(comb.transect$DORMANCY.CLASS[i] == "MPD")){
		comb.transect$DORMANCY.CLASS[i] <- "PD"
	}
}

## --------------- SUMMARIZE BY DORMANCY CLASS ---------------------------------

# Save as date
comb.transect$DATE <- as.DATE(comb.transect$DATE)

# Round the date
comb.transect <- comb.transect %>% 
	mutate(ROUNDED.DATE = round_date(DATE, unit = "week"))

# The pre-data got summarized to different weeks. Put them on the same level
for(i in 1:nrow(comb.transect)){
	if(comb.transect$ROUNDED.DATE[i] == "2019-03-17"){
		comb.transect$ROUNDED.DATE[i] <- "2019-03-10"
	}
}

# Rearrange
comb.transect <- comb.transect %>% 
	dplyr::select(Rounded.date, SITE, TREATMENT, BIOMASS, EXCLUSION,
				 TRANSECT, DISTANCE, DORMANCY.CLASS, GENUS.SPECIES, PRESENT)

# Summarize the data by dormancy class
comb.transect.dormancy <- comb.transect %>% 
	group_by(ROUNDED.DATE, SITE, TREATMENT, BIOMASS, EXCLUSION, TRANSECT,
					 DISTANCE, DORMANCY.CLASS) %>% 
	summarise(PRESENT = sum(PRESENT))

# There are too many rows because there are observations from the dropped
# site (pipeline).
comb.transect.dormancy <- comb.transect.dormancy %>% 
	filter(SITE != "Pipeline")
 
# 3 dormancy classes + NA = 4 rows per transect point per sampling event
# 4 * 16 transect points per plot = 64 rows per plot
# 64 rows per plot * 6 plots per block = 384 rows per block
# 384 rows per block * 4 blocks = 1,536 rows per sampling event
# 1,536 rows per sampling event * 7 sampling events = 10,752 rows
print(nrow(comb.transect.dormancy)) # 10,752
# Everything looks good

# We want to know whether dormancy classes colonized or were extirpated
# from sampling points. We aggregated data by functional group, 
# which means for transect points where there was more
# than one observation of a dormancy class, there will be a value greater
# than one. We need to convert those values.

comb.transect.dormancy$PRESENT[comb.transect.dormancy$PRESENT > 0] <- 1


## --------------- CREATE COLONIZATION DATAFAME --------------------------------

# Pivot wide
comb.transect.dormancy.wd <- comb.transect.dormancy |>
	pivot_wider(names_from = ROUNDED.DATE, values_from = PRESENT)

# Check the numbers
check <- comb.transect.dormancy |>
	group_by(ROUNDED.DATE, SITE, TREATMENT, TRANSECT, DISTANCE) |>
	summarize(Count = n ()) 
# All transect points are represented
# 7 dates
# 4 sites
# 6 treatments
# 4 directions
# 4 distance
# 4 dormancy class
7*4*6*4*4*4 # Nice
rm(check)

# Create colonization dataframe
dormancy.col <- comb.transect.dormancy.wd %>% 
	filter(`2019-03-10` == 0)

# Initialize column for colonization
dormancy.col["COLONIZED.END"] <- NA

# Determine if dormancy class colonized the point by the
# last sampling date

for(i in 1:nrow(dormancy.col)){
	if (isTRUE(dormancy.col[i, 14] == 1)) {
		dormancy.col$COLONIZED.END[i] <- 1
	} else {
		dormancy.col$COLONIZED.END[i] <- 0
	}
}

# Drop the NA values
dormancy.col <- dormancy.col %>% 
	filter(DORMANCY.CLASS != "NA")

# Rearrange the columns
dormancy.col <- dormancy.col %>% 
	dplyr::select(SITE, TREATMENT, BIOMASS, EXCLUSION, TRANSECT, DISTANCE, DORMANCY.CLASS,
				 COLONIZED.END, everything())

## --------------- CREATE EXTIRPATION DATAFAME ----------------------------------

# Create extinction dataframe
dormancy.ext <- comb.transect.dormancy.wd %>% 
	filter(`2019-03-10` == 1)

# Initialize columns for extirpation
dormancy.ext["EXTIRPATED.EVER"] <- NA

# Determine if dormancy class was ever extirpated from the point
zeroes <- rowSums(dormancy.ext[9:14] == 0)
dormancy.ext$zeroes <- zeroes

for (i in 1:nrow(dormancy.ext)){
	if (isTRUE(dormancy.ext$zeroes[i] >= 1)){
		dormancy.ext$EXTIRPATED.EVER[i] <- 1
	} else {
		dormancy.ext$EXTIRPATED.EVER[i] <- 0
	}
}

# Determine if dormancy class was extirpated from the point by the
# last sampling date

for (i in 1:nrow(dormancy.ext)){
	if (isTRUE(dormancy.ext$`2021-07-18`[i] == 0)){
		dormancy.ext$Extirpated.end[i] <- 1
	} else {
		dormancy.ext$Extirpated.end[i] <- 0
	}
}

# Drop the NA values
dormancy.ext <- dormancy.ext %>% 
	filter(DORMANCY.CLASS != "NA")

# Rearrange the columns
dormancy.ext <- dormancy.ext %>% 
	dplyr::select(SITE, TREATMENT, BIOMASS, EXCLUSION, TRANSECT, DISTANCE, DORMANCY.CLASS,
				 EXTIRPATED.EVER, everything(), -zeroes)

## --------------- SAVE CLEAN DATA ---------------------------------------------



write.csv(dormancy.col, "Clean-data/Plants/Dormancy-class-colonization.csv",
					row.names=FALSE)

write.csv(dormancy.ext, "Clean-data/Plants/Dormancy-class-extirpation.csv",
					row.names=FALSE)
