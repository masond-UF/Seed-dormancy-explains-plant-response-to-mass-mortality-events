## --------------- HEADER ------------------------------------------------------
## Script name: 2b_Dormancy-class-clean.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliaton: University of Florida
## Date Created: 2022-04-28
## Date Last modified: 2022-04-30
## Copyright (c) David S. Mason, 2022
## Contact: masond@ufl.edu, @EcoGraffito
## Purpose of script: This is a script for munging the unsummarized transect
 
## --------------- SET—UP WORKSPACE --------------------------------------------

# Load the packages
library(tidyverse)
library(tidylog)
library(lubridate)
library(styler)

# Bring in the data
comb.transect <- read.csv("Animals-plants-seeds/Raw-data/Plants/Combined-transect-lg.csv")

## --------------- FIX HERBIVORE LABEL -----------------------------------------

for(i in 1:nrow(comb.transect)){
	if(comb.transect$Exclusion[i] == "Herbivroe"){
		comb.transect$Exclusion[i] <- "Herbivore"
	}
}

## --------------- COMBINE LIKE SPECIES ----------------------------------------

# The 2021 survey was entered in long format (not a community matrix),
# so there are not values for every species at every sampling point. Thus,
# pivot wider introduces NA for those values.

comb.transect <- comb.transect %>%
  pivot_wider(names_from = Species, values_from = Present)

# Convert NAs to 0
comb.transect[is.na(comb.transect)] <- 0

# We have combined surveys and some of the species are listed multiple times
# under different names. Here, we will combine those before moving forward.

# Get a list of the species names
list <- as.data.frame(colnames(comb.transect[1, 8:168]))
names(list)[1] <- "Species"

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
  pivot_longer(8:131, names_to = "Species", values_to = "Present")

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
comb.transect$Species[comb.transect$Species %in% unknowns] <- "Unknown_plant"

# Summarize the unknowns data
comb.transect <- comb.transect %>%
  group_by(
    Date, Site, Treatment, Carrion, Exclusion, Transect, Distance,
    Species
  ) %>%
  summarize(Present = sum(Present))

# Summarizing unknowns caused some double values to pop up (multiple unknowns
# at the same point). However, there were already some double values for PANIC.
# I don't know where they came from (yet).
comb.transect$Present[comb.transect$Present > 0] <- 1

## --------------- DROP NONPLANTS  ---------------------------------------------

# Create a vector of non-plants
non.plants <- c(
  "DEAD", "BG", "LIT", "PIG", "COW.PATTIE", "WOODSURFACE",
  "COW PATTIE"
)

# non.plants <- c("Senescent_veg", "Bare_ground", "Plant_litter", "Pig_carcass",
# "Cow_pattie", "Wood_debris")

# Change the name of all species in the non.plants vector
comb.transect$Species[comb.transect$Species %in% non.plants] <- "Non_plant"

# Remove the non-plants from the plant survey data
comb.transect <- comb.transect %>%
  filter(!Species == "Non_plant")

## --------------- ADD GENUS SPECIES  ------------------------------------------

# Create a list of species to reference
list <- as.data.frame(unique(comb.transect$Species))
names(list)[1] <- "Species"

# Create a dataframe matching the symbols/labels in the plant survey data
# with full genus.species names.
spec.func.list <- rbind(
  data.frame(Species = c("BRJA"), Genus.species = c("Bromus japonicus")),
  data.frame(Species = c("BOCU"), Genus.species = c("Bouteloua curtipendula")),
  data.frame(Species = c("AECY"), Genus.species = c("Aegilops cylindrica")),
  data.frame(Species = c("AMPS"), Genus.species = c("Ambrosia psilostachya")),
  data.frame(Species = c("ARPU"), Genus.species = c("Aristida purpurea_Aristida purpurascens")),
  data.frame(Species = c("OPUNT"), Genus.species = c("Opuntia sp")),
  data.frame(Species = c("TRRA5"), Genus.species = c("Tragia ramosa")),
  data.frame(Species = c("APIAC"), Genus.species = c("Apiaceae sp")),
  data.frame(Species = c("BOHI2"), Genus.species = c("Bouteloua hirsuta")),
  data.frame(Species = c("GAPU"), Genus.species = c("Gaillardia pulchella")),
  data.frame(Species = c("VARA"), Genus.species = c("Valerianella radiata")),
  data.frame(Species = c("NALE3"), Genus.species = c("Nassella leucotricha")),
  data.frame(Species = c("AMDR"), Genus.species = c("Amphiachyris dracunculoides")),
  data.frame(Species = c("LOPE"), Genus.species = c("Lolium perenne")),
  data.frame(Species = c("CASTI2"), Genus.species = c("Castilleja sp")),
  data.frame(Species = c("ERLO5"), Genus.species = c("Eriogonum longifolium")),
  data.frame(Species = c("PYCA2"), Genus.species = c("Pyrrhopappus carolinianus")),
  data.frame(Species = c("ASAS"), Genus.species = c("Asclepias asperula")),
  data.frame(Species = c("SIDA"), Genus.species = c("Sida sp")),
  data.frame(Species = c("CROTON"), Genus.species = c("Croton sp")),
  data.frame(Species = c("PAJA"), Genus.species = c("Paronychia jamesii")),
  data.frame(Species = c("HEHI2"), Genus.species = c("Helianthus hirsutus")),
  data.frame(Species = c("ENPE4"), Genus.species = c("Engelmannia peristenia")),
  data.frame(Species = c("EVVE"), Genus.species = c("Evax verna")),
  data.frame(Species = c("CLOVER"), Genus.species = c("Trifolium sp")),
  data.frame(Species = c("ALLIUM"), Genus.species = c("Allium sp")),
  data.frame(Species = c("VICIA"), Genus.species = c("Vicia sp")),
  data.frame(Species = c("MOSS"), Genus.species = c("Bryophyta sp")),
  data.frame(Species = c("SPCOC2"), Genus.species = c("Sporobolus compositus")),
  data.frame(Species = c("NOBI2"), Genus.species = c("Nothoscordum bivalve")),
  data.frame(Species = c("SOLID"), Genus.species = c("Solidago sp")),
  data.frame(Species = c("LASE"), Genus.species = c("Lactuca serriola")),
  data.frame(Species = c("SCOV"), Genus.species = c("Scutellaria ovata")),
  data.frame(Species = c("BUTTON"), Genus.species = c("Diodea teres")),
  data.frame(Species = c("COEQ"), Genus.species = c("Convolvulus equitans")),
  data.frame(Species = c("PLVI"), Genus.species = c("Plantago virginica")),
  data.frame(Species = c("HECR9"), Genus.species = c("Houstonia pusilla")),
  data.frame(Species = c("LITE3"), Genus.species = c("Lindheimera texana")),
  data.frame(Species = c("ASVE"), Genus.species = c("Asclepias verticillata")),
  data.frame(Species = c("PHPO3"), Genus.species = c("Phyllanthus polygonoides")),
  data.frame(Species = c("PLAR"), Genus.species = c("Plantago aristata")),
  data.frame(Species = c("HEDR"), Genus.species = c("Hedeoma drummondii")),
  data.frame(Species = c("CITE2"), Genus.species = c("Cirsium texanum")),
  data.frame(Species = c("DICI"), Genus.species = c("Digitaria ciliaris")),
  data.frame(Species = c("SISYR"), Genus.species = c("Sisyrinchium sp")),
  data.frame(Species = c("DAPU"), Genus.species = c("Dalea purpurea")),
  data.frame(Species = c("KRLA"), Genus.species = c("Krameria lanceolata")),
  data.frame(Species = c("TROH"), Genus.species = c("Tradescantia ohiensis")),
  data.frame(Species = c("AGHE4"), Genus.species = c("Agalinis heterophylla")),
  data.frame(Species = c("ANDRO2"), Genus.species = c("Andropogon sp")),
  data.frame(Species = c("LEVI"), Genus.species = c("Lepidium virginicum")),
  data.frame(Species = c("LOBEL"), Genus.species = c("Lobelia sp")),
  data.frame(Species = c("NIP"), Genus.species = c("Lapsana communis")),
  data.frame(Species = c("OENOT"), Genus.species = c("Oenothera biennis")),
  data.frame(Species = c("SACA"), Genus.species = c("Sabatia campestris")),
  data.frame(Species = c("SETAR"), Genus.species = c("Setaria sp")),
  data.frame(Species = c("TRIOD"), Genus.species = c("Triodanis perfoliata")),
  data.frame(Species = c("HYTE2"), Genus.species = c("Hymenopappus tenuifolius")),
  data.frame(Species = c("VEHA"), Genus.species = c("Verbena halei")),
  data.frame(Species = c("ERCU"), Genus.species = c("Eragrostis curtipedicellata")),
  data.frame(Species = c("ERLE11"), Genus.species = c("Eryngium leavenworthii")),
  data.frame(Species = c("LESQU"), Genus.species = c("Lesquerella sp")),
  data.frame(Species = c("ERPI5"), Genus.species = c("Erioneuron pilosum")),
  data.frame(Species = c("VEPE2"), Genus.species = c("Veronica peregrina")),
  data.frame(Species = c("BOIS"), Genus.species = c("Bothriochloa ischaemum")),
  data.frame(Species = c("BOLA"), Genus.species = c("Bothriochloa laguroides")),
  data.frame(Species = c("GLBI"), Genus.species = c("Glandularia bipinnatifida")),
  data.frame(Species = c("PANIC"), Genus.species = c("Panicum sp")),
  data.frame(Species = c("STYLO5"), Genus.species = c("Stylosanthes biflora")),
  data.frame(Species = c("EUPE"), Genus.species = c("Chamaesyce sp")),
  data.frame(Species = c("WOODSORREL"), Genus.species = c("Oxalis sp")),
  data.frame(Species = c("SERO"), Genus.species = c("Packera obovata")),
  data.frame(Species = c("BOSA"), Genus.species = c("Bothriochloa saccharoides")),
  data.frame(Species = c("ERLO"), Genus.species = c("Erigeron sp")),
  data.frame(Species = c("DYLI"), Genus.species = c("Dyschoriste linearis")),
  data.frame(Species = c("SENO"), Genus.species = c("Cassia marilandica")),
  data.frame(Species = c("MINT"), Genus.species = c("Lamiaceae sp")),
  data.frame(Species = c("THFI"), Genus.species = c("Thelesperma filifolium")),
  data.frame(Species = c("CUD"), Genus.species = c("Gnaphalium obtusifolium")),
  data.frame(Species = c("MELU"), Genus.species = c("Medicago lupulina")),
  data.frame(Species = c("RUHI2"), Genus.species = c("Rudbeckia hirta")),
  data.frame(Species = c("LINUM"), Genus.species = c("Linum sp")),
  data.frame(Species = c("GAURA"), Genus.species = c("Gaura sp")),
  data.frame(Species = c("Unknown_plant"), Genus.species = c("Unknown plant"))
)

# Join the genus species dataframe to the plant survey dataframe
comb.transect <- full_join(comb.transect, spec.func.list,
  keep = FALSE
)

# Rearrange the columns
comb.transect <- comb.transect %>%
  dplyr::select(Date, Site, Treatment, Carrion, Exclusion, Transect, Distance, Genus.species, Present)

## --------------- MERGE WITH DORMANCY CLASS DATABASE --------------------------

# Bring in the Baskins and Baskins seed dormancy database
seed.dorm.db <- read.csv("Animals-plants-seeds/Clean-data/Seeds/Seed-dormancy-db.csv")

# Drop everything that isn't dormany class and species
seed.dorm.db <- seed.dorm.db %>% 
	dplyr::select(DormancyClass, Genus.species) %>% 
	distinct()

# Filter for species with more than 1 entry in the database
# entry.num <- seed.dorm.db %>%
	# group_by(Genus.species) %>% 
	# summarise(Observations = n()) %>% 
	# filter(Observations > 1)

# Save the summarized data as a vector
# entry.num <- as.vector(entry.num)

# Pull out the species name 
# entry.num$Genus.species <- as.character(entry.num$Genus.species)
# entry.num.names <- dplyr::pull(entry.num, "Genus.species")
# comb.transect$Genus.species <- as.character(comb.transect$Genus.species)

# Initiate a column
# comb.transect['Entries'] <- NA

# Add a value distinguishing whether each species has a single or multiple 
# entries in the seed dormancy database

# for (i in 1:nrow(comb.transect)) {
  # if (comb.transect$Genus.species[i] %in% entry.num.names) {
  	# comb.transect$Entries[i] <- "Multiple"
  # } else {
  	# comb.transect$Entries[i] <- "Single"
  # }
# }

# Remove PD dormancy observation for Digitaria ciliaris 
seed.dorm.db <- seed.dorm.db[-4084, ]

# Add seed dormancy class to the plant survey dataframe
comb.transect <- merge(comb.transect, seed.dorm.db,
  all.x = TRUE, sort = FALSE
)

# Reorder the columns
comb.transect <- comb.transect %>% 
	dplyr::select(Date, Site, Treatment, Carrion, Exclusion, Transect, Distance,
				 DormancyClass, Genus.species, Present
)

## --------------- ADD SEED DORMANCY FUNCTIONAL GROUP FOR NAs ------------------

# Filter for species without a seed dormancy class listed
comb.transect.unk <- comb.transect %>% 
	filter(is.na(DormancyClass) == TRUE
)

# Create a vector of the plant names that do not have a seed dormancy class
unk.species <- as.data.frame(unique(comb.transect.unk$Genus.species))
colnames(unk.species)[1] <- "Genus.species"

# For loop to add dormancy class to unlisted species from the dormany database
for (i in 1:nrow(comb.transect)) {
	if (comb.transect$Genus.species[i] == "Aristida purpurea_Aristida purpurascens") {
		comb.transect$DormancyClass[i] <- NA
	}
	if (comb.transect$Genus.species[i] == "Opuntia sp") {
		comb.transect$DormancyClass[i] <- "PD"
	}
	if (comb.transect$Genus.species[i] == "Tragia ramosa") {
		comb.transect$DormancyClass[i] <- NA
	}
	if (comb.transect$Genus.species[i] == "Ambrosia psilostachya") {
		comb.transect$DormancyClass[i] <- "PD"
	}
	if (comb.transect$Genus.species[i] == "Apiaceae sp") {
		comb.transect$DormancyClass[i] <- "PD"
	}
	if (comb.transect$Genus.species[i] == "Nassella leucotricha") {
		comb.transect$DormancyClass[i] <- "PD"
	}
	if (comb.transect$Genus.species[i] == "Eriogonum longifolium") {
		comb.transect$DormancyClass[i] <- "PD"
	}
	if (comb.transect$Genus.species[i] == "Lolium perenne") {
		comb.transect$DormancyClass[i] <- "PD"
	}
	if (comb.transect$Genus.species[i] == "Castilleja sp") {
		comb.transect$DormancyClass[i] <- "PD"
	}
	if (comb.transect$Genus.species[i] == "Pyrrhopappus carolinianus") {
		comb.transect$DormancyClass[i] <- "PD"
	}
	if (comb.transect$Genus.species[i] == "Asclepias asperula") {
		comb.transect$DormancyClass[i] <- "ND"
	}
	if (comb.transect$Genus.species[i] == "Sida sp") {
		comb.transect$DormancyClass[i] <- "PY"
	}
	if (comb.transect$Genus.species[i] == "Croton sp") {
		comb.transect$DormancyClass[i] <- "PD"
	}
	if (comb.transect$Genus.species[i] == "Paronychia jamesii") {
		comb.transect$DormancyClass[i] <- NA
	}
	if (comb.transect$Genus.species[i] == "Helianthus hirsutus") {
		comb.transect$DormancyClass[i] <- "PD"
	}
	if (comb.transect$Genus.species[i] == "Engelmannia peristenia") {
		comb.transect$DormancyClass[i] <- "PD"
	}
	if (comb.transect$Genus.species[i] == "Evax verna") {
		comb.transect$DormancyClass[i] <- "PD"
	}
	if (comb.transect$Genus.species[i] == "Trifolium sp") {
		comb.transect$DormancyClass[i] <- "PY"
	}
	if (comb.transect$Genus.species[i] == "Allium sp") {
		comb.transect$DormancyClass[i] <- "PD"
	}
	if (comb.transect$Genus.species[i] == "Vicia sp") {
		comb.transect$DormancyClass[i] <- "PY"
	}
	if (comb.transect$Genus.species[i] == "Bryophyta sp") {
		comb.transect$DormancyClass[i] <- NA
	}
	if (comb.transect$Genus.species[i] == "Sporobolus compositus") {
		comb.transect$DormancyClass[i] <- "PD"
	}
	if (comb.transect$Genus.species[i] == "Scutellaria ovata") {
		comb.transect$DormancyClass[i] <- "PD"
	}
	if (comb.transect$Genus.species[i] == "Solidago sp") {
		comb.transect$DormancyClass[i] <- "PD"
	}
	if (comb.transect$Genus.species[i] == "Convolvulus equitans") {
		comb.transect$DormancyClass[i] <- "PY"
	}
	if (comb.transect$Genus.species[i] == "Linum sp") {
		comb.transect$DormancyClass[i] <- "PD"
	}
	if (comb.transect$Genus.species[i] == "Diodea teres") {
		comb.transect$DormancyClass[i] <- "PD"
	}
	if (comb.transect$Genus.species[i] == "Phyllanthus polygonoides") {
		comb.transect$DormancyClass[i] <- NA
	}
	if (comb.transect$Genus.species[i] == "Houstonia pusilla") {
		comb.transect$DormancyClass[i] <- "PD"
	}
	if (comb.transect$Genus.species[i] == "Hedeoma drummondii") {
		comb.transect$DormancyClass[i] <- NA
	}
	if (comb.transect$Genus.species[i] == "Asclepias verticillata") {
		comb.transect$DormancyClass[i] <- "PD"
	}
	if (comb.transect$Genus.species[i] == "Sisyrinchium sp") {
		comb.transect$DormancyClass[i] <- "PD"
	}
	if (comb.transect$Genus.species[i] == "Cirsium texanum") {
		comb.transect$DormancyClass[i] <- "PD"
	}
	if (comb.transect$Genus.species[i] == "Lobelia sp") {
		comb.transect$DormancyClass[i] <- "PD"
	}
	if (comb.transect$Genus.species[i] == "Dalea purpurea") {
		comb.transect$DormancyClass[i] <- "PY"
	}
	if (comb.transect$Genus.species[i] == "Krameria lanceolata") {
		comb.transect$DormancyClass[i] <- "PD"
	}
	if (comb.transect$Genus.species[i] == "Agalinis heterophylla") {
		comb.transect$DormancyClass[i] <- "PD"
	}
	if (comb.transect$Genus.species[i] == "Andropogon sp") {
		comb.transect$DormancyClass[i] <- "PD"
	}
	if (comb.transect$Genus.species[i] == "Sabatia campestris") {
		comb.transect$DormancyClass[i] <- "PD"
	}
	if (comb.transect$Genus.species[i] == "Eryngium leavenworthii") {
		comb.transect$DormancyClass[i] <- "PD"
	}
	if (comb.transect$Genus.species[i] == "Hymenopappus tenuifolius") {
		comb.transect$DormancyClass[i] <- NA
	}
	if (comb.transect$Genus.species[i] == "Setaria sp") {
		comb.transect$DormancyClass[i] <- "PD"
	}
	if (comb.transect$Genus.species[i] == "Verbena halei") {
		comb.transect$DormancyClass[i] <- "PD"
	}
	if (comb.transect$Genus.species[i] == "Eragrostis curtipedicellata") {
		comb.transect$DormancyClass[i] <- "PD"
	}
	if (comb.transect$Genus.species[i] == "Lesquerella sp") {
		comb.transect$DormancyClass[i] <- "PD"
	}
	if (comb.transect$Genus.species[i] == "Erioneuron pilosum") {
		comb.transect$DormancyClass[i] <- NA
	}
	if (comb.transect$Genus.species[i] == "Panicum sp") {
		comb.transect$DormancyClass[i] <- "PD"
	}
	if (comb.transect$Genus.species[i] == "Bothriochloa ischaemum") {
		comb.transect$DormancyClass[i] <- "PD"
	}
	if (comb.transect$Genus.species[i] == "Bothriochloa laguroides") {
		comb.transect$DormancyClass[i] <- "PD"
	}
	if (comb.transect$Genus.species[i] == "Glandularia bipinnatifida") {
		comb.transect$DormancyClass[i] <- "PD"
	}
	if (comb.transect$Genus.species[i] == "Stylosanthes biflora") {
		comb.transect$DormancyClass[i] <- "PY"
	}
	if (comb.transect$Genus.species[i] == "Chamaesyce sp") {
		comb.transect$DormancyClass[i] <- NA
	}
	if (comb.transect$Genus.species[i] == "Oxalis sp") {
		comb.transect$DormancyClass[i] <- "ND"
	}
	if (comb.transect$Genus.species[i] == "Packera obovata") {
		comb.transect$DormancyClass[i] <- "ND"
	}
	if (comb.transect$Genus.species[i] == "Bothriochloa saccharoides") {
		comb.transect$DormancyClass[i] <- "PD"
	}
	if (comb.transect$Genus.species[i] == "Erigeron sp") {
		comb.transect$DormancyClass[i] <- "ND"
	}
	if (comb.transect$Genus.species[i] == "Dyschoriste linearis") {
		comb.transect$DormancyClass[i] <- NA
	}
	if (comb.transect$Genus.species[i] == "Cassia marilandica") {
		comb.transect$DormancyClass[i] <- "PY"
	}
	if (comb.transect$Genus.species[i] == "Lamiaceae sp") {
		comb.transect$DormancyClass[i] <- NA
	}
	if (comb.transect$Genus.species[i] == "Gaura sp") {
		comb.transect$DormancyClass[i] <- "PD"
	}
	if (comb.transect$Genus.species[i] == "Thelesperma filifolium") {
		comb.transect$DormancyClass[i] <- NA
	}
	if (comb.transect$Genus.species[i] == "Unknown plant") {
		comb.transect$DormancyClass[i] <- NA
	}
}

# Calculate how many unknown seed dormancy class observations remain
(sum(is.na(comb.transect$DormancyClass)))/(nrow(comb.transect))
# 15% of rows are NAs

# Check to the levels of the seed dormancy classes
unique(comb.transect$DormancyClass)

# Fix seed dormancy classes
for (i in 1:nrow(comb.transect)){
	if(comb.transect$DormancyClass[i] == "nd"){
		comb.transect$DormancyClass[i] <- "ND"
	}
	if(comb.transect$DormancyClass[i] == "pd"){
		comb.transect$DormancyClass[i] <- "PD"
	}
	if(comb.transect$DormancyClass[i] == "PYPD"){
		comb.transect$DormancyClass[i] <- "PY"
	}
	if(comb.transect$DormancyClass[i] == "MPD"){
		comb.transect$DormancyClass[i] <- "PD"
	}
}

## --------------- SUMMARIZE BY DORMANCY CLASS ---------------------------------

# Save as date
comb.transect$Date <- as.Date(comb.transect$Date)

# Round the date
comb.transect <- comb.transect %>% 
	mutate(Rounded.date = round_date(Date, unit = "week"))

# 
comb.transect <- comb.transect %>% 
	dplyr::select(Rounded.date, Site, Treatment, Carrion, Exclusion,
				 Transect, Distance, DormancyClass, Genus.species, Present)

# The pre-data got summarized to different weeks. Put them on the same level
for(i in 1:nrow(comb.transect)){
	if(comb.transect$Rounded.date[i] == "2019-03-17"){
		comb.transect$Rounded.date[i] <- "2019-03-10"
	}
}

# Summarize the data by dormancy class
comb.transect.dormancy <- comb.transect %>% 
	group_by(Rounded.date, Site, Treatment, Carrion, Exclusion, Transect,
					 Distance, DormancyClass) %>% 
	summarise(Present = sum(Present))

# There are too many rows because there are observations from the dropped
# site (pipeline).
comb.transect.dormancy <- comb.transect.dormancy %>% 
	filter(Site != "Pipeline")
 
# 3 dormancy classes + NA = 4 rows per transect point per sampling event
# 4 * 16 transect points per plot = 64 rows per plot
# 64 rows per plot * 6 plots per block = 384 rows per block
# 384 rows per block * 4 blocks = 1,536 rows per sampling event
# 1,536 rows per sampling event * 7 sampling events = 10,752 rows
print(nrow(comb.transect.dormancy)) # 10,752
# Everything looks good

# We want to know whether dormancy classes colonized or were extirpated
# from sampling points. This is a binary question. We aggregated data by
# functional group, which means for transect points where there was more
# than one observation of a dormancy class, there will be a value greater
# than one. We need to convert those values.

comb.transect.dormancy$Present[comb.transect.dormancy$Present > 0] <- 1


## --------------- CREATE COLONIZATION DATAFAME --------------------------------

# Pivot wide
comb.transect.dormancy.wd <- comb.transect.dormancy %>% 
	pivot_wider(names_from = Rounded.date, values_from = Present)

# Create colonization dataframe
dormancy.col <- comb.transect.dormancy.wd %>% 
	filter(`2019-03-10` == 0)

# Initialize column for colonization
dormancy.col["Colonized.ever"] <- NA
dormancy.col["Colonized.end"] <- NA

# Determine if dormancy class ever colonized the point
for(i in 1:nrow(dormancy.col)){
	if (sum(dormancy.col[i, 8:14]) >= 1){
		dormancy.col$Colonized.ever[i] <- 1
	} else {
		dormancy.col$Colonized.ever[i] <- 0
	}
}

# Determine if dormancy class colonized the point by the
# last sampling date

for(i in 1:nrow(dormancy.col)){
	if (dormancy.col[i, 14] == 1) {
		dormancy.col$Colonized.end[i] <- 1
	} else {
		dormancy.col$Colonized.end[i] <- 0
	}
}

# Drop the NA values
dormancy.col <- dormancy.col %>% 
	filter(DormancyClass != "NA")

# Rearrange the columns
dormancy.col <- dormancy.col %>% 
	dplyr::select(Site, Treatment, Carrion, Exclusion, Transect, Distance, DormancyClass,
				 Colonized.ever, Colonized.end, everything())

## --------------- CREATE EXTIRPATION DATAFAME ----------------------------------

# Create extinction dataframe
dormancy.ext <- comb.transect.dormancy.wd %>% 
	filter(`2019-03-10` == 1)

# Initialize columns for extirpation
dormancy.ext["Extirpated.ever"] <- NA
dormancy.ext["Extirpated.end"] <- NA

# Determine if dormancy class was ever extirpated from the point
zeroes <- rowSums(dormancy.ext[9:14] == 0)
dormancy.ext$zeroes <- zeroes

for (i in 1:nrow(dormancy.ext)){
	if (dormancy.ext$zeroes[i] >= 1){
		dormancy.ext$Extirpated.ever[i] <- 1
	} else {
		dormancy.ext$Extirpated.ever[i] <- 0
	}
}

# Determine if dormancy class was extirpated from the point by the
# last sampling date

for (i in 1:nrow(dormancy.ext)){
	if (dormancy.ext$`2021-07-18`[i] == 0){
		dormancy.ext$Extirpated.end[i] <- 1
	} else {
		dormancy.ext$Extirpated.end[i] <- 0
	}
}

# Drop the NA values
dormancy.ext <- dormancy.ext %>% 
	filter(DormancyClass != "NA")

# Rearrange the columns
dormancy.ext <- dormancy.ext %>% 
	dplyr::select(Site, Treatment, Carrion, Exclusion, Transect, Distance, DormancyClass,
				 Extirpated.ever, Extirpated.end, everything(), -zeroes)

## --------------- SAVE CLEAN DATA ---------------------------------------------

write.csv(dormancy.col, "Animals-plants-seeds/Clean-data/Plants/Dormancy-class-colonization.csv",
					row.names=FALSE)

write.csv(dormancy.ext, "Animals-plants-seeds/Clean-data/Plants/Dormancy-class-extinction.csv",
					row.names=FALSE)
