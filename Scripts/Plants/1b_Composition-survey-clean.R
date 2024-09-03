## --------------- HEADER ------------------------------------------------------
## Script name: 1b_Composition-survey-clean.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliaton: University of Florida
## Date Created: 2021-12-17
## Date Last modified: 2022-05-03
## Copyright (c) David S. Mason, 2022
## Contact: masond@ufl.edu, @EcoGraffito
## Purpose of script: This is a data munging script for the plant community data
## that combines duplicate entries and adds names to unknown entries.

## --------------- SET—UP WORKSPACE --------------------------------------------
library(tidyverse)
library(lubridate)

# Bring in the combined plant survey data
surv <- read.csv("Animals-plants-seeds/Raw-data/Plants/Combined-plot-summarized-wd.csv")

## --------------- COMBINE LIKE SPECIES ----------------------------------------

# We have combined surveys and some of the species are listed multiple times
# under different names. Here, we will combine those before moving forward.

# Get a list of the species names
list <- as.data.frame(colnames(surv[1, 9:145]))
names(list)[1] <- "Species"

# Combine like columns and drop the extra column

surv <- surv %>%
	mutate(
		AMDR = AMDR + AMOR, # Combine columns
		BOHI2 = BOHI2 + BOHI,
		BOIS = BOIS + BOYS,
		ENPE4 = ENPE4 + ENP + ENPI,
		THFI = GREENT + GREENT2 + GREENTHREAD + WIREASTER + THIFI + GREENT.,
		LASE = LASE + LACE + LASE.1,
		LEVI = LEVI + LEPIDIUM,
		APIAC = APIAC + CHAER,
		ASAS = ASAS + MILKWEED,
		PAJA = PAJA + PARON,
		PANIC = PANIC + PANICLE + DICHA2 + DICO2 + DILO + DIOL + DICO,
		NALE3 = NALE3 + NEEDLE,
		RIBS = RIBS + RIB,
		SPCOC2 = SGSC + SPCOC2,
		ERCU = ERCU + LOVE,
		LINUM = LIRI + LIPR,
		GAURA = GAVI + GABI + GASUS + GASU,
	) %>%
	dplyr::select( # Drop the old column
		-AMOR, -BOHI, -BOYS, -DICHA2, -DICO2, -DILO,
		-DIOL, -DICO, -ENP, -ENPI, -GASUS, -GASU, -GREENT, -GREENT, -GREENT2,
		-GREENTHREAD, -THIFI, -LACE, -LASE.1, -LEPIDIUM, -CHAER,
		-GABI, -MILKWEED, -PARON, -PANICLE, -NEEDLE, -RIB, -SGSC, -LOVE,
		-WIREASTER, -LIRI, -LIPR, -GAVI, -GREENT.
	)


## --------------- TRIM DATA ---------------------------------------------------
# source("Animals-plants-seeds/Functions.r")

# spec <- surv[, 9:126] # Separate species
# pred <- surv[, 1:8] # Separate predictors

# occur <- foa.plots(spec)

# rare <- which(occur[, 2] < 2)
# common <- which(occur[, 2] > 80)

# spec <- spec[, -c(rare, common)]

# surv <- cbind(pred, spec)

# Poos, M. S., & Jackson, D. A. (2012). Addressing the removal of rare species 
# in multivariate bioassessments: the impact of methodological choices. 
# Ecological Indicators, 18, 82-90.

## --------------- RENAME SPECIES ----------------------------------------------
surv$Date <- mdy(surv$Date)
is.Date(surv$Date)

surv <- surv %>% 
	mutate(Rounded.date = round_date(Date, unit = "week")) %>% 
	select(Site, Treatment, Carrion, Exclusion, Month, Day, Year,
				 Date, Rounded.date, everything()) %>% 
	select(-Date)

surv <- surv %>% 
	pivot_longer(cols = 9:115, names_to = "Species", values_to = "Cover")

## --------------- COMBINE UNKNOWNS  -------------------------------------------

# Create a vector of the unknown species
unknowns <- c(
	"RIBS", "RAGGED", "UNKF10", "UNK28", "UNKF7", "UNK24", "UNKF5",
	"UNKF8", "UNK.G6", "UNK23", "UNKF3", "UNKF3", "UNK21", "UNK26",
	"UNK27", "UNK29", "TB", "BASALASTER", "BINP", "THINAMPR", "UNKF9",
	"UNK6", "UNKC1", "UNKF", "UNKF10", "UNKF11", "UNKF12", "UNKF13",
	"UNKF14", "UNKF2", "UNKF4", "UNKF5", "UNKF9", "LF", "RM", "SG",
	"UNK.G5", "UNK2___", "UNK20", "BIHI"
)

# Change the name of all species in the unknowns vector
surv$Species[surv$Species %in% unknowns] <- "Unknown_plant"

# Summarize the unknowns data
surv <- surv %>%
	select(
		-Month, -Day, -Year
	) %>% 
	group_by(
	 Site, Treatment, Carrion, Exclusion, Rounded.date, Species
	) %>%
	summarize(Cover = sum(Cover))

## --------------- COMBINE NON-PLANTS  -----------------------------------------

# Create a vector of non-plants
non.plants <- c(
	"DEAD", "BG", "LIT", "PIG", "COW.PATTIE", "WOODSURFACE",
	"COW PATTIE"
)

# Change the name of all species in the non.plants vector
surv$Species[surv$Species %in% non.plants] <- "Non_plant"

# Summarize the unknowns data
surv <- surv %>%
	group_by(
		Site, Treatment, Carrion, Exclusion, Rounded.date, Species
	) %>%
	summarize(Cover = sum(Cover))

# Remove the non-plants from the plant survey data
# surv <- surv %>%
	# filter(!Species == "Non_plant")

## --------------- ADD GENUS SPECIES  ------------------------------------------

# Create a list of species to reference
list <- as.data.frame(unique(surv$Species))
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
	data.frame(Species = c("LINUM"), Genus.species = c("Linum sp")),
	data.frame(Species = c("GAURA"), Genus.species = c("Gaura sp")),
	data.frame(Species = c("Unknown_plant"), Genus.species = c("Unknown plant")),
	data.frame(Species = c("Non_plant"), Genus.species = c("Non plant"))
)

# Join the genus species dataframe to the plant survey dataframe
surv <- full_join(surv, spec.func.list,
													 keep = FALSE
)

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
surv <- merge(surv, seed.dorm.db,
											 all.x = TRUE, sort = FALSE
)

# Reorder the columns
surv <- surv %>% 
	dplyr::select(Rounded.date, Site, Treatment, Carrion, Exclusion,
								Genus.species, DormancyClass, Cover
	)
## --------------- ADD SEED DORMANCY FUNCTIONAL GROUP FOR NAs ------------------

# Filter for species without a seed dormancy class listed
surv.unk <- surv %>% 
	filter(is.na(DormancyClass) == TRUE
	)

# Create a vector of the plant names that do not have a seed dormancy class
unk.species <- as.data.frame(unique(surv.unk$Genus.species))
colnames(unk.species)[1] <- "Genus.species"

# For loop to add dormancy class to unlisted species from the dormany database

for (i in 1:nrow(surv)) {
	if (isTRUE(surv$Genus.species[i] == "Aristida purpurea_Aristida purpurascens")) {
		surv$DormancyClass[i] <- NA
	}
	if (isTRUE(surv$Genus.species[i] == "Opuntia sp")) {
		surv$DormancyClass[i] <- "PD"
	}
	if (isTRUE(surv$Genus.species[i] == "Tragia ramosa")) {
		surv$DormancyClass[i] <- NA
	}
	if (isTRUE(surv$Genus.species[i] == "Ambrosia psilostachya")) {
		surv$DormancyClass[i] <- "PD"
	}
	if (isTRUE(surv$Genus.species[i] == "Apiaceae sp")) {
		surv$DormancyClass[i] <- "PD"
	}
	if (isTRUE(surv$Genus.species[i] == "Nassella leucotricha")) {
		surv$DormancyClass[i] <- "PD"
	}
	if (isTRUE(surv$Genus.species[i] == "Eriogonum longifolium")) {
		surv$DormancyClass[i] <- "PD"
	}
	if (isTRUE(surv$Genus.species[i] == "Lolium perenne")) {
		surv$DormancyClass[i] <- "PD"
	}
	if (isTRUE(surv$Genus.species[i] == "Castilleja sp")) {
		surv$DormancyClass[i] <- "PD"
	}
	if (isTRUE(surv$Genus.species[i] == "Pyrrhopappus carolinianus")) {
		surv$DormancyClass[i] <- "PD"
	}
	if (isTRUE(surv$Genus.species[i] == "Asclepias asperula")) {
		surv$DormancyClass[i] <- "ND"
	}
	if (isTRUE(surv$Genus.species[i] == "Sida sp")) {
		surv$DormancyClass[i] <- "PY"
	}
	if (isTRUE(surv$Genus.species[i] == "Croton sp")) {
		surv$DormancyClass[i] <- "PD"
	}
	if (isTRUE(surv$Genus.species[i] == "Paronychia jamesii")) {
		surv$DormancyClass[i] <- NA
	}
	if (isTRUE(surv$Genus.species[i] == "Helianthus hirsutus")) {
		surv$DormancyClass[i] <- "PD"
	}
	if (isTRUE(surv$Genus.species[i] == "Engelmannia peristenia")) {
		surv$DormancyClass[i] <- "PD"
	}
	if (isTRUE(surv$Genus.species[i] == "Evax verna")) {
		surv$DormancyClass[i] <- "PD"
	}
	if (isTRUE(surv$Genus.species[i] == "Trifolium sp")) {
		surv$DormancyClass[i] <- "PY"
	}
	if (isTRUE(surv$Genus.species[i] == "Allium sp")) {
		surv$DormancyClass[i] <- "PD"
	}
	if (isTRUE(surv$Genus.species[i] == "Vicia sp")) {
		surv$DormancyClass[i] <- "PY"
	}
	if (isTRUE(surv$Genus.species[i] == "Bryophyta sp")) {
		surv$DormancyClass[i] <- NA
	}
	if (isTRUE(surv$Genus.species[i] == "Sporobolus compositus")) {
		surv$DormancyClass[i] <- "PD"
	}
	if (isTRUE(surv$Genus.species[i] == "Scutellaria ovata")) {
		surv$DormancyClass[i] <- "PD"
	}
	if (isTRUE(surv$Genus.species[i] == "Solidago sp")) {
		surv$DormancyClass[i] <- "PD"
	}
	if (isTRUE(surv$Genus.species[i] == "Convolvulus equitans")) {
		surv$DormancyClass[i] <- "PY"
	}
	if (isTRUE(surv$Genus.species[i] == "Linum sp")) {
		surv$DormancyClass[i] <- "PD"
	}
	if (isTRUE(surv$Genus.species[i] == "Diodea teres")) {
		surv$DormancyClass[i] <- "PD"
	}
	if (isTRUE(surv$Genus.species[i] == "Phyllanthus polygonoides")) {
		surv$DormancyClass[i] <- NA
	}
	if (isTRUE(surv$Genus.species[i] == "Houstonia pusilla")) {
		surv$DormancyClass[i] <- "PD"
	}
	if (isTRUE(surv$Genus.species[i] == "Hedeoma drummondii")) {
		surv$DormancyClass[i] <- NA
	}
	if (isTRUE(surv$Genus.species[i] == "Asclepias verticillata")) {
		surv$DormancyClass[i] <- "PD"
	}
	if (isTRUE(surv$Genus.species[i] == "Sisyrinchium sp")) {
		surv$DormancyClass[i] <- "PD"
	}
	if (isTRUE(surv$Genus.species[i] == "Cirsium texanum")) {
		surv$DormancyClass[i] <- "PD"
	}
	if (isTRUE(surv$Genus.species[i] == "Lobelia sp")) {
		surv$DormancyClass[i] <- "PD"
	}
	if (isTRUE(surv$Genus.species[i] == "Dalea purpurea")) {
		surv$DormancyClass[i] <- "PY"
	}
	if (isTRUE(surv$Genus.species[i] == "Krameria lanceolata")) {
		surv$DormancyClass[i] <- "PD"
	}
	if (isTRUE(surv$Genus.species[i] == "Agalinis heterophylla")) {
		surv$DormancyClass[i] <- "PD"
	}
	if (isTRUE(surv$Genus.species[i] == "Andropogon sp")) {
		surv$DormancyClass[i] <- "PD"
	}
	if (isTRUE(surv$Genus.species[i] == "Sabatia campestris")) {
		surv$DormancyClass[i] <- "PD"
	}
	if (isTRUE(surv$Genus.species[i] == "Eryngium leavenworthii")) {
		surv$DormancyClass[i] <- "PD"
	}
	if (isTRUE(surv$Genus.species[i] == "Hymenopappus tenuifolius")) {
		surv$DormancyClass[i] <- NA
	}
	if (isTRUE(surv$Genus.species[i] == "Setaria sp")) {
		surv$DormancyClass[i] <- "PD"
	}
	if (isTRUE(surv$Genus.species[i] == "Verbena halei")) {
		surv$DormancyClass[i] <- "PD"
	}
	if (isTRUE(surv$Genus.species[i] == "Eragrostis curtipedicellata")) {
		surv$DormancyClass[i] <- "PD"
	}
	if (isTRUE(surv$Genus.species[i] == "Lesquerella sp")) {
		surv$DormancyClass[i] <- "PD"
	}
	if (isTRUE(surv$Genus.species[i] == "Erioneuron pilosum")) {
		surv$DormancyClass[i] <- NA
	}
	if (isTRUE(surv$Genus.species[i] == "Panicum sp")) {
		surv$DormancyClass[i] <- "PD"
	}
	if (isTRUE(surv$Genus.species[i] == "Bothriochloa ischaemum")) {
		surv$DormancyClass[i] <- "PD"
	}
	if (isTRUE(surv$Genus.species[i] == "Bothriochloa laguroides")) {
		surv$DormancyClass[i] <- "PD"
	}
	if (isTRUE(surv$Genus.species[i] == "Glandularia bipinnatifida")) {
		surv$DormancyClass[i] <- "PD"
	}
	if (isTRUE(surv$Genus.species[i] == "Stylosanthes biflora")) {
		surv$DormancyClass[i] <- "PY"
	}
	if (isTRUE(surv$Genus.species[i] == "Chamaesyce sp")) {
		surv$DormancyClass[i] <- NA
	}
	if (isTRUE(surv$Genus.species[i] == "Oxalis sp")) {
		surv$DormancyClass[i] <- "ND"
	}
	if (isTRUE(surv$Genus.species[i] == "Packera obovata")) {
		surv$DormancyClass[i] <- "ND"
	}
	if (isTRUE(surv$Genus.species[i] == "Bothriochloa saccharoides")) {
		surv$DormancyClass[i] <- "PD"
	}
	if (isTRUE(surv$Genus.species[i] == "Erigeron sp")) {
		surv$DormancyClass[i] <- "ND"
	}
	if (isTRUE(surv$Genus.species[i] == "Dyschoriste linearis")) {
		surv$DormancyClass[i] <- NA
	}
	if (isTRUE(surv$Genus.species[i] == "Cassia marilandica")) {
		surv$DormancyClass[i] <- "PY"
	}
	if (isTRUE(surv$Genus.species[i] == "Lamiaceae sp")) {
		surv$DormancyClass[i] <- NA
	}
	if (isTRUE(surv$Genus.species[i] == "Gaura sp")) {
		surv$DormancyClass[i] <- "PD"
	}
	if (isTRUE(surv$Genus.species[i] == "Thelesperma filifolium")) {
		surv$DormancyClass[i] <- NA
	}
	if (isTRUE(surv$Genus.species[i] == "Unknown plant")) {
		surv$DormancyClass[i] <- NA
	}
}

# Calculate how many unknown seed dormancy class observations remain
(sum(is.na(surv$DormancyClass)))/(nrow(surv))
# 17% of rows are NAs

# Check to the levels of the seed dormancy classes
unique(surv$DormancyClass)

# Fix seed dormancy classes
for (i in 1:nrow(surv)){
	if(isTRUE(surv$DormancyClass[i] == "nd")){
		surv$DormancyClass[i] <- "ND"
	}
	if(isTRUE(surv$DormancyClass[i] == "pd")){
		surv$DormancyClass[i] <- "PD"
	}
	if(isTRUE(surv$DormancyClass[i] == "PYPD")){
		surv$DormancyClass[i] <- "PY"
	}
	if(isTRUE(surv$DormancyClass[i] == "MPD")){
		surv$DormancyClass[i] <- "PD"
	}
}


## --------------- WRITE DATA --------------------------------------------------
write.csv(surv, "Animals-plants-seeds/Clean-data/Plants/Community-matrix-lg.csv",
					row.names=FALSE)

