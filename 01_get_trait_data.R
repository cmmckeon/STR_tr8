## 01_get_trait_data.R
## Caroline McKeon 17/11/2020

## this script gets list of species and trait data from BIEN, and downloads (or reads in) data from traits of interest

## BIEN-----------

## get species list ---------------
## and overlap with range metric species list 

## get the list of BIEN species for which there are range metric data*
## * if you get errors connecting to server it is because the trinity network blocks SQL connections. 
## Hotspot or try another network and should work fine. 

# bien_sp <- BIEN_list_all() ## 331065 obs of 1 var
# sp.list_BIEN <- Reduce(intersect, list(unique(bien_sp$species),unique(mydata$Species))) ## 457 species
# saveRDS(sp.list_BIEN, "Data_sp.list_BIEN.rds")

## or read back in 
sp.list_BIEN <-readRDS("Data_sp.list_BIEN.rds") ## 

## done --------------------

## extract traits for specific species -----------------------

## get list of traits available from BIEN
# trait_info_bien <- BIEN_trait_list() ## 53 traits
# saveRDS(trait_info_bien, "Data_trait_info_bien.rds")

#dput(trait_info_bien)
## all traits
# c("diameter at breast height (1.3 m)",
#   "flower color", "flower pollination syndrome", "fruit type",
#   "inflorescence length", "leaf area", "leaf area per leaf dry mass",
#   "leaf carbon content per leaf dry mass", "leaf carbon content per leaf nitrogen content",
#   "leaf compoundness", "leaf dry mass", "leaf dry mass per leaf fresh mass",
#   "leaf fresh mass", "Leaf lamina fracture toughness", "leaf life span",
#   "leaf nitrogen content per leaf area", "leaf nitrogen content per leaf dry mass",
#   "leaf phosphorus content per leaf area", "leaf phosphorus content per leaf dry mass",
#   "leaf photosynthetic rate per leaf area", "leaf photosynthetic rate per leaf dry mass",
#   "leaf relative growth rate", "leaf stomatal conductance for H2O per leaf area",
#   "leaf thickness", "longest whole plant longevity", "maximum fruit length",
#   "maximum leaf length", "maximum leaf width", "maximum whole plant height",
#   "maximum whole plant longevity", "minimum fruit length", "minimum leaf length",
#   "minimum leaf width", "minimum whole plant height", "plant flowering begin",
#   "plant flowering duration", "plant fruiting duration", "root dry mass",
#   "seed length", "seed mass", "stem dry mass", "stem relative growth rate",
#   "stem wood density", "vessel lumen area", "vessel number", "whole plant dispersal syndrome",
#   "whole plant growth form", "whole plant growth form diversity",
#   "whole plant height", "whole plant primary juvenile period length",
#   "whole plant sexual system", "whole plant vegetative phenology",
#   "whole plant woodiness")
## relevant traits
## currently unsure - get them all!

# traits_bien <- BIEN_trait_traitbyspecies(c("diameter at breast height (1.3 m)", 
#                                            "flower color", "flower pollination syndrome", "fruit type",
#                                            "inflorescence length", "leaf area", "leaf area per leaf dry mass",
#                                            "leaf carbon content per leaf dry mass", "leaf carbon content per leaf nitrogen content",
#                                            "leaf compoundness", "leaf dry mass", "leaf dry mass per leaf fresh mass",
#                                            "leaf fresh mass", "Leaf lamina fracture toughness", "leaf life span",
#                                            "leaf nitrogen content per leaf area", "leaf nitrogen content per leaf dry mass",
#                                            "leaf phosphorus content per leaf area", "leaf phosphorus content per leaf dry mass",
#                                            "leaf photosynthetic rate per leaf area", "leaf photosynthetic rate per leaf dry mass",
#                                            "leaf relative growth rate", "leaf stomatal conductance for H2O per leaf area",
#                                            "leaf thickness", "longest whole plant longevity", "maximum fruit length",
#                                            "maximum leaf length", "maximum leaf width", "maximum whole plant height",
#                                            "maximum whole plant longevity", "minimum fruit length", "minimum leaf length",
#                                            "minimum leaf width", "minimum whole plant height", "plant flowering begin",
#                                            "plant flowering duration", "plant fruiting duration", "root dry mass",
#                                            "seed length", "seed mass", "stem dry mass", "stem relative growth rate",
#                                            "stem wood density", "vessel lumen area", "vessel number", "whole plant dispersal syndrome",
#                                            "whole plant growth form", "whole plant growth form diversity",
#                                            "whole plant height", "whole plant primary juvenile period length",
#                                            "whole plant sexual system", "whole plant vegetative phenology",
#                                            "whole plant woodiness"), species=sp.list_BIEN) ## 84206 obs of 13 vars
# saveRDS(traits_bien, "Data_traits_bien.rds")
## or read back in
traits_bien <-readRDS("Data_traits_bien.rds") ## 1867 obs of 13 vars
## BIEN doesn't have trait data for alot of the species we want unfortunately

## done --------------------

## clean the trait data -------------------------------

## make strings factors
for (i in names(Filter(is.character, traits_bien))) {
  traits_bien[,i] <- factor(traits_bien[,i])
}

## Quick look at model dataframe
## Factors
# for (i in names(Filter(is.factor, traits_bien))) {
#   plot(traits_bien[,i],
#        main = paste(i))
# }
# ## Numeric variables
# for (i in names(Filter(is.numeric, traits_bien))) {
#   hist(traits_bien[,i],
#        breaks = 3000,
#        main = paste(i),
#        xlab = paste(i))
# }

#dput(levels(traits_bien$method))

## just take the columns with species name, trait names and trait value and data collection method. 
traits_bien <- unique(traits_bien[which(names(traits_bien) %in% c("scrubbed_species_binomial", "trait_name", "trait_value", "unit", "method"))])

## fix ------------

# dput(levels(traits_bien$trait_name))
# 
# c("longest whole plant longevity", "maximum whole plant height", 
#   "maximum whole plant longevity", "seed mass", "whole plant growth form", 
#   "whole plant growth form diversity", "whole plant height", "whole plant vegetative phenology", 
#   "whole plant woodiness")


# traits_bien$trait_name[traits_bien$trait_name == "longest whole plant longevity"] <- "maximum whole plant longevity"
# traits_bien$trait_name[traits_bien$trait_name == "whole plant height"] <- "maximum whole plant height"
# traits_bien <- droplevels(traits_bien)

## a) drop problem records -------------

## b) consolitrait ---------------

## TRY -------------

## TRY data has to be mannually downloaded from the TRY plant trait data base, but there is some handling required first to get the right species list

## get species list ------------

##(Mannually download and) read in TRY species info data (version 5)
#sp.info_try <- read.csv("Data_try_species_info.csv") ## 279875 obs of 7 vars

## Get the list of TRY species for which there are range metric data
# sp.list_TRY <- Reduce(intersect, list(unique(sp.info_try$AccSpeciesName),unique(mydata$Species))) ## species
# saveRDS(sp.list_TRY, "Data_sp.list_TRY.rds")
sp.list_TRY <- readRDS('Data_sp.list_TRY.rds')

## done ----------

## extract traits for specific species ----------------

# Get TRY species IDs to extract from database 
#cat(paste(sp.info_try$AccSpeciesID[sp.info_try$AccSpeciesName %in% sp.list_TRY], collapse = ", ")) 
# Now request data corresponding to these lists from the TRY Dataportal on the website.
# Then download and read in this data.

trait_info_try <- read.csv("Data_try_trait_info.csv")
# 
# #Get list of TRY trait codes for traits related to disperal, establishment and persistance
# trait_info_try <- trait_info_try[trait_info_try$Trait %in% c("Cone (strobilus) dry mass", "Dispersal distance", "Dispersal syndrome", "Dispersal unit appendages", "Dispersal unit dry mass",
#   "Dispersal unit type", "Flower insemination autogamous or xenogamous", 'Flower pollinator and type of reward', 
#   "Flower sexual syndrome (dichogamy, cleistogamy, dioecious, monoecious)", "Fruit mass", "Fruit type", "Fruit surface type", 
#   "Mycorrhiza type", "Plant clonal growth form", "Plant height vegetative", "Plant height generative", "Plant mating system",
#   "Plant ontogeny: age of maturity (first flowering)", "Plant recruitment efficiency", "Plant propagation type", "Plant reproductive phenology timing",
#   "Plant resprouting capacity", "Plant vegetative reproduction: clonal growth organ", "Plant vegetative regeneration capacity", 
#   "Plant vegetative reproduction: role of clonal growth organ in plant growth", 'Plant vital attributes of persistence and establishment', 
#   'Plant woodiness', "Pollination syndrome", 'Seed (seedbank) longevity', 'Seed dry mass', 'Seed germination rate (germination efficiency)',
#   "Seed germination type", 'Seed morphology type', 'Seedbank type', "Species generation overlap", "Species reproduction type", 
#   'Species strategy type according to Grime', 'Species tolerance to human impact'),]
# 
# try_traitIDs <- paste(trait_info_try$TraitID, collapse = ", ") ## 38 traits 

## fix 
# Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA) petiole, rhachis and midrib excluded	645	645	403
# 814	3115	Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole excluded	64838	53275	7558
# 815	3116	Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole included	88490	79896	7068
# 816	3117	Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): undefined if petiole is in- or exclu	146315	127020	13101
# 817	125	Leaf area per leaf fresh mass (specific leaf area (SLA or 1/LMA) based on leaf fresh mass)	13300	12770	743
# 
# 
# 3117	Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): undefined if petiole is in- or exclu	146315	127020	13101
# 1033	22	Leaf photosynthesis pathway	143148	62684	37315
# 985	14	Leaf nitrogen (N) content per leaf dry mass	129895	111855	12238
# 871	17	Leaf compoundness	115459	30133	57922
# 3441	Plant biomass and allometry: Leaf mass (dry or fresh) per plant	17069	16907	587


## having downloaded the traits you want for the species you want, read in your TRY trait data
traits_try <- read.csv('Data_try_traits_tr8.csv')

## done -----------------------

## clean the trait data -----------------

## find the overlap in species between try and bien
#sp.list_TRY_BIEN <- Reduce(intersect, list(unique(traits_bien$scrubbed_species_binomial),unique(traits_try$AccSpeciesName))) ## 168 species 
sp.list_full_TRY_BIEN <- unique(c(sp.list_TRY, sp.list_BIEN)) ## 473 species

## fix
## you've to come back and make decisions about which records to exclude based on how they were collected, expertimental data ect. 


## for now, just take the columns with species name, trait names and trait value. You can come back to this point to look at more of the info if you need to
traits_try <- unique(traits_try[which(names(traits_try) %in% c("AccSpeciesName", "TraitName", "OrigValueStr"))])
traits_try <- unique(traits_try[traits_try$TraitName != "",])

## short term subset of bien data
traits_bien <- unique(traits_bien[which(names(traits_bien) %in% c("scrubbed_species_binomial", "trait_name", "trait_value"))])

names(traits_try) <- c("species", "trait_name", "trait_value")
names(traits_bien) <- c("species", "trait_name", "trait_value")



## create trait dataframes ------------------

## make one big traits df to tidy
names(traits_try) <- names(traits_bien)
traits <- droplevels(rbind(traits_bien, traits_try))
length(unique(traits$species)) ## 288 species

## restructure the dataframe for your own nefarious purposes
## create columns for each trait
for (i in levels(traits$trait_name)){
  traits[,i] <- factor(NA) 
  levels(traits[,i]) <- levels(traits$trait_value)
  }

## populate the columns with values for that trait
for (i in levels(traits$trait_name)){
  traits[,i][traits$trait_name == i] <- traits$trait_value[traits$trait_name == i]
}

## create list of dataframes of species values for single traits
x <- list()
for (i in levels(traits$trait_name)){
  x[[i]] <- droplevels(unique(traits[,which(names(traits) %in% c('species', i))][which(!is.na(traits[,i])),], 
                              by = "species"))
}

## done --------------------


#traits <- unique(traits[,which(names(traits) %nin% c("trait_value", "trait_name"))])

## for maybe if you wanted this list all together in a data frame
b <- data.frame(unique(traits$species))
names(b) <- "species"
for (i in levels(traits$trait_name)){
  b <- unique(merge(b, traits[,which(names(traits) %in% c('species', i))], by = "species"))
}

for (i in levels(traits$trait_name)){
  i <- b[[i]]
}
#dput(levels(traits$trait_name))

























