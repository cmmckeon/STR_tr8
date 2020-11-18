## 01_get_trait_data.R
## Caroline McKeon 17/11/2020

## this script gets list of species and trait data from BIEN, and downloads (or reads in) data from traits of interst

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

## get list of traits available from BIEN
#trait_info_bien <- BIEN_trait_list() ## 53 traits
#saveRDS(trait_info_bien, "Data_trait_info_bien.rds")

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
# c("flower pollination syndrome", "fruit type", 
#   "longest whole plant longevity", "maximum fruit length", 
#   "maximum leaf length", "maximum leaf width", "maximum whole plant height", 
#   "maximum whole plant longevity",
#   "plant flowering duration", "plant fruiting duration",
#   "seed length", "seed mass", "stem relative growth rate", 
#   "whole plant dispersal syndrome", 
#   "whole plant growth form", "whole plant growth form diversity", 
#   "whole plant height", "whole plant primary juvenile period length", 
#   "whole plant sexual system", "whole plant vegetative phenology", 
#   "whole plant woodiness")

## get traits for specific species -----------------------
# traits_bien <- BIEN_trait_traitbyspecies(c("flower pollination syndrome", "fruit type", 
#                                      "longest whole plant longevity", "maximum fruit length", 
#                                      "maximum leaf length", "maximum leaf width", "maximum whole plant height", 
#                                      "maximum whole plant longevity",
#                                      "plant flowering duration", "plant fruiting duration",
#                                      "seed length", "seed mass", "stem relative growth rate", 
#                                      "whole plant dispersal syndrome", 
#                                      "whole plant growth form", "whole plant growth form diversity", 
#                                      "whole plant height", "whole plant primary juvenile period length", 
#                                      "whole plant sexual system", "whole plant vegetative phenology", 
#                                      "whole plant woodiness"),species=sp.list_BIEN) ## 84206 obs of 13 vars
# #saveRDS(traits_bien, "Data_traits_bien.rds")
## or read back in
traits_bien <-readRDS("Data_traits_bien.rds") ## 1867 obs of 13 vars
## BIEN doesn't have trait data for alot of the species we want unfortunately

## clean the trait data -------------------------------

## make strings factors
for (i in names(Filter(is.character, traits_bien))) {
  traits_bien[,i] <- factor(traits_bien[,i])
}

## Quick look at model dataframe
# Factors
for (i in names(Filter(is.factor, traits_bien))) {
  plot(traits_bien[,i],
       main = paste(i))
}
## Numeric variables
for (i in names(Filter(is.numeric, traits_bien))) {
  hist(traits_bien[,i],
       breaks = 3000,
       main = paste(i),
       xlab = paste(i))
}



# dput(levels(traits_bien$trait_name))
# 
# c("longest whole plant longevity", "maximum whole plant height", 
#   "maximum whole plant longevity", "seed mass", "whole plant growth form", 
#   "whole plant growth form diversity", "whole plant height", "whole plant vegetative phenology", 
#   "whole plant woodiness")


traits_bien$trait_name[traits_bien$trait_name == "longest whole plant longevity"] <- "maximum whole plant longevity"
traits_bien$trait_name[traits_bien$trait_name == "whole plant height"] <- "maximum whole plant height"
traits_bien <- droplevels(traits_bien)

## for now, just take the columns with species name, trait names and trait value. You can come back to this point to look at more of the info if you need to
traits_bien <- unique(traits_bien[which(names(traits_bien) %in% c("scrubbed_species_binomial", "trait_name", "trait_value"))])



## TRY -------------

## TRY data has to be mannually downloaded from the TRY plant trait data base, but there is some handling required first to get the right species list

##(Mannually download and) read in TRY species info data (version 5)
#sp.info_try <- read.csv("Data_try_species_info.csv") ## 279875 obs of 7 vars

## Get the list of TRY species for which there are range metric data
# sp.list_TRY <- Reduce(intersect, list(unique(sp.info_try$AccSpeciesName),unique(mydata$Species))) ## species
# saveRDS(sp.list_TRY, "Data_sp.list_TRY.rds")
sp.list_TRY <- readRDS('Data_sp.list_TRY.rds')

# Get TRY species IDs to extract from database 
#cat(paste(sp.info_try$AccSpeciesID[sp.info_try$AccSpeciesName %in% sp.list_TRY], collapse = ", ")) 
# Now request data corresponding to these lists from the TRY Dataportal on the website.
# Then download and read in this data.


# trait_info_try <- read.csv("Data_try_trait_info.csv")
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


## having downloaded the traits you want for the species you want, read in your TRY trait data
traits_try <- read.csv('Data_try_traits_tr8.csv')

## for now, just take the columns with species name, trait names and trait value. You can come back to this point to look at more of the info if you need to
traits_try <- unique(traits_try[which(names(traits_try) %in% c("AccSpeciesName", "TraitName", "OrigValueStr"))])
traits_try <- unique(traits_try[traits_try$TraitName != "",])


## find the overlap in species between try and bien
sp.list_TRY_BIEN <- Reduce(intersect, list(unique(traits_bien$scrubbed_species_binomial),unique(traits_try$AccSpeciesName))) ## 457 species

## now create one big traits df to tidy
names(traits_try) <- names(traits_bien)
traits <- droplevels(rbind(traits_bien, traits_try))
length(unique(traits$scrubbed_species_binomial)) ## 287 species

## restructure the dataframe for your own nefarious purposes
for (i in levels(traits$trait_name)){
  traits[,i] <- factor(NA) 
  levels(traits[,i]) <- levels(traits$trait_value)
  }


for (i in levels(traits$trait_name)){
  traits[,i][traits$trait_name == i] <- traits$trait_value[traits$trait_name == i]
}

#traits <- unique(traits[,which(names(traits) %nin% c("trait_value", "trait_name"))])

x <- list()
for (i in levels(traits$trait_name)){
  x[[i]] <- droplevels(unique(traits[,which(names(traits) %in% c('scrubbed_species_binomial', i))][which(!is.na(traits[,i])),], 
                              by = "scrubbed_species_binomial"))
}

## for maybe if you wanted this list all together in a data frame
# x <- data.frame(traits$scrubbed_species_binomial)
# names(x) <- "scrubbed_species_binomial"
# for (i in levels(traits$trait_name)){
#   x <- unique(merge(x, traits[,which(names(traits) %in% c('scrubbed_species_binomial', i))], by = "scrubbed_species_binomial")) 
# }

for (i in levels(traits$trait_name)){
  i <- x[[i]]
}

