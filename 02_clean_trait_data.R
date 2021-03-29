## 02_clean_trait_data
## cleaning TRY and BIEN trait data
## Feburary 2021
## Caroline McKeon

## having looked at Dr Ruth Kelly's detailed cleaning notes, I will take a rule based exclusion approach to data from plants that are:
## juvenile, manipulated or unhealthy.
## I then consolidate suitable data into the traits of interest

## name the trait dataset "mydata"
mydata <- droplevels(traits)

mydata <- mydata[which(!is.na(mydata$trait_value)),]
mydata <- mydata[mydata$trait_value %nin% c("na", "NA", "Na"),]
mydata <- mydata[mydata$trait_value != "",]

## group trait categories
#dput(levels(mydata$trait_name))

mydata <- droplevels(mydata[mydata$trait_name %in% c("Seed dry mass","seed mass", "Dispersal unit dry mass", 
  "Plant woodiness", "whole plant woodiness", "Seed (seedbank) longevity", "Seedbank type", "Dispersal syndrome", "Dispersal unit appendages", 
  "Dispersal unit type", "Fruit type", "Seed morphology type", "Flower insemination autogamous or xenogamous", 
  "Flower pollinator and type of reward", "Flower sexual syndrome (dichogamy, cleistogamy, dioecious, monoecious)", 
  "Pollination syndrome", "Plant mating system", "Plant ontogeny: age of maturity (first flowering)", 
  "plant flowering begin", "Plant height generative", "Plant height vegetative", "maximum whole plant height",
  "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole excluded", 
  "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole included", 
  "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): undefined if petiole is in- or excluded", 
  "leaf area per leaf dry mass",  "Leaf nitrogen (N) content per leaf dry mass",  "leaf nitrogen content per leaf dry mass", 
  "leaf area", "leaf dry mass", "leaf nitrogen content per leaf area", "Plant propagation type", 
  "Plant reproductive phenology timing", "Plant resprouting capacity", 
  "Plant vegetative regeneration capacity", "Plant vegetative reproduction: role of clonal growth organ in plant growth", 
  "Species reproduction type", "whole plant vegetative phenology", "Plant clonal growth form"),])

## drop these levels
# c("whole plant growth form", "Species tolerance to human impact", 
#   "Species strategy type according to Grime", "whole plant vegetative phenology",
#   "Species generation overlap",   "Mycorrhiza type", "leaf dry mass",   
#    "Plant ontogeny: age of maturity (first flowering)", "plant flowering begin", "Dispersal unit dry mass")


## look at levels
#dput(levels(factor(mydata$unit)))

# c("g", "g.cm-3", "kg.m-2", "m", "m2.kg-1", "mg", "mg.g-1", 
#   "mm2", "month", "", "%", "1/pound", "cm", 
#  "feet", "g / 1000 seeds", "g/m2", "Julian Day", 
#   "mg/g", "mm", "text", "year", "cm2 g-1", "cm2.g-1", "cm2/g", 
#   "cm2/g (n.r.)", "g N g-1 DW", "g/kg", "gDM m-2", "m\xb2 kg-1", 
#   "m2 kg-1", "m2/g", "m2/kg", "mg / g", "mg g-1", "mg N g-1", "mg/mg *100", 
#   "mm\xb2/mg", "mm2 / mg", "mm2 mg-1", "mm2*mg-1", "mm2/mg", "SLA cm2/g")

mydata$trait_value[mydata$unit == "date"] <- 6
mydata$unit[mydata$unit == "date"] <- "month"
mydata$unit[mydata$unit == "dimensionless"] <- ""
#mydata$trait_value[mydata$unit == "Julian Day"] <- "5.819178"
sub <- c("yr", "years")
mydata$unit <- gsub(paste(sub, collapse="|"), "year", mydata$unit) 

## look at what these values for "plant vegatative height" came in as originally
#mydata$OriglName %>% factor(.) %>% table(.) %>% .[order(.)]

#dput(levels(mydata$OriglName))

## dropping obviously silly data sources containing miniumum plant height
mydata <- mydata[mydata$OriglName %nin% c("Height (seedling)", "HEIGHT min",  "Plant height [min]", "Stem length (Height)"),]  

#dput(levels(factor(mydata$Dataset)))

## drop databases which Ruth excluded on the basis of juvenile trees or experimental treament
mydata <- mydata %>% subset(., .$Dataset %nin% c("Growth and Herbivory of Juvenil Trees", "The Functional Ecology of Trees (FET) Database  - Jena", 
                                                 "Biomass allocation in beech and spruce seedlings",
                                                 "BIOTREE Trait Shade Experiment", "Canopy Traits for Temperate Tree Species Under High N-Deposition",
                                                 "Leaf Economic Traits Across Varying Environmental Conditions", 
                                                 "Plant Traits from Romania","Leaf Structure and Chemistry",
                                                 "ECOCRAFT","Leaf Physiology Database","The DIRECT Plant Trait Database", 
                                                 "French Weeds Trait Database"))

#mydata$ValueKindName %>% factor(.) %>% levels(.) %>% dput(.)

## select the higher measurements of height, dropping "Minimum", 
mydata <- mydata %>% subset(., .$ValueKindName %in% c("", "Best estimate", "Maximum", "Mean", "Median",
                                                      "Single", "Species mean", NA))

#mydata$species %>% unique(.) %>% length(.) ## 284 species


## make datasets for cleaning by trait type
# y <- mydata[mydata$trait_name == "plant flowering begin",]
# length(unique(y$species))
# levels(droplevels(y$trait_value))

for (i in names(mydata)){
  mydata[, i] <- as.character(mydata[,i])
}
mydata$species <- factor(mydata$species)

## clean the datasets

## seedbank ----------------
bank <- droplevels(mydata[mydata$trait_name %in% c("Seed (seedbank) longevity", "Seedbank type"),])

# bank$trait_name %>% factor(.) %>% table(.) %>% .[order(.)]
# dput(levels(factor(bank$trait_value)))

persistent <- c( ">1", "1-5 yrs", "100", "2", "44.44444444", 
  "5", "86.36363636", "long-term persistent", "mid-persistent/unk", 
  "persistent/unk", "present", "seedbank present, type uncertain", 
  "short-term persistent", "soil")
bank$trait_value[bank$trait_value %in% persistent] <- "persistent"
  
transient <- c("1", "0.5", "<=1", "0", "transient")
bank$trait_value[bank$trait_value %in% transient] <- "transient"

bank$trait_value[grep("transient", bank$trait_value)] <- "transient"

bank$StdValue <- bank$trait_value
bank$trait_name <- "seed_bank"
#plot(factor(bank$StdValue))
## check for duplicate categories
#x <- unique(bank[, which(names(bank) %in% c("species", "StdValue"))])
## clean 

## dry_Nmass -------------
dry_Nmass<- droplevels(mydata[mydata$trait_name %in% c("Leaf nitrogen (N) content per leaf dry mass",  "leaf nitrogen content per leaf dry mass"),])

# dry_Nmass$trait_name %>% factor(.) %>% table(.) %>% .[order(.)]
# dput(levels(factor(dry_Nmass$trait_value)))
dry_Nmass$StdValue[which(is.na(dry_Nmass$StdValue))] <- dry_Nmass$trait_value[which(is.na(dry_Nmass$StdValue))] 
dry_Nmass$trait_name <- "Nmass_drymass"
#hist(as.numeric(dry_Nmass$StdValue))
## clean

## SLA ------------
sla <- droplevels(mydata[mydata$trait_name %in% c("Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole excluded"
                                                  # "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole included",
                                                  # "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): undefined if petiole is in- or excluded",
                                                  # "leaf area per leaf dry mass"
                                                  ),])
## 

# sla$trait_name %>% factor(.) %>% table(.) %>% .[order(.)]
# dput(levels(factor(sla$trait_value)))
sla$StdValue[which(is.na(sla$StdValue))] <- sla$trait_value[which(is.na(sla$StdValue))] 
sla$StdValue <- as.numeric(sla$StdValue)
sla$trait_name <- "sla"

#par(mfrow = c(2,2))
# hist(sla$StdValue[sla$trait_name == "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole included"], breaks = 60)
# hist(sla$StdValue[sla$trait_name == "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole excluded"&
#                     sla$StdValue <600], breaks = 60)
# hist((sla$StdValue[sla$trait_name == "leaf area per leaf dry mass"]), breaks = 60)
# hist(sla$StdValue[sla$trait_name == "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): undefined if petiole is in- or excluded"], breaks = 60)

## not clean; has outliers...

## woody ------
woody <- droplevels(mydata[mydata$trait_name %in% c("Plant woodiness", "whole plant woodiness"),])

# woody$trait_name %>% factor(.) %>% table(.) %>% .[order(.)]
# dput(levels(factor(woody$trait_value)))
woody$trait_value <- tolower(woody$trait_value)
woody$trait_value[grep("h", woody$trait_value, ignore.case = TRUE)] <- "non-woody"
woody$trait_value[woody$trait_value == "w"] <- "woody"
woody$trait_value[woody$trait_value == "2" | woody$trait_value == "3"] <- "woody"
woody <- woody[woody$trait_value %in% c("non-woody", "woody"),]
woody$trait_name <- "woodiness"
woody$StdValue <- woody$trait_value
#plot(factor(woody$StdValue))
x <- unique(woody[, which(names(woody) %in% c("species", "StdValue"))])

## find species with multiple categories
f <- c()
for(i in levels(x$species)){
  f <- append(f, length(x$species[x$species == i]))
}
f <- as.data.frame(cbind(f, levels(x$species)))
f <- f[f$f != 1,]

y <- x[x$species %in% f$V2,]

## mannually correct mistaken duplicate catergories
woody$StdValue[woody$species == 'Salix repens'] <- "woody"
woody$StdValue[woody$species == 'Helleborus foetidus'] <- "non-woody"
woody$StdValue[woody$species == 'Silene mollissima'] <- "non-woody"
# clean

## seeds -----------
seeds <- droplevels(mydata[mydata$trait_name %in% c("Seed dry mass","seed mass"),])

# seeds$trait_name %>% factor(.) %>% table(.) %>% .[order(.)]
# dput(levels(factor(seeds$trait_value)))
seeds$StdValue[which(is.na(seeds$StdValue))] <- seeds$trait_value[which(is.na(seeds$StdValue))] 
seeds <- seeds[seeds$StdValue != "heavy",]
seeds$StdValue <- as.numeric(seeds$StdValue)
seeds$trait_name <- "seed_mass"
#hist(log(seeds$StdValue))
## clean

## height ---------------
height <- droplevels(mydata[mydata$trait_name %in% c("Plant height generative", "Plant height vegetative", "maximum whole plant height"),])

# height$trait_name %>% factor(.) %>% table(.) %>% .[order(.)]
# dput(levels(factor(height$StdValue)))
height$StdValue[which(is.na(height$StdValue))] <- height$trait_value[which(is.na(height$StdValue))] 
## removed "lower value dash" where a range of values was entered, leaving just the upper value
dash <- c("[[:digit:]]-","[[:digit:]][[:digit:]]-","[[:digit:]][[:digit:]][[:digit:]]-", "[[:digit:]] - ","[[:digit:]][[:digit:]] - ", "[[:digit:]] -")
height$StdValue <-gsub(paste(dash, collapse = "|"), "", height$StdValue)
## removed brackets and keep higher values within
brackets <- c("[[:digit:]] \\(", "\\(", "\\)")
height$StdValue <-gsub(paste(brackets, collapse = "|"), "", height$StdValue)
height$comment[grep("cm", height$StdValue)] <- "tofixcm"
height$comment <- factor(height$comment)
## remove text from the measurements
words <- c("to ","cm", " m", "\\*", "<", " ")
height$StdValue <-gsub(paste(words, collapse = "|"), "", height$StdValue)
height <- height[height$StdValue %nin% c("herb", 0),]
height$StdValue <-gsub(",", ".", height$StdValue)
height$StdValue <- as.numeric(height$StdValue)
height <- height[which(!is.na(height$StdValue)),]
height <- height[height$comment %nin% c("0, no seed produced; 1, seed too small to be measured; 2,<0.2 mg; 3, 0.2-0.5 mg; 4,  
                                        0.5-1.0 mg; 5,  1.0-2.0 mg; 6,  2.0-10.0 mg; 7,  >10 mg"),]

height$StdValue[height$comment %in% c("tofixcm")] <- height$StdValue[height$comment %in% c("tofixcm")]/100
height$trait_name <- "height"
#hist(log(height$StdValue))

## clean

## la ----------
la <- droplevels(mydata[mydata$trait_name %in% c("leaf area"),])

# la$trait_name %>% factor(.) %>% table(.) %>% .[order(.)]
# dput(levels(factor(la$trait_value)))
la$trait_value <- as.numeric(la$trait_value)
la <- la[la$comment != "leaf area x number of leaves",]
la$StdValue <- la$trait_value
la <- la[la$StdValue != 0,]
la$trait_name <- "leaf_area"
#hist(log(la$StdValue))

## clean

## Nmass -------------------
Nmass <- droplevels(mydata[mydata$trait_name %in% c("leaf nitrogen content per leaf area"),])

# Nmass$trait_name %>% factor(.) %>% table(.) %>% .[order(.)]
# dput(levels(factor(Nmass$trait_value)))
Nmass$StdValue <- Nmass$trait_value
Nmass$StdValue <- as.numeric(Nmass$StdValue)
#hist(Nmass$StdValue)
## clean


## clonal --------
## shelving for now, as the information is vey mixed and it's not a key target trait in this proejct anyway
# clonal <- droplevels(mydata[mydata$trait_name %in% c("Plant propagation type", "Plant reproductive phenology timing", "Plant resprouting capacity", 
#                                                      "Plant vegetative regeneration capacity", "Plant vegetative reproduction: role of clonal growth organ in plant growth", 
#                                                      "Species reproduction type",  "Plant clonal growth form"),])
# 
# clonal$trait_name %>% factor(.) %>% table(.) %>% .[order(.)]
# dput(levels(factor(clonal$trait_value)))
# dput(levels(factor(clonal$trait_name)))


## dispersal ------------------
dispersal <- droplevels(mydata[mydata$trait_name %in% c("Dispersal syndrome", "Dispersal unit appendages", 
                                                       # "Dispersal unit type", "Fruit type", 
                                                       "Seed morphology type"),])
# dispersal$trait_name %>% factor(.) %>% table(.) %>% .[order(.)]
# dput(levels(factor(dispersal$trait_value)))
dispersal$trait_value <- tolower(dispersal$trait_value)
dispersal <- dispersal[dispersal$trait_value %nin% c("generative dispersule", "germinule", "disp",
                                                     "multi-seeded generative dispersule", "one-seeded generative dispersule"),]

dispersal$trait_value[dispersal$trait_name == "Dispersal unit appendages" & dispersal$trait_value == "wings"] <- "wind"
dispersal$trait_value[dispersal$trait_name == "Dispersal unit appendages" & dispersal$trait_value == "elaiosome"] <- "animal"
dispersal$trait_value[dispersal$trait_name == "Seed morphology type" & dispersal$trait_value == "elaiosome"] <- "animal"

#levels(factor(dispersal$trait_value[dispersal$trait_name == "Dispersal syndrome"]))
dispersal$trait_value[grep("anemochory", dispersal$trait_value)] <- "wind"
animals <- c("animal", "badger", "mammal", "cattle", "deer", "sheep", "roe", "squirrels", "hare","mouse", "Zoochor", "hoarding", "pine marten", "bird",
             "ant",  "dysochor", "chamois")
dispersal$trait_value[grep(paste(animals, collapse = "|"), dispersal$trait_value, ignore.case = T)] <- "animal"
water <- c("water", "nautochor", "bythisochor")
dispersal$trait_value[grep(paste(water, collapse = "|"), dispersal$trait_value, ignore.case = T)] <- "water"
wind <- c("meteorochor", "wind")
dispersal$trait_value[grep(paste(wind, collapse = "|"), dispersal$trait_value, ignore.case = T)] <- "wind"

self <- c("autochor", "ombrochor", "ballochor", "baro", "blastochor", "boleochor", "chamaechor", "unspecialised" ,"unassisted")
dispersal$trait_value[grep(paste(self, collapse = "|"), dispersal$trait_value, ignore.case = T)] <- "self"
anthro <- c("hemerochor",  "car", "clothes","speirochor" , "agochor", "man", "commerce", "ethelochor") 
dispersal$trait_value[grep(paste(anthro, collapse = "|"), dispersal$trait_value, ignore.case = T)] <- "anthro"
other <- c( "other", "unknown" )
dispersal$trait_value[grep(paste(other, collapse = "|"), dispersal$trait_value, ignore.case = T)] <- "unknown"
dispersal$trait_value[dispersal$trait_value == "1"] <- "unknown"
dispersal$trait_value[dispersal$trait_value == "2" | dispersal$trait_value == "3"] <- "animal"

dispersal <- dispersal[dispersal$trait_value %in% c("animal", "anthro", "self", "water", "wind"),]
#levels(factor(dispersal$trait_value))
dispersal$StdValue <- dispersal$trait_value
dispersal$trait_name <- "dispersal"
#plot(factor(dispersal$StdValue))
## clean. there are multiple categories per species though...


## pollentation ---------------
pollenation <- droplevels(mydata[mydata$trait_name %in% c("Flower insemination autogamous or xenogamous", 
                                                          "Plant mating system",
                                                         # "Flower pollinator and type of reward", 
                                                          "Pollination syndrome",
                                                          "Flower sexual syndrome (dichogamy, cleistogamy, dioecious, monoecious)"),])


# selfing <- c("Flower insemination autogamous or xenogamous", 
#              
# "Plant mating system",
# "Flower pollinator and type of reward", 
# "Pollination syndrome",
# "Flower sexual syndrome (dichogamy, cleistogamy, dioecious, monoecious)", 

# pollenation$trait_name %>% factor(.) %>% table(.) %>% .[order(.)]
# pollenation$trait_value %>% factor(.) %>% table(.) %>% .[order(.)]
#dput(levels(factor(pollenation$trait_value)))

## don't think I'm going to use pollination to creating a selfing dataset, as I'd just be selecting for rows
## that mention selfing, failing to capture many outcrossing species with any certainty.



check<- droplevels(unique(rbind(height, la, seeds, sla, woody)))

check <-check[,which(names(check) %in% c("species", "trait_name", "StdValue", "comment"))]
#check$comment %>% factor(.) %>% table(.) %>% .[order(.)]
## comments all fine, removal of final problem records inorporated into script above
check <-check[,which(names(check) %in% c("species", "trait_name", "StdValue"))]

## life form ---------------
lifeform <- readRDS("~/OneDrive/PhD/PREDICTS/PREDICTS_Data/Data_lifeform.rds")
lifeform$AccSpeciesName <- as.character(lifeform$AccSpeciesName)
lifeform <- as.data.frame(cbind(lifeform$AccSpeciesName, c(1:length(lifeform$raunk_lf)), lifeform$raunk_lf))
names(lifeform) <- c("species", "trait_name", "StdValue")
lifeform$trait_name <- as.character(lifeform$trait_name)
lifeform$trait_name <- "lifeform"

lifeform$StdValue <- factor(lifeform$StdValue, levels = c("phanerophyte", "chamaephyte", "hemicryptophyte", "cryptophyte", "therophyte"))

lifeform <- lifeform[lifeform$species %in% sp.list_full_TRY_BIEN,]

check <- rbind(check, lifeform)
mydata <- droplevels(unique(check))
#length(unique(mydata$species)) ## 278 species
mydata$trait_name <- factor(mydata$trait_name)

list <- c()
for (i in names(mydata)){
  list[i] <-length(which(is.na(mydata[,i])))
}
print(list)

mydata <- drop_na(mydata)

## make one big traits df to tidy

## restructure the dataframe for your own nefarious purposes
## create columns for each trait
for (i in levels(mydata$trait_name)){
  mydata[,i] <- as.character(NA)
}

## populate the columns with values for that trait
for (i in levels(mydata$trait_name)){
 # print(i)
  mydata[,i][mydata$trait_name == i] <- mydata$StdValue[mydata$trait_name == i]
}

## create list of dataframes of species values for single traits
x <- list()
for (i in levels(mydata$trait_name)){
  x[[i]] <- droplevels(unique(mydata[,which(names(mydata) %in% c('species', i))][which(!is.na(mydata[,i])),],
                              by = "species"))
}

rm(animals, anthro, brackets, dash, other, persistent, self, sub, transient, water, wind, words, y)
rm(bank, dispersal, dry_Nmass, f, height, la, lifeform, Nmass, pollenation, seeds, sla, woody)

## the end 



