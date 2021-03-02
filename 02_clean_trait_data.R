## cleaning TRY and BIEN trait data

## having looked at Dr Ruth Kelly's detailed cleaning notes, I will take a rule based exclusion approach to data from plants that are:
## juvenile, manipulated or unhealthy.
## also seeking to address the units discrepencies

## name the trait dataset "mydata"
mydata <- droplevels(traits)

mydata <- mydata[which(!is.na(mydata$trait_value)),]
mydata <- mydata[mydata$trait_value %nin% c("na", "NA", "Na"),]
mydata <- mydata[mydata$trait_value != "",]

## group trait categories
dput(levels(mydata$trait_name))

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
#   "Species strategy type according to Grime", 
#   "Species generation overlap",   "Mycorrhiza type", "leaf dry mass",   
#    "Plant ontogeny: age of maturity (first flowering)", "plant flowering begin",)


## look at levels
dput(levels(factor(mydata$unit)))

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

dput(levels(mydata$OriglName))

## dropping obviously silly data sources containing miniumum plant height
mydata <- mydata[mydata$OriglName %nin% c("Height (seedling)", "HEIGHT min",  "Plant height [min]", "Stem length (Height)"),]  

dput(levels(factor(mydata$Dataset)))

## drop databases which Ruth excluded on the basis of juvenile trees or experimental treament
mydata <- mydata %>% subset(., .$Dataset %nin% c("Growth and Herbivory of Juvenil Trees", "The Functional Ecology of Trees (FET) Database  - Jena", 
                                                 "Biomass allocation in beech and spruce seedlings",
                                                 "BIOTREE Trait Shade Experiment", "Canopy Traits for Temperate Tree Species Under High N-Deposition",
                                                 "Leaf Economic Traits Across Varying Environmental Conditions", 
                                                 "Plant Traits from Romania","Leaf Structure and Chemistry",
                                                 "ECOCRAFT","Leaf Physiology Database","The DIRECT Plant Trait Database", 
                                                 "French Weeds Trait Database"))

mydata$ValueKindName %>% factor(.) %>% levels(.) %>% dput(.)

## select the higher measurements of height, dropping "Minimum", 
mydata <- mydata %>% subset(., .$ValueKindName %in% c("", "Best estimate", "Maximum", "Mean", "Median",
                                                      "Single", "Species mean", NA))

mydata$species %>% unique(.) %>% length(.) ## 284 species


## make datasets for cleaning by trait type
# y <- mydata[mydata$trait_name == "plant flowering begin",]
# length(unique(y$species))
# levels(droplevels(y$trait_value))

for (i in names(mydata)){
  mydata[, i] <- as.character(mydata[,i])
}

seeds <- droplevels(mydata[mydata$trait_name %in% c("Seed dry mass","seed mass", "Dispersal unit dry mass"),])

woody <- droplevels(mydata[mydata$trait_name %in% c("Plant woodiness", "whole plant woodiness"),])

bank <- droplevels(mydata[mydata$trait_name %in% c("Seed (seedbank) longevity", "Seedbank type"),])

dispersal <- droplevels(mydata[mydata$trait_name %in% c("Dispersal syndrome", "Dispersal unit appendages", 
                                                        "Dispersal unit type", "Fruit type", "Seed morphology type"),])

height <- droplevels(mydata[mydata$trait_name %in% c("Plant height generative", "Plant height vegetative", "maximum whole plant height"),])

pollenation <- droplevels(mydata[mydata$trait_name %in% c("Flower insemination autogamous or xenogamous", 
                                                          "Flower pollinator and type of reward", "Flower sexual syndrome (dichogamy, cleistogamy, dioecious, monoecious)", 
                                                          "Pollination syndrome", "Plant mating system"),])

sla <- droplevels(mydata[mydata$trait_name %in% c("Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole excluded", 
                                                  "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole included", 
                                                  "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): undefined if petiole is in- or excluded",  
                                                  "leaf area per leaf dry mass"),])

dry_Nmass<- droplevels(mydata[mydata$trait_name %in% c("Leaf nitrogen (N) content per leaf dry mass",  "leaf nitrogen content per leaf dry mass"),])

la <- droplevels(mydata[mydata$trait_name %in% c("leaf area"),])

Nmass <- droplevels(mydata[mydata$trait_name %in% c("leaf nitrogen content per leaf area"),])

clonal <- droplevels(mydata[mydata$trait_name %in% c("Plant propagation type", "Plant reproductive phenology timing", "Plant resprouting capacity", 
                                                     "Plant vegetative regeneration capacity", "Plant vegetative reproduction: role of clonal growth organ in plant growth", 
                                                     "Species reproduction type", "whole plant vegetative phenology", "Plant clonal growth form"),])


## clean the datasets
bank$trait_name %>% factor(.) %>% table(.) %>% .[order(.)]
dput(levels(factor(bank$trait_value)))

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
plot(factor(bank$StdValue))
## clean

dry_Nmass$trait_name %>% factor(.) %>% table(.) %>% .[order(.)]
dput(levels(factor(dry_Nmass$trait_value)))
dry_Nmass$StdValue[which(is.na(dry_Nmass$StdValue))] <- dry_Nmass$trait_value[which(is.na(dry_Nmass$StdValue))] 
dry_Nmass$trait_name <- "Nmass_drymass"
hist(as.numeric(dry_Nmass$StdValue))
## clean

sla$trait_name %>% factor(.) %>% table(.) %>% .[order(.)]
dput(levels(factor(sla$trait_value)))

clonal$trait_name %>% factor(.) %>% table(.) %>% .[order(.)]
dput(levels(factor(clonal$trait_value)))

dispersal$trait_name %>% factor(.) %>% table(.) %>% .[order(.)]
dput(levels(factor(dispersal$trait_value)))

pollenation$trait_name %>% factor(.) %>% table(.) %>% .[order(.)]
dput(levels(factor(pollenation$trait_value)))





bank$trait_value <-gsub(paste(transient, collapse = "|"), "transient", ignore.case = TRUE, bank$trait_value)
bank$trait_value[grep(paste(persistent, collapse="|"), bank$trait_value)] <- "persistent"



####################################################################
## Start of useful but unecessicary script where I clean "plant height" 
## (previously named "trait_value"), even though TRY have already done it in StdValue...

#levels(mydata$UnitName)
## try hase pre-convert cm measurements to m

#mydata$trait_value %>% factor(.) %>% table(.) %>% .[order(.)]

## remove rows where the measurement value is nonsense
mydata <- mydata[mydata$trait_value %nin% c("", ".", "]", "unknown"),]

## remove text from the measurements
words <- c("to ", " m", "\\*", "<")
mydata$trait_value <-gsub(paste(words, collapse = "|"), "", mydata$trait_value)


length(which(is.na(mydata$trait_value)))
## this changes when data is plant_height (character) is converted to numeric, so there must still be some non-numeric characters in there

## removed "lower value dash" where a range of values was entered, leaving just the upper value
dash <- c("[[:digit:]]-", "[[:digit:]] - ", "[[:digit:]] -")
mydata$trait_value <-gsub(paste(dash, collapse = "|"), "", mydata$trait_value)

## removed brackets and keep higher values within
brackets <- c("[[:digit:]] \\(", "\\(", "\\)")
mydata$trait_value <-gsub(paste(brackets, collapse = "|"), "", mydata$trait_value)

## remove characters and convert out of ft to m (in theory, it's so messy I amn't sure I trust these values)
check <- mydata[grep("ft", mydata$trait_value),]
check$trait_value <-gsub(" ft", "", check$trait_value)
check$trait_value <- as.numeric(check$trait_value)*0.3048
mydata$trait_value[grep("ft", mydata$trait_value)] <- check$trait_value

## do the same for "cm" data NOTE : also dropping lower set of digits befor a space
check <- mydata[grep("cm", mydata$trait_value),]
cm <- c(" cm", "cm")
check$trait_value <-gsub(paste(cm, collapse = "|"), "", check$trait_value)
## deal with entries that have a range of values and no dash
sub_check <- check[grep("[[:digit:]] [[:digit:]]", check$trait_value),]
sub_check$trait_value <-gsub("[[:digit:]] ", "", sub_check$trait_value)
check$trait_value[grep("[[:digit:]] [[:digit:]]", check$trait_value)] <- sub_check$trait_value

## proceed with cm data again
check$trait_value <- as.numeric(check$trait_value)/100
mydata$trait_value[grep("cm", mydata$trait_value)] <- check$trait_value

rm(words, dash, brackets, cm)

## look at the values we're left with
summary(as.numeric(mydata$trait_value))

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
#    0.00    0.80   10.00   52.12   30.00 6500.00     592


####################################################################
## End of useful but unecessicary script where I clean "plant height" 
## (previously named "trait_value"), even though TRY have already done it in StdValue...

mydata <- droplevels(unique(mydata))


hist((mydata$plant_height), main = "Histogram of plant heights", breaks = 1000)

summary(as.numeric(mydata$plant_height))

try_height <- mydata

## the end 
