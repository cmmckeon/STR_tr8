#data collection script

install.packages("TR8", dependencies = TRUE)
install.packages("stringr")
install.packages("reshape")
# library loads and attaches add-on packages
library(stringr)
#rearrange trait dataframes
library(reshape)
library(TR8)
setwd("C:/Users/Caroline/Dropbox/Thesis/") 
#anna's final dataset
afe<-read.csv(file.choose())
attach(afe)

#obtaining shortcodes for traits of interest
#traits <-available_traits()
#LIST1 components only work with GUI window, not with download_list
LIST1 <-c("li_form", "reprod_meth", "poll_vect", "propag", "bud_bank_seasonality_soil")
#LIST2 works fine as a download_list
LIST2 <-c("buoyancy", "canopy_height", "dispersal", "leaf_size", "dispersal_morphology", "growth_form", "releasing_height", "sbank", "seed_mass", "seed_number_per_shoot", "woodiness", "velocity", "li_form_B", "reprod_B", "strategy", "poll_vect_B", "inflorescence_fr", "sex_reprod_fr", "poll_vect_fr", "fruit_type_fr", "dissemination_fr", "li_form_fr", "sla_cal", "wood_dens", "DispMode", "FruitType", "LeafSize", "Propagule", "SeedBank", "SeedBankLong", "Growth.Form", "Growth.Rate", "Height..Mature", "Lifespan", "Fruit.Seed.Abundance", "Fruit.Seed.Persistence", "Propagated.by.Bare.Root", "Propagated.by.Bulb", "Propagated.by.Container", "Propagated.by.Corm", "Propagated.by.Cuttings", "Propagated.by.Seed", "Propagated.by.Sod", "Propagated.by.Sprigs", "Propagated.by.Tubers", "Seed.Spread.Rate", "Seedling.Vigor")
#can't use LIST unfortunately as you get "subscript out of bounds" error code. But hopefully will be able to fix this somehow
LIST <-c(LIST1, LIST2)

#*************************

#creating a species list to use with tr8 function
splist<- as.vector(unique(Species.x))
#small subsets of species list
spl1 <-splist[1:100]
#making species names compatable with names in queried databases
spl1NODASH <-gsub("_", " ", spl1)
#obtaining traits for short species list
OLIST2 <- tr8(spl1NODASH, LIST2, gui_config = FALSE, synonyms=TRUE)
GUILIST1 <- tr8(spl1NODASH, gui_config = TRUE, synonyms=TRUE)
#converting obtained trait lists into dataframes
LIST2DF <-extract_traits(OLIST2)
GUIDF <-extract_traits(GUILIST1)
#merging GUI and download_list data.frames
DF1 <-merge(LIST2DF, GUIDF, by.x=c("original_names", "synonyms"), by.y=c("original_names", "synonyms"), all.x=TRUE)
#making queried species names compatablw with Anna's original data set
DF1$original_names <-gsub(" ", "_", DF1$original_names)
str(DF1)

#making subset of original data set to merge new trait data.frame with
afe100 <-afe[1:595,]
#merge all traits for first 100 species with Anna's dataset
DF1merged <-merge(afe100, DF1, by.x="Species.x", by.y="original_names",all.x=TRUE)

#*****************************

#second 100 species
spl2 <-splist[100:110]
spl2NODASH <-gsub("_", " ", spl2)
Short2 <- tr8(spl2NODASH, LIST2, gui_config = FALSE, synonyms=TRUE)
GUI2 <- tr8(spl2NODASH, gui_config = TRUE, synonyms=TRUE)
L2DF <-extract_traits(Short2)
GUI2DF <-extract_traits(GUI2)
afe200 <-afe[496:550,]
DF2 <-merge(L2DF, GUI2DF, by.x=c("original_names", "synonyms"), by.y=c("original_names", "synonyms"), all.x=TRUE)
DF2$original_names <-gsub(" ", "_", DF2$original_names)
str(DF2)
DF110merged <-merge(afe200, DF2, by.x="Species.x", by.y="original_names",all.x=TRUE)

write.csv(DF110merged, "DF110merged.csv", row.names = FALSE)

#*************************

#repeating opperation for more subsets of speceis list
spl3 <-splist[201:300]
spl3NODASH <-gsub("_", " ", spl3)
Short3 <- tr8(spl3NODASH, LIST2, gui_config = FALSE, synonyms=TRUE)
GUI3 <- tr8(spl3NODASH, gui_config = TRUE, synonyms=TRUE)
L3DF <-extract_traits(Short3)
GUI3DF <-extract_traits(GUI3)
afe300 <-afe[1001:1500,]
DF3 <-merge(L3DF, GUI3DF, by.x=c("original_names", "synonyms"), by.y=c("original_names", "synonyms"), all.x=TRUE)
DF3$original_names <-gsub(" ", "_", DF3$original_names)
DF3merged <-merge(afe300, DF3, by.x="Species.x", by.y="original_names",all.x=TRUE)

write.csv(DF3merged, "DF3merged.csv", row.names = FALSE)

#***************************
spl4 <-splist[420:450]
spl4NODASH <-gsub("_", " ", spl4)
Short4 <- tr8(spl4NODASH, LIST2, gui_config = FALSE, synonyms=TRUE)
GUI4 <- tr8(spl4NODASH, gui_config = TRUE, synonyms=TRUE)
L4DF <-extract_traits(Short4)
GUI4DF <-extract_traits(GUI4)
DF4 <-merge(L4DF, GUI4DF, by.x=c("original_names", "synonyms"), by.y=c("original_names", "synonyms"), all.x=TRUE)
DF4$original_names <-gsub(" ", "_", DF4$original_names)
afe450 <-afe[2096:2250,]
DF450merged <-merge(afe450, DF4, by.x="Species.x", by.y="original_names",all.x=TRUE)

write.csv(DF450merged, "DF450merged.csv", row.names = FALSE)


#************************************

afe500 <-afe[2246:2500,]

#reading in previously obtained data.frames of species traits
Sp500 <-read.csv(file.choose())
Sp500GUI <-read.csv(file.choose())
DF500 <-merge(Sp500, Sp500GUI, by.x=c("original_names", "synonyms"), by.y=c("original_names", "synonyms"), all.x=TRUE)
DF500$original_names <-gsub(" ", "_", DF500$original_names)
DF500merged <-merge(afe500, DF500, by.x="Species.x", by.y="original_names",all.x=TRUE)


write.csv(DF500merged, "DF500merged.csv")

#*************************************

afe600 <-afe[2496:2996,]
Sp600 <-read.csv(file.choose())
Sp600GUI <-read.csv(file.choose())
DF6 <-merge(Sp600, Sp600GUI, by.x=c("original_names", "synonyms"), by.y=c("original_names", "synonyms"), all.x=TRUE)
DF6$original_names <-gsub(" ", "_", DF6$original_names)
DF6merged <-merge(afe600, DF6, by.x="Species.x", by.y="original_names",all.x=TRUE)

write.csv(DF6merged, "DF6merged.csv")

#*****************************

afe700 <-afe[2997:3500,]
Sp700 <-read.csv(file.choose())
Sp700GUI <-read.csv(file.choose())
DF7 <-merge(Sp700, Sp700GUI, by.x=c("original_names", "synonyms"), by.y=c("original_names", "synonyms"), all.x=TRUE)
DF7$original_names <-gsub(" ", "_", DF7$original_names)
DF7merged <-merge(afe700, DF7, by.x="Species.x", by.y="original_names",all.x=TRUE)


write.csv(DF7merged, "DF7merged.csv")

#*******************************8

afe800 <-afe[3501:4000,]
Sp800 <-read.csv(file.choose())
Sp800GUI <-read.csv(file.choose())
DF8 <-merge(Sp800, Sp800GUI, by.x=c("original_names", "synonyms"), by.y=c("original_names", "synonyms"), all.x=TRUE)
DF8$original_names <-gsub(" ", "_", DF8$original_names)
DF8merged <-merge(afe800, DF8, by.x="Species.x", by.y="original_names",all.x=TRUE)


write.csv(DF8merged, "DF8merged.csv")

#****************************

afe852 <-afe[4001:4260,]
Sp852 <-read.csv(file.choose())
Sp852GUI <-read.csv(file.choose())
DF852 <-merge(Sp852, Sp852GUI, by.x=c("original_names", "synonyms"), by.y=c("original_names", "synonyms"), all.x=TRUE)
DF852$original_names <-gsub(" ", "_", DF852$original_names)
DF852merged <-merge(afe852, DF852, by.x="Species.x", by.y="original_names",all.x=TRUE)


write.csv(DF852merged, "DF852merged.csv")

#************************

#**********************

#full species list to be retrieved in the case this is possible
fullListNODASH <-gsub("_", " ", splist)
fulltraitobject <- tr8(fullListNODASH, download_list = NULL, gui_config = TRUE, synonyms=TRUE)
newDF <-extract_traits(fulltraitobject)
newDF$original_names <-gsub(" ", "_", newDF$original_names)


completeDF <-merge(afe, newDF, by.x="Species.x", by.y="original_names",all.x=TRUE)

#***************************

#retriving data.frames when starting new R session
DF1merged <-read.csv(file.choose())
DF110merged <-read.csv(file.choose())
DF2merged <-read.csv(file.choose())
DF3merged <-read.csv(file.choose())
DF395merged <-read.csv(file.choose())
DF450merged <-read.csv(file.choose())
DF500merged <-read.csv(file.choose())
DF6merged <-read.csv(file.choose())
DF7merged <-read.csv(file.choose())
DF8merged <-read.csv(file.choose())
DF852merged <-read.csv(file.choose())

#*************************

#finding which columns need to be removed
names(DF852merged[107])
#Looking for errors at far end of columns
View(DF852merged[,50:110])

#removing extra lable columns so all data.frames have same number of variables and same columns
#DF852merged <- DF852merged[,-(107), drop=TRUE]
#DF2merged <- DF2merged[,-(107), drop=TRUE]
#DF2merged <- DF2merged[,-(112), drop=TRUE]
 
#trying to stack all the data.frames sequentialy to produce complete data.frame of all species
finalDF <-rbind(DF1merged, DF110merged, DF2merged, DF3merged, DF395merged, DF450merged, DF500merged, DF6merged, DF7merged, DF8merged, DF852merged, deparse.level = 0, make.row.names = TRUE)


write.csv(finalDF, "finalDF.csv", row.names = FALSE)
