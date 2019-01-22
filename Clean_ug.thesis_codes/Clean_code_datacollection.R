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
traits <-available_traits()
#LIST1 traits can only be obtained using GUI window, not with download_list
LIST1 <-c("li_form", "reprod_meth", "poll_vect", "propag", "bud_bank_seasonality_soil")
#LIST2 works fine as a download_list
LIST2 <-c("buoyancy", "canopy_height", "dispersal", "leaf_size", "dispersal_morphology", 
          "growth_form", "releasing_height", "sbank", "seed_mass", "seed_number_per_shoot", 
          "woodiness", "velocity", "li_form_B", "reprod_B", "strategy", "poll_vect_B", "inflorescence_fr", 
          "sex_reprod_fr", "poll_vect_fr", "fruit_type_fr", "dissemination_fr", "li_form_fr", "sla_cal", 
          "wood_dens", "DispMode", "FruitType", "LeafSize", "Propagule", "SeedBank", "SeedBankLong", 
          "Growth.Form", "Growth.Rate", "Height..Mature", "Lifespan", "Fruit.Seed.Abundance", 
          "Fruit.Seed.Persistence", "Propagated.by.Bare.Root", "Propagated.by.Bulb", "Propagated.by.Container", 
          "Propagated.by.Corm", "Propagated.by.Cuttings", "Propagated.by.Seed", "Propagated.by.Sod", "Propagated.by.Sprigs", 
          "Propagated.by.Tubers", "Seed.Spread.Rate", "Seedling.Vigor")


#*************************
#this section didn't work for me (gave "internal serve error")
#it be much easier than what I did, so maybe try it just incase

#creating a species list to search tr8 function
splist<- as.vector(unique(Species.x))
#full species list to be retrieved in the case this is possible
fullListNODASH <-gsub("_", " ", splist)
fulltraitobject <- tr8(fullListNODASH, download_list = NULL, gui_config = TRUE, synonyms=TRUE)
newDF <-extract_traits(fulltraitobject)
newDF$original_names <-gsub(" ", "_", newDF$original_names)

completeDF <-merge(afe, newDF, by.x="Species.x", by.y="original_names",all.x=TRUE)

#*************************
#What follows from here is the method that worked for me, but it is time consuming; 
#the dataframes are in the dropbox if you don't want to run the whole thing

#creating a species list to search tr8 function
splist<- as.vector(unique(Species.x))
#small subsets of species list
spl1 <-splist[1:100]
#making species names compatable with names in queried databases
spl1NODASH <-gsub("_", " ", spl1)
#obtaining traits for subset of species list
OLIST2 <- tr8(spl1NODASH, LIST2, gui_config = FALSE, synonyms=TRUE)
GUILIST1 <- tr8(spl1NODASH, gui_config = TRUE, synonyms=TRUE)
#converting obtained trait lists into dataframes
LIST2DF <-extract_traits(OLIST2)
GUIDF <-extract_traits(GUILIST1)
#merging GUI and download_list data.frames
DF1 <-merge(LIST2DF, GUIDF, by.x=c("original_names", "synonyms"), by.y=c("original_names", "synonyms"), all.x=TRUE)
#making queried species names compatable with Anna's original data set
DF1$original_names <-gsub(" ", "_", DF1$original_names)
str(DF1)

#making subset of original data set to merge new trait data.frame with
afe100 <-afe[1:500,]
#merge all traits for first 100 species with Anna's dataset
DF100merged <-merge(afe100, DF1, by.x="Species.x", by.y="original_names",all.x=TRUE)

#*****************************

#second small subset of species
spl2 <-splist[101:110]
spl2NODASH <-gsub("_", " ", spl2)
Short2 <- tr8(spl2NODASH, LIST2, gui_config = FALSE, synonyms=TRUE)
GUI2 <- tr8(spl2NODASH, gui_config = TRUE, synonyms=TRUE)
L2DF <-extract_traits(Short2)
GUI2DF <-extract_traits(GUI2)
afe200 <-afe[501:550,]
DF2 <-merge(L2DF, GUI2DF, by.x=c("original_names", "synonyms"), by.y=c("original_names", "synonyms"), all.x=TRUE)
DF2$original_names <-gsub(" ", "_", DF2$original_names)
DF110merged <-merge(afe200, DF2, by.x="Species.x", by.y="original_names",all.x=TRUE)

write.csv(DF110merged, "DF110merged.csv", row.names = FALSE)

#*************************

spl2 <-splist[121:200]
spl2NODASH <-gsub("_", " ", spl2)
Short2 <- tr8(spl2NODASH, LIST2, gui_config = FALSE, synonyms=TRUE)
GUI2 <- tr8(spl2NODASH, gui_config = TRUE, synonyms=TRUE)
L2DF <-extract_traits(Short2)
GUI2DF <-extract_traits(GUI2)
afe200 <-afe[600:1000,]
DF2 <-merge(L2DF, GUI2DF, by.x=c("original_names", "synonyms"), by.y=c("original_names", "synonyms"), all.x=TRUE)
DF2$original_names <-gsub(" ", "_", DF2$original_names)
DF200merged <-merge(afe200, DF2, by.x="Species.x", by.y="original_names",all.x=TRUE)

write.csv(DF200merged, "DF200merged.csv", row.names = FALSE)

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
DF300merged <-merge(afe300, DF3, by.x="Species.x", by.y="original_names",all.x=TRUE)

write.csv(DF300merged, "DF300merged.csv", row.names = FALSE)

#***************************
spl4 <-splist[301:395]
spl4NODASH <-gsub("_", " ", spl4)
Short4 <- tr8(spl4NODASH, LIST2, gui_config = FALSE, synonyms=TRUE)
GUI4 <- tr8(spl4NODASH, gui_config = TRUE, synonyms=TRUE)
L4DF <-extract_traits(Short4)
GUI4DF <-extract_traits(GUI4)
DF4 <-merge(L4DF, GUI4DF, by.x=c("original_names", "synonyms"), by.y=c("original_names", "synonyms"), all.x=TRUE)
DF4$original_names <-gsub(" ", "_", DF4$original_names)
afe395 <-afe[1501:1975,]
DF395merged <-merge(afe395, DF4, by.x="Species.x", by.y="original_names",all.x=TRUE)

write.csv(DF395merged, "DF395merged.csv", row.names = FALSE)


#************************************
spl5 <-splist[420:500]
spl5NODASH <-gsub("_", " ", spl5)
Short5 <- tr8(spl5NODASH, LIST2, gui_config = FALSE, synonyms=TRUE)
GUI5 <- tr8(spl5NODASH, gui_config = TRUE, synonyms=TRUE)
L5DF <-extract_traits(Short5)
GUI5DF <-extract_traits(GUI5)
DF5 <-merge(L5DF, GUI5DF, by.x=c("original_names", "synonyms"), by.y=c("original_names", "synonyms"), all.x=TRUE)
DF5$original_names <-gsub(" ", "_", DF5$original_names)
afe500 <-afe[2096:2500,]
DF500merged <-merge(afe500, DF5, by.x="Species.x", by.y="original_names",all.x=TRUE)

write.csv(DF500merged, "DF500merged.csv", row.names = FALSE)

#*************************************
spl6 <-splist[501:600]
spl6NODASH <-gsub("_", " ", spl6)
Short6 <- tr8(spl6NODASH, LIST2, gui_config = FALSE, synonyms=TRUE)
GUI6 <- tr8(spl6NODASH, gui_config = TRUE, synonyms=TRUE)
L6DF <-extract_traits(Short6)
GUI6DF <-extract_traits(GUI6)
DF6 <-merge(L6DF, GUI6DF, by.x=c("original_names", "synonyms"), by.y=c("original_names", "synonyms"), all.x=TRUE)
DF6$original_names <-gsub(" ", "_", DF6$original_names)
afe600 <-afe[2501:3000,]
DF600merged <-merge(afe600, DF6, by.x="Species.x", by.y="original_names",all.x=TRUE)

write.csv(D600merged, "DF600merged.csv", row.names = FALSE)

#*************************************
spl7 <-splist[601:700]
spl6NODASH <-gsub("_", " ", spl6)
Short7 <- tr8(spl7NODASH, LIST2, gui_config = FALSE, synonyms=TRUE)
GUI7 <- tr8(spl7NODASH, gui_config = TRUE, synonyms=TRUE)
L7DF <-extract_traits(Short7)
GUI7DF <-extract_traits(GUI7)
DF7 <-merge(L7DF, GUI7DF, by.x=c("original_names", "synonyms"), by.y=c("original_names", "synonyms"), all.x=TRUE)
DF7$original_names <-gsub(" ", "_", DF7$original_names)
afe700 <-afe[3001:3500,]
DF700merged <-merge(afe700, DF7, by.x="Species.x", by.y="original_names",all.x=TRUE)

write.csv(D700merged, "DF700merged.csv", row.names = FALSE)

#*************************************
spl8 <-splist[701:852]
spl8NODASH <-gsub("_", " ", spl8)
Short8 <- tr8(spl8NODASH, LIST2, gui_config = FALSE, synonyms=TRUE)
GUI8 <- tr8(spl8NODASH, gui_config = TRUE, synonyms=TRUE)
L8DF <-extract_traits(Short8)
GUI8DF <-extract_traits(GUI8)
DF8 <-merge(L8DF, GUI8DF, by.x=c("original_names", "synonyms"), by.y=c("original_names", "synonyms"), all.x=TRUE)
DF8$original_names <-gsub(" ", "_", DF8$original_names)
afe852 <-afe[3501:4260,]
DF852merged <-merge(afe852, DF8, by.x="Species.x", by.y="original_names",all.x=TRUE)

write.csv(D852merged, "DF852merged.csv", row.names = FALSE)

#*************************************

#***************************

#retriving data.frames when starting new R session
DF100merged <-read.csv(file.choose())
DF110merged <-read.csv(file.choose())
DF200merged <-read.csv(file.choose())
DF300merged <-read.csv(file.choose())
DF395merged <-read.csv(file.choose())
DF500merged <-read.csv(file.choose())
DF600merged <-read.csv(file.choose())
DF700merged <-read.csv(file.choose())
DF852merged <-read.csv(file.choose())

#*************************
 
#stacking all the data.frames sequentialy to produce complete data.frame of all species
full_DF <-rbind(DF100merged, DF110merged, DF200merged, DF300merged, DF395merged, DF500merged, DF600merged, DF700merged, DF852merged, deparse.level = 0, make.row.names = TRUE)


write.csv(full_DF, "full_DF.csv", row.names = FALSE)
search()
detach(afe)
