
library(stringr)
library(reshape)#rearrange trait dataframes


setwd("/Volumes/geodem/Paper1_Fragmentation/Caroline thesis")

#read endemic AFE data; will need to match traits to the species (Species.x column); you have one species more than once in that column, need to match them all
afe.end <- read.csv ("/Volumes/geodem/Paper1_Fragmentation/Caroline thesis/Final_dataset.csv", stringsAsFactors=F)


#seed bank LEDA Traitbase
sb.l <- read.csv ('~/Google Drive/GEODEM/Paper1_Fragmentation/Data/Traits/RawData/LEDA_accessed18March2016/seed bank.csv', header=T) #with tr8 you will not need to do this
sb.l_spp <- unique(as.character(sb.l$X.1))#choose species
overlap_seedbank2 <- intersect(unique(afe.end$Species.x), sb.l_spp)#65 species overlap


#seed longevity (LEDA Traitbase)
sl <- read.csv ('~/Google Drive/GEODEM/Paper1_Fragmentation/Data/Traits/RawData/LEDA_accessed18March2016/seed longevity.csv', header=T)
sl_spp <- unique(as.character(sl$SBS.name))
overlap_sl <- intersect(afe_spp3, sl_spp)#65 species overlap


#seed mass (LEDA Traitbase)
sm <- read.csv ('~/Google Drive/GEODEM/Paper1_Fragmentation/Data/Traits/RawData/LEDA_accessed18March2016/seed mass.csv', header=T)
sm_spp <- unique(as.character(sm$SBS.name))
overlap_sm <- intersect(afe_spp3, sm_spp)#131 species overlap


#dispersal data
#disp <- read.csv('~/Google Drive/GEODEM/Paper1_Fragmentation/Data/Traits/RawData/DispersalDistanceData1.csv', header = TRUE) 
#d_spp <- as.character(disp$Species)
#overlap_disp <- intersect(afe_spp3, d_spp) # 17 spp overlap

#dispersal type LEDA Traitbase
disp.l <- read.csv ('~/Google Drive/GEODEM/Paper1_Fragmentation/Data/Traits/RawData/LEDA_accessed18March2016/dispersal type.csv', header=T)
d.l_spp <- as.character(disp.l$SBS.name)
overlap_disp.l <- intersect(afe_spp3, d.l_spp) #385 spp overlap


#growth form (LEDA Traitbase)
gforms=read.csv("~/Google Drive/GEODEM/Paper1_Fragmentation/Data/Traits/RawData/LEDA_accessed18March2016/growth form.csv")
head(gforms)
gfSpecies=unique(as.character(gforms$SBS.name))
gfSpecies <- as.data.frame(str_split_fixed(gfSpecies, " ", n=3))[,1:2]#select species not infrataxa
gfSpecies <- paste(gfSpecies $V1, gfSpecies $V2, sep=" ")
overlap_growthform=intersect(afe_spp3, gfSpecies)#133 spp overlap


#clonal growth organ (LEDA Traitbase)
cl_organ <- read.csv("~/Google Drive/GEODEM/Paper1_Fragmentation/Data/Traits/RawData/LEDA_accessed18March2016/clonal growth organ.csv")
cl.sp <- unique(as.character(cl_organ$SBS.name))
overlap_cl.organ <- intersect(cl.sp, afe_spp3) #132 spp overlap

# plant life span (LEDA Traitbase)
lifespan <- read.csv("~/Google Drive/GEODEM/Paper1_Fragmentation/Data/Traits/RawData/LEDA_accessed18March2016/plant life span.csv")
lifespan_spp <- unique(as.character(lifespan$SBS.name))
overlap_lifespan <- intersect(lifespan_spp, afe_spp3) #90 spp overlap


#specific leaf area (LEDA Traitbase)
sla <- read.csv("~/Google Drive/GEODEM/Paper1_Fragmentation/Data/Traits/RawData/LEDA_accessed18March2016/specific leaf area.csv")
sla_spp <- unique(as.character(sla$SBS.name))
overlap_sla <- intersect(sla_spp, afe_spp3) #101 spp overlap


# terminal velocity (LEDA Traitbase)
veloc <- read.csv("~/Google Drive/GEODEM/Paper1_Fragmentation/Data/Traits/RawData/LEDA_accessed18March2016/terminal velocity.csv")
veloc_spp <- unique(as.character(veloc$SBS.name))
overlap_veloc <- intersect(veloc_spp, afe_spp3) #78 spp overlap

# woodiness (LEDA Traitbase)
wood <- read.csv("~/Google Drive/GEODEM/Paper1_Fragmentation/Data/Traits/RawData/LEDA_accessed18March2016/woodiness.csv")
wood_spp <- unique(as.character(wood$SBS.name))
overlap_wood <- intersect(wood_spp, afe_spp3) #150 spp overlap


#COMPADRE overlap
compadre <- read.csv('~/Dropbox/IslandBiogeography_new/COMPADRE.csv', header=TRUE)
head(compadre)

compadre_spp <- gsub("_"," ", compadre$SpeciesAuthor)

compadre_spp2 <-as.data.frame(str_split_fixed(compadre_spp, " ", n=3)[,1:2])#select species not infrataxa
compadre_spp3 <- paste(compadre_spp2$V1, compadre_spp2$V2, sep=" ")

overlap_compadre <- intersect(compadre_spp3, afe_spp3)# 9 species overlap




#merge trait data with existing datasets
#you might need to use the "merge" command; an example below:
geoniche<- merge(geo, niche, by.x="Species", by.y="sp.names",all.x=TRUE)
#two spreadsheets were merged "geo" and "niche", where "Species" is the species names in "geo" and "sp.names" is the species names in "niche"


