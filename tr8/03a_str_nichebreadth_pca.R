## this script is adapted by CM from work by Anna Csergo and Olivier Brönnimann

#################################################################################################
############################## preparation of datasets ##########################################
#################################################################################################

#load environmental data
mat <-raster('bio1.bil') ## mean annual temperature (C*10)
map <-raster('bio12.bil') ## mean annual precipatation (mm)
map_var <-raster('bio15.bil') ## mean annual precip coeff variation
mat_var <-raster('bio4.bil') ## mean annual temp SD*100

## make climate variables into one object (raster brick)
clim_map <- brick(map, mat, map_var, mat_var) 

## get occurrence data
endem <- read.delim2("AFE_endemics.txt")
endem <- endem[, which(names(endem) %in% c("afe", "lon", "lat"))]
endem$lon <- as.numeric(as.character(endem$lon))
endem$lat <- as.numeric(as.character(endem$lat))
endem$afe <- gsub(" ssp.*", "", endem$afe)
endem$afe <- gsub(" ssp.*", "", endem$afe)
endem$afe <- gsub(" s.lat.", "", endem$afe)
endem$afe <- gsub(" s.str.", "", endem$afe)
endem$afe <- gsub("coll.", "coll", endem$afe)
endem$afe[endem$afe == "Saxifraga osloënsis"] <- "Saxifraga osloensis"
endem$afe <- as.character(endem$afe)
endem$afe[endem$afe == "Arabis collna"] <- "Arabis collina"
endem$afe[endem$afe == "Dianthus collnus"] <- "Dianthus collinus"
names(endem) <- c("species", "y", "x") 
sp <- endem

sp <- sp[, c("species", "x", "y")] ## 735 unique species

## make dataframe with just the lat and long co-ordinates of data that is relevant to my analysis
sp_co <- sp %>% .[, which(names(.) %in% c("x", "y"))]

## #extract climate values for coordinates in (full) dataset
full_clim <- data.frame(raster::extract(clim_map,sp_co)) 
## create dataset with both climate values and co-ordinates of the values
full_clim <- cbind(full_clim,sp)
env <- unique(full_clim)
names(env) <- c("map", "mat", "map_var", "mat_var", "species", "Longitude", "Latitude") 
env <- drop_na(env)
#saveRDS(env, "Data_occurences_climate_values.rds")
env <- env[, which(names(env) %in% c("map", "mat", "map_var", "mat_var", "Longitude", "Latitude") )]


# sample environmental values for all occurences
occ.sp<-na.exclude(ecospat.sample.envar(dfsp=sp, colspxy=2:3,
                                        colspkept=1:3, dfvar=env,
                                        colvarxy=5:6, colvar="all",
                                        resolution=1))
# list of species
sp.list<-unique(occ.sp$species)


#################################################################################################
#################################### PCA-ENVIRONMENT ############################################
#################################################################################################

## i don't have study areas, i just have occurrences. 

#dataset for the analysis,includes all the sites of the study area + the occurences for all the species
data<-rbind(occ.sp[,4:7],env[,1:4]) 
#vector of weight, 0 for the occurences, 1 for the sites of the study area
w<-c(rep(0,nrow(occ.sp)),rep(1,nrow(env))) 
# the pca is calibrated on all the sites of the study area; occurences are not used for the calibration, 
# but their scores are calculated
pca.cal <-dudi.pca(data, center = T, row.w = w, scale = T, scannf = F) 


scores.clim<- pca.cal$li[(nrow(occ.sp)+1):nrow(data),]	#scores for global climate


nb <- as.data.frame(cbind(sp.list, c(1:length(sp.list))))
names(nb) <- c("sp.list", "num")

for(i in 1:length(sp.list)) {
  row.sp1<-which(occ.sp[,1] == sp.list[i]) # rows in data corresponding to sp1

  # predict the scores on the axes
  #scores for sp1
  scores.sp1<- pca.cal$li[row.sp1,]		

  #niche breadth: weighted average across the two PCA axes in one value
  nb$nb[nb$num == i] <- mean(sd(scores.sp1[,1])*pca.cal$eig[1]+sd(scores.sp1[,2])*pca.cal$eig[2])
}

## tidy up to make sure this dataset is maximally compatible with traits and range metrics
levels(nb$sp.list) <- gsub("_", " ", levels(nb$sp.list))

saveRDS(nb, "Data_nichebreadth.rds")

