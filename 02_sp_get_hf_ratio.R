## 02_sp_get_human_footprint_ratio

## load environmental data ----------------
# mat <-raster('bio1.bil') ## mean annual temperature (C*10)
# map <-raster('bio12.bil') ## mean annual precipatation (mm)
# map_var <-raster('bio15.bil') ## mean annual precip coeff variation
# mat_var <-raster('bio4.bil') ## mean annual temp SD*100

mat <- raster("wc2/wc2.1_30s_bio_1.tif") ## mean annual temperature (C*10)
map <- raster("wc2/wc2.1_30s_bio_12.tif") ## mean annual precipatation (mm)
map_var <- raster("wc2/wc2.1_30s_bio_15.tif")  ## mean annual precip coeff variation
mat_var <- raster("wc2/wc2.1_30s_bio_4.tif") ## mean annual temp SD*100

## crop to europe
map <- crop(map, extent(-33,67,30, 82))
mat <- crop(mat, extent(-33,67,30, 82))
mat_var <- crop(mat_var, extent(-33,67,30, 82))
map_var <- crop(map_var, extent(-33,67,30, 82))


# From Sandle et al 2011 - availble from datadryad
# This raster describes the local average displacement rate of mean annual temperature since the Last Glacial Maximum. It is in units of m/yr. 
# It is derived from the WorldClim (http://www.worldclim.org/, Hijmans et al. 2005) modern climate data, 
# and the PMIPII (http://pmip2.lsce.ipsl.fr/) paleoclimate data for the CCSM3 and MIROC3.2 climate models.
# References
# Hijmans, R.J., Cameron, S.E., Parra, J.L., Jones, P.G., and Jarvis, A. 2005. 
# Very high resolution interpolated climate surfaces for global land areas. International Journal of Climatology 25: 1965-1978

vel <- raster("Velocity.tif") ## approx 1km resolution
## read in the humanfootprint raster
hf <- raster("Data_wildareas-v3-2009-human-footprint.tif") ## approx 1km resolution

## harmonise projections ---------------
## get data into same crs at approx 1km spatial resolution
# vel2 <- projectRaster(vel, crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") ## bioclim crs
# vel3 <- crop(vel2, extent(-33,67,30, 82))
hf2 <- projectRaster(hf, mat)
vel2 <- projectRaster(vel, mat)

## make climate variables into one object (raster brick)
clim_map <- brick(map, mat, map_var, mat_var) 

# par(mfrow=c(1,2))
# plot(vel2)
# plot(hf2)
# plot(clim_map)


## get occurrence data --------------
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
sp <- endem ## 735 unique species

# sp$species <- factor(sp$species)
# levels(sp$species) <- gsub(" ", "_", levels(sp$species))
# setdiff(sp$species, metrics$species)

sp <- sp[, c("species", "x", "y")] ## 735 unique species

## make dataframe with just the lat and long co-ordinates of data that is relevant to my analysis
sp_co <- sp %>% .[, which(names(.) %in% c("x", "y"))]

## #extract climate values for coordinates in (full) dataset --------------
full_clim <- data.frame(raster::extract(clim_map,sp_co)) 
## create dataset with both climate values and co-ordinates of the values
full_clim <- cbind(full_clim,sp)
env <- unique(full_clim)
names(env) <- c("map", "mat", "map_var", "mat_var", "species", "Longitude", "Latitude") 
env <- drop_na(env)
env$species <- gsub(" ", "_", env$species)
#saveRDS(env, "Data_occurences_climate_values.rds")
env <- env[, which(names(env) %in% c("map", "mat", "map_var", "mat_var", "Longitude", "Latitude") )]


## current vs historic calc.
# current_historic <- brick(vel2,mat_var)
# current_historic <- drop_na(data.frame(raster::extract(current_historic,sp_co))) 
# cor(scale(current_historic$Velocity), scale(current_historic$bio4))
# plot(scale(current_historic$wc2.1_30s_bio_4), scale(current_historic$Velocity))
# 
# x <- lm(scale(log(current_historic$Velocity)) ~ scale(log(current_historic$wc2.1_30s_bio_4)))
# summary(x)
# 
# par(mfrow = c(1,2))
# hist(log(current_historic$Velocity))
# hist(log(current_historic$wc2.1_30s_bio_4))


grid <- shapefile("AFEcells/cgrs_grid.shp")
## Set up a raster "template" for a 0.5 degree grid
# ext <- extent(-33,67,30, 82)
# gridsize <- 0.5
# rr <- raster(ext, res=gridsize)
## Rasterize the shapefile
rr <- rasterize(grid, rr)
## Plot raster
plot(rr)


## make dataframe with just the lat and long co-ordinates of data that is relevant to my analysis
sp <- unique(env[, c("species", "Longitude", "Latitude")]) ## 735 unique species
names(sp) <- c("species", "x", "y")
sp_co <- sp %>% .[, which(names(.) %in% c("x", "y"))]
names(sp_co) <- c("x", "y")

par(mfrow = c(1,1))
plot(mat)
points(sp_co$x, sp_co$y, type = "p", col = "black", lwd = 0.1)

## pause ------------

## Map human footprint data
pal <- colorRampPalette(c('#0C276C', '#3B9088', '#EEFF00', '#ffffff'))
hf_map <- calc(hf, fun=function(x){ x[x > 100] <- NA; return(x)} )
#par(bty = "n", mar=c(0.02,0.02,2,0.2))
#plot(hf_map, col = pal(50), main = "Human footprint 2009", yaxt="n", xaxt="n")
hf_repro <- projectRaster(hf_map, mat)
plot(hf_repro)

#r <- calc(hf_repro, fun=function(x){ x[x > 50] <- NA; return(x)} )
r <- hf_repro
#plot(r, col = pal(50))
temp <- calc(r, fun=function(x){ x[x >= 0] <- 0; return(x)} )
#plot(temp)
#plot(temp, col = pal(50))

sp <- sp[, c("x", "y", "species")]
plot(sp)

rast_list <- list()
for (i in unique(sp$species)){
  s <- sp[sp$species == i,]
  sp2 <- SpatialPointsDataFrame(s[,c("x", "y")], 
                                as.data.frame(s[,3]),
                                proj4string =  temp@crs)
  rast_list[i] <- rasterize(sp2, temp, field = 1)
}

all <- brick(rast_list)        
all <- brick(r, temp, all, clim_map)
#plot(all)


## #extract hf and climate values for coordinates in dataset
ratio_data <- as.data.frame(all, xy = T)
colnames(ratio_data)[which(names(ratio_data) %in% c("bio1", "bio4", "bio12", "bio15"))] <- c("mat", "mat_var", "map", "map_var")

for (i in names(ratio_data)[which(names(ratio_data) %nin% c("x", "y", "layer.1", "layer.2", "mat", "mat_var", "map", "map_var"))]){
  ratio_data[,i][which(is.na(ratio_data[,i]))] <- 0
}

f <- ratio_data

rat <- as.data.frame(names(ratio_data)[which(names(ratio_data) %nin% c("x", "y", "layer.1", "layer.2", "mat", "mat_var", "map", "map_var"))])
names(rat) <- "species"

for (i in names(ratio_data)[which(names(ratio_data) %nin% c("x", "y", "layer.1", "layer.2", "mat", "mat_var", "map", "map_var"))]){
  rat$hf_range[rat$species == i] <- max(ratio_data$layer.1[f$layer.2 == 0 & ratio_data[,i] == 1],na.rm=TRUE) -min(
    ratio_data$layer.1[f$layer.2 == 0 & ratio_data[,i] == 1],na.rm=TRUE)
  rat$mat_range[rat$species == i] <- max(ratio_data$mat[f$layer.2 == 0 & ratio_data[,i] == 1],na.rm=TRUE) -min(
    ratio_data$mat[f$layer.2 == 0 & ratio_data[,i] == 1],na.rm=TRUE)
  rat$mat_var_range[rat$species == i] <- max(ratio_data$mat_var[f$layer.2 == 0 & ratio_data[,i] == 1],na.rm=TRUE) -min(
    ratio_data$mat_var[f$layer.2 == 0 & ratio_data[,i] == 1],na.rm=TRUE)
  rat$map_range[rat$species == i] <- max(ratio_data$map[f$layer.2 == 0 & ratio_data[,i] == 1],na.rm=TRUE) -min(
    ratio_data$map[f$layer.2 == 0 & ratio_data[,i] == 1],na.rm=TRUE)
  rat$map_var_range[rat$species == i] <- max(ratio_data$map_var[f$layer.2 == 0 & ratio_data[,i] == 1],na.rm=TRUE) -min(
    ratio_data$map_var[f$layer.2 == 0 & ratio_data[,i] == 1],na.rm=TRUE)
  }

for (i in names(ratio_data)[which(names(ratio_data) %nin% c("x", "y", "layer.1", "layer.2", "mat", "mat_var", "map", "map_var"))]){
  rat$max_mat_var[rat$species == i] <- max(ratio_data$mat_var[f$layer.2 == 0 & ratio_data[,i] == 1],na.rm=TRUE) 
  }

ranges <- rat


rat <- as.data.frame(names(ratio_data)[which(names(ratio_data) %nin% c("x", "y", "layer.1", "layer.2", "mat", "mat_var", "map", "map_var"))])
names(rat) <- "species"
for (i in names(ratio_data)[which(names(ratio_data) %nin% c("x", "y", "layer.1", "layer.2", "mat", "mat_var", "map", "map_var"))]){
  rat$gm_mean[rat$species == i] <- gm_mean(ratio_data$layer.1[f$layer.2 == 0 & ratio_data[,i] == 1])/
    gm_mean(ratio_data$layer.1[ratio_data$layer.2 == 0 & ratio_data[,i] == 0])
  rat$hf_mean[rat$species == i] <- mean(ratio_data$layer.1[f$layer.2 == 0 & ratio_data[,i] == 1], na.rm = TRUE)/
    mean(ratio_data$layer.1[ratio_data$layer.2 == 0 & ratio_data[,i] == 0], na.rm = TRUE)
  rat$median[rat$species == i] <- median(ratio_data$layer.1[f$layer.2 == 0 & ratio_data[,i] == 1], na.rm = TRUE)/
    median(ratio_data$layer.1[ratio_data$layer.2 == 0 & ratio_data[,i] == 0], na.rm = TRUE)
  rat$reg_mean[rat$species == i] <- mean(ratio_data$layer.1[f$layer.2 == 0 & ratio_data[,i] == 1], na.rm = TRUE)
  rat$mat_mean[rat$species == i] <- mean(ratio_data$mat[f$layer.2 == 0 & ratio_data[,i] == 1], na.rm = TRUE)
  rat$mat_var_mean[rat$species == i] <- mean(ratio_data$mat_var[f$layer.2 == 0 & ratio_data[,i] == 1], na.rm = TRUE)
  rat$map_mean[rat$species == i] <- mean(ratio_data$map[f$layer.2 == 0 & ratio_data[,i] == 1], na.rm = TRUE)
  rat$map_var_mean[rat$species == i] <- mean(ratio_data$map_var[f$layer.2 == 0 & ratio_data[,i] == 1], na.rm = TRUE)
}



par(mfrow = c(4,2), mar = c(4,4,4,4))
hist(rat$gm_mean, breaks = 100)
hist(rat$hf_mean, breaks = 100)
hist(rat$median, breaks = 100)
hist(rat$reg_mean, breaks = 100)
hist(rat$mat_mean, breaks = 100)
hist(rat$mat_var_mean, breaks = 100)
hist(rat$map_mean, breaks = 100)
hist(rat$map_var_mean, breaks = 100)


#saveRDS(rat, "Data_occ_humanfootprint_ratio.rds")
#saveRDS(ranges, "Data_occ_clim_hf_value_ranges.rds")



