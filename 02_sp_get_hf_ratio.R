## 02_sp_get_human_footprint_ratio

 # temporary --------------- for sourcing script
 metrics <- read.csv("GRSmetrics_AFE_pol_50km_pol_line_mcp.csv") ## metrics provided by Anna Csergo in Spring 2022. 4260 obs of 42 vars
 metrics <- metrics[metrics$Model == "Occurrence",] ## 810 species
 metrics <- metrics[which(!is.na(metrics$Species)),]  ## 809 species
 metrics$Species <- gsub(" ssp.*", "", metrics$Species) ## remove subspecies (for now). 714 unique species
 metrics$Species <- gsub("[[:space:]]*$", "", metrics$Species) ##  692 unique species
 small <- c(" s.lat.", " s.str.")
 metrics$Species <- gsub(paste(small, collapse="|"), "", metrics$Species) ## 691
 metrics$Species <- gsub("//.", "", metrics$Species) 
 
 # Quercus petraea.
 # Ranunculus montanus.
 # Pinus uncinata.
 
 metrics <- unique(metrics) # 805 obs of 42 vars
 metrics <- unique(metrics[, which(names(metrics) %in% c("Species", "total.area", "range.size", "effective.mesh.size", 
                                                         "mean.shape.index", "prop.landscape", "perimeter.area.frac.dim"))])
 metrics <- metrics[c("Species", "total.area", "range.size", "effective.mesh.size", "prop.landscape","mean.shape.index", "perimeter.area.frac.dim")]
 metrics <- unique(metrics) # 810 obs of 7 vars
 names(metrics) <- c("species","Occupied area", "Geographic range size", "Patch size distribution", "Geographic range filling", "Patch shape complexity", "Geographic range fractality") 
 
 ## tidy names
 metrics$species[grep("oslo", metrics$species, useBytes = TRUE)] <- "Saxifraga osloensis"
 metrics$species[metrics$species == "Salix repens coll."] <- "Salix repens"
 
 
 ## log and scale raw data
 metrics$'Geographic range fractality' <- (metrics$'Geographic range fractality' + sqrt(min(metrics$'Geographic range fractality', na.rm = T)^2)) + 1
 for (i in names(Filter(is.numeric, metrics[, which(names(metrics) %nin% c("Geographic range filling"))]))) {
   metrics[, i] <- c(log(metrics[,i]))
 }
 for (i in names(Filter(is.numeric, metrics))) {
   metrics[, i] <- c(scale(metrics[,i]))
 }
 length(unique(metrics$species)) ## 690 unique species
 
 metrics$species <- factor(metrics$species)
 levels(metrics$species) <- gsub(" ", "_", levels(metrics$species))
 
 # temporary --------------

# ## load environmental data ----------------
# 
# ## 1km2 resolution climate data
# mat <- raster("wc2/wc2.1_30s_bio_1.tif") ## mean annual temperature (C*10)
# map <- raster("wc2/wc2.1_30s_bio_12.tif") ## mean annual precipatation (mm)
# map_var <- raster("wc2/wc2.1_30s_bio_15.tif")  ## mean annual precip coeff variation
# mat_var <- raster("wc2/wc2.1_30s_bio_4.tif") ## mean annual temp SD*100
# 
# ## crop to europe
# map <- crop(map, extent(-33,67,30, 82))
# mat <- crop(mat, extent(-33,67,30, 82))
# mat_var <- crop(mat_var, extent(-33,67,30, 82))
# map_var <- crop(map_var, extent(-33,67,30, 82))
# 
# print("climate data loaded")
# 
# # From Sandle et al 2011 - availble from datadryad
# # This raster describes the local average displacement rate of mean annual temperature since the Last Glacial Maximum. It is in units of m/yr. 
# # It is derived from the WorldClim (http://www.worldclim.org/, Hijmans et al. 2005) modern climate data, 
# # and the PMIPII (http://pmip2.lsce.ipsl.fr/) paleoclimate data for the CCSM3 and MIROC3.2 climate models.
# # References
# # Hijmans, R.J., Cameron, S.E., Parra, J.L., Jones, P.G., and Jarvis, A. 2005. 
# # Very high resolution interpolated climate surfaces for global land areas. International Journal of Climatology 25: 1965-1978
# 
# vel <- raster("Velocity.tif") ## approx 1km resolution
# ## read in the humanfootprint raster
# hf <- raster("Data_wildareas-v3-2009-human-footprint.tif") ## approx 1km resolution
# hf <- calc(hf, fun=function(x){ x[x > 100] <- NA; return(x)} )
# gc()
# 
# print("vel and hf loaded")
# 
# ## harmonise projections ---------------
# ## get data into same crs at approx 1km spatial resolution
# # vel2 <- projectRaster(vel, crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") ## bioclim crs
# # vel3 <- crop(vel2, extent(-33,67,30, 82))
# hf <- projectRaster(hf, mat)
# vel <- projectRaster(vel, mat)
# 
# print("vel and hf reprojected")
# 
# ## make climate variables into one object (raster brick)
# clim_map <- brick(map, mat, map_var, mat_var) 
# gc()
# 
# saveRDS(vel, "Data_1km_EU_vel.rds")
# saveRDS(hf, "Data_1km_EU_hf.rds")
# saveRDS(clim_map, "Data_1km_EU_clim.rds")

# or read back in
vel <- readRDS("Data_1km_EU_vel.rds")
hf <- readRDS("Data_1km_EU_hf.rds")
clim_map <- readRDS("Data_1km_EU_clim.rds")
# par(mfrow=c(1,2))
# plot(vel)
# plot(hf)
# plot(clim_map)

# 
# ## get occurrence data --------------
# endem <- read.delim2("AFE_endemics.txt")
# endem <- endem[, which(names(endem) %in% c("afe", "lon", "lat"))]
# endem$lon <- as.numeric(as.character(endem$lon))
# endem$lat <- as.numeric(as.character(endem$lat)) ## 864 unique species
# endem$afe <- gsub(" ssp.*", "", endem$afe) ## 736
# endem$afe <- gsub(" s.lat.", "", endem$afe) 
# endem$afe <- gsub(" s.str.", "", endem$afe) ## 735
# endem$afe[endem$afe == "Saxifraga osloënsis"] <- "Saxifraga osloensis"
# endem$afe <- as.character(endem$afe)
# endem$afe[endem$afe == "Arabis collna"] <- "Arabis collina"
# endem$afe[endem$afe == "Dianthus collnus"] <- "Dianthus collinus"
# endem$afe[endem$afe == "Salix repens coll."] <- "Salix repens"
# 
# names(endem) <- c("species", "y", "x")
# sp <- unique(endem) ## 734 unique species
# 
# # sp$species <- factor(sp$species)
# # levels(sp$species) <- gsub(" ", "_", levels(sp$species))
# # x <-  as.data.frame(setdiff(sp$species, metrics$species)) ## 44 species for which we have not metrics data
# 
# sp <- sp[, c("species", "x", "y")] ## 734 unique species
# 
# ## make dataframe with just the lat and long co-ordinates of data that is relevant to my analysis
# sp_co <- sp %>% .[, which(names(.) %in% c("x", "y"))]
# 
# ## #extract climate values for coordinates in (full) dataset --------------
# full_clim <- data.frame(raster::extract(clim_map,sp_co))
# ## create dataset with both climate values and co-ordinates of the values
# full_clim <- cbind(full_clim,sp)
# gc()
# env <- unique(full_clim)
# names(env) <- c("map", "mat", "map_var", "mat_var", "species", "Longitude", "Latitude")
# env <- drop_na(env) ## droping NAs here gives 725 species (only lossing 3 species for which we have metrics, so now at 687 with metricss)
# env$species <- gsub(" ", "_", env$species)
# saveRDS(env, "Data_occurences_climate_values.rds")

## 0r read back in 
env <-readRDS("Data_occurences_climate_values.rds")


## make dataframe with just the lat and long co-ordinates of data that is relevant to my analysis
sp <- unique(env[, c("species", "Longitude", "Latitude")]) ## 735 unique species
names(sp) <- c("species", "x", "y")
sp_co <- sp %>% .[, which(names(.) %in% c("x", "y"))]
names(sp_co) <- c("x", "y")

# par(mfrow = c(1,1))
# plot(mat)
# points(sp_co$x, sp_co$y, type = "p", col = "black", lwd = 0.1)

## create empty template raster
temp <- calc(hf, fun=function(x){ x[x >= 0] <- 0; return(x)} )
#plot(temp, col = pal(50))

sp <- sp[, c("x", "y", "species")]
#plot(sp)

## create occurrence rasters for each species, based on AFE irregular 50km2 occurrence grid, rasterised onto approx 1km2 regular raster
rast_list <- list()
for (i in unique(sp$species)){
  s <- sp[sp$species == i,]
  sp2 <- SpatialPointsDataFrame(s[,c("x", "y")],
                                as.data.frame(s[,3]),
                                proj4string =  temp@crs) 
  gc()
  Sys.sleep(60)
  rast_list[i] <- rasterize(sp2, temp, field = 1)
}
gc()

ocs <- brick(rast_list)
gc()
all <- brick(hf, vel, temp, ocs, clim_map)
#plot(all)
saveRDS(all, "all_occurrence_rasters.rds")

print(end)

# ## #extract hf and climate values for coordinates in dataset
# ratio_data <- as.data.frame(all, xy = T)
# colnames(ratio_data)[which(names(ratio_data) %in% c("bio1", "bio4", "bio12", "bio15"))] <- c("mat", "mat_var", "map", "map_var")
# 
# for (i in names(ratio_data)[which(names(ratio_data) %nin% c("x", "y", "layer.1", "layer.2", "mat", "mat_var", "map", "map_var"))]){
#   ratio_data[,i][which(is.na(ratio_data[,i]))] <- 0
# }
# 
# f <- ratio_data
# 
# rat <- as.data.frame(names(ratio_data)[which(names(ratio_data) %nin% c("x", "y", "layer.1", "layer.2", "mat", "mat_var", "map", "map_var"))])
# names(rat) <- "species"
# 
# for (i in names(ratio_data)[which(names(ratio_data) %nin% c("x", "y", "layer.1", "layer.2", "mat", "mat_var", "map", "map_var"))]){
#   rat$hf_range[rat$species == i] <- max(ratio_data$layer.1[f$layer.2 == 0 & ratio_data[,i] == 1],na.rm=TRUE) -min(
#     ratio_data$layer.1[f$layer.2 == 0 & ratio_data[,i] == 1],na.rm=TRUE)
#   rat$mat_range[rat$species == i] <- max(ratio_data$mat[f$layer.2 == 0 & ratio_data[,i] == 1],na.rm=TRUE) -min(
#     ratio_data$mat[f$layer.2 == 0 & ratio_data[,i] == 1],na.rm=TRUE)
#   rat$mat_var_range[rat$species == i] <- max(ratio_data$mat_var[f$layer.2 == 0 & ratio_data[,i] == 1],na.rm=TRUE) -min(
#     ratio_data$mat_var[f$layer.2 == 0 & ratio_data[,i] == 1],na.rm=TRUE)
#   rat$map_range[rat$species == i] <- max(ratio_data$map[f$layer.2 == 0 & ratio_data[,i] == 1],na.rm=TRUE) -min(
#     ratio_data$map[f$layer.2 == 0 & ratio_data[,i] == 1],na.rm=TRUE)
#   rat$map_var_range[rat$species == i] <- max(ratio_data$map_var[f$layer.2 == 0 & ratio_data[,i] == 1],na.rm=TRUE) -min(
#     ratio_data$map_var[f$layer.2 == 0 & ratio_data[,i] == 1],na.rm=TRUE)
#   }
# 
# for (i in names(ratio_data)[which(names(ratio_data) %nin% c("x", "y", "layer.1", "layer.2", "mat", "mat_var", "map", "map_var"))]){
#   rat$max_mat_var[rat$species == i] <- max(ratio_data$mat_var[f$layer.2 == 0 & ratio_data[,i] == 1],na.rm=TRUE)
#   }
# 
# ranges <- rat
# 
# 
# rat <- as.data.frame(names(ratio_data)[which(names(ratio_data) %nin% c("x", "y", "layer.1", "layer.2", "mat", "mat_var", "map", "map_var"))])
# names(rat) <- "species"
# for (i in names(ratio_data)[which(names(ratio_data) %nin% c("x", "y", "layer.1", "layer.2", "mat", "mat_var", "map", "map_var"))]){
#   rat$gm_mean[rat$species == i] <- gm_mean(ratio_data$layer.1[f$layer.2 == 0 & ratio_data[,i] == 1])/
#     gm_mean(ratio_data$layer.1[ratio_data$layer.2 == 0 & ratio_data[,i] == 0])
#   rat$hf_mean[rat$species == i] <- mean(ratio_data$layer.1[f$layer.2 == 0 & ratio_data[,i] == 1], na.rm = TRUE)/
#     mean(ratio_data$layer.1[ratio_data$layer.2 == 0 & ratio_data[,i] == 0], na.rm = TRUE)
#   rat$median[rat$species == i] <- median(ratio_data$layer.1[f$layer.2 == 0 & ratio_data[,i] == 1], na.rm = TRUE)/
#     median(ratio_data$layer.1[ratio_data$layer.2 == 0 & ratio_data[,i] == 0], na.rm = TRUE)
#   rat$reg_mean[rat$species == i] <- mean(ratio_data$layer.1[f$layer.2 == 0 & ratio_data[,i] == 1], na.rm = TRUE)
#   rat$mat_mean[rat$species == i] <- mean(ratio_data$mat[f$layer.2 == 0 & ratio_data[,i] == 1], na.rm = TRUE)
#   rat$mat_var_mean[rat$species == i] <- mean(ratio_data$mat_var[f$layer.2 == 0 & ratio_data[,i] == 1], na.rm = TRUE)
#   rat$map_mean[rat$species == i] <- mean(ratio_data$map[f$layer.2 == 0 & ratio_data[,i] == 1], na.rm = TRUE)
#   rat$map_var_mean[rat$species == i] <- mean(ratio_data$map_var[f$layer.2 == 0 & ratio_data[,i] == 1], na.rm = TRUE)
# }
# 
# 
# 
# par(mfrow = c(4,2), mar = c(4,4,4,4))
# hist(rat$gm_mean, breaks = 100)
# hist(rat$hf_mean, breaks = 100)
# hist(rat$median, breaks = 100)
# hist(rat$reg_mean, breaks = 100)
# hist(rat$mat_mean, breaks = 100)
# hist(rat$mat_var_mean, breaks = 100)
# hist(rat$map_mean, breaks = 100)
# hist(rat$map_var_mean, breaks = 100)
# 
# 
# #saveRDS(rat, "Data_occ_humanfootprint_ratio.rds")
# #saveRDS(ranges, "Data_occ_clim_hf_value_ranges.rds")
# 
# 
# 
