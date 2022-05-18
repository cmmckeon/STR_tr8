# temporary --------------- for sourcing script
metrics <- read.csv("GRSmetrics_AFE_pol_50km_pol_line_mcp.csv") ## metrics provided by Anna Csergo in Spring 2022. 4260 obs of 42 vars
metrics <- metrics[metrics$Model == "Occurrence",] ## 810 species
metrics <- metrics[which(!is.na(metrics$Species)),]  ## 809 species
metrics$Species <- gsub(" ssp.*", "", metrics$Species) ## remove subspecies (for now). 714 unique species
metrics$Species <- gsub("[[:space:]]*$", "", metrics$Species) ##  692 unique species
small <- c(" s.lat.", " s.str.")
metrics$Species <- gsub(paste(small, collapse="|"), "", metrics$Species) ## 691
metrics$Species <- gsub("//.", "", metrics$Species) 

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

vel <- readRDS("Data_1km_EU_vel.rds")
hf <- readRDS("Data_1km_EU_hf.rds")
clim_map <- readRDS("Data_1km_EU_clim.rds")

## 0r read back in
env <-readRDS("Data_occurences_climate_values.rds")


## make dataframe with just the lat and long co-ordinates of data that is relevant to my analysis
sp <- unique(env[, c("species", "Longitude", "Latitude")]) ## 735 unique species
names(sp) <- c("species", "x", "y")
sp_co <- sp %>% .[, which(names(.) %in% c("x", "y"))]
names(sp_co) <- c("x", "y")

sp <- sp[, c("x", "y", "species")]
sp <- sp[order(sp$species, decreasing = TRUE),]

#plot(sp)


## create empty template raster
temp <- calc(hf, fun=function(x){ x[x >= 0] <- 0; return(x)} )

## read in AFE grid
grid <- shapefile("AFEcells/cgrs_grid.shp")


if(exists("Data_ratios_dataframe.rds")) {
  rat <- readRDS("Data_ratios_dataframe.rds")
} else rat <- as.data.frame(unique(sp$species))

names(rat) <- "species"
saveRDS(rat, "Data_ratios_dataframe.rds")

## create occurrence rasters for each species, based on AFE irregular 50km2 occurrence grid, rasterised onto approx 1km2 regular raster
rast_list <- list()
print(Sys.time())

if(file.exists(paste("occ_rasters/occ", i, ".tif", sep = ""))) {
  print(c("have", i)) } else

for (i in unique(sp$species)){
  if(!file.exists(paste("occ_rasters/occ", i, ".tif", sep = ""))) {
    # print(c("have", i)) 
    # sp <- sp[sp$species != i,] }
    
    s <- sp[sp$species == i,]
  
    print(i)
    sp2 <- SpatialPointsDataFrame(s[,c("x", "y")],
                                as.data.frame(s[,3]),
                                proj4string =  temp@crs)
    print("get polygons")
    poly_list <- over(SpatialPolygons(grid@polygons), SpatialPoints(sp2))
    gc()
    print("subset grid")
    poly_occ <- grid[which(!is.na(poly_list)),]
    gc()
    print("rasterise occurrence")
    rast <- rasterize(poly_occ, temp, field = 1)
    
    gc()
    
    if(!file.exists(paste("occ_rasters/occ", i, ".tif", sep = ""))) {
    writeRaster(rast, paste("occ_rasters/occ", i, ".tif", sep = ""))}
    print("occurrence raster saved")
    gc() }}
  


