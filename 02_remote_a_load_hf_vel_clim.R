## 02_remote_a_load_data.R

print("loading metrics")

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

print("metrics loaded")

## load environmental data ----------------
print("load hf, vel and clim_map 1km rasters")
vel <- readRDS("Data_1km_EU_vel.rds")
hf <- readRDS("Data_1km_EU_hf.rds")
clim_map <- readRDS("Data_1km_EU_clim.rds")

## create empty template raster
temp <- calc(hf, fun=function(x){ x[x >= 0] <- 0; return(x)} )

print("load AFE occurrences")
env <-readRDS("Data_occurences_climate_values.rds")

## make dataframe with just the lat and long co-ordinates of data that is relevant to my analysis
sp <- unique(env[, c("species", "Longitude", "Latitude")]) ## 735 unique species
names(sp) <- c("species", "x", "y")
sp_co <- sp %>% .[, which(names(.) %in% c("x", "y"))]
names(sp_co) <- c("x", "y")

clean_tips <- readRDS("clean_tips_653.rds")
sp <- sp[sp$species %in% clean_tips,]

sp <- sp[, c("x", "y", "species")]

sp <- sp[order(sp$species, decreasing = TRUE),]

## read in AFE grid
grid <- shapefile("AFEcells/cgrs_grid.shp")

## read in existing dataframe
rat <- readRDS("Data_ratios_dataframe.rds")

print("end 02_remote_a_load_hf_vel_clim.R")