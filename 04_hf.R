# setwd("/Users/macbookpro/Library/CloudStorage/OneDrive-Personal/PhD/spatialpattern_climate_humanfootprint")
# source("00_sp_functions.R")
# source("03_ratios_script.R")


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

hf <- readRDS("Data_1km_EU_hf.rds")


## 0r read back in
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
#plot(sp)


## create empty template raster
temp <- calc(hf, fun=function(x){ x[x >= 0] <- 0; return(x)} )


# if(file.exists("Data_ratios_dataframe.rds")) {
rat <- readRDS("Data_ratios_hf.rds")
  
# rat <- rat[rat$species %in% clean_tips,]
# #setdiff(clean_tips, rat$species)
# } else rat <- as.data.frame(unique(sp$species))
# 
# rat$hf_mean <- rep(NA, length(rat$species))
# rat$vel_mean <- NA
# rat$mat_mean <- NA
# rat$mat_var_mean <- NA
# rat$map_mean <- NA
# rat$map_var_mean <- NA

# rat <- as.data.frame(t(c("species", "hf_mean", "vel_mean", "mat_mean", "mat_var_mean", "map_mean", "map_var_mean")))
# names(rat) <- c("species", "hf_mean", "vel_mean", "mat_mean", "mat_var_mean", "map_mean", "map_var_mean")
# rat <- rat[0,1:7]

print("as far as loop")

## extract hf and climate values for coordinates in dataset

## obtain values for each
print(Sys.time())
for (i in unique(sp$species)){
  
  if(file.exists(paste("occ_rasters/occ", i, ".tif", sep = ""))) {
    if(i %in% rat$species[which(is.na(rat$hf_mean))]){
      print(i)
      
      rast <- raster(paste("occ_rasters/occ", i, ".tif", sep = ""))
      print("bricking raster")
      all <- brick(hf, temp, rast)
      print("raster bricked")
      gc()
      rast_data <- as.data.frame(all, xy = T)
      print("values extracted")
      names(rast_data) <- c("x", "y", "hf", "template", paste(i))
      rast_data[,i][which(is.na(rast_data[i]))] <- 0
      
      print("getting ratios")
      rat$hf_mean[rat$species == i] <- mean(rast_data$hf[rast_data$template == 0 & rast_data[,i] == 1], na.rm = TRUE)
 
      print("got ratios")
      
      saveRDS(rat, "Data_ratios_hf.rds") 
      rat <- readRDS("Data_ratios_hf.rds")} } }


print(end)





