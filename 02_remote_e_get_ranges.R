## 02_remote_e_get_ranges.R

# source("02_remote_e_get_ranges.R")

# get these values once
# all <- brick(hf, vel, clim_map)
# gc()
# val <- as.data.frame(all, xy = T)
# names(val) <- c("x", "y", "hf", "Velocity", "map", "mat","map_var","mat_var")
# val <- drop_na(val)
# saveRDS(val, "Data_hf_vel_clim_map_values.rds")

vals <- readRDS("Data_hf_vel_clim_map_values.rds")

land_co <- vals[,1:2]

print(Sys.time())
for (i in unique(sp$species)){
  
  if(file.exists(paste("occ_rasters/occ", i, ".tif", sep = ""))) {
    if(i %in% rat$species#[which(is.na(rat$vel))]
       ){
      print(i)
      
      all <- raster(paste("occ_rasters/occ", i, ".tif", sep = ""))
      print("bricking raster")
      all <- brick(temp, rast)
      print("raster bricked")
      gc()
      rast_data  <- extract(all, land_co)
      print("values extracted")
      rast_data <- cbind(vals, rast_data)
      names(rast_data) <- c("x", "y", "hf", "Velocity", "map", "mat", "map_var", "mat_var", "template", paste(i)) 
      rast_data[,i][which(is.na(rast_data[i]))] <- 0
      
      
      print("getting ranges")
      
      
      rat$hf_range[rat$species == i] <- max(rast_data$hf[rast_data$template == 0 & rast_data[,i] == 1], na.rm = TRUE) - min(rast_data$hf[rast_data$template == 0 & rast_data[,i] == 1], na.rm = TRUE)
      rat$vel_range[rat$species == i] <- max(rast_data$Velocity[rast_data$template == 0 & rast_data[,i] == 1], na.rm = TRUE) - min(rast_data$Velocity[rast_data$template == 0 & rast_data[,i] == 1], na.rm = TRUE)
      rat$mat_range[rat$species == i] <- max(rast_data$mat[rast_data$template == 0 & rast_data[,i] == 1], na.rm = TRUE) - min(rast_data$mat[rast_data$template == 0 & rast_data[,i] == 1], na.rm = TRUE)
      rat$mat_var_range[rat$species == i] <- max(rast_data$mat_var[rast_data$template == 0 & rast_data[,i] == 1], na.rm = TRUE) - min(rast_data$mat_var[rast_data$template == 0 & rast_data[,i] == 1], na.rm = TRUE)
      rat$map_range[rat$species == i] <- max(rast_data$map[rast_data$template == 0 & rast_data[,i] == 1], na.rm = TRUE) - min(rast_data$map[rast_data$template == 0 & rast_data[,i] == 1], na.rm = TRUE)
      rat$map_var_range[rat$species == i] <- max(rast_data$map_var[rast_data$template == 0 & rast_data[,i] == 1], na.rm = TRUE) - min(rast_data$map_var[rast_data$template == 0 & rast_data[,i] == 1], na.rm = TRUE)
      
      print("got ranges")
      
      saveRDS(rat, "Data_ratios_dataframe.rds") 
      rm(rat, rast, all, rast_data)
      gc()
      rat <- readRDS("Data_ratios_dataframe.rds")} } }


print("end")





