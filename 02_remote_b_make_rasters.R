## 02_remote_b_make_rasters.R

print("this is Decreasing occurrence raster making script")
sp <- sp[order(sp$species, decreasing = TRUE),]

## create occurrence rasters for each species, based on AFE irregular 50km2 occurrence grid, rasterised onto approx 1km2 regular raster
rast_list <- list()
print(Sys.time())
for (i in unique(sp$species)){
  if(!file.exists(paste("occ_rasters/occ", i, ".tif", sep = ""))) {
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
    gc() } }

print("end")

