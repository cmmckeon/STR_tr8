## 08_str_humanfootprint_analysis
## 18/05/2021 cm 

## read in the humanfootprint raster
hf <- raster("Data_wildareas-v3-2009-human-footprint.tif")

## Map human footprint dataa
pal <- colorRampPalette(c('#0C276C', '#3B9088', '#EEFF00', '#ffffff'))
hf_map <- calc(hf, fun=function(x){ x[x > 100] <- NA; return(x)} )
par(bty = "n", mar=c(0.02,0.02,2,0.2))
plot(hf_map, col = pal(50), main = "Human footprint 2009", yaxt="n", xaxt="n")
hf_repro <- projectRaster(hf, clim_map)

r <- crop(hf_repro, extent(-25,60,35, 75))
plot(r, main = "map")

r <- calc(r, fun=function(x){ x[x > 70] <- NA; return(x)} )
plot(r, col = pal(50))
temp <- calc(r, fun=function(x){ x[x >= 0] <- 0; return(x)} )
plot(temp)

temp <- brick(r, temp)

#colnames(sp) <- c("species", "x", "y")
sp <- sp[, c("x", "y", "species")]

a <- rasterFromXYZ(sp[sp$species == Aremonia_agrimonoides,])


polygons <- shapefile("AFEcells/crgs_AFEocc.shp")
polygons <- shapefile("AFEcells/crgs_grid.shp")
sp

## #extract climate values for coordinates in (full) PREDICTS dataset
occurrence <- as.data.frame(temp, xy = T)


dfr <- rasterFromXYZ(occurrence) 

sp2 <- SpatialPointsDataFrame(sp[,c("x", "y")], 
                              as.data.frame(sp[,3]),
                              proj4string =  temp@crs)

fidy <- raster(xmn = -33, xmx = 66, 
            ymn = 35, ymx = 75, 
            vals = 1, 
            resolution = 0.8333, 
            crs = "+proj=longlat +datum=WGS84 +no_defs +towgs84=0,0,0 +units=m" )

t <- projectRaster(temp, fidy)
plot(t)

bees <- rasterize(sp2, t, field = 1)

plot(bees)

#b2 <- aggregate(bees, fact= 2)

plot(b2)


ras <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 +units=m"
dd <- projectRaster(temp, crs = ras)

j <- sp
names(j) <- c("y", "x", "species")
 j$species <- as.numeric(factor(j$species))

 j <- sp[1:6,]
 j2 <- SpatialPointsDataFrame(j[,c("x", "y")], 
                              as.data.frame(j[,3]),
                              proj4string =  temp@crs)
 
 
hh <- sp
hh$species <- as.numeric(factor(hh$species))
bees <- rasterFromXYZ(sp)
plot(bees)

plot(temp)
points(occ_Aremonia_agrimonoides$ o$Longitude, PR_co$Latitude, type = "p", col = "orange")


## create dataset with both climate values and co-ordinates of the values
full_clim <- cbind(full_clim,endem_co)
env <- unique(full_clim)
names(env) <- c("map", "mat", "map_var", "mat_var","Longitude", "Latitude") 
env <- drop_na(env)

check <- read.delim2("AFEcells/Afe2005_dot.txt")
hf_repro
