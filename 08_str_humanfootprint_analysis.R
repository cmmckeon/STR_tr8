## 08_str_humanfootprint_analysis
## 18/05/2021 cm 

#load environmental data
mat <-raster('bio1.bil') ## mean annual temperature (C*10)
mat <- crop(mat, extent(-33,67,30, 82))
plot(mat)
#mat <- projectRaster(mat, crs = "+proj=laea +lat_0=53 +lon_0=9 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")


# grid <- shapefile("AFEcells/cgrs_grid.shp")
# ## Set up a raster "template" for a 0.5 degree grid
# # ext <- extent(-33,67,30, 82)
# # gridsize <- 0.5
# # rr <- raster(ext, res=gridsize)
# ## Rasterize the shapefile
# rr <- rasterize(grid, rr)
# ## Plot raster
# plot(rr)

## get occurrence data
endem <- read.delim2("AFE_endemics.txt")
endem <- endem[, which(names(endem) %in% c("afe", "lon", "lat"))]
endem$lon <- as.numeric(as.character(endem$lon))
endem$lat <- as.numeric(as.character(endem$lat))
endem$afe <- gsub(" ssp.*", "", endem$afe)
endem$afe <- gsub(" ", "_", endem$afe)
names(endem) <- c("species", "y", "x")
sp <- endem

## make dataframe with just the lat and long co-ordinates of PREDICTS data that is relevant to my analysis
sp_co <- sp %>% .[, which(names(.) %in% c("x", "y"))]

par(mfrow = c(1,1))
plot(mat)
# sp_co$y <- sp_co$y*100000
# sp_co$x <- sp_co$x*100000
points(sp_co$x, sp_co$y, type = "p", col = "black")

## read in the humanfootprint raster
hf <- raster("Data_wildareas-v3-2009-human-footprint.tif")

# fidy <- raster(xmn = -30, xmx = 66, 
#                ymn = 35, ymx = 75, 
#                vals = 1, 
#                resolution = 0.083333333, 
#                crs = "+proj=longlat +datum=WGS84 +no_defs +towgs84=0,0,0 +units=m")


## Map human footprint dataa
pal <- colorRampPalette(c('#0C276C', '#3B9088', '#EEFF00', '#ffffff'))
hf_map <- calc(hf, fun=function(x){ x[x > 100] <- NA; return(x)} )
par(bty = "n", mar=c(0.02,0.02,2,0.2))
plot(hf_map, col = pal(50), main = "Human footprint 2009", yaxt="n", xaxt="n")
hf_repro <- projectRaster(hf, mat)
plot(hf_repro)

r <- calc(hf_repro, fun=function(x){ x[x > 50] <- NA; return(x)} )
plot(r, col = pal(50))
temp <- calc(r, fun=function(x){ x[x >= 0] <- 0; return(x)} )
plot(temp)
plot(temp, col = pal(50))

sp <- sp[, c("x", "y", "species")]

# sp$val <- 1
# bees <- rasterFromXYZ(sp[, c("x", "y", "val")])
# bees <- aggregate(bees, fact = 36)
# plot(bees)

plot(sp)
sp2 <- SpatialPointsDataFrame(sp[,c("x", "y")], 
                              as.data.frame(sp[,3]),
                              proj4string =  temp@crs)


rast_list <- list()
for (i in unique(sp$species)){
        s <- sp[sp$species == i, ]
        sp2 <- SpatialPointsDataFrame(s[,c("x", "y")], 
                                      as.data.frame(s[,3]),
                                      proj4string =  temp@crs)
        rast_list[i] <- rasterize(sp2, temp, field = 1)
}
        
all <- brick(r, temp, rast_list)
plot(all)
all <- aggregate(all, fact= 6)

        
plot(sp2)
sp3 <- rasterize(sp2, temp, field = 1)
plot(sp3)
all <- brick(r, temp, sp3)
plot(all)
all <- aggregate(all, fact= 6)

sp4 <- projectRaster(all, crs = "+proj=laea +lat_0=53 +lon_0=9 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
plot(sp4)


## #extract climate values for coordinates in (full) PREDICTS dataset
ratio_data <- as.data.frame(sp4, xy = T)


 # library(sf)
 # h <- crop(hf_map, extent(-33,67,30, 82))
 # d <- st_make_grid(temp, cellsize = 0.5)
 # par(bg = "white")
 # plot(temp)
 # par(bg = "transparent")
 # plot(d)
 # plot(h)
 # plot(bees)
 
 
 
 plot()
 
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
