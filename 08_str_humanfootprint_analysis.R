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
plot(sp)

rast_list <- list()
for (i in unique(sp$species)){
        s <- sp[sp$species == i,]
        sp2 <- SpatialPointsDataFrame(s[,c("x", "y")], 
                                      as.data.frame(s[,3]),
                                      proj4string =  temp@crs)
        rast_list[i] <- rasterize(sp2, temp, field = 1)
}
        
all <- brick(r, temp, rast_list)
plot(all)
all <- aggregate(all, fact= 6)

sp4 <- projectRaster(all, crs = "+proj=laea +lat_0=53 +lon_0=9 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
plot(sp4)


## #extract climate values for coordinates in (full) PREDICTS dataset
ratio_data <- as.data.frame(all, xy = T)



f <- ratio_data

for (i in names(ratio_data)[which(names(ratio_data) %nin% c("x", "y", "layer.1", "layer.2"))]){
       ratio_data[,i][which(is.na(ratio_data[,i]))] <- 0
        }
        

rat <- as.data.frame(names(ratio_data)[which(names(ratio_data) %nin% c("x", "y", "layer.1", "layer.2"))])
names(rat) <- "species"
for (i in names(ratio_data)[which(names(ratio_data) %nin% c("x", "y", "layer.1", "layer.2"))]){
        # rat$gm_mean[rat$species == i] <- gm_mean(ratio_data$layer.1[f$layer.2 == 0 & ratio_data[,i] == 1])/
        #         gm_mean(ratio_data$layer.1[ratio_data$layer.2 == 0 & ratio_data[,i] == 0])
        rat$mean[rat$species == i] <- mean(ratio_data$layer.1[f$layer.2 == 0 & ratio_data[,i] == 1], na.rm = TRUE)/
                mean(ratio_data$layer.1[ratio_data$layer.2 == 0 & ratio_data[,i] == 0], na.rm = TRUE)
        rat$median[rat$species == i] <- median(ratio_data$layer.1[f$layer.2 == 0 & ratio_data[,i] == 1], na.rm = TRUE)/
                median(ratio_data$layer.1[ratio_data$layer.2 == 0 & ratio_data[,i] == 0], na.rm = TRUE)
}

par(mfrow = c(3,1))
hist(rat$gm_mean, breaks = 100)
hist(rat$mean, breaks = 100)
hist(rat$median, breaks = 100)



## metrics ~ human footprint ratio --------
## read in and handle data------------------------------------------------------------------------------------------------

metrics <- read.csv("Data_range_metrics.csv") ## metrics provided by Anna Csergo in spring 2019
metrics <- metrics[metrics$Model == "Occurrence",]
metrics <- unique(metrics[, which(names(metrics) %in% 
                                          c("Species", "total.area", "range.size", "effective.mesh.size", "mean.shape.index", "prop.landscape",
                                            "perimeter.area.frac.dim"))])
metrics <- metrics[c("Species", "total.area", "range.size", "effective.mesh.size", "mean.shape.index", "prop.landscape", "perimeter.area.frac.dim")]
names(metrics) <- c("species", "total.area", "range.size", "effective.mesh.size", "mean.shape.index", "prop.landscape", "perimeter.area.frac.dim")

metrics$species <- gsub(" ssp.*", "", metrics$species)
metrics$species <- factor(metrics$species)
levels(metrics$species) <- gsub(" ", "_", levels(metrics$species))
levels(metrics$species) <- gsub("-", ".", levels(metrics$species))
metrics <- metrics[metrics$perimeter.area.frac.dim != "Inf",]

m <- drop_na(metrics)
m$perimeter.area.frac.dim <- (m$perimeter.area.frac.dim + sqrt(min(m$perimeter.area.frac.dim)^2)) + 1
for (i in names(Filter(is.numeric, m[, which(names(m) %nin% c("mean.shape.index", "prop.landscape"))]))) {
        m[, i] <- c(log(m[,i]))
}
for (i in names(Filter(is.numeric, m))) {
        m[, i] <- c(scale(m[,i]))
}


for (i in names(Filter(is.numeric, m))) {
        hist((m[,i]),
             breaks = 3000,
             main = paste(i),
             xlab = paste(i))
}

m <- merge(m, rat, by = "species")

mcmc_data <- m
mcmc_data$animal <- mcmc_data$species
## create comparative dataset
comp_data <- clean.data(mcmc_data, clean_tree, data.col = "animal")

# # priors----------------------------------------------------------------------------------------
## normal prior 
prior <- list(R = list(V=1, nu=0.002), 
              G = list(G1 = list(V=1, nu=0.002)))
# ## this is a parameter expanded prior
a <- 1000
b <- 1
prior<- list(R = list(V=1, nu=0.002),
             G = list(G1 = list(V = diag(b), nu =0.002, alpha.mu = 0, alpha.V = diag(b)*a)))


## parameters-------------------------------------------------------------------------------------
nitt <- c(240000) #no. of interations
burnin <- nitt/6 #length of burnin
thin <- c(20) #amount of thinning
eff_ss <- (nitt-burnin)/thin
print(c("effect size will be:", eff_ss))

## formula ------------------
## set the formula for each spatial pattern metric
f <- list()
f[["total.area"]]  <- total.area ~ gm_mean   
f[["range.size"]] <- range.size ~ gm_mean  
f[["effective.mesh.size"]] <-  effective.mesh.size ~ gm_mean     
f[["mean.shape.index"]] <- mean.shape.index ~ gm_mean        
f[["prop.landscape"]] <- prop.landscape ~ gm_mean
f[["perimeter.area.frac.dim"]] <- perimeter.area.frac.dim ~ gm_mean



## model ---------------
m_metric_hf <- list()

for(j in names(comp_data[["data"]][which(names(comp_data[["data"]]) %in% c("total.area", "range.size", "effective.mesh.size", "mean.shape.index", 
                                                                           "prop.landscape", "perimeter.area.frac.dim"))])){
        formula <- f[[j]]
        
        m_metric_hf[[j]][["hf"]]  <-mod_list <- mclapply(1:2, function(i) {
                MCMCglmm(fixed = formula,
                         random = ~ animal,
                         rcov = ~units,
                         family= "gaussian",
                         pedigree = comp_data$tree,
                         data = comp_data$data,
                         nitt = nitt,
                         burnin = burnin,
                         thin = thin,
                         prior = prior)
        }, mc.cores=2)}

#saveRDS(m_metric_hf, "m_metric_hf.rds")

## diagnostics -------------
z <- "total.area"
z <- "range.size"
z <- 'effective.mesh.size'
z <- "mean.shape.index"
z <- "prop.landscape"
z <- "perimeter.area.frac.dim"

mod_mcmc <- m_metric_hf[[z]][["hf"]][[1]]
mod_mcmc_2 <- m_metric_hf[[z]][["hf"]][[2]]

bay_phylo_dia(mod_mcmc)






 # library(sf)
 # h <- crop(hf_map, extent(-33,67,30, 82))
 # d <- st_make_grid(temp, cellsize = 0.5)
 # par(bg = "white")
 # plot(temp)
 # par(bg = "transparent")
 # plot(d)
 # plot(h)
 # plot(bees)
 



##
# sp2 <- SpatialPointsDataFrame(sp[,c("x", "y")], 
# as.data.frame(sp[,3]),
# proj4string =  temp@crs)
# plot(sp2)
# sp3 <- rasterize(sp2, temp, field = 1)
# plot(sp3)
# all <- brick(r, temp, sp3)
# plot(all)
# all <- aggregate(all, fact= 6)



# sp$val <- 1
# bees <- rasterFromXYZ(sp[, c("x", "y", "val")])
# bees <- aggregate(bees, fact = 36)
# plot(bees)

