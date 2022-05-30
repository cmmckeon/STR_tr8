## 03d_sp_sourceable_map_analysis

# setwd("/Users/macbookpro/Library/CloudStorage/OneDrive-Personal/PhD/spatialpattern_climate_humanfootprint")
# source("03e_sourceable_map_analysis.R")

print("sourcing map models script")

source("00_sp_functions.R")
## metrics ~ human footprint, climate change velocity and climate data --------
## read in and handle data------------------------------------------------------------------------------------------------

metrics <- readRDS("Data_metrics_for_hf_analysis.rds")
clean_tree <- read.tree("Data_sp_clean_tree.tre")

names(metrics) <- c("species", "total.area", "range.size", "effective.mesh.size", "prop.landscape", "mean.shape.index", "perimeter.area.frac.dim", 
                    "hf_mean", "vel_mean", "mat_mean", "mat_var_mean", "map_mean", "map_var_mean")


## prep analysis ---------------------------------
mcmc_data <- metrics
mcmc_data$animal <- mcmc_data$species
## create comparative dataset
comp_data <- clean.data(mcmc_data, clean_tree, data.col = "animal")

# # priors----------------------------------------------------------------------------------------
print("this is a parameter expanded prior")
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
f[["total.area"]]  <- total.area ~ map_mean
f[["range.size"]] <- range.size ~ map_mean
f[["effective.mesh.size"]] <-  effective.mesh.size ~ map_mean
f[["prop.landscape"]] <- prop.landscape ~ map_mean
f[["mean.shape.index"]] <- mean.shape.index ~ map_mean
f[["perimeter.area.frac.dim"]] <- perimeter.area.frac.dim ~ map_mean



print("map forumal set")

print("start running map models")

## model ---------------
m_metric <- list()

for(j in names(comp_data[["data"]][which(names(comp_data[["data"]]) %in% c("total.area", "range.size", "effective.mesh.size", "prop.landscape", "mean.shape.index", 
                                                                           "perimeter.area.frac.dim"))])){
  formula <- f[[j]]
  
  print(j)
  m_metric[[j]][["hf"]]  <- mod_list <- mclapply(1:2, function(i) {
    MCMCglmm(fixed = formula,
             random = ~ animal,
             rcov = ~ units,
             family= "gaussian",
             pedigree = comp_data$tree,
             data = comp_data$data,
             nitt = nitt,
             burnin = burnin,
             thin = thin,
             prior = prior)
  }, mc.cores=2)
  Sys.sleep(20)}

print("saving models")
saveRDS(m_metric, "m_metric_map.rds")

print("end")

