## 03f_sourceable_range_analysis

# setwd("/Users/macbookpro/Library/CloudStorage/OneDrive-Personal/PhD/spatialpattern_climate_humanfootprint")
# source("03f_sourceable_range_analysis.R")
 
print("sourcing range models script")

source("00_sp_functions.R")
## metrics ~ human footprint, climate change velocity and climate data --------
## read in and handle data------------------------------------------------------------------------------------------------

metrics <- readRDS("Data_metrics_for_range_analysis.rds")
clean_tree <- read.tree("Data_sp_clean_tree.tre")

names(metrics) <- c("species", "total.area", "range.size", "effective.mesh.size", "prop.landscape", "mean.shape.index", "perimeter.area.frac.dim", 
                    "hf_mean", "vel_mean", "mat_mean", "mat_var_mean", "map_mean", "map_var_mean",
                    "hf_range", "vel_range", "mat_range", "mat_var_range", "map_range", "map_var_range")


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


## hf model --------------
# print("prep hf model")
# 
# ## formula ------------------
# ## set the formula for each spatial pattern metric
# f <- list()
# f[["total.area"]]  <- total.area ~ hf_range
# f[["range.size"]] <- range.size ~ hf_range
# f[["effective.mesh.size"]] <-  effective.mesh.size ~ hf_range
# f[["prop.landscape"]] <- prop.landscape ~ hf_range
# f[["mean.shape.index"]] <- mean.shape.index ~ hf_range
# f[["perimeter.area.frac.dim"]] <- perimeter.area.frac.dim ~ hf_range
# 
# print("hf forumal set")
# 
# print("start running hf models")
# 
#
## model ---------------
# m_metric <- list()
# 
#  for(j in names(comp_data[["data"]][which(names(comp_data[["data"]]) %in% c("total.area", "range.size", "effective.mesh.size", "prop.landscape", "mean.shape.index", 
#                                                                             "perimeter.area.frac.dim"))])){
# 
#                                                                              
#     formula <- f[[j]]
#   
#   print(j)
#   m_metric[[j]][["hf"]]  <- mod_list <- mclapply(1:2, function(i) {
#     MCMCglmm(fixed = formula,
#              random = ~ animal,
#              rcov = ~ units,
#              family= "gaussian",
#              pedigree = comp_data$tree,
#              data = comp_data$data,
#              nitt = nitt,
#              burnin = burnin,
#              thin = thin,
#              prior = prior)
#   }, mc.cores=2)
#   gc()
#   Sys.sleep(20)}
# 
# print("saving models")
# saveRDS(m_metric, "m_metric_range_hf.rds")
# rm(m_metric)
# gc()

# ## climate model --------------
# print("prep climate model")
# 
# ## formula ------------------
# ## set the formula for each spatial pattern metric
# f <- list()
# f[["total.area"]]  <- total.area ~ map_range*mat_range*map_var_range*mat_var_range
# f[["range.size"]] <- range.size ~ map_range*mat_range*map_var_range*mat_var_range
# f[["effective.mesh.size"]] <-  effective.mesh.size ~ map_range*mat_range*map_var_range*mat_var_range
# f[["prop.landscape"]] <- prop.landscape ~ map_range*mat_range*map_var_range*mat_var_range
# f[["mean.shape.index"]] <- mean.shape.index ~ map_range*mat_range*map_var_range*mat_var_range
# f[["perimeter.area.frac.dim"]] <- perimeter.area.frac.dim ~ map_range*mat_range*map_var_range*mat_var_range
# 
# print("climate forumal set")
# 
# print("start running climate models")
# 
# ## model ---------------
# m_metric <- list()
# 
# for(j in names(comp_data[["data"]][which(names(comp_data[["data"]]) %in% c("total.area", "range.size", "effective.mesh.size", "prop.landscape", "mean.shape.index", 
#                                                                            "perimeter.area.frac.dim"))])){
#   formula <- f[[j]]
#   
#   print(j)
#   m_metric[[j]][["hf"]]  <- mod_list <- mclapply(1:2, function(i) {
#     MCMCglmm(fixed = formula,
#              random = ~ animal,
#              rcov = ~ units,
#              family= "gaussian",
#              pedigree = comp_data$tree,
#              data = comp_data$data,
#              nitt = nitt,
#              burnin = burnin,
#              thin = thin,
#              prior = prior)
#   }, mc.cores=2)
#   gc()
#   Sys.sleep(20)}
# 
# print("saving models")
# saveRDS(m_metric, "m_metric_range_clim.rds")
# rm(m_metric)
# gc()
# 
# ## climate and hf model --------------
# print("prep climate and hf model")
# 
# ## formula ------------------
# ## set the formula for each spatial pattern metric
# f <- list()
# f[["total.area"]]  <- total.area ~ hf_range*map_range*mat_range*map_var_range*mat_var_range
# f[["range.size"]] <- range.size ~ hf_range*map_range*mat_range*map_var_range*mat_var_range
# f[["effective.mesh.size"]] <-  effective.mesh.size ~ hf_range*map_range*mat_range*map_var_range*mat_var_range
# f[["prop.landscape"]] <- prop.landscape ~ hf_range*map_range*mat_range*map_var_range*mat_var_range
# f[["mean.shape.index"]] <- mean.shape.index ~ hf_range*map_range*mat_range*map_var_range*mat_var_range
# f[["perimeter.area.frac.dim"]] <- perimeter.area.frac.dim ~ hf_range*map_range*mat_range*map_var_range*mat_var_range
# 
# print("climate and hf forumal set")
# 
# print("start running climate and hf models")
# 
# ## model ---------------
# m_metric <- list()
# 
# for(j in names(comp_data[["data"]][which(names(comp_data[["data"]]) %in% c("total.area", "range.size", "effective.mesh.size", "prop.landscape", "mean.shape.index", 
#                                                                            "perimeter.area.frac.dim"))])){
#   formula <- f[[j]]
#   
#   print(j)
#   m_metric[[j]][["hf"]]  <- mod_list <- mclapply(1:2, function(i) {
#     MCMCglmm(fixed = formula,
#              random = ~ animal,
#              rcov = ~ units,
#              family= "gaussian",
#              pedigree = comp_data$tree,
#              data = comp_data$data,
#              nitt = nitt,
#              burnin = burnin,
#              thin = thin,
#              prior = prior)
#   }, mc.cores=2)
#   gc()
#   Sys.sleep(20)}
# 
# print("saving models")
# saveRDS(m_metric, "m_metric_range_clim_hf.rds")
# rm(m_metric)
# gc()
# 
# ## vel, climate and hf model --------------
# print("prep vel, climate and hf model")
# 
# ## formula ------------------
# ## set the formula for each spatial pattern metric
# f <- list()
# f[["total.area"]]  <- total.area ~ vel_range*hf_range*map_range*mat_range*map_var_range*mat_var_range
# f[["range.size"]] <- range.size ~ vel_range*hf_range*map_range*mat_range*map_var_range*mat_var_range
# f[["effective.mesh.size"]] <-  effective.mesh.size ~ vel_range*hf_range*map_range*mat_range*map_var_range*mat_var_range
# f[["prop.landscape"]] <- prop.landscape ~ vel_range*hf_range*map_range*mat_range*map_var_range*mat_var_range
# f[["mean.shape.index"]] <- mean.shape.index ~ vel_range*hf_range*map_range*mat_range*map_var_range*mat_var_range
# f[["perimeter.area.frac.dim"]] <- perimeter.area.frac.dim ~ vel_range*hf_range*map_range*mat_range*map_var_range*mat_var_range
# 
# print("vel, climate and hf forumal set")
# 
# print("start running vel, climate and hf models")
# 
# ## model ---------------
# m_metric <- list()
# 
# for(j in names(comp_data[["data"]][which(names(comp_data[["data"]]) %in% c("total.area", "range.size", "effective.mesh.size", "prop.landscape", "mean.shape.index", 
#                                                                            "perimeter.area.frac.dim"))])){
#   formula <- f[[j]]
#   
#   print(j)
#   m_metric[[j]][["hf"]]  <- mod_list <- mclapply(1:2, function(i) {
#     MCMCglmm(fixed = formula,
#              random = ~ animal,
#              rcov = ~ units,
#              family= "gaussian",
#              pedigree = comp_data$tree,
#              data = comp_data$data,
#              nitt = nitt,
#              burnin = burnin,
#              thin = thin,
#              prior = prior)
#   }, mc.cores=2)
#   gc()
#   Sys.sleep(20)}
# 
# print("saving models")
# saveRDS(m_metric, "m_metric_range_vel_clim_hf.rds")
# rm(m_metric)
# gc()
# 


## vel and climate model --------------
print("prep vel abd climate model")

## formula ------------------
## set the formula for each spatial pattern metric
f <- list()
f[["total.area"]]  <- total.area ~ vel_range*map_range*mat_range*map_var_range*mat_var_range
f[["range.size"]] <- range.size ~ vel_range*map_range*mat_range*map_var_range*mat_var_range
f[["effective.mesh.size"]] <-  effective.mesh.size ~ vel_range*map_range*mat_range*map_var_range*mat_var_range
f[["prop.landscape"]] <- prop.landscape ~ vel_range*map_range*mat_range*map_var_range*mat_var_range
f[["mean.shape.index"]] <- mean.shape.index ~ vel_range*map_range*mat_range*map_var_range*mat_var_range
f[["perimeter.area.frac.dim"]] <- perimeter.area.frac.dim ~ vel_range*map_range*mat_range*map_var_range*mat_var_range

print("vel and climate forumal set")

print("start running vel and climate models")

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
  gc()
  Sys.sleep(20)}

print("saving models")
saveRDS(m_metric, "m_metric_range_vel_clim.rds")
rm(m_metric)
gc()



# ## vel only --------------
# print("prep vel model")
# 
# ## formula ------------------
# ## set the formula for each spatial pattern metric
# f <- list()
# f[["total.area"]]  <- total.area ~ vel_range
# f[["range.size"]] <- range.size ~ vel_range
# f[["effective.mesh.size"]] <-  effective.mesh.size ~ vel_range
# f[["prop.landscape"]] <- prop.landscape ~ vel_range
# f[["mean.shape.index"]] <- mean.shape.index ~ vel_range
# f[["perimeter.area.frac.dim"]] <- perimeter.area.frac.dim ~ vel_range
# 
# print("vel set")
# 
# print("start running vel models")
# 
# ## model ---------------
# m_metric <- list()
# 
# for(j in names(comp_data[["data"]][which(names(comp_data[["data"]]) %in% c("total.area", "range.size", "effective.mesh.size", "prop.landscape", "mean.shape.index",
#                                                                            "perimeter.area.frac.dim"))])){
#   formula <- f[[j]]
# 
#   print(j)
#   m_metric[[j]][["hf"]]  <- mod_list <- mclapply(1:2, function(i) {
#     MCMCglmm(fixed = formula,
#              random = ~ animal,
#              rcov = ~ units,
#              family= "gaussian",
#              pedigree = comp_data$tree,
#              data = comp_data$data,
#              nitt = nitt,
#              burnin = burnin,
#              thin = thin,
#              prior = prior)
#   }, mc.cores=2)
#   gc()
#   Sys.sleep(20)}
# 
# print("saving models")
# saveRDS(m_metric, "m_metric_range_vel.rds")
# rm(m_metric)
# gc()

print("end")
 
