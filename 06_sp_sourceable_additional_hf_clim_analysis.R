## 06_sp_sourceable_additional_hf_clim_analysis

# setwd("/Users/caroline/OneDrive/PhD/spatialpattern_climate_humanfootprint")
# source("06_sp_sourceable_additional_hf_clim_analysis.R")

print("sourcing hf and climate models script")

source("00_sp_functions.R")
## metrics ~ human footprint, climate change velocity and climate data --------
## read in and handle data------------------------------------------------------------------------------------------------

metrics <- readRDS("Data_metrics_for_hf_analysis.rds")
clean_tree <- read.tree("Data_sp_clean_tree.tre")
rat <- readRDS("Data_ratios_dataframe.rds")

metrics <- left_join(metrics, rat[,c("species", "hf_range", "vel_range", "mat_range", 
                                     "mat_var_range", "map_range", "map_var_range")], by = "species")
metrics <- metrics[, c("species", "Occupied area", "Geographic range size", "Patch size distribution", 
                       "Geographic range filling", 
                       "hf_mean", "vel_mean", "mat_mean", "mat_var_mean", "map_mean", 
                       "map_var_mean", "hf_range", "vel_range", "mat_range", "mat_var_range", 
                       "map_range", "map_var_range")]

par(mfrow = c(3,3))
for (i in names(Filter(is.numeric, metrics))) {
  hist((metrics[,i]),
       breaks = 3000,
       main = paste(i),
       xlab = paste(i))
}


## log and scale raw data
l <- c("vel_range", "mat_var_range")

for (i in l) {
  metrics[, i] <- c(log(metrics[,i]))
}
for (i in names(Filter(is.numeric, metrics))) {
  metrics[, i] <- c(scale(metrics[,i]))
}
length(unique(metrics$species)) ## 650 unique species


names(metrics) <- c("species", "total.area", "range.size", "effective.mesh.size", "prop.landscape",
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
print("prep hf model")

## formula ------------------
## set the formula for each spatial pattern metric
f <- list()
f[["total.area"]]  <- total.area ~ hf_range
f[["range.size"]] <- range.size ~ hf_range
f[["effective.mesh.size"]] <-  effective.mesh.size ~ hf_range
f[["prop.landscape"]] <- prop.landscape ~ hf_range


print("hf forumal set")

print("start running hf models")

## model ---------------
m_metric <- list()

for(j in names(comp_data[["data"]][which(names(comp_data[["data"]]) %in% c("total.area", "range.size", "effective.mesh.size", "prop.landscape"))])){
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
saveRDS(m_metric, "06_m_metric_hf.rds")

# 
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
# 
# 
# print("climate forumal set")
# 
# print("start running hf and climate models")
# 
# ## model ---------------
# m_metric <- list()
# 
# for(j in names(comp_data[["data"]][which(names(comp_data[["data"]]) %in% c("total.area", "range.size", "effective.mesh.size", "prop.landscape"))])){
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
#   Sys.sleep(20)}
# 
# print("saving models")
# saveRDS(m_metric, "06_m_metric_clim.rds")
# 
# 
# ## hf and climate model --------------
# print("prep hf and climate model")
# 
# ## formula ------------------
# ## set the formula for each spatial pattern metric
# f <- list()
# f[["total.area"]]  <- total.area ~ hf_range*map_range*mat_range*map_var_range*mat_var_range
# f[["range.size"]] <- range.size ~ hf_range*map_range*mat_range*map_var_range*mat_var_range
# f[["effective.mesh.size"]] <-  effective.mesh.size ~ hf_range*map_range*mat_range*map_var_range*mat_var_range
# f[["prop.landscape"]] <- prop.landscape ~ hf_range*map_range*mat_range*map_var_range*mat_var_range
# 
# 
# print("hf and climate forumal set")
# 
# print("start running hf and climate models")
# 
# ## model ---------------
# m_metric <- list()
# 
# for(j in names(comp_data[["data"]][which(names(comp_data[["data"]]) %in% c("total.area", "range.size", "effective.mesh.size", "prop.landscape"))])){
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
#   Sys.sleep(20)}
# 
# print("saving models")
# saveRDS(m_metric, "06_m_metric_clim_hf.rds")
# 
# ## vel and climate model --------------
# print("prep vel and climate model")
# 
# 
# ## formula ------------------
# ## set the formula for each spatial pattern metric
# f <- list()
# f[["total.area"]]  <- total.area ~ vel_range*map_range*mat_range*map_var_range*mat_var_range
# f[["range.size"]] <- range.size ~ vel_range*map_range*mat_range*map_var_range*mat_var_range
# f[["effective.mesh.size"]] <-  effective.mesh.size ~ vel_range*map_range*mat_range*map_var_range*mat_var_range
# f[["prop.landscape"]] <- prop.landscape ~ vel_range*map_range*mat_range*map_var_range*mat_var_range
# 
# 
# print("vel and climate forumal set")
# 
# print("start running vel and climate models")
# 
# ## model ---------------
# m_metric <- list()
# 
# for(j in names(comp_data[["data"]][which(names(comp_data[["data"]]) %in% c("total.area", "range.size", "effective.mesh.size", "prop.landscape"))])){
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
#   Sys.sleep(20)}
# 
# print("saving models")
# saveRDS(m_metric, "06_m_metric_vel_clim.rds")
# 
# ## vel, hf and climate model --------------
# print("prep vel, hf and climate model")
# 
# 
# ## formula ------------------
# ## set the formula for each spatial pattern metric
# f <- list()
# f[["total.area"]]  <- total.area ~ vel_range*hf_range*map_range*mat_range*map_var_range*mat_var_range
# f[["range.size"]] <- range.size ~ vel_range*hf_range*map_range*mat_range*map_var_range*mat_var_range
# f[["effective.mesh.size"]] <-  effective.mesh.size ~ vel_range*hf_range*map_range*mat_range*map_var_range*mat_var_range
# f[["prop.landscape"]] <- prop.landscape ~ vel_range*hf_range*map_range*mat_range*map_var_range*mat_var_range
# 
# 
# print("vel, hf and climate forumal set")
# 
# print("start running vel, hf and climate models")
# 
# ## model ---------------
# m_metric <- list()
# 
# for(j in names(comp_data[["data"]][which(names(comp_data[["data"]]) %in% c("total.area", "range.size", "effective.mesh.size", "prop.landscape"))])){
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
#   Sys.sleep(20)}
# 
# print("saving models")
# saveRDS(m_metric, "06_m_metric_vel_hf_clim.rds")
# 
# print("end")

## two way full model --------------
print("prep two way full model")


## formula ------------------
## set the formula for each spatial pattern metric
f <- list()
f[["total.area"]]  <- total.area ~ vel_mean*hf_mean + 
  vel_mean*map_mean +
  vel_mean*mat_mean + 
  vel_mean*map_var_mean + 
  vel_mean*mat_var_mean +
  map_mean*hf_mean +
  map_mean*mat_mean +
  map_mean*map_var_mean +
  map_mean*mat_var_mean +
  mat_mean*hf_mean +
  mat_mean*mat_var_mean +
  mat_mean*map_var_mean +
  map_var_mean*hf_mean +
  map_var_mean*mat_var_mean +
  mat_var_mean*hf_mean
f[["range.size"]] <- range.size ~ vel_mean*hf_mean + 
  vel_mean*map_mean +
  vel_mean*mat_mean + 
  vel_mean*map_var_mean + 
  vel_mean*mat_var_mean +
  map_mean*hf_mean +
  map_mean*mat_mean +
  map_mean*map_var_mean +
  map_mean*mat_var_mean +
  mat_mean*hf_mean +
  mat_mean*mat_var_mean +
  mat_mean*map_var_mean +
  map_var_mean*hf_mean +
  map_var_mean*mat_var_mean +
  mat_var_mean*hf_mean
f[["effective.mesh.size"]] <-  effective.mesh.size ~ vel_mean*hf_mean + 
  vel_mean*map_mean +
  vel_mean*mat_mean + 
  vel_mean*map_var_mean + 
  vel_mean*mat_var_mean +
  map_mean*hf_mean +
  map_mean*mat_mean +
  map_mean*map_var_mean +
  map_mean*mat_var_mean +
  mat_mean*hf_mean +
  mat_mean*mat_var_mean +
  mat_mean*map_var_mean +
  map_var_mean*hf_mean +
  map_var_mean*mat_var_mean +
  mat_var_mean*hf_mean
f[["prop.landscape"]] <- prop.landscape ~ vel_mean*hf_mean + 
  vel_mean*map_mean +
  vel_mean*mat_mean + 
  vel_mean*map_var_mean + 
  vel_mean*mat_var_mean +
  map_mean*hf_mean +
  map_mean*mat_mean +
  map_mean*map_var_mean +
  map_mean*mat_var_mean +
  mat_mean*hf_mean +
  mat_mean*mat_var_mean +
  mat_mean*map_var_mean +
  map_var_mean*hf_mean +
  map_var_mean*mat_var_mean +
  mat_var_mean*hf_mean


print("two way full forumal set")

print("start running two way full models")

## model ---------------
m_metric <- list()

for(j in names(comp_data[["data"]][which(names(comp_data[["data"]]) %in% c("total.area", "range.size", "effective.mesh.size", "prop.landscape"))])){
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
saveRDS(m_metric, "06_m_metric_full2way.rds")



## Nakagawa's R2 for MCMCGlmm ------------------
## THANK YOU NAKAGAWA
r <- c("total.area", "range.size", "effective.mesh.size", "prop.landscape")

m_metric_hf <- readRDS("06_m_metric_hf.rds")
m_metric_clim <- readRDS("06_m_metric_clim.rds")
m_metric_clim_hf <- readRDS("06_m_metric_clim_hf.rds")
m_metric_vel_clim <- readRDS("06_m_metric_vel_clim.rds")
m_metric_vel_hf_clim <- readRDS("06_m_metric_vel_hf_clim.rds")

## hf  models
rsqrd <- as.data.frame(r)
for(i in r){
  mmF <- m_metric_hf[[i]][["hf"]][[1]]
  
  # MCMCglmm - marginal with crebile intervals
  vmVarF<-numeric(1000)
  for(j in 1:1000){
    Var<-var(as.vector(mmF$Sol[j,] %*% t(mmF$X)))
    vmVarF[j]<-Var}
  
  R2m<-vmVarF/(vmVarF+mmF$VCV[,1]+mmF$VCV[,2])
  rsqrd$mean_r2_mar[rsqrd$r == i] <- mean(R2m)
  rsqrd$mode_r2_mar[rsqrd$r == i] <-  posterior.mode(R2m)
  rsqrd$lower_r2_mar[rsqrd$r == i] <- HPDinterval(R2m)[1]
  rsqrd$upper_r2_mar[rsqrd$r == i] <- HPDinterval(R2m)[2]
  
  # MCMCglmm - conditional with crebile intervals
  R2c<-(vmVarF+mmF$VCV[,1])/(vmVarF+mmF$VCV[,1]+mmF$VCV[,2])
  rsqrd$mean_r2_cond[rsqrd$r == i] <-mean(R2c)
  rsqrd$mode_r2_cond[rsqrd$r == i] <-posterior.mode(R2c)
  rsqrd$lower_r2_cond[rsqrd$r == i] <-HPDinterval(R2c)[1]
  rsqrd$upper_r2_cond[rsqrd$r == i] <-HPDinterval(R2c)[2]
}

r2_hf <- rsqrd
r2_hf$model <- "hf"

## climate  models
rsqrd <- as.data.frame(r)
for(i in r){
  mmF <- m_metric_clim[[i]][["hf"]][[1]]
  
  # MCMCglmm - marginal with crebile intervals
  vmVarF<-numeric(1000)
  for(j in 1:1000){
    Var<-var(as.vector(mmF$Sol[j,] %*% t(mmF$X)))
    vmVarF[j]<-Var}
  
  R2m<-vmVarF/(vmVarF+mmF$VCV[,1]+mmF$VCV[,2])
  rsqrd$mean_r2_mar[rsqrd$r == i] <- mean(R2m)
  rsqrd$mode_r2_mar[rsqrd$r == i] <-  posterior.mode(R2m)
  rsqrd$lower_r2_mar[rsqrd$r == i] <- HPDinterval(R2m)[1]
  rsqrd$upper_r2_mar[rsqrd$r == i] <- HPDinterval(R2m)[2]
  
  # MCMCglmm - conditional with crebile intervals
  R2c<-(vmVarF+mmF$VCV[,1])/(vmVarF+mmF$VCV[,1]+mmF$VCV[,2])
  rsqrd$mean_r2_cond[rsqrd$r == i] <-mean(R2c)
  rsqrd$mode_r2_cond[rsqrd$r == i] <-posterior.mode(R2c)
  rsqrd$lower_r2_cond[rsqrd$r == i] <-HPDinterval(R2c)[1]
  rsqrd$upper_r2_cond[rsqrd$r == i] <-HPDinterval(R2c)[2]
}

r2_clim <- rsqrd
r2_clim$model <- "clim"

## climate and human footprint models
rsqrd <- as.data.frame(r)
for(i in r){
  mmF <- m_metric_clim_hf[[i]][["hf"]][[1]]
  
  # MCMCglmm - marginal with crebile intervals
  vmVarF<-numeric(1000)
  for(j in 1:1000){
    Var<-var(as.vector(mmF$Sol[j,] %*% t(mmF$X)))
    vmVarF[j]<-Var}
  
  R2m<-vmVarF/(vmVarF+mmF$VCV[,1]+mmF$VCV[,2])
  rsqrd$mean_r2_mar[rsqrd$r == i] <- mean(R2m)
  rsqrd$mode_r2_mar[rsqrd$r == i] <-  posterior.mode(R2m)
  rsqrd$lower_r2_mar[rsqrd$r == i] <- HPDinterval(R2m)[1]
  rsqrd$upper_r2_mar[rsqrd$r == i] <- HPDinterval(R2m)[2]
  
  # MCMCglmm - conditional with crebile intervals
  R2c<-(vmVarF+mmF$VCV[,1])/(vmVarF+mmF$VCV[,1]+mmF$VCV[,2])
  rsqrd$mean_r2_cond[rsqrd$r == i] <-mean(R2c)
  rsqrd$mode_r2_cond[rsqrd$r == i] <-posterior.mode(R2c)
  rsqrd$lower_r2_cond[rsqrd$r == i] <-HPDinterval(R2c)[1]
  rsqrd$upper_r2_cond[rsqrd$r == i] <-HPDinterval(R2c)[2]
}

r2_clim_hf <- rsqrd
r2_clim_hf$model <- "clim_hf"


## vel_clim
rsqrd <- as.data.frame(r)
for(i in r){
  mmF <- m_metric_vel_clim[[i]][["hf"]][[1]]
  
  # MCMCglmm - marginal with crebile intervals
  vmVarF<-numeric(1000)
  for(j in 1:1000){
    Var<-var(as.vector(mmF$Sol[j,] %*% t(mmF$X)))
    vmVarF[j]<-Var}
  
  R2m<-vmVarF/(vmVarF+mmF$VCV[,1]+mmF$VCV[,2])
  rsqrd$mean_r2_mar[rsqrd$r == i] <- mean(R2m)
  rsqrd$mode_r2_mar[rsqrd$r == i] <-  posterior.mode(R2m)
  rsqrd$lower_r2_mar[rsqrd$r == i] <- HPDinterval(R2m)[1]
  rsqrd$upper_r2_mar[rsqrd$r == i] <- HPDinterval(R2m)[2]
  
  # MCMCglmm - conditional with crebile intervals
  R2c<-(vmVarF+mmF$VCV[,1])/(vmVarF+mmF$VCV[,1]+mmF$VCV[,2])
  rsqrd$mean_r2_cond[rsqrd$r == i] <-mean(R2c)
  rsqrd$mode_r2_cond[rsqrd$r == i] <-posterior.mode(R2c)
  rsqrd$lower_r2_cond[rsqrd$r == i] <-HPDinterval(R2c)[1]
  rsqrd$upper_r2_cond[rsqrd$r == i] <-HPDinterval(R2c)[2]
}

r2_vel_clim <- rsqrd
r2_vel_clim$model <- "vel_clim"


## vel hf clim
rsqrd <- as.data.frame(r)
for(i in r){
  mmF <- m_metric_vel_hf_clim[[i]][["hf"]][[1]]
  
  # MCMCglmm - marginal with crebile intervals
  vmVarF<-numeric(1000)
  for(j in 1:1000){
    Var<-var(as.vector(mmF$Sol[j,] %*% t(mmF$X)))
    vmVarF[j]<-Var}
  
  R2m<-vmVarF/(vmVarF+mmF$VCV[,1]+mmF$VCV[,2])
  rsqrd$mean_r2_mar[rsqrd$r == i] <- mean(R2m)
  rsqrd$mode_r2_mar[rsqrd$r == i] <-  posterior.mode(R2m)
  rsqrd$lower_r2_mar[rsqrd$r == i] <- HPDinterval(R2m)[1]
  rsqrd$upper_r2_mar[rsqrd$r == i] <- HPDinterval(R2m)[2]
  
  # MCMCglmm - conditional with crebile intervals
  R2c<-(vmVarF+mmF$VCV[,1])/(vmVarF+mmF$VCV[,1]+mmF$VCV[,2])
  rsqrd$mean_r2_cond[rsqrd$r == i] <-mean(R2c)
  rsqrd$mode_r2_cond[rsqrd$r == i] <-posterior.mode(R2c)
  rsqrd$lower_r2_cond[rsqrd$r == i] <-HPDinterval(R2c)[1]
  rsqrd$upper_r2_cond[rsqrd$r == i] <-HPDinterval(R2c)[2]
}

r2_vel_hf_clim <- rsqrd
r2_vel_hf_clim$model <- "vel_hf_clim"

rmean <- readRDS("Data_r2_all_models.rds")
rmean <- rmean[rmean$model %in% c("null"),]
r2 <- rbind(rmean, r2_hf, r2_clim, r2_clim_hf,
            r2_vel_clim, r2_vel_hf_clim)


saveRDS(r2, "06_r2_additional_models.rds")

print("end")


