## 03_sp_range_analysis


## metrics ~ human footprint, climate change velocity and climate data --------
## read in and handle data------------------------------------------------------------------------------------------------

rat <- readRDS("Data_ratios_dataframe.rds")
metrics <- merge(metrics, rat, by = "species") 


par(mfrow = c(3,3))
for (i in names(Filter(is.numeric, metrics))) {
  hist((metrics[,i]),
       breaks = 3000,
       main = paste(i),
       xlab = paste(i))
}

## divide the ranges of covars by the total occupied area
# save <- metrics
# l <- c("hf_range", "vel_range", "mat_range", "mat_var_range", 
#   "map_range", "map_var_range")
# for(i in l){
#   metrics[,paste(i, "_div", sep = "")] <- metrics[,i]/metrics$`Geographic range size`
# }
# par(mfrow = c(3,4))
# for(i in l){
#   plot(log(metrics$`Geographic range size`) ~ log(metrics[,i]), main = paste(i))
#   plot(metrics$`Geographic range size` ~ metrics[,paste(i, "_div", sep = "")], main = paste(i))
#   plot(log(metrics$`Geographic range size`) ~ metrics[,paste(i, "_div", sep = "")], main = paste(i))
#   plot(log(metrics$`Geographic range size`) ~ log(metrics[,paste(i, "_div", sep = "")]), main = paste(i))
# 
# }
## prove to yourself that dividing one column by another will make the product related to the denominator
# metrics$x <- sample(1:25, 764, replace=T)
# plot(log(metrics$`Geographic range size`) ~ metrics$x)
# plot(log(metrics$`Geographic range size`) ~ log(metrics$x/(metrics$`Geographic range size`)))
## so i should not divide by geographic range

## log and scale raw data
metrics$'Geographic range fractality' <- (metrics$'Geographic range fractality' + sqrt(min(metrics$'Geographic range fractality', na.rm = T)^2)) + 1

l <- c("Occupied area", "Geographic range size", "Patch size distribution", 
       "Patch shape complexity","vel_mean", "mat_var_mean", "map_mean", "map_var_mean", "vel_range", "mat_var_range")
 
for (i in l) {
  metrics[, i] <- c(log(metrics[,i]))
}
for (i in names(Filter(is.numeric, metrics))) {
  metrics[, i] <- c(scale(metrics[,i]))
}
length(unique(metrics$species)) ## 650 unique species

#metrics <- readRDS("Data_metrics_for_range_analysis.rds")

# list <- c()
# for (i in names(metrics)){
#   list[i] <-length(which(is.na(metrics[,i]))) }
# print(list) ## all variables should be zero

#saveRDS(metrics, "Data_metrics_for_range_analysis.rds")


## visuallise -------------------------------------------
upper.panel<-function(x, y){
  points(x,y, pch=21, col=c("grey"), cex = 0.5)
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  text(0.8, 0.9, txt, cex =0.7)
}

r <- c("Occupied area", "Geographic range size", "Patch size distribution",  "Geographic range filling", "Patch shape complexity", "Geographic range fractality")

m <- drop_na(metrics)
pairs(m[, which(names(m) %nin% c("species"))], 
      lower.panel = NULL, upper.panel = upper.panel)
pairs(m[, which(names(m) %in% c(r,"hf_range", "vel_range", "mat_range", "mat_var_range", "map_range",
                                "map_var_range"))], 
      lower.panel = NULL, upper.panel = upper.panel)
pairs(m[, which(names(m) %in% c("hf_mean", "vel_range", "mat_range", "mat_var_range", "map_range",
                                "map_var_range"))], 
      lower.panel = NULL, upper.panel = upper.panel)

res <- rcorr(as.matrix(m[, which(names(m) %in% c(r, "hf_range", "vel_range", "mat_range", "mat_var_range", "map_range", "map_var_range"))]))
res <- rcorr(as.matrix(m[, which(names(m) %in% c("hf_range", "vel_range", "mat_range", "mat_var_range", "map_range", "map_var_range"))]))


## source modelling code -------------------

#source("03f_sourceable_range_analysis.R")

m_metric_range_hf <- readRDS("m_metric_range_hf.rds")
m_metric_range_clim <- readRDS("m_metric_range_clim.rds")
m_metric_range_clim_hf <- readRDS("m_metric_range_clim_hf.rds")
m_metric_range_vel_clim_hf <- readRDS("m_metric_range_vel_clim_hf.rds")
m_metric_range_vel <- readRDS("m_metric_range_vel.rds")

m_metric <- m_metric_range_hf

#m_metric <- readRDS("m_metric_clim.rds")
## diagnostics -------------
z <- "total.area"
z <- "range.size"
z <- 'effective.mesh.size'
z <- "mean.shape.index"
z <- "prop.landscape"
z <- "perimeter.area.frac.dim"

mod_mcmc <- m_metric[[z]][["hf"]][[1]]
mod_mcmc_2 <- m_metric[[z]][["hf"]][[2]]

bay_phylo_dia(mod_mcmc)

r <- c("total.area", "range.size", "effective.mesh.size", "prop.landscape", "mean.shape.index", "perimeter.area.frac.dim")
for(i in r){
  print(summary(m_metric[[i]][["hf"]][[1]]))
}


## Nakagawa's R2 for MCMCGlmm ------------------
## THANK YOU NAKAGAWA

## null  models
rsqrd <- as.data.frame(r)
for(i in r){
  mmF <- m_metric_null[[i]][["hf"]][[1]]
  
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

r2_null <- rsqrd
r2_null$model <- "null"

## climate  models
rsqrd <- as.data.frame(r)
for(i in r){
  mmF <- m_metric_range_clim[[i]][["hf"]][[1]]
  
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


## human footprint models
rsqrd <- as.data.frame(r)
for(i in r){
  mmF <- m_metric_range_hf[[i]][["hf"]][[1]]
  
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

## climate and human footprint models
rsqrd <- as.data.frame(r)
for(i in r){
  mmF <- m_metric_range_clim_hf[[i]][["hf"]][[1]]
  
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

## vel 
rsqrd <- as.data.frame(r)
for(i in r){
  mmF <- m_metric_range_vel[[i]][["hf"]][[1]]
  
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

r2_vel <- rsqrd
r2_vel$model <- "vel"

## vel clim hf
rsqrd <- as.data.frame(r)
for(i in r){
  mmF <- m_metric_range_vel_clim_hf[[i]][["hf"]][[1]]
  
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

r2_vel_clim_hf <- rsqrd
r2_vel_clim_hf$model <- "vel_clim_hf"

r2 <- rbind(r2_null, r2_hf, r2_clim, r2_clim_hf, r2_vel, r2_vel_clim_hf)
saveRDS(r2, "Data_r2_range_models.rds")


