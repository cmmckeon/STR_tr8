## 03_sp_analysis
 

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


## log and scale raw data
metrics$'Geographic range fractality' <- (metrics$'Geographic range fractality' + sqrt(min(metrics$'Geographic range fractality', na.rm = T)^2)) + 1

l <- c("Occupied area", "Geographic range size", "Patch size distribution", 
       "Patch shape complexity","vel_mean", "mat_var_mean", "map_mean", "map_var_mean")

for (i in l) {
  metrics[, i] <- c(log(metrics[,i]))
}
for (i in names(Filter(is.numeric, metrics))) {
  metrics[, i] <- c(scale(metrics[,i]))
}
length(unique(metrics$species)) ## 650 unique species

#metrics <- readRDS("Data_metrics_for_hf_analysis.rds")

# list <- c()
# for (i in names(metrics)){
#   list[i] <-length(which(is.na(metrics[,i]))) }
# print(list) ## all variables should be zero

#saveRDS(metrics, "Data_metrics_for_hf_analysis.rds")


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

res <- rcorr(as.matrix(m[, which(names(m) %in% c(r, "hf_mean", "vel_mean", "mat_mean", "mat_var_mean", "map_mean", "map_var_mean"))]))


## prep analysis ---------------------------------
mcmc_data <- metrics
mcmc_data$animal <- mcmc_data$species
## create comparative dataset
comp_data <- clean.data(mcmc_data, clean_tree, data.col = "animal")


## source modelling code -------------------

source("03a_sourceable_clim_analysis.R")
source("03b_sourceable_hf_analysis.R")
source("03c_sourceable_hf_clim_analysis.R")

m_metric_null <- readRDS("m_metric_null.rds")
m_metric_hf <- readRDS("m_metric_hf.rds")
m_metric_clim <- readRDS("m_metric_clim.rds")
m_metric_clim_hf <- readRDS("m_metric_clim_hf.rds")
m_metric_clim_vel <- readRDS("m_metric_vel_clim.rds")
m_metric_clim_hf_vel <- readRDS("m_metric_vel_hf_clim.rds")


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


## rough plots ----------
par(mfrow = c(2,3))
## extract the posterior estimates
rr <- c()
c <- tibble("a", "b")
for(i in r) {
  sum <- as.data.frame(summary(m_metric[[i]][["hf"]][[1]][["Sol"]])[["statistics"]]); 
  c <- rbind(c, c(sum$Mean[1], sum$Mean[2])); 
  rr <- append(rr, paste(i))}
c <- cbind(c[-1,], rr)

k <- list("total.area", "range.size", "effective.mesh.size", "prop.landscape", "mean.shape.index", "perimeter.area.frac.dim")
names(k) <- r

colz <- as.data.frame(cbind(c("total.area", "range.size", "effective.mesh.size", "prop.landscape", "mean.shape.index", "perimeter.area.frac.dim"),
                            c("#7000A8FF","#7000A8FF","grey", "#7000A8FF", "grey",  "grey")))
#par(mfrow = c(2,3), mar=c(4.5,4.5,2,2), col="black", col.main = "black", col.lab = "black")
#par(mfrow = c(2,3), mar=c(4.5,4,2,2), col="white", col.main = "white", col.lab = "white", bg="transparent")
for(i in names(comp_data$data[which(names(comp_data$data) %in%c("total.area", "range.size", "effective.mesh.size", "prop.landscape", 
                                                                "mean.shape.index", "perimeter.area.frac.dim"))])) {
  plot(f[[i]], data = comp_data$data, col = "grey", cex = 0.7, cex.lab = 1.5, cex.main = 1.8,
       ylab = paste(k[[i]]), main = paste(k[[i]]), xlab = paste("nb"), bty = "n")
  abline(c[,1][c$rr ==i], c[,2][c$rr ==i], col = paste(colz$V2[colz$V1 ==i ]), lwd = 6)
}


# xa <- lm(total.area ~ mat_mean*mat_var_mean*map_mean*map_var_mean*mean, data = metrics)
# xb <- lm(total.area ~ mat_mean*mat_var_mean*map_mean*map_var_mean, data = metrics)
# xc <- lm(total.area ~ mean, data = metrics)
# xd <- lm(total.area ~ 1, data = metrics)
# summary(x)


## Nakagawa's R2 for MCMCGlmm ------------------
## THANK YOU NAKAGAWA

## null
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


## human footprint models
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


## vel
rsqrd <- as.data.frame(r)
for(i in r){
  mmF <- m_metric_vel[[i]][["hf"]][[1]]
  
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


## vel_clim
rsqrd <- as.data.frame(r)
for(i in r){
  mmF <- m_metric_clim_vel[[i]][["hf"]][[1]]
  
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
  mmF <- m_metric_clim_hf_vel[[i]][["hf"]][[1]]
  
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


r2 <- rbind(r2_null, r2_clim, r2_hf, r2_clim_hf, #r2_vel, 
            r2_vel_clim, r2_vel_hf_clim)
 saveRDS(r2, "Data_r2_all_models.rds")


