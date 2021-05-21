## 06_str_metric_phylogeny_analysis
## cm 06/05/2021

## metrics ~ phylogeny --------
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
f[["total.area"]]  <- total.area ~ 1   
f[["range.size"]] <- range.size ~ 1  
f[["effective.mesh.size"]] <-  effective.mesh.size ~ 1     
f[["mean.shape.index"]] <- mean.shape.index ~ 1        
f[["prop.landscape"]] <- prop.landscape ~ 1
f[["perimeter.area.frac.dim"]] <- perimeter.area.frac.dim ~ 1



## model ---------------
m_metric_phy <- list()

for(j in names(comp_data[["data"]][which(names(comp_data[["data"]]) %in% c("total.area", "range.size", "effective.mesh.size", "mean.shape.index", 
                                                                           "prop.landscape", "perimeter.area.frac.dim"))])){
  formula <- f[[j]]
  
  m_metric_phy[[j]][["phy"]]  <-mod_list <- mclapply(1:2, function(i) {
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
  }, mc.cores=2)
  
  mod_mcmc <-  m_metric_phy[["phy"]][[j]][[1]]
  mod_mcmc_2 <-  m_metric_phy[["phy"]][[j]][[1]]}

#saveRDS(m_metric_phy, "m_metric_phy.rds")

## diagnostics -------------
z <- "total.area"
z <- "range.size"
z <- 'effective.mesh.size'
z <- "mean.shape.index"
z <- "prop.landscape"
z <- "perimeter.area.frac.dim"

mod_mcmc <- m_metric_phy[[z]][["phy"]][[1]]
mod_mcmc_2 <- m_metric_phy[[z]][["phy"]][[2]]

bay_phylo_dia(mod_mcmc)

r <- c("total.area", "range.size", "effective.mesh.size", "mean.shape.index", "prop.landscape", "perimeter.area.frac.dim")

par(mfrow = c(1, 7))
plot.new()
for(i in r){
  mod_mcmc <- m_metric_phy[[i]][["phy"]][[1]]
  print(i)
  H <- as.numeric(mod_mcmc$VCV[,"animal"]/
    (mod_mcmc$VCV[,"animal"] + mod_mcmc$VCV[,"units"]))
  print(summary(H))
  hist(H, main = paste("phy est", i, sep = " "), breaks = 100, xlim = c(0,1))
}

k <- as.numeric(H)
summary(k)

## traits -------
## height --------

mcmc_data <- height
mcmc_data$animal <- mcmc_data$species
comp_data <- clean.data(mcmc_data, clean_tree, data.col = "animal")

m_height_phy  <- mod_list <- mclapply(1:2, function(i) {
    MCMCglmm(fixed = height_max ~ 1,
             random = ~ animal,
             rcov = ~units,
             family= "gaussian",
             pedigree = comp_data$tree,
             data = comp_data$data,
             nitt = nitt,
             burnin = burnin,
             thin = thin,
             prior = prior)
  }, mc.cores=2)
  
mod_mcmc <-  m_height_phy[[1]]
mod_mcmc_2 <-  m_height_phy[[2]]

bay_phylo_dia(mod_mcmc)

# H <- mod_mcmc$VCV[,"animal"]/
#   (mod_mcmc$VCV[,"animal"] + mod_mcmc$VCV[,"units"])
print(summary(H))


## sla--------

mcmc_data <- sla
mcmc_data$animal <- mcmc_data$species
comp_data <- clean.data(mcmc_data, clean_tree, data.col = "animal")

m_sla_phy  <- mod_list <- mclapply(1:2, function(i) {
  MCMCglmm(fixed = sla_max ~ 1,
           random = ~ animal,
           rcov = ~units,
           family= "gaussian",
           pedigree = comp_data$tree,
           data = comp_data$data,
           nitt = nitt,
           burnin = burnin,
           thin = thin,
           prior = prior)
}, mc.cores=2)

mod_mcmc <-  m_sla_phy[[1]]
mod_mcmc_2 <-  m_sla_phy[[2]]

bay_phylo_dia(mod_mcmc)

H <- mod_mcmc$VCV[,"animal"]/
  (mod_mcmc$VCV[,"animal"] + mod_mcmc$VCV[,"units"])
print(summary(H))

## seed mass--------

mcmc_data <- seed_mass
mcmc_data$animal <- mcmc_data$species
comp_data <- clean.data(mcmc_data, clean_tree, data.col = "animal")

m_seed_mass_phy  <- mod_list <- mclapply(1:2, function(i) {
  MCMCglmm(fixed = seed_mass_max ~ 1,
           random = ~ animal,
           rcov = ~units,
           family= "gaussian",
           pedigree = comp_data$tree,
           data = comp_data$data,
           nitt = nitt,
           burnin = burnin,
           thin = thin,
           prior = prior)
}, mc.cores=2)

mod_mcmc <-  m_seed_mass_phy[[1]]
mod_mcmc_2 <-  m_seed_mass_phy[[2]]

bay_phylo_dia(mod_mcmc)

H <- mod_mcmc$VCV[,"animal"]/
  (mod_mcmc$VCV[,"animal"] + mod_mcmc$VCV[,"units"])
print(summary(H))


## la--------

mcmc_data <- leaf_area
mcmc_data$animal <- mcmc_data$species
comp_data <- clean.data(mcmc_data, clean_tree, data.col = "animal")

m_leaf_area_phy  <- mod_list <- mclapply(1:2, function(i) {
  MCMCglmm(fixed = leaf_area_max ~ 1,
           random = ~ animal,
           rcov = ~units,
           family= "gaussian",
           pedigree = comp_data$tree,
           data = comp_data$data,
           nitt = nitt,
           burnin = burnin,
           thin = thin,
           prior = prior)
}, mc.cores=2)

mod_mcmc <-  m_leaf_area_phy[[1]]
mod_mcmc_2 <-  m_leaf_area_phy[[2]]

bay_phylo_dia(mod_mcmc)

H <- mod_mcmc$VCV[,"animal"]/
  (mod_mcmc$VCV[,"animal"] + mod_mcmc$VCV[,"units"])
print(summary(H))

## woodiness--------
woodiness$binary <- as.numeric(woodiness$woodiness)

mcmc_data <- woodiness
mcmc_data$animal <- mcmc_data$species
comp_data <- clean.data(mcmc_data, clean_tree, data.col = "animal")

m_woodiness_phy  <- mod_list <- mclapply(1:2, function(i) {
  MCMCglmm(fixed = binary ~ 1,
           random = ~ animal,
           rcov = ~units,
           family= "gaussian",
           pedigree = comp_data$tree,
           data = comp_data$data,
           nitt = nitt,
           burnin = burnin,
           thin = thin,
           prior = prior)
}, mc.cores=2)

mod_mcmc <-  m_woodiness_phy[[1]]
mod_mcmc_2 <-  m_woodiness_phy[[2]]

bay_phylo_dia(mod_mcmc)

H <- mod_mcmc$VCV[,"animal"]/
  (mod_mcmc$VCV[,"animal"] + mod_mcmc$VCV[,"units"])
print(summary(H))


## lifeform--------
lifeform$binary <- as.numeric(lifeform$lifeform)

mcmc_data <- lifeform
mcmc_data$animal <- mcmc_data$species
comp_data <- clean.data(mcmc_data, clean_tree, data.col = "animal")

m_lifeform_phy  <- mod_list <- mclapply(1:2, function(i) {
  MCMCglmm(fixed = binary ~ 1,
           random = ~ animal,
           rcov = ~units,
           family= "gaussian",
           pedigree = comp_data$tree,
           data = comp_data$data,
           nitt = nitt,
           burnin = burnin,
           thin = thin,
           prior = prior)
}, mc.cores=2)

mod_mcmc <-  m_lifeform_phy[[1]]
mod_mcmc_2 <-  m_lifeform_phy[[2]]

bay_phylo_dia(mod_mcmc)

H <- mod_mcmc$VCV[,"animal"]/
  (mod_mcmc$VCV[,"animal"] + mod_mcmc$VCV[,"units"])
print(summary(H))


##end








