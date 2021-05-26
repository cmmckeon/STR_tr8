## 07_str_multitrait_analysis
## cm
## 12/04/21
 
# make multi-trait model data frame
multi <- data.frame(unique(mydata[, which(names(mydata) %in% c("species", "total.area", "range.size", "effective.mesh.size", 
                                                           "mean.shape.index", "prop.landscape", "perimeter.area.frac.dim"))]))
# for (i in levels(mydata$trait_name)){
#   b <- unique(merge(b, mydata[,which(names(mydata) %in% c('species', i))], by = "species"))
# }

multi <- droplevels(unique(merge(woodiness[, which(names(woodiness) %in% c("species", "woodiness"))], lifeform, by = "species"))) ## 126 unique
multi <- droplevels(unique(merge(woodiness[, which(names(woodiness) %in% c("species", "woodiness"))], height, by = "species"))) ## 
multi <- droplevels(unique(merge(height[, which(names(height) %in% c("species", "height_max"))], seed_mass, by = "species"))) ## 
multi <- droplevels(unique(merge(lifeform[, which(names(lifeform) %in% c("species", "lifeform"))], height, by = "species"))) ## 
multi <- droplevels(unique(merge(lifeform[, which(names(lifeform) %in% c("species", "lifeform"))], sla, by = "species"))) ## 
multi <- droplevels(unique(merge(woodiness[, which(names(woodiness) %in% c("species", "woodiness"))], seed_mass, by = "species"))) ## 126 unique
multi <- droplevels(unique(merge(multi, height[, which(names(height) %in% c("species", "height_max"))], by = "species"))) ## 124 unique
multi <- droplevels(unique(merge(multi, seed_mass[, which(names(seed_mass) %in% c("species", "seed_mass_max"))], by = "species"))) ## 77 unique
multi <- droplevels(unique(merge(multi, sla[, which(names(sla) %in% c("species", "sla_max"))], by = "species"))) ## 77 unique


## set up ###################

## read in and handle data------------------------------------------------------------------------------------------------
mcmc_data <- multi
mcmc_data$animal <- mcmc_data$species
## create comparative dataset
comp_data <- clean.data(mcmc_data, clean_tree, data.col = "animal")

# # priors----------------------------------------------------------------------------------------
## normal prior (for models including phyeny)
prior <- list(R = list(V=1, nu=0.002), 
              G = list(G1 = list(V=1, nu=0.002)))

## no phyeny prior
 prior <- list(R = list(V=1, nu=0.002))
# 
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

## raw data---------------------------------------------------------------------------------

## look at raw data
par(mfrow = c(2,3))

## Quick look at model dataframe

#Numeric variables
for (i in names(Filter(is.numeric, multi))) {
  hist((multi[,i]),
       breaks = 30,
       main = paste(i),
       xlab = paste(i))
}


## formula ------------------
## set the formula for each spatial pattern metric
f <- list()
f[["total.area"]]  <- (total.area) ~ height_max*woodiness
f[["range.size"]] <- (range.size) ~ height_max*woodiness
f[["effective.mesh.size"]] <-  (effective.mesh.size) ~ height_max*woodiness
f[["mean.shape.index"]] <- mean.shape.index ~ height_max*woodiness     
f[["prop.landscape"]] <- prop.landscape ~ height_max*woodiness
f[["perimeter.area.frac.dim"]] <- (perimeter.area.frac.dim) ~ height_max*woodiness


## for quick checks
m_list <-mod_list <- mclapply(1:2, function(i) {
  MCMCglmm(fixed = (perimeter.area.frac.dim) ~ woodiness+ lifeform + woodiness:lifeform,
         #  random = ~ animal,
           rcov = ~units,
           family= "gaussian",
           pedigree = comp_data$tree,
           data = comp_data$data,
           nitt = nitt,
           burnin = burnin,
           thin = thin,
           prior = prior)
}, mc.cores=2)

mod_mcmc <-  m_list[[1]]
mod_mcmc_2 <- m_list[[2]]



## run a model for each spatial pattern metric
m_multi <- list()

for(j in names(comp_data[["data"]][which(names(comp_data[["data"]]) %in% c("total.area", "range.size", "effective.mesh.size", "mean.shape.index", 
                                                                           "prop.landscape", "perimeter.area.frac.dim"))])){
  formula <- f[[j]]
  
  m_multi[[j]][["multi"]] <-mod_list <- mclapply(1:2, function(i) {
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
  
  mod_mcmc <-  m_multi[["multi"]][[j]][[1]]
  mod_mcmc_2 <-  m_multi[["multi"]][[j]][[2]]}

#saveRDS(m_multi, "m_multi/m_multi_h_w.rds")


## Diagnositcs ----------------------------
z <- "total.area"
z <- "range.size"
z <- 'effective.mesh.size'
z <- "mean.shape.index"
z <- "prop.landscape"
z <- "perimeter.area.frac.dim"

mod_mcmc <- m_multi[[z]][["multi"]][[1]]
mod_mcmc_2 <- m_multi[[z]][["multi"]][[2]]

bay_phylo_dia(mod_mcmc)
summary(mod_mcmc)
#bay_dia(mod_mcmc)

for(i in r){
  print(summary(m_multi[[i]][["multi"]][[1]]))
}


## Climate -------------
#
#
#
#


## formula ------------------
## set the formula for each spatial pattern metric
f <- list()
f[["effective.mesh.size"]] <-  (effective.mesh.size) ~ nb*height_max*lifeform*seed_mass_max     
f[["mean.shape.index"]] <- mean.shape.index ~ nb*height_max*lifeform*seed_mass_max        
f[["prop.landscape"]] <- prop.landscape ~ nb*height_max*lifeform*seed_mass_max 
f[["perimeter.area.frac.dim"]] <- (perimeter.area.frac.dim) ~ nb*height_max*lifeform*seed_mass_max  


## run a model for each spatial pattern metric
m_multi <- list()

for(j in names(comp_data[["data"]][which(names(comp_data[["data"]]) %in% c("effective.mesh.size", "mean.shape.index", 
                                                                           "prop.landscape", "perimeter.area.frac.dim"))])){
  formula <- f[[j]]
  
  m_multi[[j]][["multi"]] <-mod_list <- mclapply(1:2, function(i) {
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
  
  mod_mcmc <-  m_multi[["multi"]][[j]][[1]]
  mod_mcmc_2 <-  m_multi[["multi"]][[j]][[2]]}

#saveRDS(m_multi, "m_multi/m_multi_nb_h_lf_sm.rds")

## Diagnositcs ----------------------------
z <- 'effective.mesh.size'
z <- "mean.shape.index"
z <- "prop.landscape"
z <- "perimeter.area.frac.dim"

mod_mcmc <- m_multi[[z]][["multi"]][[1]]
mod_mcmc_2 <- m_multi[[z]][["multi"]][[2]]

#bay_phylo_dia(mod_mcmc)
summary(mod_mcmc)

for(i in r){
  print(summary(m_multi[[i]][["multi"]][[1]]))
}
