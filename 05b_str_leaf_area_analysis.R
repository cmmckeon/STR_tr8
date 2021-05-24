#' ---
#' title: "05b_str_bayesian_bivariate_analysis"
#' author: "adapted by Caroline McKeon from script by Kevin Healy"
#' date: ' March 2021'
#' ---
#' 
#' more good resources
#' https://ourcodingclub.github.io/tutorials/mcmcglmm/
#' 
#' https://groups.nceas.ucsb.edu/non-linear-modeling/projects/owls/WRITEUP/owls.pdf/@@download
#'  
#'  http://www.wildanimalmodels.org/tiki-download_wiki_attachment.php?attId=24


## set up ###################

## read in and handle data------------------------------------------------------------------------------------------------
mcmc_data <- leaf_area
mcmc_data$animal <- mcmc_data$species
## create comparative dataset
comp_data <- clean.data(mcmc_data, clean_tree, data.col = "animal")

# # priors----------------------------------------------------------------------------------------
## normal prior (for models including phyeny)
# prior <- list(R = list(V=1, nu=0.002), 
#               G = list(G1 = list(V=1, nu=0.002)))

## no phyeny prior
prior <- list(R = list(V=1, nu=0.002))
# 
# ## this is a parameter expanded prior
# a <- 1000
# b <- 1
# prior<- list(R = list(V=1, nu=0.002),
#              G = list(G1 = list(V = diag(b), nu =0.002, alpha.mu = 0, alpha.V = diag(b)*a)))


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

## Numeric variables
# for (i in names(Filter(is.numeric, leaf_area))) {
#   hist((leaf_area[,i]),
#        breaks = 30,
#        main = paste(i),
#        xlab = paste(i))
# }

plot((total.area) ~ (leaf_area_max), data = comp_data$data)
plot((range.size) ~ (leaf_area_max), data = comp_data$data)
plot((effective.mesh.size) ~ (leaf_area_max), data = comp_data$data)
plot(mean.shape.index~ (leaf_area_max), data = comp_data$data)
plot(prop.landscape~ (leaf_area_max), data = comp_data$data)
plot((perimeter.area.frac.dim) ~ (leaf_area_max), data = comp_data$data)

## formula ------------------
## set the formula for each spatial pattern metric
f <- list()
f[["total.area"]]  <- (total.area) ~ (leaf_area_max)   
f[["range.size"]] <- (range.size) ~ (leaf_area_max)  
f[["effective.mesh.size"]] <-  (effective.mesh.size) ~ (leaf_area_max)     
f[["mean.shape.index"]] <- mean.shape.index ~ (leaf_area_max)        
f[["prop.landscape"]] <- prop.landscape ~ (leaf_area_max)
f[["perimeter.area.frac.dim"]] <- (perimeter.area.frac.dim) ~ (leaf_area_max)


## for quick checks
# m_list <-mod_list <- mclapply(1:2, function(i) {
#   MCMCglmm(fixed = (perimeter.area.frac.dim) ~ (leaf_area_max),
#           #random = ~ animal,
#            rcov = ~units,
#            family= "gaussian",
#            #pedigree = comp_data$tree,
#            data = comp_data$data,
#            nitt = nitt,
#            burnin = burnin,
#            thin = thin,
#            prior = prior)
# }, mc.cores=2)
# 
# mod_mcmc <-  m_list[[1]]
# mod_mcmc_2 <- m_list[[2]]



## run a model for each spatial pattern metric
m_indiv_leaf_area <- list()

for(j in names(comp_data[["data"]][which(names(comp_data[["data"]]) %in% c("total.area", "range.size", "effective.mesh.size", "mean.shape.index", 
                                                                           "prop.landscape", "perimeter.area.frac.dim"))])){
  formula <- f[[j]]
  
  m_indiv_leaf_area[[j]][["leaf_area"]] <-mod_list <- mclapply(1:2, function(i) {
    MCMCglmm(fixed = formula,
            # random = ~ animal,
             rcov = ~units,
             family= "gaussian",
            # pedigree = comp_data$tree,
             data = comp_data$data,
             nitt = nitt,
             burnin = burnin,
             thin = thin,
             prior = prior)
  }, mc.cores=2)
  
  mod_mcmc <-  m_indiv_leaf_area[["leaf_area"]][[j]][[1]]
  mod_mcmc_2 <-  m_indiv_leaf_area[["leaf_area"]][[j]][[2]]}

#saveRDS(m_indiv_leaf_area, "m_leaf_area.rds")

## Diagnositcs ----------------------------
z <- "perimeter.area.frac.dim"

mod_mcmc <- m_indiv_leaf_area[[z]][["leaf_area"]][[1]]
mod_mcmc_2 <- m_indiv_leaf_area[[z]][["leaf_area"]][[2]]

#bay_phylo_dia(mod_mcmc)
bay_dia(mod_mcmc)


## rough plots ----------

par(mfrow = c(2,3))

## extract the posterior estiamtes
rr <- c()
r <- c("total.area", "range.size", "effective.mesh.size", "mean.shape.index", "prop.landscape", "perimeter.area.frac.dim")
c <- tibble("a", "b")
for(i in r) {
  sum <- as.data.frame(summary(m_indiv_leaf_area[[i]][["leaf_area"]][[1]][["Sol"]])[["statistics"]]); 
  c <- rbind(c, c(sum$Mean[1], sum$Mean[2])); 
  rr <- append(rr, paste(i))
}

c <- cbind(c[-1,], rr)

k <- list(" total area", " range size", " effective mesh size", "mean shape index",
          "proportion of landscape", " perimeter area fractality")
names(k) <- r

#par(mfrow = c(2,3), mar=c(4.5,4.5,2,2), col="black", col.main = "black", col.lab = "black")
#par(mfrow = c(2,3), mar=c(4.5,4,2,2), col="white", col.main = "white", col.lab = "white", bg="transparent")
for(i in names(leaf_area[which(names(leaf_area) %in% c("total.area", "range.size", "effective.mesh.size", "mean.shape.index", "prop.landscape", 
                                                 "perimeter.area.frac.dim"))])) {
  plot(f[[i]], data = comp_data$data, col = "grey", cex = 0.7, cex.lab = 1.5, cex.main = 1.8,
       ylab = paste(k[[i]]), main = paste(k[[i]]), xlab = paste(" max leaf_area"), bty = "n")
  abline(c[,1][c$rr ==i], c[,2][c$rr ==i], col = "#7000A8FF", lwd = 6)
}


for(i in r){
  print(summary(m_indiv_leaf_area[[i]][["leaf_area"]][[1]]))
}



## climate ------------
#
#
#
#


## formula ------------------
## set the formula for each spatial pattern metric
f <- list()
f[["effective.mesh.size"]] <-  (effective.mesh.size) ~ leaf_area_max*nb     
f[["mean.shape.index"]] <- mean.shape.index ~ leaf_area_max*nb        
f[["prop.landscape"]] <- prop.landscape ~ leaf_area_max*nb
f[["perimeter.area.frac.dim"]] <- (perimeter.area.frac.dim) ~ leaf_area_max*nb


## run a model for each spatial pattern metric
m_nb_leaf_area <- list()

for(j in names(comp_data[["data"]][which(names(comp_data[["data"]]) %in% 
                                         c("effective.mesh.size", "mean.shape.index", 
                                           "prop.landscape", "perimeter.area.frac.dim"))])){
  formula <- f[[j]]
  
  m_nb_leaf_area[[j]][["leaf_area"]] <-mod_list <- mclapply(1:2, function(i) {
    MCMCglmm(fixed = formula,
             # random = ~ animal,
             rcov = ~units,
             family= "gaussian",
             # pedigree = comp_data$tree,
             data = comp_data$data,
             nitt = nitt,
             burnin = burnin,
             thin = thin,
             prior = prior)
  }, mc.cores=2)
  
  mod_mcmc <-  m_nb_leaf_area[["leaf_area"]][[j]][[1]]
  mod_mcmc_2 <-  m_nb_leaf_area[["leaf_area"]][[j]][[2]]}

#saveRDS(m_nb_leaf_area, "m_nb_leaf_area.rds")

## Diagnositcs ----------------------------
z <- 'effective.mesh.size'
z <- "mean.shape.index"
z <- "prop.landscape"
z <- "perimeter.area.frac.dim"

mod_mcmc <- m_nb_leaf_area[[z]][["leaf_area"]][[1]]
mod_mcmc_2 <- m_nb_leaf_area[[z]][["leaf_area"]][[2]]

bay_dia(mod_mcmc)

for(i in r){
  print(summary(m_nb_leaf_area[[i]][["leaf_area"]][[1]]))
}


