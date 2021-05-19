#' ---
#' title: "05e_str_lifeform_analysis"
#' author: "adapted by Caroline McKeon from script by Kevin Healy"
#' date: ' November 2020'
#' ---
#' 
#' 
#' more good resources
#' https://ourcodingclub.github.io/tutorials/mcmcglmm/
#' 
#' https://groups.nceas.ucsb.edu/non-linear-modeling/projects/owls/WRITEUP/owls.pdf/@@download
#'  
#'  http://www.wildanimalmodels.org/tiki-download_wiki_attachment.php?attId=24


## set up ###################

## read in and handle data------------------------------------------------------------------------------------------------
## set up for weighted effects coding
# ## main effects
# lifeform$lifeform.wec <- factor(lifeform$lifeform)
# contrasts(lifeform$lifeform.wec) <- contr.wec(lifeform$lifeform, "therophyte")

mcmc_data <- lifeform
mcmc_data$animal <- mcmc_data$species
## create comparative dataset
comp_data <- clean.data(mcmc_data, clean_tree, data.col = "animal")

# # priors----------------------------------------------------------------------------------------
## normal prior (for models including phyeny)
# prior <- list(R = list(V=1, nu=0.002), 
#               G = list(G1 = list(V=1, nu=0.002)))

## no phyeny prior
# prior <- list(R = list(V=1, nu=0.002))
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
# for (i in names(Filter(is.factor, lifeform))) {
#   plot(lifeform[,i],
#        main = paste(i),
#        xlab = paste(i))
# }

plot((total.area) ~ lifeform, data = comp_data$data)
plot((range.size) ~ lifeform, data = comp_data$data)
plot((effective.mesh.size) ~ lifeform, data = comp_data$data)
plot(mean.shape.index~ lifeform, data = comp_data$data)
plot(prop.landscape~ lifeform, data = comp_data$data)
plot((perimeter.area.frac.dim) ~ lifeform, data = comp_data$data)


## formula ------------------
## set the formula for each spatial pattern metric
f <- list()
f[["total.area"]]  <- (total.area) ~ lifeform 
f[["range.size"]] <- (range.size) ~ lifeform
f[["effective.mesh.size"]] <-  (effective.mesh.size) ~ lifeform    
f[["mean.shape.index"]] <- mean.shape.index ~ lifeform       
f[["prop.landscape"]] <- prop.landscape ~ lifeform
f[["perimeter.area.frac.dim"]] <- (perimeter.area.frac.dim) ~ lifeform


## for quick checks
# m_list <-mod_list <- mclapply(1:2, function(i) {
#   MCMCglmm(fixed = (perimeter.area.frac.dim) ~ lifeform,
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
m_indiv_lifeform <- list()

for(j in names(comp_data[["data"]][which(names(comp_data[["data"]]) %in% c("total.area", "range.size", "effective.mesh.size", "mean.shape.index",
                                                                           "prop.landscape", "perimeter.area.frac.dim"))])){
  formula <- f[[j]]
  
  m_indiv_lifeform[[j]][["lifeform"]] <-mod_list <- mclapply(1:2, function(i) {
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
  
  mod_mcmc <-  m_indiv_lifeform[["lifeform"]][[j]][[1]]
  mod_mcmc_2 <-  m_indiv_lifeform[["lifeform"]][[j]][[2]]}

#saveRDS(m_indiv_lifeform, "m_lifeform_phlyo_parexp.rds")

## Diagnositcs ----------------------------
z <- "total.area"
z <- "range.size"
z <- 'effective.mesh.size'
z <- "mean.shape.index"
z <- "prop.landscape"
z <- "perimeter.area.frac.dim"


mod_mcmc <- m_indiv_lifeform[[z]][["lifeform"]][[1]]
mod_mcmc_2 <- m_indiv_lifeform[[z]][["lifeform"]][[2]]

bay_phylo_dia(mod_mcmc)
#bay_dia(mod_mcmc)


## rough plots ----------

par(mfrow = c(2,3))

## extract the posterior estiamtes
rr <- c()
r <- c("total.area", "range.size", "effective.mesh.size", "mean.shape.index", "prop.landscape", "perimeter.area.frac.dim")
c <- tibble("a", "b")
for(i in r) {
  sum <- as.data.frame(summary(m_indiv_lifeform[[i]][["lifeform"]][[1]][["Sol"]])[["statistics"]]); 
  c <- rbind(c, c(sum$Mean[1], sum$Mean[2])); 
  rr <- append(rr, paste(i))
}

c <- cbind(c[-1,], rr)

k <- list(" total area", " range size", " effective mesh size", "mean shape index",
          "proportion of landscape", "perimeter area fractality")
names(k) <- r

par(mfrow = c(2,3), mar=c(4.5,4.5,2,2), col="black", col.main = "black", col.lab = "black")
#par(mfrow = c(2,3), mar=c(4.5,4,2,2), col="white", col.main = "white", col.lab = "white", bg="transparent")
for(i in names(lifeform[which(names(lifeform) %in% c("total.area", "range.size", "effective.mesh.size", "mean.shape.index", 
                                                     "prop.landscape", "perimeter.area.frac.dim"))])) {
  plot(f[[i]], data = comp_data$data, col = "grey", cex = 0.7, cex.lab = 1.5, cex.main = 1.8,
       ylab = paste(k[[i]]), main = paste(k[[i]]), xlab = paste("lifeform"), bty = "n")
 # abline(c[,1][c$rr ==i], c[,2][c$rr ==i], col = "#7000A8FF", lwd = 6)
}


for(i in r){
  print(summary(m_indiv_lifeform[[i]][["lifeform"]][[1]]))
}


## Climate --------------------
#
#
#

## formula ------------------
## set the formula for each spatial pattern metric
f <- list()
f[["effective.mesh.size"]] <-  (effective.mesh.size) ~ lifeform*nb    
f[["mean.shape.index"]] <- mean.shape.index ~ lifeform*nb       
f[["prop.landscape"]] <- prop.landscape ~ lifeform*nb
f[["perimeter.area.frac.dim"]] <- (perimeter.area.frac.dim) ~ lifeform*nb


## for quick checks
# m_list <-mod_list <- mclapply(1:2, function(i) {
#   MCMCglmm(fixed = (perimeter.area.frac.dim) ~ lifeform,
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
m_nb_lifeform <- list()

for(j in names(comp_data[["data"]][which(names(comp_data[["data"]]) %in% c("effective.mesh.size", "mean.shape.index",
                                                                           "prop.landscape", "perimeter.area.frac.dim"))])){
  formula <- f[[j]]
  
  m_nb_lifeform[[j]][["lifeform"]] <-mod_list <- mclapply(1:2, function(i) {
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
  
  mod_mcmc <-  m_nb_lifeform[["lifeform"]][[j]][[1]]
  mod_mcmc_2 <-  m_nb_lifeform[["lifeform"]][[j]][[2]]}

#saveRDS(m_nb_lifeform, "m_lifeform_nb.rds")

## Diagnositcs ----------------------------
z <- 'effective.mesh.size'
z <- "mean.shape.index"
z <- "prop.landscape"
z <- "perimeter.area.frac.dim"


mod_mcmc <- m_nb_lifeform[[z]][["lifeform"]][[1]]
mod_mcmc_2 <- m_nb_lifeform[[z]][["lifeform"]][[2]]

bay_phylo_dia(mod_mcmc)

