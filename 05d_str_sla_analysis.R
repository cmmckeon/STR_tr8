#' ---
#' title: "05d_str_sla_analysis"
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
mcmc_data <- sla[sla$sla_max < 600,]
mcmc_data$animal <- mcmc_data$species
## create comparative dataset
comp_data <- clean.data(mcmc_data, clean_tree, data.col = "animal")

# # priors----------------------------------------------------------------------------------------
## normal prior (for models including phylogeny)
# prior <- list(R = list(V=1, nu=0.002), 
#               G = list(G1 = list(V=1, nu=0.002)))

## no phylogeny prior
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

## Numeric variables
# for (i in names(Filter(is.numeric, sla))) {
#   hist(log(sla[,i]),
#        breaks = 30,
#        main = paste(i),
#        xlab = paste(i))
# }

plot(log(total.area) ~ log(sla_max), data = comp_data$data)
plot(log(range.size) ~ log(sla_max), data = comp_data$data)
plot(log(effective.mesh.size) ~ log(sla_max), data = comp_data$data)
plot(mean.shape.index~ log(sla_max), data = comp_data$data)
plot(prop.landscape~ log(sla_max), data = comp_data$data)
plot(log(perimeter.area.frac.dim) ~ log(sla_max), data = comp_data$data)

## formula ------------------
## set the formula for each spatial pattern metric
f <- list()
f[["total.area"]]  <- log(total.area) ~ log(sla_max)   
f[["range.size"]] <- log(range.size) ~ log(sla_max)  
f[["effective.mesh.size"]] <-  log(effective.mesh.size) ~ log(sla_max)     
f[["mean.shape.index"]] <- mean.shape.index ~ log(sla_max)        
f[["prop.landscape"]] <- prop.landscape ~ log(sla_max)
f[["perimeter.area.frac.dim"]] <- log(perimeter.area.frac.dim) ~ log(sla_max)


## for quick checks
# m_list <-mod_list <- mclapply(1:2, function(i) {
#   MCMCglmm(fixed = log(perimeter.area.frac.dim) ~ log(sla_max),
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
m_indiv_sla <- list()

for(j in names(comp_data[["data"]][which(names(comp_data[["data"]]) %in% c("total.area", "range.size", "effective.mesh.size", "mean.shape.index", 
                                                                           "prop.landscape", "perimeter.area.frac.dim"))])){
  formula <- f[[j]]
  
  m_indiv_sla[[j]][["sla"]] <-mod_list <- mclapply(1:2, function(i) {
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
  
  mod_mcmc <-  m_indiv_sla[["sla"]][[j]][[1]]
  mod_mcmc_2 <-  m_indiv_sla[["sla"]][[j]][[2]]}

saveRDS(m_indiv_sla, "m_sla_phylo_parexp.rds")

## Diagnositcs ----------------------------
z <- "perimeter.area.frac.dim"

mod_mcmc <- m_indiv_sla[[z]][["sla"]][[1]]
mod_mcmc_2 <- m_indiv_sla[[z]][["sla"]][[2]]

bay_phylo_dia(mod_mcmc)
bay_dia(mod_mcmc)


## rough plots ----------

par(mfrow = c(2,3))

## extract the posterior estiamtes
rr <- c()
r <- c("total.area", "range.size", "effective.mesh.size", "mean.shape.index", "prop.landscape", "perimeter.area.frac.dim")
c <- tibble("a", "b")
for(i in r) {
  sum <- as.data.frame(summary(m_indiv_sla[[i]][["sla"]][[1]][["Sol"]])[["statistics"]]); 
  c <- rbind(c, c(sum$Mean[1], sum$Mean[2])); 
  rr <- append(rr, paste(i))
}

c <- cbind(c[-1,], rr)

k <- list("log total area", "log range size", "log effective mesh size", "mean shape index",
          "proportion of landscape", "log perimeter area fractality")
names(k) <- r

par(mfrow = c(2,3), mar=c(4.5,4.5,2,2), col="black", col.main = "black", col.lab = "black")
#par(mfrow = c(2,3), mar=c(4.5,4,2,2), col="white", col.main = "white", col.lab = "white", bg="transparent")
for(i in names(sla[which(names(sla) %in% c("total.area", "range.size", "effective.mesh.size", "mean.shape.index", "prop.landscape", 
                                                 "perimeter.area.frac.dim"))])) {
  plot(f[[i]], data = comp_data$data, col = "grey", cex = 0.7, cex.lab = 1.5, cex.main = 1.8,
       ylab = paste(k[[i]]), main = paste(k[[i]]), xlab = paste("log max sla"), bty = "n")
  abline(c[,1][c$rr ==i], c[,2][c$rr ==i], col = "#7000A8FF", lwd = 6)
}


for(i in r){
  print(summary(m_indiv_sla[[i]][["sla"]][[1]]))
}

