#' ---
#' title: "05a_str_bayesian_bivariate_analysis"
#' author: "adapted by Caroline McKeon from script by Kevin Healy"
#' date: ' ANovember 2020'
#' ---
#' 
#' 
#' more good resources
#' https://ourcodingclub.github.io/tutorials/mcmcglmm/
#' 
#' https://groups.nceas.ucsb.edu/non-linear-modeling/projects/owls/WRITEUP/owls.pdf/@@download
#'  

print("This is the bayesian bivariate script")

## set up ###################


## read in and handle data------------------------------------------------------------------------------------------------
mcmc_data <- height
mcmc_data$animal <- mcmc_data$species

## create comparative dataset
comp_data <- clean.data(mcmc_data, clean_tree, data.col = "animal")

#' To give priors for MCMCglmm we need to make an object that is in a list format that includes terms of B (fixed effect), 
#' R (residual terms) and G (random terms which we will come to later).
#' 
# # priors----------------------------------------------------------------------------------------
## normal prior (for models including phylogeny)
prior <- list(R = list(V=1, nu=0.002), 
              G = list(G1 = list(V=1, nu=0.002)))

## no phylogeny prior
#prior <- list(R = list(V=1, nu=0.002))

## this is a parameter expanded prior
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
# for (i in names(Filter(is.numeric, height))) {
#   hist(log(height[,i]),
#        breaks = 30,
#        main = paste(i),
#        xlab = paste(i))
# }


plot(log(effective.mesh.size) ~ log(height_max), data = height)
plot(mean.shape.index~ log(height_max), data = height)
plot(prop.landscape~ log(height_max), data = height)
plot(log(total.area) ~ log(height_max), data = height)
plot(log(perimeter.area.frac.dim) ~ log(height_max), data = height)
plot(log(range.size) ~ log(height_max), data = height)

## formula ------------------
## set the formula for each spatial pattern metric
f <- list()
f[["effective.mesh.size"]] <-  log(effective.mesh.size) ~ log(height_max)     
f[["mean.shape.index"]] <- mean.shape.index ~ log(height_max)        
f[["prop.landscape"]] <- prop.landscape ~ log(height_max)
f[["total.area"]]  <- log(total.area) ~ log(height_max)   
f[["perimeter.area.frac.dim"]] <- log(perimeter.area.frac.dim) ~ log(height_max)
f[["range.size"]] <- log(range.size) ~ log(height_max)  


## for quick checks
m_list <-mod_list <- mclapply(1:2, function(i) {
  MCMCglmm(fixed = log(total.area) ~ log(height_max),
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

mod_mcmc <-  m_list[[1]]
mod_mcmc_2 <- m_list[[2]]



## run a model for each spatial pattern metric
m_indiv_height <- list()

for(j in names(comp_data[["data"]][which(names(comp_data[["data"]]) %in% c("effective.mesh.size", "mean.shape.index", "prop.landscape", 
                                                                           "total.area", "perimeter.area.frac.dim", "range.size"))])){
   formula <- f[[j]]
   
m_indiv_height[[j]][["height"]] <-mod_list <- mclapply(1:2, function(i) {
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

mod_mcmc <-  m_indiv_height[["height"]][[j]][[1]]
mod_mcmc_2 <-  m_indiv_height[["height"]][[j]][[2]]}


## Diagnositcs ----------------------------
# mod_mcmc <- mod_list[[1]]
# mod_mcmc_2 <- mod_list[[2]]

mod_mcmc <- m_indiv_height[["total.area"]][["height"]][[1]]
mod_mcmc_2 <- m_indiv_height[["total.area"]][["height"]][[2]]


## How do the trace plots look?
allChains <- as.mcmc(cbind(mod_mcmc$Sol,mod_mcmc$VCV))
plotTrace(allChains,axes=TRUE,las=1)
vcChain <- log10(mod_mcmc$VCV)
plotTrace(vcChain)

## estimates give the mean and the lower and higher 95% credible interval (CI) and effective sample size
print(summary(mod_mcmc))

## testing whether the random effects are significant
# Plot the posterior distribution as a histogram to check for significance and whether it's been well estimated or not
# Variance cannot be zero, and therefore if the mean value is pushed up against zero your effect is not significant
# The larger the spread of the histogram, the less well estimated the distribution is.
par(mfrow=c(2,2))
hist(mcmc(mod_mcmc$VCV)) ## very 
hist(mcmc(mod_mcmc$VCV)[,"units"]) ## not as significant

## plot the fist fixed term, the intercpet.
plot(mod_mcmc$Sol)

#plot the fist variance term, the residual error term.
plot(mod_mcmc$VCV)

#' On the right hand side of the plots is the posterior distributions for each of the terms. 
#' On the left side of these plots are the traces of the mcmc chain for each estimate 
median(mod_mcmc$Sol[,1])
#' #calulate the mode of the slope as:
hdr(mod_mcmc$Sol[,2])$mode
#' and the 50%, 95% and 99% credibility intervals
hdr(mod_mcmc$Sol[,2])$hdr



#' The `pMCMC` value can be treated like a p-value, although its technically not. 
#' If data is z-scored my data; can just check if both 95% credibility 
#' intervals are above or below zero.

#trace of the intercept MCMC chain; should be "hairy caterpillars"
traceplot(mod_mcmc$Sol[,1])

## look at the autocorrelation values for all of the fixed effects terms
autocorr.diag(mod_mcmc$Sol, lag = 1)
x <- autocorr.diag(mod_mcmc$Sol, lag = 1)
x <-as.data.frame(t(x))
hist(x$'Lag 20')

## random effects autocorrelation
autocorr.diag(mod_mcmc$VCV, lag = 1)
#acf plot for the first fixed estimate in our model (the intercept)
acf(mod_mcmc$Sol, lag.max =100)
acf(mod_mcmc$VCV, lag.max =100)
# low autocorrelation means good mixing
# To check convergence run a second model and see if it converges on the same estimates as first. 
# if result is below 1.1 you ca be happy they've converged
#checking convergence for our fixed factors
gelman.diag(mcmc.list(mod_mcmc$Sol, mod_mcmc_2$Sol)) ## 1
#checking convergence for our random terms
gelman.diag(mcmc.list(mod_mcmc$VCV, mod_mcmc_2$VCV)) ## 1
## look at the pairwise distributions
plotSplom(mod_mcmc$VCV,pch=".")

H <- mod_mcmc$VCV[,"animal"]/
  (mod_mcmc$VCV[,"animal"] + mod_mcmc$VCV[,"units"])
summary(H)



## rough plots ----------


par(mfrow = c(2,3))

## extract the posterior estiamtes
rr <- c()
r <- c("effective.mesh.size", "mean.shape.index", "prop.landscape", "total.area", "perimeter.area.frac.dim", "range.size")
c <- data_frame("a", "b")
  for(i in r) {
    sum <- as.data.frame(summary(m_indiv_height[[i]][["height"]][[1]][["Sol"]])[["statistics"]]); 
    c <- rbind(c, c(sum$Mean[1], sum$Mean[2])); 
    rr <- append(rr, paste(i))
  }

c <- cbind(c[-1,], rr)

k <- list("log effective mesh size", "mean shape index", "proportion of landscape", "log total area", 
          "log perimeter area fractality", "log range size")
names(k) <- c("effective.mesh.size", "mean.shape.index", "prop.landscape", "total.area", 
              "perimeter.area.frac.dim", "range.size")

par(mfrow = c(2,3), mar=c(4.5,4.5,2,2), col="black", col.main = "black", col.lab = "black")
#par(mfrow = c(2,3), mar=c(4.5,4,2,2), col="white", col.main = "white", col.lab = "white", bg="transparent")
for(i in names(height[which(names(height) %in% c("effective.mesh.size", "mean.shape.index", "prop.landscape", "total.area", 
                                                 "perimeter.area.frac.dim", "range.size"))])) {
  plot(f[[i]], data = height, col = "grey", cex = 0.7, cex.lab = 1.5, cex.main = 1.8,
       ylab = paste(k[[i]]), main = paste(k[[i]]), xlab = paste("log max height"), bty = "n")
  abline(c[,1][c$rr ==i], c[,2][c$rr ==i], col = "#7000A8FF", lwd = 6)
}



#### Plotting results -----------------------------------
### Effect size -----------------------------------------

## first lets get effect sizes
sum <- as.data.frame(summary(mod_mcmc)[["solutions"]])
sum <- setDT(sum, keep.rownames = TRUE)[]

par(mfrow = c(1,1))
plot((total.area) ~ log(height_max), data = height)
abline(sum$post.mean[1], sum$post.mean[2])

###---Finished-----------------------------

