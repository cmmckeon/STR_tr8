#' ---
#' title: "04_str_bayesian_bivariate_analysis"
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


## read in and handle data-------------------------------------------------------------------------------------------------------------------------------

mcmc_data <- height
mcmc_data$animal <- mcmc_data$species

## create comparative dataset
comp_data <- clean.data(mcmc_data, clean_tree, data.col = "animal")

#' To give priors for MCMCglmm we need to make an object that is in a list format that includes terms of B (fixed effect), 
#' R (residual terms) and G (random terms which we will come to later).
#' 
# # ----priors----------------------------------------------------------------------------------------
prior <- list(R = list(V=1, nu=0.002), 
              G = list(G1 = list(V=1, nu=0.002)))

print("priors set")
## ----parameters-------------------------------------------------------------------------------------
#no. of interations
nitt <- c(240000)
#length of burnin
burnin <- nitt/6
#amount of thinning
thin <- c(20)

eff_ss <- (nitt-burnin)/thin
print(c("effect size will be:", eff_ss))
print("parameters set")
## ----forumla---------------------------------------------------------------------------------
##
formula_a <- log(total.area) ~ log(height_max)
print("Formula set")

## ----MCMCglmm_run---------------------------------------------------------------------------------
print("beginning running mod_mcmc")

mod_list <- mclapply(1:2, function(i) {
  MCMCglmm(fixed = formula_a,
           random= ~ animal,
           rcov = ~units,
           family= "gaussian",
           pedigree = comp_data$tree,
           data = comp_data$data,
           nitt = nitt,
           burnin = burnin,
           thin = thin,
           prior = prior)
}, mc.cores=2)

print("end")

bay_dia(mod_list)

bay_dia <- function(mod_list){ 
  ## extract the 2 seperate models from model list for dioagnostics 
  mod_mcmc <- mod_list[[1]]
  mod_mcmc_2 <- mod_list[[2]]
  #### Diagnositcs ----------------------------
  ## How do the trace plots look?
  allChains <- as.mcmc(cbind(mod_mcmc$Sol,mod_mcmc$VCV))
  vcChain <- log10(mod_mcmc$VCV)
  plotTrace(vcChain)
  plotTrace(allChains)
  
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
  return(plotTrace(allChains)) ## looking good
  }


bay_dia(mod_list)


#### Plotting results -----------------------------------
### Effect size -----------------------------------------

## first lets get effect sizes
sum <- as.data.frame(summary(mod_mcmc)[["solutions"]])
sum <- setDT(sum, keep.rownames = TRUE)[]

par(mfrow = c(1,1))
plot((total.area) ~ log(height_max), data = height)
abline(sum$post.mean[1], sum$post.mean[2])

###---Finished-----------------------------

