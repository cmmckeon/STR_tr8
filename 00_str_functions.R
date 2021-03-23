## functions script for STR project
## Caroline McKeon 22/03/21


bay_dia <- function(mod_list){ 
  ## extract the 2 seperate models from model list for dioagnostics 
  mod_mcmc <- mod_list[[1]]
  mod_mcmc_2 <- mod_list[[2]]
  #### Diagnositcs ----------------------------
  ## How do the trace plots look?
  allChains <- as.mcmc(cbind(mod_mcmc$Sol,mod_mcmc$VCV))
  plotTrace(allChains)
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
