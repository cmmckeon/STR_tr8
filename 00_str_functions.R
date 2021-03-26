## 00_str_functions.R
## Caroline McKeon 22/03/21

library(BIEN)
library(tidyverse)
library(glmmTMB)
library(tidyverse)
library(MCMCglmm)
library(lme4)
library(invgamma)
library(glmmTMB)
library(mulTree)
library(phytools)
library(doParallel)
library(optimx) 
library(tidyverse)
library(MCMCglmm)
library(lme4)
library(glmmTMB)
library(invgamma)
library(mulTree)
library(phytools)
library(doParallel)
library(runjags)
library(plotMCMC)
library(coda)
library(tidybayes)
library(emmeans)
library(boot)
library(sjPlot)
library(effects)
library(ggridges)
library(ggeffects)
library(cowplot)
library(gridExtra)
library(MuMIn)
library(LaplacesDemon)
library("kableExtra")
library(ggpubr)
library(data.table)
library(gridBase)
library(viridis)
library(ggplot2)

'%nin%' = Negate('%in%')

gm_mean = function(x, na.rm=TRUE, zero.propagate = FALSE){
  if(any(x < 0, na.rm = TRUE)){
    return(NaN)
  }
  if(zero.propagate){
    if(any(x == 0, na.rm = TRUE)){
      return(0)
    }
    exp(mean(log(x), na.rm = na.rm))
  } else {
    exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
  }
}

bay_phylo_dia <- function(mod_list){ 
  
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
  par(mfrow=c(3,1))
  hist(mcmc(mod_mcmc$VCV), main = "looking at significance of random effects: VCV")
  hist(mcmc(mod_mcmc$VCV)[, "animal"]) 
  hist(mcmc(mod_mcmc$VCV)[,"units"]) 
  
  ## plot the fist fixed term, the intercpet.
  plot(mod_mcmc$Sol)
  #plot the fist variance term, the residual error term.
  plot(mod_mcmc$VCV)
  
  #acf plot for the fixed estimate in our model 
  acf(mod_mcmc$Sol, lag.max =100)
  acf(mod_mcmc$VCV, lag.max =100)
  # low autocorrelation means good mixing
  # To check convergence run a second model and see if it converges on the same estimates as first. 
  # if result is below 1.1 you ca be happy they've converged
  #checking convergence for our fixed factors
  print("gelman.diagnostic - Sol")
  print(gelman.diag(mcmc.list(mod_mcmc$Sol, mod_mcmc_2$Sol))) ## 1
  #checking convergence for our random terms
  print("gelman.diagnostic - VCV")
  print(gelman.diag(mcmc.list(mod_mcmc$VCV, mod_mcmc_2$VCV))) ## 1
  ## look at the pairwise distributions
  plotSplom(mod_mcmc$VCV,pch=".")
  
  H <- mod_mcmc$VCV[,"animal"]/
    (mod_mcmc$VCV[,"animal"] + mod_mcmc$VCV[,"units"])
  print("summary H^2 (pagel's lambda")
  print(summary(H))
  plot(H)
}

bay_dia <- function(mod_list){ 
  
  ## How do the trace plots look?
  allChains <- as.mcmc(cbind(mod_mcmc$Sol,mod_mcmc$VCV))
  plotTrace(allChains,axes=TRUE,las=1)
  
  ## estimates give the mean and the lower and higher 95% credible interval (CI) and effective sample size
  print(summary(mod_mcmc))
  
  ## testing whether the random effects are significant
  # Plot the posterior distribution as a histogram to check for significance and whether it's been well estimated or not
  # Variance cannot be zero, and therefore if the mean value is pushed up against zero your effect is not significant
  # The larger the spread of the histogram, the less well estimated the distribution is.
  par(mfrow=c(2,1))
  hist(mcmc(mod_mcmc$VCV), main = "looking at significance of random effects: VCV")
  hist(mcmc(mod_mcmc$VCV)[,"units"]) 
  
  ## plot the fist fixed term, the intercpet.
  plot(mod_mcmc$Sol)
  #plot the fist variance term, the residual error term.
  plot(mod_mcmc$VCV)
  
  #acf plot for the fixed estimate in our model 
  acf(mod_mcmc$Sol, lag.max =100)
  acf(mod_mcmc$VCV, lag.max =100)
  # low autocorrelation means good mixing
  # To check convergence run a second model and see if it converges on the same estimates as first. 
  # if result is below 1.1 you ca be happy they've converged
  #checking convergence for our fixed factors
  print("gelman.diagnostic - Sol")
  print(gelman.diag(mcmc.list(mod_mcmc$Sol, mod_mcmc_2$Sol))) ## 1
  #checking convergence for our random terms
  print("gelman.diagnostic - VCV")
  print(gelman.diag(mcmc.list(mod_mcmc$VCV, mod_mcmc_2$VCV))) ## 1
}
  
  #bay_dia(mod_mcmc)
  