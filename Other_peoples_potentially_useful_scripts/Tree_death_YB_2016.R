
## tree death.r for Data Handling ZO4030
## Yvonne Buckley, 27/11/2016
## These data are on mortality (0) or survival (1) of an Australian tree 
## Eucalyptus melanophloia
## I show how to model survival/mortality binary data using a Binomial errors 
## model and demonstrate a few different ways of visualising binary data and 
## comparing binary data with model predictions.
## Data are from Dwyer, Fensham et al. 2010

## Inputs: tree_death.csv
## Outputs: None

## Download the data tree_death.csv and set your working directory using setwd()

e_mel_mort  <- read.csv("tree_death.csv", header=T)  
summary(e_mel_mort)   

head(e_mel_mort)      ## gives first 6 records in data file
e_mel_mort$site <- as.factor(e_mel_mort$site)  
levels(e_mel_mort$site)
## specify this grouping variable as a factor and see how many levels (sites)

table(e_mel_mort$live) #### look at how many records of deaths (0) vs. survived (1) 

## I want to find out how site and tree basal diameter (a measure of size)
## affect whether trees live or die
## I need to model the response variable "live" which is a binary variable
## as a function of the explanatory variables "site" and "diam" (diameter)
## My response variable is binary & will have non-normal errors
## so I will try a GLM with a binomial error distribution
m1 <- glm(live ~ site* diam, data = e_mel_mort, family = binomial)
summary(m1)  
## check residuals, look at parameters, check model structure, 
## check dispersion (residual deviance/residual DF) 
coef(m1)    ## this gives the parameter estimates of the intercepts & slopes

## this gives the order of the explanatory varaibles to include in the plogis function below

## VISUALISING BINARY DATA

par(mfrow = c(2,2)) ##sets up a 2 x 2 graphing window
site1_dat <- subset(e_mel_mort, site==1) ## extracts just the site 1 data

with(site1_dat, plot(live ~ diam, pch=1, bty="l", ylab="Survival", cex.lab=1.5,
                     xlab="", main="(a) Observations")) 
## plots survival (0 or 1) on the y axis against diameter on the x-axis for site 1
## it can be difficult to see patterns in binary data if you have alot of observations

with(site1_dat, plot(jitter(live, amount = 0.1) ~ diam, pch=1, bty="l", 
                     cex.lab=1.5, xlim=range(site1_dat$diam), xlab="", ylab="", 
                     main="(b) Observations with jitter"))     
## the jitter function spreads out the 1s and 0s to make viewing of data easier, 
## compare this with the unjittered plot using plot(live ~ diam, ...) we did earlier

## Below I set up a vector of categories evenly spaced along the diameter values
## I then take mean survival in each category in order to plot probabilities of 
## survival in each size category 
h1 <- hist(site1_dat$diam) ## a histogram of diameter values
## you can see from the histogram that I have lots of smaller values (around 10-15)
## and a long tail of larger diameter values (>20)
## set lower and upper bounds for averages
lower <- h1$breaks[-length(h1$breaks)]    
upper <- h1$breaks[-1]
live_cat <- rep(NA, length(h1$breaks[-1]))

for(i in 1:length(upper)) {
  live_cat <- ifelse(site1_dat$diam <= upper[i], 
                     ifelse(site1_dat$diam >= lower[i],i, live_cat),live_cat)
}   ## this code assigns a category to each "live" observation so that I will
## be able to get an average of all the "live" observations for a particular diameter
##category (e.g. what is the average survival of trees between 0 and 5 cm diam?)
## I will take the mean of category 1 in live_cat to find that out.

live_cat <- as.factor(live_cat)
bin_live1 <- tapply(site1_dat$live,live_cat ,mean) 
## bin_live1 gets mean survival of trees in each diameter category
plot(h1$mids, bin_live1, pch = 16, bty = "l", xlab = "Diameter cm", 
     ylab = "Survival", cex.lab = 1.5, ylim = c(0,1), 
     xlim = range(site1_dat$diam), main = "(c) Av. evenly spaced obs.")

## Below I make a vector of categories but with the same number of observations
## in each category
live_cat <- gl(n = 11, k = 40,length = length(site1_dat$diam))
bin_live2 <- tapply(site1_dat$live, live_cat, mean)
bin_diam <- tapply(site1_dat$diam, live_cat, mean)
plot(bin_diam, bin_live2, pch = 16, bty = "l", xlab = "Diameter cm", ylab = "", 
     cex.lab = 1.5, ylim = c(0,1), xlim = range(site1_dat$diam), 
     main = "(d) Av. equal no. obs.")

## plotting data and models
## 1. plotting the linear predictor for each site
e_mel_mort$lin_pred <- m1$linear.predictors ## this gives the linear predictor value for each data point 
## you can see that the linear predictors can be negative or positive & are continuous
e_mel_mort$lin_pred

par(mfrow = c(1,2))
with(e_mel_mort, plot(diam, m1$linear.predictors, type = "n", bty="l", 
                      xlab = "Diameter cm", ylab = "Linear predictor of survival"))
## the plot command above sets up the axes with no data as I'll plot each site
## separately to compare them
lines(e_mel_mort$diam[e_mel_mort$site == 1], e_mel_mort$lin_pred[e_mel_mort$site == 1], 
      lty = 1)
lines(e_mel_mort$diam[e_mel_mort$site == 2], e_mel_mort$lin_pred[e_mel_mort$site == 2], 
      lty = 2)
lines(e_mel_mort$diam[e_mel_mort$site == 3], e_mel_mort$lin_pred[e_mel_mort$site == 3], 
      lty = 3)
legend("topleft", legend = c("site 1", "site 2", "site 3"), lty = c(1,2,3), bty = "n")

## Now we will convert the linear predictor values back to predicted probabilities 
## using the inverse link function for logit which is: (exp(eta)/(exp(eta)+1))
## 
e_mel_mort$resp_pred <- exp(e_mel_mort$lin_pred)/(exp(e_mel_mort$lin_pred) + 1)
e_mel_mort$resp_pred ## check that these values are indeed probabilities now

with(e_mel_mort, 
     plot(diam, resp_pred, type = "n", bty="l", xlab = "Diameter cm", 
          ylab = "Linear predictor of survival"), main = "Predicted probabilities")
## the plot command above sets up the axes with no data as I'll plot each site
## separately to compare them
lines(e_mel_mort$diam[e_mel_mort$site == 1], e_mel_mort$resp_pred[e_mel_mort$site == 1], 
      lty = 1)
lines(e_mel_mort$diam[e_mel_mort$site == 2], e_mel_mort$resp_pred[e_mel_mort$site == 2], 
      lty = 2)
lines(e_mel_mort$diam[e_mel_mort$site == 3], e_mel_mort$resp_pred[e_mel_mort$site == 3], 
      lty = 3)
legend("topleft", legend = c("site 1", "site 2", "site 3"), lty = c(1,2,3), bty = "n")



