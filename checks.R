## checks

f <- list()
f[["total.area"]]  <- total.area ~ vel_mean*hf_mean*map_mean*mat_mean*map_var_mean*mat_var_mean



print("vel, hf and climate forumal set")

print("start running vel, hf and climate models")

## model ---------------
m_metric <- list()

  formula <- total.area ~ vel_mean*hf_mean*map_mean*mat_mean*map_var_mean*mat_var_mean
  
  m_metric <- mod_list <- mclapply(1:2, function(i) {
    MCMCglmm(fixed = formula,
             random = ~ animal,
             rcov = ~ units,
             family= "gaussian",
             pedigree = comp_data$tree,
             data = comp_data$data,
             nitt = nitt,
             burnin = burnin,
             thin = thin,
             prior = prior)
  }, mc.cores=2)

  
  summary(m_metric[[1]])

## check
  check <- list()
  
  formula <- total.area ~ vel_mean*hf_mean + 
    vel_mean*map_mean +
    vel_mean*mat_mean + 
    vel_mean*map_var_mean + 
    vel_mean*mat_var_mean +
    
    map_mean*hf_mean +
    map_mean*mat_mean +
    map_mean*map_var_mean +
    map_mean*mat_var_mean +
    
    mat_mean*hf_mean +
    mat_mean*mat_var_mean +
    mat_mean*map_var_mean +
    
    map_var_mean*hf_mean +
    map_var_mean*mat_var_mean +
    
    mat_var_mean*hf_mean
  
  check <- mod_list <- mclapply(1:2, function(i) {
    MCMCglmm(fixed = formula,
             random = ~ animal,
             rcov = ~ units,
             family= "gaussian",
             pedigree = comp_data$tree,
             data = comp_data$data,
             nitt = nitt,
             burnin = burnin,
             thin = thin,
             prior = prior)
  }, mc.cores=2)
  
  summary(check[[1]])
  
  
  ## simmulate
  set.seed(18)
  
  # Generate 3 correlated variables
  x <- rnorm(n=1000, mean=1.20, sd=1.5)
  y <- 0.7*x + 1 + rnorm(n=1000, mean = 0, sd = 1)
  z <- log((2*y*x + 13 + rnorm(n=1000, mean = 0, sd = 2)) + 2)
  v <- 2*z + 1.8 + rnorm(n=1000, mean = 1, sd = 0.4)
  
  plot(z,x)
d <- as.data.frame(cbind(x,y,z,v))
cor(d)

m <- lm(y ~ x)
summary(m)

m <- lm(y ~ z)
summary(m)

m <- lm(y ~ x + z)
summary(m)

m <- lm(y ~ x*z)
summary(m)

m <- lm(y ~ x+z+v)
summary(m)

m <- lm(y ~ x*z*v)
summary(m)

m <- lm(total.area ~ mat_var_mean + map_mean + mat_mean, data = metrics)
summary(m)

m <- lm(total.area ~ mat_var_mean*map_mean*mat_mean, data = metrics)
summary(m)


m <- lm(log(rat$mat_var_range) ~ log(rat$mat_var_mean))
summary(m)







  