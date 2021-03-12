## str_functions_and_unclean_code
mydata <- unique(merge(metrics, height, by.x = "Species", by.y = "species", all.x = T))
mydata$range.size <- as.numeric(mydata$range.size)
mydata <- mydata[mydata$height != 0,]



## Frequentist model plotting funtion
y_list <- c("effective.mesh.size", "mean.shape.index", "prop.landscape", "total.area", "perimeter.area.frac.dim", "range.size")
model_ranges <- function(mydata, y_list, x){
  m_indiv <- list() 
  #range_metrics <- c("effective.mesh.size", "mean.shape.index", "prop.landscape", "total.area", "perimeter.area.frac.dim", "range.size")
  for(i in names(mydata[which(names(mydata) %in% y_list)])){
    par(mfrow = c(2,2), mar=c(4,4,4,4))
    m_indiv[[i]] <- lm(log(mydata[,i]) ~ log(x), data = mydata)
    print(summary(m_indiv[[i]]))
    plot(m_indiv[[i]], main = paste(i))
    #plot(log(mydata[,i]) ~ log(x), data = mydata, main = paste(i)); abline(m_indiv[[i]])
  }   
  return(m_indiv)
  par(mfrow = c(2,3))
  plot <- for(i in names(mydata[which(names(mydata) %in% c("effective.mesh.size", "mean.shape.index", "prop.landscape", "total.area", "perimeter.area.frac.dim", "range.size"))])){
    plot(log(mydata[,i]) ~ log(x), data = mydata, main = paste(i), ylab = paste("log", i, sep = " "), xlab = paste(x)); abline(m_indiv[[i]])
  }
  return(plot)
}


## Bayesian model plotting funtion
model_ranges_bayes <- function(mydata, y_list, x){
  m_indiv_bayes <- list() 
  for(i in names(mydata[which(names(mydata) %in% y_list)])){
    m_indiv_bayes[[i]] <-mod_list <- mclapply(1:2, function(i) {
      MCMCglmm(fixed = log(mydata[,i]) ~ log(x),
               rcov = ~units,
               family= "gaussian",
               pedigree = comp_data$tree,
               data = comp_data$data,
               nitt = nitt,
               burnin = burnin,
               thin = thin,
               prior = prior_indiv)
    }, mc.cores=2)
  }   
  return(m_indiv_bayes)
}

c <- model_ranges_bayes(indiv_data, y_list, indiv_data$mean)


m_indiv_bayes <- list()
for(i in names(indiv_data[which(names(indiv_data) %in% c("mean", "median", "max", "coeff_var"))])){
  m_indiv_bayes[[i]] <- model_ranges_bayes(mydata = indiv_data, y_list, x = indiv_data[,i])
  # print(model_ranges(mydata = indiv_data, y_list, x = indiv_data[,i]))
}

# Numeric variables
for (i in names(Filter(is.numeric, indiv_data))) {
  hist((indiv_data[,i]),
       breaks = 3000,
       main = paste(i),
       xlab = paste(i))
}


m_indiv <- list()
for(i in names(indiv_data[which(names(indiv_data) %in% c("mean", "median", "max", "coeff_var"))])){
  m_indiv[[i]] <- model_ranges(mydata = indiv_data, y_list, x = indiv_data[,i])
  # print(model_ranges(mydata = indiv_data, y_list, x = indiv_data[,i]))
}


par(mfrow = c(2,3))

for(i in names(mydata[which(names(mydata) %in% c("effective.mesh.size", "mean.shape.index", "prop.landscape", "total.area", "perimeter.area.frac.dim", "range.size"))])){
  plot(log(mydata[,i]) ~ log(height), data = mydata, main = paste(i), ylab = paste("log", i, sep = " "), col = "grey", bty = "n")
  abline(m_indiv[["mean"]][[i]], col = "red", lwd = 4)
  abline(m_indiv[["median"]][[i]], col = "blue", lwd = 4)
  abline(m_indiv[["max"]][[i]], col = "green", lwd = 4)
  abline(m_indiv[["coeff_var"]][[i]], col = "purple", lwd = 4)
}

par(mfrow = c(2,3))

for(i in names(mydata[which(names(mydata) %in% c("effective.mesh.size", "mean.shape.index", "prop.landscape", "total.area", "perimeter.area.frac.dim", "range.size"))])){
  plot(log(mydata[,i]) ~ log(height), data = mydata, main = paste(i), ylab = paste("log", i, sep = " "), col = "grey", bty = "n")
  print(abline(m_indiv[["mean"]][[i]], col = "red", lwd = 4))
  abline(m_indiv[["median"]][[i]], col = "blue", lwd = 4)
  abline(m_indiv[["max"]][[i]], col = "green", lwd = 4)
  abline(m_indiv[["coeff_var"]][[i]], col = "purple", lwd = 4)
}

m_height <- list()
#range_metrics <- c("effective.mesh.size", "mean.shape.index", "prop.landscape", "total.area", "perimeter.area.frac.dim", "range.size")
for(i in names(mydata[which(names(mydata) %in% c("effective.mesh.size", "mean.shape.index", "prop.landscape", "total.area", "perimeter.area.frac.dim", "range.size"))])){
  par(mfrow = c(2,2))
  m_height[[i]] <- lm(log(mydata[,i]) ~ log(height), data = mydata)
  print(summary(m_height[[i]]))
  plot(m_height[[i]])
  plot(log(mydata[,i]) ~ log(height), data = mydata, main = paste(i)); abline(m_height[[i]])
}
par(mfrow = c(2,3))
for(i in names(mydata[which(names(mydata) %in% c("effective.mesh.size", "mean.shape.index", "prop.landscape", "total.area", "perimeter.area.frac.dim", "range.size"))])){
  plot(log(mydata[,i]) ~ log(height), data = mydata, main = paste(i), xlab = paste("log", i, sep = " ")); abline(m_height[[i]])
}




m_mean <- list()
#range_metrics <- c("effective.mesh.size", "mean.shape.index", "prop.landscape", "total.area", "perimeter.area.frac.dim", "range.size")
for(i in names(mean_data[which(names(mean_data) %in% c("effective.mesh.size", "mean.shape.index", "prop.landscape", "total.area", "perimeter.area.frac.dim", "range.size"))])){
  par(mfrow = c(2,2))
  m_mean[[i]] <- lm(log(mean_data[,i]) ~ log(mean), data = mean_data)
  print(summary(m_mean[[i]]))
  plot(m_mean[[i]])
  plot(log(mean_data[,i]) ~ log(mean), data = mean_data, main = paste(i)); abline(m_mean[[i]])
}
par(mfrow = c(2,3))
for(i in names(mean_data[which(names(mean_data) %in% c("effective.mesh.size", "mean.shape.index", "prop.landscape", "total.area", "perimeter.area.frac.dim", "range.size"))])){
  plot(log(mean_data[,i]) ~ log(mean), data = mean_data, main = paste(i), xlab = paste("log", i, sep = " ")); abline(m_mean[[i]])
}




m <- lm(log(total.area) ~ log(height+0.000001), data = mydata)
summary(m)
plot(m)
plot(log(total.area) ~ log(height), data = mydata)
abline(m)

# m <- lm(log(total.area) ~ log(meid), data = mydata)
# summary(m)
# plot(m)
# plot(log(total.area) ~ log(meid), data = mydata)
# abline(m)




m <- list()
#range_metrics <- c("effective.mesh.size", "mean.shape.index", "prop.landscape", "total.area", "perimeter.area.frac.dim", "range.size")
for(i in names(mydata[which(names(mydata) %in% c("effective.mesh.size", "mean.shape.index", "prop.landscape", "total.area", "perimeter.area.frac.dim", "range.size"))])){
  par(mfrow = c(2,2))
  m[[i]] <- lm(log(mydata[,i]) ~ log(meid), data = mydata)
  print(summary(m[[i]]))
  plot(m[[i]])
  plot(log(mydata[,i]) ~ log(meid), data = mydata, main = paste(i)); abline(m[[i]])
}




par(mfrow = c(2,3))
for(i in names(mydata[which(names(mydata) %in% c("effective.mesh.size", "mean.shape.index", "prop.landscape", "total.area", "perimeter.area.frac.dim", "range.size"))])){
  plot(log(mydata[,i]) ~ log(meid), data = mydata, main = paste(i), xlab = paste("log", i, sep = " ")); abline(m[[i]])
}





















