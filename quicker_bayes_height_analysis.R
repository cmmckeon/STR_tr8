## quicker bayes height analysis


m_indiv_bayes <- list()
#for(i in names(comp_data[["data"]][which(names(comp_data[["data"]]) %in% c("mean", "median", "max", "coeff_var"))])){

  m_indiv_bayes[["mean"]][["total.area"]] <-mod_list <- mclapply(1:2, function(i) {
  MCMCglmm(fixed = log(total.area) ~ log(mean),
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
  
mod_mcmc <-  m_indiv_bayes[["mean"]][["total.area"]][[1]]
mod_mcmc_2 <-  m_indiv_bayes[["mean"]][["total.area"]][[2]]


m_indiv_bayes[["coeff_var"]][["prop.landscape"]] <-mod_list <- mclapply(1:2, function(i) {
  MCMCglmm(fixed = log(prop.landscape) ~ log(coeff_var),
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

m_indiv_bayes[["coeff_var"]][["perimeter.area.frac.dim"]] <-mod_list <- mclapply(1:2, function(i) {
  MCMCglmm(fixed = log(perimeter.area.frac.dim) ~ log(coeff_var),
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

m_indiv_bayes[["coeff_var"]][["mean.shape.index"]] <-mod_list <- mclapply(1:2, function(i) {
  MCMCglmm(fixed = log(mean.shape.index) ~ log(coeff_var),
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

m_indiv_bayes[["coeff_var"]][["effective.mesh.size"]] <-mod_list <- mclapply(1:2, function(i) {
  MCMCglmm(fixed = log(effective.mesh.size) ~ log(coeff_var),
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

m_indiv_bayes[["coeff_var"]][["range.size"]] <-mod_list <- mclapply(1:2, function(i) {
  MCMCglmm(fixed = log(range.size) ~ log(coeff_var),
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



#save <- m_indiv_bayes
#saveRDS(m_indiv_bayes, "quick_bayes_height_phylo.rds")
#m_indiv_bayes[["median"]] <- m_indiv_bayes[["median"]][-7]

m_indiv_bayes <- readRDS("quick_bayes_height_phylo.rds")

par(mfrow = c(2,3))

hh <- c()
rr <- c()
h <- c("mean", "median", "max", "coeff_var")
r <- c("effective.mesh.size", "mean.shape.index", "prop.landscape", "total.area", "perimeter.area.frac.dim", "range.size")
c <- data_frame("a", "b")
for(j in h){
  for(i in r) {
    sum <- as.data.frame(summary(m_indiv_bayes[[j]][[i]][[1]][["Sol"]])[["statistics"]]); c <- rbind(c, c(sum$Mean[1], sum$Mean[2])); hh <- append(hh, paste(j)); rr <- append(rr, paste(i))
    }
}

one <- cbind(hh, rr)
c<- cbind(c[-1,], one)

par(mfrow = c(2,3), mar=c(4.5,4,2,2), col="black", col.main = "black", col.lab = "black", bg="transparent")
#par(mfrow = c(2,3), mar=c(4.5,4,2,2), col="white", col.main = "white", col.lab = "white", bg="transparent")
for(i in names(mydata[which(names(mydata) %in% c("effective.mesh.size", "mean.shape.index", "prop.landscape", "total.area", "perimeter.area.frac.dim", "range.size"))])) {
   plot(log(mydata[,i]) ~ log(height), data = mydata, col = "grey", cex = 0.7, cex.lab = 1.5, cex.main = 1.8,
        ylab = paste("log(", i, ")", sep = ""), main = paste(i))#, bty = "n")
  abline(c[,1][c$hh== "mean" & c$rr ==i], c[,2][c$hh== "mean" & c$rr ==i], col = "#F0F921FF", lwd = 6)
  abline(c[,1][c$hh== "median" & c$rr ==i], c[,2][c$hh== "median" & c$rr ==i], col = "#CC4678FF", lwd = 6)
  abline(c[,1][c$hh== "max" & c$rr ==i], c[,2][c$hh== "max" & c$rr ==i], col = "#FBA139FF", lwd = 6)
  abline(c[,1][c$hh== "coeff_var" & c$rr ==i], c[,2][c$hh== "coeff_var" & c$rr ==i], col = "#7000A8FF", lwd = 6)
}

par(mfrow = c(2,3))
for(i in names(mydata[which(names(mydata) %in% c("effective.mesh.size", "mean.shape.index", "prop.landscape", "total.area", "perimeter.area.frac.dim", "range.size"))])) {
  plot(log(mydata[,i]) ~ log(height), data = mydata, col = "dark grey")
  abline(c[,1][c$hh== "mean" & c$rr ==i], c[,2][c$hh== "mean" & c$rr ==i], col = "#BB3754FF", lwd = 4)
  abline(c[,1][c$hh== "median" & c$rr ==i], c[,2][c$hh== "median" & c$rr ==i], col = "#FB9E07FF", lwd = 4)
  abline(c[,1][c$hh== "max" & c$rr ==i], c[,2][c$hh== "max" & c$rr ==i], col = "#F0F921FF", lwd = 4)
  abline(c[,1][c$hh== "coeff_var" & c$rr ==i], c[,2][c$hh== "coeff_var" & c$rr ==i], col = "#7000A8FF", lwd = 4)
}

# for(j in unique(levels(c$rr))){
#   for(i in unique(levels(c$hh))) {
#     abline(c[,1][c$hh== "mean" & c$rr ==j], c[,2][c$hh== i & c$rr ==j])
#     }}
# 
# for(i in names(mydata[which(names(mydata) %in% c("effective.mesh.size", "mean.shape.index", "prop.landscape", "total.area", "perimeter.area.frac.dim", "range.size"))])){
#   plot(log(mydata[,i]) ~ log(height), data = mydata, main = paste(i), ylab = paste("log", i, sep = " "), col = "grey", bty = "n")
#   abline(m_indiv[["mean"]][[i]], col = "red", lwd = 4)
#   abline(m_indiv[["median"]][[i]], col = "blue", lwd = 4)
#   abline(m_indiv[["max"]][[i]], col = "green", lwd = 4)
#   abline(m_indiv[["coeff_var"]][[i]], col = "purple", lwd = 4)
# }
# 
# abline(12, 1)


# x <- MCMCglmm(fixed = log(total.area) ~ log(mean),
#               random = ~ animal,
#          rcov = ~units,
#          family= "gaussian",
#          pedigree = comp_data$tree,
#          data = comp_data$data,
#          nitt = nitt,
#          burnin = burnin,
#          thin = thin,
#          prior = prior)


