## quicker bayes height analysis

m_indiv_bayes[["mean"]][["total.area"]] <-mod_list <- mclapply(1:2, function(i) {
  MCMCglmm(fixed = log(total.area) ~ log(mean),
           rcov = ~units,
           family= "gaussian",
           pedigree = comp_data$tree,
           data = comp_data$data,
           nitt = nitt,
           burnin = burnin,
           thin = thin,
           prior = prior_indiv)
}, mc.cores=2)

m_indiv_bayes[["mean"]][["prop.landscape"]] <-mod_list <- mclapply(1:2, function(i) {
  MCMCglmm(fixed = log(prop.landscape) ~ log(mean),
           rcov = ~units,
           family= "gaussian",
           pedigree = comp_data$tree,
           data = comp_data$data,
           nitt = nitt,
           burnin = burnin,
           thin = thin,
           prior = prior_indiv)
}, mc.cores=2)

m_indiv_bayes[["mean"]][["perimeter.area.frac.dim"]] <-mod_list <- mclapply(1:2, function(i) {
  MCMCglmm(fixed = log(perimeter.area.frac.dim) ~ log(mean),
           rcov = ~units,
           family= "gaussian",
           pedigree = comp_data$tree,
           data = comp_data$data,
           nitt = nitt,
           burnin = burnin,
           thin = thin,
           prior = prior_indiv)
}, mc.cores=2)

m_indiv_bayes[["mean"]][["mean.shape.index"]] <-mod_list <- mclapply(1:2, function(i) {
  MCMCglmm(fixed = log(mean.shape.index) ~ log(mean),
           rcov = ~units,
           family= "gaussian",
           pedigree = comp_data$tree,
           data = comp_data$data,
           nitt = nitt,
           burnin = burnin,
           thin = thin,
           prior = prior_indiv)
}, mc.cores=2)

m_indiv_bayes[["mean"]][["effective.mesh.size"]] <-mod_list <- mclapply(1:2, function(i) {
  MCMCglmm(fixed = log(effective.mesh.size) ~ log(mean),
           rcov = ~units,
           family= "gaussian",
           pedigree = comp_data$tree,
           data = comp_data$data,
           nitt = nitt,
           burnin = burnin,
           thin = thin,
           prior = prior_indiv)
}, mc.cores=2)

m_indiv_bayes[["mean"]][["range.size"]] <-mod_list <- mclapply(1:2, function(i) {
  MCMCglmm(fixed = log(range.size) ~ log(mean),
           rcov = ~units,
           family= "gaussian",
           pedigree = comp_data$tree,
           data = comp_data$data,
           nitt = nitt,
           burnin = burnin,
           thin = thin,
           prior = prior_indiv)
}, mc.cores=2)

save <- m_indiv_bayes
#m_indiv_bayes[["median"]] <- m_indiv_bayes[["median"]][-7]

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


par(mfrow = c(2,3))
for(i in names(mydata[which(names(mydata) %in% c("effective.mesh.size", "mean.shape.index", "prop.landscape", "total.area", "perimeter.area.frac.dim", "range.size"))])) {
   plot(log(mydata[,i]) ~ log(height), data = mydata)
  }

for(j in h){
  for(i in r) {
    abline(c$"a"[c$hh== j & c$rr ==i], c$"b"[c$hh== j & c$rr ==i])}}


for(i in names(mydata[which(names(mydata) %in% c("effective.mesh.size", "mean.shape.index", "prop.landscape", "total.area", "perimeter.area.frac.dim", "range.size"))])){
  plot(log(mydata[,i]) ~ log(height), data = mydata, main = paste(i), ylab = paste("log", i, sep = " "), col = "grey", bty = "n")
  abline(m_indiv[["mean"]][[i]], col = "red", lwd = 4)
  abline(m_indiv[["median"]][[i]], col = "blue", lwd = 4)
  abline(m_indiv[["max"]][[i]], col = "green", lwd = 4)
  abline(m_indiv[["coeff_var"]][[i]], col = "purple", lwd = 4)
}

abline(12, 1)





