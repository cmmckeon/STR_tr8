## 08b_str_humanfootprint_analysis
## 18/05/2021 cm 

## metrics ~ human footprint ratio --------
## read in and handle data------------------------------------------------------------------------------------------------

rat <- readRDS("Data_occ_humanfootprint_ratio.rds")
nb <- readRDS("Data_nichebreadth.rds")
nb$sp.list <- gsub(" ", "_", nb$sp.list)
metrics$species  <- gsub(" ", "_", metrics$species)

## can hopefully drop this chunk if I run again
rat$species <- gsub(".regis", "-regis", rat$species)
rat$species <- gsub("Chenopodium_bonus.henricus", "Chenopodium_bonus-henricus", rat$species)
rat$species <- gsub("Drymocallis-regis.borisii", "Drymocallis_regis-borisii", rat$species)
rat$species <- gsub("Arabis_collna", "Arabis_collina", rat$species)
rat$species <- gsub("Dianthus_collnus", "Dianthus_collinus", rat$species)
rat$species <- gsub("Lychnis_flos.cuculi", "Lychnis_flos-cuculi", rat$species)
rat$species <- gsub("Lychnis_flos.jovis", "Lychnis_flos-jovis", rat$species)
rat$species <- gsub("Saxifraga_federici.augusti", "Saxifraga_federici-augusti", rat$species)


metrics <- merge(metrics, rat, by = "species")

#m <- drop_na(metrics)
for (i in names(Filter(is.numeric, metrics[, which(names(metrics) %in% 
                                                   c("map_mean", "map_var_mean"))]))) {
  metrics[, i] <- c(log(metrics[,i]))
}

for (i in names(Filter(is.numeric, metrics[, which(names(metrics) %in% 
                                                   c("gm_mean", "mean", "median", "reg_mean", "mat_mean",
                                                     "mat_var_mean", "map_mean", "map_var_mean"))]))) {
  metrics[, i] <- c(scale(metrics[,i]))
}


for (i in names(Filter(is.numeric, metrics))) {
        hist((metrics[,i]),
             breaks = 3000,
             main = paste(i),
             xlab = paste(i))
}


upper.panel<-function(x, y){
        points(x,y, pch=21, col=c("grey"), cex = 0.5)
        r <- round(cor(x, y), digits=2)
        txt <- paste0("R = ", r)
        usr <- par("usr"); on.exit(par(usr))
        par(usr = c(0, 1, 0, 1))
        text(0.8, 0.9, txt, cex =0.7)
}

rat_ <- drop_na(metrics)
pairs(rat_[, which(names(rat_) %nin% c("species"))], 
      lower.panel = NULL, upper.panel = upper.panel)

# list <- c()
# for (i in names(metrics)){
#   list[i] <-length(which(is.na(metrics[,i])))
# }
# 
# print(list) ## all variables should be zero

metrics <- metrics[which(!is.na(metrics$mean)),]
#saveRDS(metrics, "Data_metrics_for_hf_analysis.rds")

mcmc_data <- metrics
mcmc_data$animal <- mcmc_data$species
## create comparative dataset
comp_data <- clean.data(mcmc_data, clean_tree, data.col = "animal")

# # priors----------------------------------------------------------------------------------------
## normal prior 
prior <- list(R = list(V=1, nu=0.002), 
              G = list(G1 = list(V=1, nu=0.002)))
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

## formula ------------------
## set the formula for each spatial pattern metric
f <- list()
f[["total.area"]]  <- total.area ~ mean #mat_mean*mat_var_mean*map_mean*map_var_mean
f[["range.size"]] <- range.size ~ mean #mat_mean*mat_var_mean*map_mean*map_var_mean
f[["effective.mesh.size"]] <-  effective.mesh.size ~ mean #mat_mean*mat_var_mean*map_mean*map_var_mean
f[["prop.landscape"]] <- prop.landscape ~ mean #mat_mean*mat_var_mean*map_mean*map_var_mean
f[["mean.shape.index"]] <- mean.shape.index ~ mean #mat_mean*mat_var_mean*map_mean*map_var_mean  
f[["perimeter.area.frac.dim"]] <- perimeter.area.frac.dim ~ mean #mat_mean*mat_var_mean*map_mean*map_var_mean



## model ---------------
m_metric_hf <- list()

for(j in names(comp_data[["data"]][which(names(comp_data[["data"]]) %in% c("total.area", "range.size", "effective.mesh.size", "mean.shape.index", 
                                                                           "prop.landscape", "perimeter.area.frac.dim"))])){
        formula <- f[[j]]
        
        m_metric_hf[[j]][["hf"]]  <-mod_list <- mclapply(1:2, function(i) {
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
        Sys.sleep(20)}

#saveRDS(m_metric_hf, "m_metric_hf.rds")
#saveRDS(m_metric_hf, "m_metric_clim_hf.rds")

#m_metric_hf <- readRDS("m_metric_hf.rds")
## diagnostics -------------
z <- "total.area"
z <- "range.size"
z <- 'effective.mesh.size'
z <- "mean.shape.index"
z <- "prop.landscape"
z <- "perimeter.area.frac.dim"

mod_mcmc <- m_metric_clim_hf[[z]][["hf"]][[1]]
mod_mcmc_2 <- m_metric_clim_hf[[z]][["hf"]][[2]]

bay_phylo_dia(mod_mcmc)

for(i in r){
        print(summary(m_metric_clim_hf[[i]][["hf"]][[1]]))
}



## rough plots ----------
par(mfrow = c(2,3))
## extract the posterior estimates
rr <- c()
r <- c("total.area", "range.size", "effective.mesh.size", "mean.shape.index", "prop.landscape", "perimeter.area.frac.dim")
c <- tibble("a", "b")
for(i in r) {
        sum <- as.data.frame(summary(m_metric_hf[[i]][["hf"]][[1]][["Sol"]])[["statistics"]]); 
        c <- rbind(c, c(sum$Mean[1], sum$Mean[2])); 
        rr <- append(rr, paste(i))}
c <- cbind(c[-1,], rr)

k <- list(" total area", " range size", " effective mesh size", "mean shape index",
          "proportion of landscape", " perimeter area fractality")
names(k) <- r

colz <- as.data.frame(cbind(c("total.area", "range.size", "effective.mesh.size", "mean.shape.index", "prop.landscape", 
                              "perimeter.area.frac.dim") ,c("#7000A8FF","#7000A8FF","grey", "#7000A8FF", "grey",  "grey")))
#par(mfrow = c(2,3), mar=c(4.5,4.5,2,2), col="black", col.main = "black", col.lab = "black")
#par(mfrow = c(2,3), mar=c(4.5,4,2,2), col="white", col.main = "white", col.lab = "white", bg="transparent")
for(i in names(comp_data$data[which(names(comp_data$data) %in% c("total.area", "range.size", "effective.mesh.size", "mean.shape.index", "prop.landscape", 
                                                       "perimeter.area.frac.dim"))])) {
        plot(f[[i]], data = comp_data$data, col = "grey", cex = 0.7, cex.lab = 1.5, cex.main = 1.8,
             ylab = paste(k[[i]]), main = paste(k[[i]]), xlab = paste("nb"), bty = "n")
        abline(c[,1][c$rr ==i], c[,2][c$rr ==i], col = paste(colz$V2[colz$V1 ==i ]), lwd = 6)
}





## no phyeny prior
# prior <- list(R = list(V=1, nu=0.002))
# 
# m_metric_hf_no_phy <- list()
# 
# for(j in names(comp_data[["data"]][which(names(comp_data[["data"]]) %in% c("total.area", "range.size", "effective.mesh.size", "mean.shape.index", 
#                                                                            "prop.landscape", "perimeter.area.frac.dim"))])){
#         formula <- f[[j]]
#         
#         m_metric_hf_no_phy[[j]][["hf"]]  <-mod_list <- mclapply(1:2, function(i) {
#                 MCMCglmm(fixed = formula,
#                          rcov = ~units,
#                          family= "gaussian",
#                          pedigree = comp_data$tree,
#                          data = comp_data$data,
#                          nitt = nitt,
#                          burnin = burnin,
#                          thin = thin,
#                          prior = prior)
#         }, mc.cores=2)}
# 
# 
# 
# mod_mcmc <- m_metric_hf_no_phy[[z]][["hf"]][[1]]
# mod_mcmc_2 <- m_metric_hf_no_phy[[z]][["hf"]][[2]]
# 
# bay_dia(mod_mcmc)
# 
# for(i in r){
#         print(summary(m_metric_hf_no_phy[[i]][["hf"]][[1]]))
# }



