##09_ str_plots

# ## metric colinearity -----------------
# m <- metrics
# 
# upper.panel<-function(x, y){
#   points(x,y, pch=21, col=c("grey"))
#   r <- round(cor(x, y), digits=2)
#   txt <- paste0("R = ", r)
#   usr <- par("usr"); on.exit(par(usr))
#   par(usr = c(0, 1, 0, 1))
#   text(0.8, 0.9, txt, cex =1)
# }
# 
# pairs(drop_na(m[, which(names(m) %nin% c("species"))]), lower.panel = NULL, upper.panel = upper.panel)
# 
# 
# 
# ## trait colinearity ------------------
# par(mfrow = c(5,5), mar =c(4,4,1.4,1), col = "black") ## b,l,t,r
# ## height 
# a <- droplevels(unique(merge(height, lifeform, by = "species"))) 
# b <- droplevels(unique(merge(height, woodiness, by = "species")))
# c <- droplevels(unique(merge(height, sla, by = "species"))) 
# d <- droplevels(unique(merge(height, seed_mass, by = "species"))) 
# e <- droplevels(unique(merge(height, leaf_area, by = "species")))
# 
# ma <- lm(
#   height_max ~ lifeform, data = a)
# mb <- lm(
#   height_max ~ woodiness, data = b)
# mc <- lm(
#   height_max ~ 
#     sla_max, data = c)
# md <- lm(
#   height_max ~ 
#     (seed_mass_max), data = d)
# me <- lm(
#   height_max ~ 
#     leaf_area_max, data = e)
# 
# plot(
#   height_max ~ lifeform, data = a)
# mtext(paste("r2", round(summary(ma)$adj.r.squared, 4), col = ""), side =3)
# plot(
#   height_max ~ woodiness, data = b)
# mtext(paste("r2", round(summary(mb)$adj.r.squared, 4), col = ""), side =3)
# plot(
#   (c$height_max) ~ 
#     (c$sla_max))
# abline(mc)
# mtext(paste("r2", round(summary(mc)$adj.r.squared, 4), col = ""), side =3)
# plot(
#   (d$height_max) ~ 
#     (d$seed_mass_max))
# abline(md)
# mtext(paste("r2", round(summary(md)$adj.r.squared, 4), col = ""), side =3)
# plot(
#   (e$height_max) ~ 
#     (e$leaf_area_max))
# abline(me)
# mtext(paste("r2", round(summary(me)$adj.r.squared, 4), col = ""), side =3)
# 
# ## leaf_area
# a <- droplevels(unique(merge(leaf_area, lifeform, by = "species"))) 
# b <- droplevels(unique(merge(leaf_area, woodiness, by = "species")))
# c <- droplevels(unique(merge(leaf_area, sla, by = "species"))) 
# d <- droplevels(unique(merge(leaf_area, seed_mass, by = "species"))) 
# 
# ma <- lm(
#   leaf_area_max ~ lifeform, data = a)
# mb <- lm(
#   leaf_area_max ~ woodiness, data = b)
# mc <- lm(
#   leaf_area_max ~ 
#     sla_max, data = c)
# md <- lm(
#   leaf_area_max ~ 
#     (seed_mass_max), data = d)
# 
# plot.new()
# plot(
#   leaf_area_max ~ lifeform, data = a)
# mtext(paste("r2", round(summary(ma)$adj.r.squared, 4), col = ""), side =3)
# plot(
#   leaf_area_max ~ woodiness, data = b)
# mtext(paste("r2", round(summary(mb)$adj.r.squared, 4), col = ""), side =3)
# plot(
#   (c$leaf_area_max) ~ 
#     (c$sla_max))
# abline(mc)
# mtext(paste("r2", round(summary(mc)$adj.r.squared, 4), col = ""), side =3)
# plot(
#   (d$leaf_area_max) ~ 
#     (d$seed_mass_max))
# abline(md)
# mtext(paste("r2", round(summary(md)$adj.r.squared, 4), col = ""), side =3)
# 
# 
# 
# ## seed_mass
# a <- droplevels(unique(merge(seed_mass, lifeform, by = "species"))) 
# b <- droplevels(unique(merge(seed_mass, woodiness, by = "species")))
# c <- droplevels(unique(merge(seed_mass, sla, by = "species"))) 
# 
# ma <- lm(
#   seed_mass_max~ lifeform, data = a)
# mb <- lm(
#   seed_mass_max~ woodiness, data = b)
# mc <- lm(
#   seed_mass_max~ 
#     sla_max, data = c)
# 
# plot.new()
# plot.new()
# plot(
#   seed_mass_max~ lifeform, data = a)
# mtext(paste("r2", round(summary(ma)$adj.r.squared, 4), col = ""), side =3)
# plot(
#   seed_mass_max~ woodiness, data = b)
# mtext(paste("r2", round(summary(mb)$adj.r.squared, 4), col = ""), side =3)
# plot(
#   (c$seed_mass_max) ~ 
#     (c$sla_max))
# abline(mc)
# mtext(paste("r2", round(summary(mc)$adj.r.squared, 4), col = ""), side =3)
# 
# 
# ## sla
# a <- droplevels(unique(merge(sla, lifeform, by = "species"))) 
# b <- droplevels(unique(merge(sla, woodiness, by = "species")))
# 
# ma <- lm(
#   sla_max ~ lifeform, data = a)
# mb <- lm(
#   sla_max ~ woodiness, data = b)
# 
# plot.new()
# plot.new()
# plot.new()
# plot(
#   sla_max ~ lifeform, data = a)
# mtext(paste("r2", round(summary(ma)$adj.r.squared, 4), col = ""), side =3)
# plot(
#   sla_max ~ woodiness, data = b)
# mtext(paste("r2", round(summary(mb)$adj.r.squared, 4), col = ""), side =3)
# 
# 
# 
# ## woodiness
# a <- droplevels(unique(merge(woodiness, lifeform, by = "species"))) 
# a$woodiness <- as.numeric(a$woodiness)
# a$woodiness <- a$woodiness -1
# 
# ma <- glm(woodiness ~ lifeform, family = binomial, data = a)
# # mb <- glm(woodiness ~ 1, family = binomial, data = a)
# 
# # simulationOutput <- simulateResiduals(fittedModel = ma, plot = F)
# # plot(simulateResiduals(ma))
# 
# plot.new()
# plot.new()
# plot.new()
# plot.new()
# plot((woodiness) ~ lifeform, data = a)
# mtext(paste("r2", round(summary(mb)$adj.r.squared, 4), col = ""), side =3)


## individual traits ------------

## height  -------------
mcmc_data <- height
mcmc_data$animal <- mcmc_data$species
comp_data <- clean.data(mcmc_data, clean_tree, data.col = "animal")
f <- list()
f[["total.area"]]  <- (total.area) ~ (height_max)   
f[["range.size"]] <- (range.size) ~ (height_max)  
f[["effective.mesh.size"]] <-  (effective.mesh.size) ~ (height_max)     
f[["mean.shape.index"]] <- mean.shape.index ~ (height_max)        
f[["prop.landscape"]] <- prop.landscape ~ (height_max)
f[["perimeter.area.frac.dim"]] <- (perimeter.area.frac.dim) ~ (height_max)


## extract the posterior estiamtes
rr <- c()
r <- c("total.area", "range.size", "effective.mesh.size", "mean.shape.index", "prop.landscape", "perimeter.area.frac.dim")
c <- tibble("a", "b")
for(i in r) {
  sum <- as.data.frame(summary(m_indiv_height[[i]][["height"]][[1]][["Sol"]])[["statistics"]]); 
  c <- rbind(c, c(sum$Mean[1], sum$Mean[2])); 
  rr <- append(rr, paste(i))
}

c <- cbind(c[-1,], rr)

k <- list("total area", "range size", "effective mesh size", "mean shape index",
          "proportion of landscape", "perimeter area fractality")
names(k) <- r

colz <- as.data.frame(cbind(c("total.area", "range.size", "effective.mesh.size", "mean.shape.index", "prop.landscape", 
                              "perimeter.area.frac.dim") ,c("#7000A8FF","#7000A8FF","#7000A8FF", "grey", "grey", "grey"), 
                            c("total.area", "range.size", "mesh.size", "shape.index", "prop.land", 
                              "frac.dim")))
## b,l,t,r
par(mfrow = c(6,6), mar=c(4,1, 1,1), col="black", col.main = "black", col.lab = "black", cex.axis = 0.5)
#par(mfrow = c(2,3), mar=c(4.5,4,2,2), col="white", col.main = "white", col.lab = "white", bg="transparent")
for(i in names(height[which(names(height) %in% c("total.area", "range.size", "effective.mesh.size", "mean.shape.index", "prop.landscape", 
                                                 "perimeter.area.frac.dim"))])) {
  plot(f[[i]], data = comp_data$data, col = "grey", cex = 0.7, cex.lab = 1.5, cex.main = 1.8,
      # ylab = paste(k[[i]]), main = paste(k[[i]]), 
      main = colz$V3[colz$V1 ==i], xlab = paste(" max height"), bty = "n")
  abline(c[,1][c$rr ==i], c[,2][c$rr ==i], col = paste(colz$V2[colz$V1 ==i ]), lwd = 6)
}

## seedmass -----
mcmc_data <- seed_mass
mcmc_data$animal <- mcmc_data$species
comp_data <- clean.data(mcmc_data, clean_tree, data.col = "animal")
f <- list()
f[["total.area"]]  <- (total.area) ~ (seed_mass_max)   
f[["range.size"]] <- (range.size) ~ (seed_mass_max)  
f[["effective.mesh.size"]] <-  (effective.mesh.size) ~ (seed_mass_max)     
f[["mean.shape.index"]] <- mean.shape.index ~ (seed_mass_max)        
f[["prop.landscape"]] <- prop.landscape ~ (seed_mass_max)
f[["perimeter.area.frac.dim"]] <- (perimeter.area.frac.dim) ~ (seed_mass_max)

## extract the posterior estiamtes
rr <- c()
r <- c("total.area", "range.size", "effective.mesh.size", "mean.shape.index", "prop.landscape", "perimeter.area.frac.dim")
c <- tibble("a", "b")
for(i in r) {
  sum <- as.data.frame(summary(m_indiv_seed_mass[[i]][["seed_mass"]][[1]][["Sol"]])[["statistics"]]); 
  c <- rbind(c, c(sum$Mean[1], sum$Mean[2])); 
  rr <- append(rr, paste(i))
}
c <- cbind(c[-1,], rr)

for(i in names(seed_mass[which(names(seed_mass) %in% c("total.area", "range.size", "effective.mesh.size", "mean.shape.index", "prop.landscape", 
                                                       "perimeter.area.frac.dim"))])) {
  plot(f[[i]], data = comp_data$data, col = "grey", cex = 0.7, cex.lab = 1.5, cex.main = 1.8,
       #ylab = paste(k[[i]]), main = paste(k[[i]]), 
       xlab = paste("max seed_mass"), bty = "n",  main = "", ylab = "")
  abline(c[,1][c$rr ==i], c[,2][c$rr ==i], col = "grey", lwd = 6)
}

## sla --------
mcmc_data <- sla[sla$sla_max < 600,]
mcmc_data$animal <- mcmc_data$species
comp_data <- clean.data(mcmc_data, clean_tree, data.col = "animal")
f <- list()
f[["total.area"]]  <- (total.area) ~ (sla_max)   
f[["range.size"]] <- (range.size) ~ (sla_max)  
f[["effective.mesh.size"]] <-  (effective.mesh.size) ~ (sla_max)     
f[["mean.shape.index"]] <- mean.shape.index ~ (sla_max)        
f[["prop.landscape"]] <- prop.landscape ~ (sla_max)
f[["perimeter.area.frac.dim"]] <- (perimeter.area.frac.dim) ~ (sla_max)

## extract the posterior estiamtes
rr <- c()
r <- c("total.area", "range.size", "effective.mesh.size", "mean.shape.index", "prop.landscape", "perimeter.area.frac.dim")
c <- tibble("a", "b")
for(i in r) {
  sum <- as.data.frame(summary(m_indiv_sla[[i]][["sla"]][[1]][["Sol"]])[["statistics"]]); 
  c <- rbind(c, c(sum$Mean[1], sum$Mean[2])); 
  rr <- append(rr, paste(i))
}
c <- cbind(c[-1,], rr)


for(i in names(sla[which(names(sla) %in% c("total.area", "range.size", "effective.mesh.size", "mean.shape.index", "prop.landscape", 
                                           "perimeter.area.frac.dim"))])) {
  plot(f[[i]], data = comp_data$data, col = "grey", cex = 0.7, cex.lab = 1.5, cex.main = 1.8,
       #ylab = paste(k[[i]]), main = paste(k[[i]]), 
       xlab = "max sla", bty = "n", main = "", ylab = "")
  abline(c[,1][c$rr ==i], c[,2][c$rr ==i], col = "grey", lwd = 6)
}

## leaf_area ----
mcmc_data <- leaf_area
mcmc_data$animal <- mcmc_data$species
comp_data <- clean.data(mcmc_data, clean_tree, data.col = "animal")
f <- list()
f[["total.area"]]  <- (total.area) ~ (leaf_area_max)   
f[["range.size"]] <- (range.size) ~ (leaf_area_max)  
f[["effective.mesh.size"]] <-  (effective.mesh.size) ~ (leaf_area_max)     
f[["mean.shape.index"]] <- mean.shape.index ~ (leaf_area_max)        
f[["prop.landscape"]] <- prop.landscape ~ (leaf_area_max)
f[["perimeter.area.frac.dim"]] <- (perimeter.area.frac.dim) ~ (leaf_area_max)

## extract the posterior estiamtes
rr <- c()
r <- c("total.area", "range.size", "effective.mesh.size", "mean.shape.index", "prop.landscape", "perimeter.area.frac.dim")
c <- tibble("a", "b")
for(i in r) {
  sum <- as.data.frame(summary(m_indiv_leaf_area[[i]][["leaf_area"]][[1]][["Sol"]])[["statistics"]]); 
  c <- rbind(c, c(sum$Mean[1], sum$Mean[2])); 
  rr <- append(rr, paste(i))
}
c <- cbind(c[-1,], rr)

for(i in names(leaf_area[which(names(leaf_area) %in% c("total.area", "range.size", "effective.mesh.size", "mean.shape.index", "prop.landscape", 
                                                       "perimeter.area.frac.dim"))])) {
  plot(f[[i]], data = comp_data$data, col = "grey", cex = 0.7, cex.lab = 1.5, cex.main = 1.8,
       #ylab = paste(k[[i]]), main = paste(k[[i]]), 
       xlab = paste("max leaf_area"), bty = "n", main = "", ylab = "")
  abline(c[,1][c$rr ==i], c[,2][c$rr ==i], col = "grey", lwd = 6)
}


## woodiness ----------
mcmc_data <- woodiness
mcmc_data$animal <- mcmc_data$species
comp_data <- clean.data(mcmc_data, clean_tree, data.col = "animal")
f <- list()
f[["total.area"]]  <- (total.area) ~ woodiness 
f[["range.size"]] <- (range.size) ~ woodiness
f[["effective.mesh.size"]] <-  (effective.mesh.size) ~ woodiness    
f[["mean.shape.index"]] <- mean.shape.index ~ woodiness       
f[["prop.landscape"]] <- prop.landscape ~ woodiness
f[["perimeter.area.frac.dim"]] <- (perimeter.area.frac.dim) ~ woodiness

## extract the posterior estiamtes
rr <- c()
r <- c("total.area", "range.size", "effective.mesh.size", "mean.shape.index", "prop.landscape", "perimeter.area.frac.dim")
c <- tibble("a", "b")
for(i in r) {
  sum <- as.data.frame(summary(m_indiv_woodiness[[i]][["woodiness"]][[1]][["Sol"]])[["statistics"]]); 
  c <- rbind(c, c(sum$Mean[1], sum$Mean[2])); 
  rr <- append(rr, paste(i))
}
c <- cbind(c[-1,], rr)

colz <- as.data.frame(cbind(c("total.area", "range.size", "effective.mesh.size", "mean.shape.index", "prop.landscape", 
                              "perimeter.area.frac.dim") ,c("purple","purple","#7000A8FF", "#7000A8FF", "purple", "grey")))
for(i in names(woodiness[which(names(woodiness) %in% c("total.area", "range.size", "effective.mesh.size", "mean.shape.index", 
                                                       "prop.landscape", "perimeter.area.frac.dim"))])) {
  plot(f[[i]], data = comp_data$data, cex = 0.7, cex.lab = 1.5, cex.main = 1.8,
       #ylab = paste(k[[i]]), main = paste(k[[i]]), 
       xlab = paste("woodiness"), bty = "n", col = paste(colz$V2[colz$V1 ==i ]), main = "", ylab = "")
  # abline(c[,1][c$rr ==i], c[,2][c$rr ==i], col = "#7000A8FF", lwd = 6)
}

## lifeform ----
mcmc_data <- lifeform
mcmc_data$animal <- mcmc_data$species
comp_data <- clean.data(mcmc_data, clean_tree, data.col = "animal")
f <- list()
f[["total.area"]]  <- (total.area) ~ lifeform 
f[["range.size"]] <- (range.size) ~ lifeform
f[["effective.mesh.size"]] <-  (effective.mesh.size) ~ lifeform    
f[["mean.shape.index"]] <- mean.shape.index ~ lifeform       
f[["prop.landscape"]] <- prop.landscape ~ lifeform
f[["perimeter.area.frac.dim"]] <- (perimeter.area.frac.dim) ~ lifeform


## extract the posterior estiamtes
rr <- c()
r <- c("total.area", "range.size", "effective.mesh.size", "mean.shape.index", "prop.landscape", "perimeter.area.frac.dim")
c <- tibble("a", "b")
for(i in r) {
  sum <- as.data.frame(summary(m_indiv_lifeform[[i]][["lifeform"]][[1]][["Sol"]])[["statistics"]]); 
  c <- rbind(c, c(sum$Mean[1], sum$Mean[2])); 
  rr <- append(rr, paste(i))
}
c <- cbind(c[-1,], rr)

for(i in names(lifeform[which(names(lifeform) %in% c("total.area", "range.size", "effective.mesh.size", "mean.shape.index", 
                                                     "prop.landscape", "perimeter.area.frac.dim"))])) {
  plot(f[[i]], data = comp_data$data, col = "grey", cex = 0.7, cex.lab = 1.5, cex.main = 1.8,
       #ylab = paste(k[[i]]), main = paste(k[[i]]), 
       xlab = paste("lifeform"), bty = "n", 
       main = "", ylab = "")
  # abline(c[,1][c$rr ==i], c[,2][c$rr ==i], col = "#7000A8FF", lwd = 6)
}



par(mfrow = c(1,6), margin(4,4,4,4))
ts <- list(height, sla, seed_mass, leaf_area, woodiness, lifeform)
t <- c("height", "sla", "seed_mass", "leaf_area", "woodiness", "lifeform")
for(i in 1:length(ts)){
  new_tree <- drop.tip(clean_tree, as.character(setdiff(clean_tree$tip.label, 
                                                        unique(ts[[i]][["species"]]))))
  plotTree(new_tree,type="fan",fsize=0.1,lwd=0.5, ftype="i", part = 0.93, main = paste(t[i]))
  mtext(paste(t[i]))
  
}







