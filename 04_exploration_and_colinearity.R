## 04_exploratory_plots_and_colinearity 
## CM 24/03/2021



## range metric histograms in each trait dataset --------------
par(mfrow = c(6,6), mar =c(4,4,1,1))

for (i in names(height[,2:7])) {
  hist(log(height[,i]),
       breaks = 30,
       main = "height",
       xlab = paste(c("log",i), collapse = ""))
}

for (i in names(height[,2:7])) {
  hist(log(leaf_area[,i]),
       main = "LA",
       xlab = paste(c("log",i), collapse = ""))
}

for (i in names(sla[,2:7])) {
  hist(log(sla[,i]),
       breaks = 30,
       main = "sla",
       xlab = paste(c("log",i), collapse = ""))
}

for (i in names(seed_mass[,2:7])) {
  hist(log(seed_mass[,i]),
       breaks = 30,
       main = "seed mass",
       xlab = paste(c("log",i), collapse = ""))
}

for (i in names(lifeform[,2:7])) {
 hist(log(lifeform[,i]),
      breaks = 30,
       main = "lifeform",
      xlab = paste(c("log",i), collapse = ""))
}

for (i in names(woodiness[,2:7])) {
  hist(log(woodiness[,i]),
       breaks = 30,
       main = "wood",
       xlab = paste(c("log",i), collapse = ""))
}


## range metric ~ trait relationshiops -----------------

par(mfrow = c(6,6), mar =c(4,4,1,1)) ## b,l,t,r
j <- list(height, leaf_area, sla, seed_mass)
k <- list(lifeform, woodiness)
## total.area
for(i in 1:length(j)){
  plot(log(total.area) ~ log(j[[i]][,8]), data = j[[i]], col = "black", cex = 0.7, cex.lab = 1.5, cex.main = 1.8)}
for(i in 1:length(k)){
  plot(log(total.area) ~ k[[i]][,8], data = k[[i]], col = "black", cex = 0.7, cex.lab = 1.5, cex.main = 1.8)}

## range.size
for(i in 1:length(j)){
  plot(log(range.size) ~ log(j[[i]][,8]), data = j[[i]], col = "black", cex = 0.7, cex.lab = 1.5, cex.main = 1.8)}
for(i in 1:length(k)){
  plot(log(range.size) ~ k[[i]][,8], data = k[[i]], col = "black", cex = 0.7, cex.lab = 1.5, cex.main = 1.8)}

## effective.mesh.size
for(i in 1:length(j)){
  plot(log(effective.mesh.size) ~ log(j[[i]][,8]), data = j[[i]], col = "black", cex = 0.7, cex.lab = 1.5, cex.main = 1.8)}
for(i in 1:length(k)){
plot(log(effective.mesh.size) ~ k[[i]][,8], data = k[[i]], col = "black", cex = 0.7, cex.lab = 1.5, cex.main = 1.8)}

## mean.shape.index
for(i in 1:length(j)){
  plot((mean.shape.index) ~ log(j[[i]][,8]), data = j[[i]], col = "black", cex = 0.7, cex.lab = 1.5, cex.main = 1.8)}
for(i in 1:length(k)){
  plot((mean.shape.index) ~ k[[i]][,8], data = k[[i]], col = "black", cex = 0.7, cex.lab = 1.5, cex.main = 1.8)}

## prop.landscape
for(i in 1:length(j)){
  plot((prop.landscape) ~ log(j[[i]][,8]), data = j[[i]], col = "black", cex = 0.7, cex.lab = 1.5, cex.main = 1.8)}
for(i in 1:length(k)){
  plot((prop.landscape) ~ k[[i]][,8], data = k[[i]], col = "black", cex = 0.7, cex.lab = 1.5, cex.main = 1.8)}

## perimeter.area.frac.dim
for(i in 1:length(j)){
  plot(log(perimeter.area.frac.dim) ~ log(j[[i]][,8]), data = j[[i]], col = "black", cex = 0.7, cex.lab = 1.5, cex.main = 1.8)}
for(i in 1:length(k)){
  plot(log(perimeter.area.frac.dim) ~ k[[i]][,8], data = k[[i]], col = "black", cex = 0.7, cex.lab = 1.5, cex.main = 1.8)}



## trait colinearity ------------------
par(mfrow = c(5,5), mar =c(4,4,1.4,1)) ## b,l,t,r
## height
a <- droplevels(unique(merge(height, lifeform, by = "species"))) 
b <- droplevels(unique(merge(height, woodiness, by = "species")))
c <- droplevels(unique(merge(height, sla, by = "species"))) 
d <- droplevels(unique(merge(height, seed_mass, by = "species"))) 
e <- droplevels(unique(merge(height, leaf_area, by = "species")))

ma <- lm(log(height_max) ~ lifeform, data = a)
mb <- lm(log(height_max) ~ woodiness, data = b)
mc <- lm(log(height_max) ~ log(sla_max), data = c)
md <- lm(log(height_max) ~ log(seed_mass_max), data = d)
me <- lm(log(height_max) ~ log(leaf_area_max), data = e)

plot(log(height_max) ~ lifeform, data = a)
mtext(paste("r2", round(summary(ma)$adj.r.squared, 4), col = ""), side =3)
plot(log(height_max) ~ woodiness, data = b)
mtext(paste("r2", round(summary(mb)$adj.r.squared, 4), col = ""), side =3)
plot(log(c$height_max) ~ log(c$sla_max))
abline(mc)
mtext(paste("r2", round(summary(mc)$adj.r.squared, 4), "ns", col = ""), side =3, col = "red")
plot(log(d$height_max) ~ log(d$seed_mass_max))
abline(md)
mtext(paste("r2", round(summary(md)$adj.r.squared, 4), col = ""), side =3)
plot(log(e$height_max) ~ log(e$leaf_area_max))
abline(me)
mtext(paste("r2", round(summary(me)$adj.r.squared, 4), "ns", col = ""), side =3, col = "red")

## leaf_area
a <- droplevels(unique(merge(leaf_area, lifeform, by = "species"))) 
b <- droplevels(unique(merge(leaf_area, woodiness, by = "species")))
c <- droplevels(unique(merge(leaf_area, sla, by = "species"))) 
d <- droplevels(unique(merge(leaf_area, seed_mass, by = "species"))) 

ma <- lm(log(leaf_area_max) ~ lifeform, data = a)
mb <- lm(log(leaf_area_max) ~ woodiness, data = b)
mc <- lm(log(leaf_area_max) ~ log(sla_max), data = c)
md <- lm(log(leaf_area_max) ~ log(seed_mass_max), data = d)

plot(log(leaf_area_max) ~ lifeform, data = a)
mtext(paste("r2", round(summary(ma)$adj.r.squared, 4), col = ""), side =3)
plot(log(leaf_area_max) ~ woodiness, data = b)
mtext(paste("r2", round(summary(mb)$adj.r.squared, 4), "ns", col = ""), side =3, col = "red")
plot(log(c$leaf_area_max) ~ log(c$sla_max))
abline(mc)
mtext(paste("r2", round(summary(mc)$adj.r.squared, 4), col = ""), side =3, col = "red")
plot(log(d$leaf_area_max) ~ log(d$seed_mass_max))
abline(md)
mtext(paste("r2", round(summary(md)$adj.r.squared, 4), col = ""), side =3)
plot.new()

## seed_mass
a <- droplevels(unique(merge(seed_mass, lifeform, by = "species"))) 
b <- droplevels(unique(merge(seed_mass, woodiness, by = "species")))
c <- droplevels(unique(merge(seed_mass, sla, by = "species"))) 

ma <- lm(log(seed_mass_max) ~ lifeform, data = a)
mb <- lm(log(seed_mass_max) ~ woodiness, data = b)
mc <- lm(log(seed_mass_max) ~ log(sla_max), data = c)

plot(log(seed_mass_max) ~ lifeform, data = a)
mtext(paste("r2", round(summary(ma)$adj.r.squared, 4), col = ""), side =3)
plot(log(seed_mass_max) ~ woodiness, data = b)
mtext(paste("r2", round(summary(mb)$adj.r.squared, 4), col = ""), side =3)
plot(log(c$seed_mass_max) ~ log(c$sla_max))
abline(mc)
mtext(paste("r2", round(summary(mc)$adj.r.squared, 4), "ns", col = ""), side =3, col = "red")
plot.new()
plot.new()

## sla
a <- droplevels(unique(merge(sla, lifeform, by = "species"))) 
b <- droplevels(unique(merge(sla, woodiness, by = "species")))

ma <- lm(log(sla_max) ~ lifeform, data = a)
mb <- lm(log(sla_max) ~ woodiness, data = b)

plot(log(sla_max) ~ lifeform, data = a)
mtext(paste("r2", round(summary(ma)$adj.r.squared, 4), col = ""), side =3)
plot(log(sla_max) ~ woodiness, data = b)
mtext(paste("r2", round(summary(mb)$adj.r.squared, 4), col = ""), side =3, col = "red")
plot.new()
plot.new()
plot.new()

## woodiness
a <- droplevels(unique(merge(woodiness, lifeform, by = "species"))) 

ma <- glm(woodiness ~ lifeform, family = binomial, data = a)
# mb <- glm(woodiness ~ 1, family = binomial, data = a)
# summary(mb)

# simulationOutput <- simulateResiduals(fittedModel = ma, plot = F)
# plot(simulateResiduals(ma))


plot((woodiness) ~ lifeform, data = a)
mtext(paste("re-do this model"))
#mtext(paste("r2", round(summary(ma)$adj.r.squared, 4), col = ""), side =3)
plot.new()
plot.new()
plot.new()
plot.new()



rm(a,b,c,d,e, j,k,ma,mb,mc,md,me)

## metric colinearity -----------------

f <- list()
f[["total.area"]]  <- log(total.area) ~ log(height_max)   
f[["range.size"]] <- log(range.size) ~ log(height_max)  
f[["effective.mesh.size"]] <-  log(effective.mesh.size) ~ log(height_max)     
f[["mean.shape.index"]] <- mean.shape.index ~ log(height_max)        
f[["prop.landscape"]] <- prop.landscape ~ log(height_max)
f[["perimeter.area.frac.dim"]] <- log(perimeter.area.frac.dim) ~ log(height_max)

pairs(log(metrics[, which(names(metrics) %nin% c("species"))]))
cor(log(metrics[, which(names(metrics) %nin% c("species"))]))

m <- drop_na(metrics)
cor(log(m[, which(names(m) %nin% c("species"))]))

g <- lm(scale(log(total.area)) ~ scale(log(range.size)), data = metrics)
summary(g)

cor.test(log(metrics$total.area),log(metrics$range.size))

## the end ------------




















