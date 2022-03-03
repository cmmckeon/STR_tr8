## 04_exploratory_plots_and_colinearity 
## CM 24/03/2021



## range metric histograms in each trait dataset --------------
par(mfrow = c(6,6), mar =c(4,4,1,1))

for (i in names(height[,2:7])) {
  hist(
    (height[,i]),
       breaks = 30,
       main = "height",
       xlab = paste(c("",i), collapse = ""))
}

for (i in names(height[,2:7])) {
  hist(
    (leaf_area[,i]),
       main = "LA",
       xlab = paste(c("",i), collapse = ""))
}

for (i in names(sla[,2:7])) {
  hist(
    (sla[,i]),
       breaks = 30,
       main = "sla",
       xlab = paste(c("",i), collapse = ""))
}

for (i in names(seed_mass[,2:7])) {
  hist(
    (seed_mass[,i]),
       breaks = 30,
       main = "seed mass",
       xlab = paste(c("",i), collapse = ""))
}

for (i in names(lifeform[,2:7])) {
 hist(
   (lifeform[,i]),
      breaks = 30,
       main = "lifeform",
      xlab = paste(c("",i), collapse = ""))
}

for (i in names(woodiness[,2:7])) {
  hist(
       (woodiness[,i]),
       breaks = 30,
       main = "wood",
       xlab = paste(c("",i), collapse = ""))
}


## range metric ~ trait relationshiops -----------------

par(mfrow = c(6,6), mar =c(4,4,1,1)) ## b,l,t,r
j <- list(height, leaf_area, sla, seed_mass)
k <- list(lifeform, woodiness)
## total.area
for(i in 1:length(j)){
  plot(
    (total.area) ~ 
    (j[[i]][,9]), data = j[[i]], col = "black", cex = 0.7, cex.lab = 1.5, cex.main = 1.8)}
for(i in 1:length(k)){
  plot(
       (total.area) ~ k[[i]][,9], data = k[[i]], col = "black", cex = 0.7, cex.lab = 1.5, cex.main = 1.8)}

## range.size
for(i in 1:length(j)){
  plot(
       (range.size) ~ 
       (j[[i]][,9]), data = j[[i]], col = "black", cex = 0.7, cex.lab = 1.5, cex.main = 1.8)}
for(i in 1:length(k)){
  plot(
       (range.size) ~ k[[i]][,9], data = k[[i]], col = "black", cex = 0.7, cex.lab = 1.5, cex.main = 1.8)}

## effective.mesh.size
for(i in 1:length(j)){
  plot(
       (effective.mesh.size) ~ 
       (j[[i]][,9]), data = j[[i]], col = "black", cex = 0.7, cex.lab = 1.5, cex.main = 1.8)}
for(i in 1:length(k)){
plot(
     (effective.mesh.size) ~ k[[i]][,9], data = k[[i]], col = "black", cex = 0.7, cex.lab = 1.5, cex.main = 1.8)}

## mean.shape.index
for(i in 1:length(j)){
  plot((mean.shape.index) ~ 
       (j[[i]][,9]), data = j[[i]], col = "black", cex = 0.7, cex.lab = 1.5, cex.main = 1.8)}
for(i in 1:length(k)){
  plot((mean.shape.index) ~ k[[i]][,9], data = k[[i]], col = "black", cex = 0.7, cex.lab = 1.5, cex.main = 1.8)}

## prop.landscape
for(i in 1:length(j)){
  plot((prop.landscape) ~ 
       (j[[i]][,9]), data = j[[i]], col = "black", cex = 0.7, cex.lab = 1.5, cex.main = 1.8)}
for(i in 1:length(k)){
  plot((prop.landscape) ~ k[[i]][,9], data = k[[i]], col = "black", cex = 0.7, cex.lab = 1.5, cex.main = 1.8)}

## perimeter.area.frac.dim
for(i in 1:length(j)){
  plot(
       (perimeter.area.frac.dim) ~ 
       (j[[i]][,9]), data = j[[i]], col = "black", cex = 0.7, cex.lab = 1.5, cex.main = 1.8)}
for(i in 1:length(k)){
  plot(
       (perimeter.area.frac.dim) ~ k[[i]][,9], data = k[[i]], col = "black", cex = 0.7, cex.lab = 1.5, cex.main = 1.8)}



## trait colinearity ------------------
par(mfrow = c(5,5), mar =c(4,4,1.4,1), col = "black") ## b,l,t,r
## height
a <- droplevels(unique(merge(height, lifeform, by = "species"))) 
b <- droplevels(unique(merge(height, woodiness, by = "species")))
c <- droplevels(unique(merge(height, sla, by = "species"))) 
d <- droplevels(unique(merge(height, seed_mass, by = "species"))) 
e <- droplevels(unique(merge(height, leaf_area, by = "species")))

ma <- lm(
         height_max ~ lifeform, data = a)
mb <- lm(
         height_max ~ woodiness, data = b)
mc <- lm(
         height_max ~ 
         sla_max, data = c)
md <- lm(
         height_max ~ 
         (seed_mass_max), data = d)
me <- lm(
         height_max ~ 
         leaf_area_max, data = e)

plot(
     height_max ~ lifeform, data = a)
mtext(paste("r2", round(summary(ma)$adj.r.squared, 4), col = ""), side =3)
plot(
     height_max ~ woodiness, data = b)
mtext(paste("r2", round(summary(mb)$adj.r.squared, 4), col = ""), side =3)
plot(
     (c$height_max) ~ 
     (c$sla_max))
abline(mc)
mtext(paste("r2", round(summary(mc)$adj.r.squared, 4), col = ""), side =3)
plot(
     (d$height_max) ~ 
     (d$seed_mass_max))
abline(md)
mtext(paste("r2", round(summary(md)$adj.r.squared, 4), col = ""), side =3)
plot(
     (e$height_max) ~ 
     (e$leaf_area_max))
abline(me)
mtext(paste("r2", round(summary(me)$adj.r.squared, 4), col = ""), side =3)

## leaf_area
a <- droplevels(unique(merge(leaf_area, lifeform, by = "species"))) 
b <- droplevels(unique(merge(leaf_area, woodiness, by = "species")))
c <- droplevels(unique(merge(leaf_area, sla, by = "species"))) 
d <- droplevels(unique(merge(leaf_area, seed_mass, by = "species"))) 

ma <- lm(
         leaf_area_max ~ lifeform, data = a)
mb <- lm(
         leaf_area_max ~ woodiness, data = b)
mc <- lm(
         leaf_area_max ~ 
         sla_max, data = c)
md <- lm(
         leaf_area_max ~ 
         (seed_mass_max), data = d)

plot.new()
plot(
     leaf_area_max ~ lifeform, data = a)
mtext(paste("r2", round(summary(ma)$adj.r.squared, 4), col = ""), side =3)
plot(
     leaf_area_max ~ woodiness, data = b)
mtext(paste("r2", round(summary(mb)$adj.r.squared, 4), col = ""), side =3)
plot(
     (c$leaf_area_max) ~ 
       (c$sla_max))
abline(mc)
mtext(paste("r2", round(summary(mc)$adj.r.squared, 4), col = ""), side =3)
plot(
     (d$leaf_area_max) ~ 
     (d$seed_mass_max))
abline(md)
mtext(paste("r2", round(summary(md)$adj.r.squared, 4), col = ""), side =3)



## seed_mass
a <- droplevels(unique(merge(seed_mass, lifeform, by = "species"))) 
b <- droplevels(unique(merge(seed_mass, woodiness, by = "species")))
c <- droplevels(unique(merge(seed_mass, sla, by = "species"))) 

ma <- lm(
         seed_mass_max~ lifeform, data = a)
mb <- lm(
         seed_mass_max~ woodiness, data = b)
mc <- lm(
         seed_mass_max~ 
         sla_max, data = c)

plot.new()
plot.new()
plot(
     seed_mass_max~ lifeform, data = a)
mtext(paste("r2", round(summary(ma)$adj.r.squared, 4), col = ""), side =3)
plot(
     seed_mass_max~ woodiness, data = b)
mtext(paste("r2", round(summary(mb)$adj.r.squared, 4), col = ""), side =3)
plot(
     (c$seed_mass_max) ~ 
     (c$sla_max))
abline(mc)
mtext(paste("r2", round(summary(mc)$adj.r.squared, 4), col = ""), side =3)


## sla
a <- droplevels(unique(merge(sla, lifeform, by = "species"))) 
b <- droplevels(unique(merge(sla, woodiness, by = "species")))

ma <- lm(
         sla_max ~ lifeform, data = a)
mb <- lm(
         sla_max ~ woodiness, data = b)

plot.new()
plot.new()
plot.new()
plot(
     sla_max ~ lifeform, data = a)
mtext(paste("r2", round(summary(ma)$adj.r.squared, 4), col = ""), side =3)
plot(
     sla_max ~ woodiness, data = b)
mtext(paste("r2", round(summary(mb)$adj.r.squared, 4), col = ""), side =3)



## woodiness
a <- droplevels(unique(merge(woodiness, lifeform, by = "species"))) 
a$woodiness <- as.numeric(a$woodiness)
a$woodiness <- a$woodiness -1

ma <- glm(woodiness ~ lifeform, family = binomial, data = a)
# mb <- glm(woodiness ~ 1, family = binomial, data = a)

 # simulationOutput <- simulateResiduals(fittedModel = ma, plot = F)
 # plot(simulateResiduals(ma))

plot.new()
plot.new()
plot.new()
plot.new()
plot((woodiness) ~ lifeform, data = a)
mtext(paste("r2", round(summary(mb)$adj.r.squared, 4), col = ""), side =3)
#mtext(paste("r2", round(summary(ma)$adj.r.squared, 4), col = ""), side =3)




rm(a,b,c,d,e, j,k,ma,mb,mc,md,me)

## metric colinearity -----------------
m <- metrics

upper.panel<-function(x, y){
  points(x,y, pch=21, col=c("grey"))
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  text(0.8, 0.9, txt, cex =1)
}

pairs(drop_na(m[, which(names(m) %nin% c("species"))]), lower.panel = NULL, upper.panel = upper.panel)

# print(cor(metrics[, which(names(metrics) %nin% c("species"))]))
# print(cor(log(metrics[, which(names(metrics) %nin% c("species"))])))
print(cor(m[, which(names(m) %nin% c("species"))]))

m <- drop_na(metrics)
print(cor((m[, which(names(m) %nin% c("species"))])))

# g <- lm(total.area ~ range.size, data = metrics)
# summary(g)
# g <- lm(scale(log(total.area)) ~ scale(log(range.size)), data = metrics)
# summary(g)
# 
# print(cor.test(
#                (metrics$total.area),
#                (metrics$range.size)))

## the end ------------



















