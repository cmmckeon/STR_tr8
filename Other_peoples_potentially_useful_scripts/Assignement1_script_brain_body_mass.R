# seed weight exercise

getwd()
setwd("C:/Users/Caroline/OneDrive/Documents/trinity/SS/datahandling/datasets")
bb = read.csv(file.choose("brain_data_clean.csv"))

str(bb)
head(bb)
print(bb)

plot(bb$body_mass_g, bb$brain_mass_g)
plot(bb$class, bb$brain_mass_g)

ratio <-bb$brain_mass_g/bb$body_mass_g
plot(bb$class, bb$ratio)
#it now becomes clear there are some wild outliers; 
#no brain/body ratio should be close to 0.5, and three data points are well above this

idx <- order(bb$ratio)
print(bb[idx, ])

#the largest 3 ratios are over 0.3, the next largest is 0.05, which is not large enough to discard
#clumsily removing the unwanted rows

bb1 <- bb[2:30,]
bb2 <- bb[32:40,]
bb3 <- bb[42:50,]

BB <-rbind(bb1, bb2, bb3)
print(BB)

ratio <-BB$brain_mass_g/BB$body_mass_g
?boxplot
?plot
plot(BB$class, BB$ratio)

par(col="gold")

meanaves <-mean(BB$ratio[BB$class=="aves"])
meanmammalia <-mean(BB$ratio[BB$class=="mammalia"])


  

plot(BB$class, BB$ratio, main = "Comparision of Brain/Body mass Ratio in Aves & Mammalia", xlab = "Class", ylab = "Ratio of Brain/Body mass", mean, col=(c("gold","gold")))
text(BB$class[BB$class=="aves"], 0.05, "N = 6", cex = 1)
text(BB$class[BB$class=="mammalia"], 0.05, "N = 41")
mean <-tapply(BB$ratio, BB$class, mean)
points(1:2, mean, pch = 16, cex = 1, col=(c("black","black")))

#***********************
lratio <-log10(BB$ratio)
par(bty = "l")
plot(BB$class, BB$lratio, xlab = "Class", ylab = "Ratio of Brain/Body mass (grams)", col=(c("blue","red")))
text(BB$class[BB$class=="aves"], -1.25, "N = 6", cex = 1, adj = c(2,1) )
text(BB$class[BB$class=="mammalia"], -1.3, "N = 41", adj = c(2,1))
     
lmean <-tapply(BB$lratio, BB$class, mean) 
points(1:2, lmean, pch = 16, cex = 1, col=(c("black","black")))

summary(BB$lratio[BB$class=="aves"])
summary(BB$lratio[BB$class=="mammalia"])


#scatter plot
par(bty = "l")
plot(BB$lbody, BB$lbrain, type = "n", xlab = "Log10 Body mass (g)", ylab = "Log10 Brain mass (g)")


#subset or [] the data to have two different colours 
points(BB$lbody[BB$class=="aves"],BB$lbrain[BB$class=="aves"], col = "blue", pch = 16)

points(BB$lbody[BB$class=="mammalia"],BB$lbrain[BB$class=="mammalia"], col = "red", pch = 15)
#trend line
#this one doesn't work
trendlineaves <-lm(BB$lbody[BB$class=="aves"], BB$lbrain[BB$class=="aves"])


trendlineaves <-lm(BB$lbrain[BB$class=="aves"] ~ BB$lbody[BB$class=="aves"])
trendlinemam <-lm(BB$lbrain[BB$class=="mammalia"] ~ BB$lbody[BB$class=="mammalia"])
#and for mammalia
#must now name so that you can use it on graph
abline(trendlinemam, col = "red", lwd = 2)
abline(trendlineaves, col = "blue", lwd = 2)

legend("bottomright", levels(BB$class), bty = "n", col = c("blue", "red"), pch = c(16, 15))

summary(BB$lbody[BB$class=="aves"], BB$lbrain[BB$class=="aves"])
summary(BB$lbody[BB$class=="mammalia"], BB$lbrain[BB$class=="mammalia"])

labels = formatC(means, format = "f", digits = 1)

?stripchart
stripchart(BB$class~BB$ratio, vertical = T, pch = 16, cex = 0.5)





legend("topleft" , pch=c(2,2), col=c("darkorchid" , "darkcyan") , c("aves" , "mammalia"), bty="o", cex=.8, box.col="black"






stripchart(dat1$wt ~ dat1$country, vertical = T, pch = 16, cex = 0.5)

boxplot(dat1$wt~dat1$country)
stripchart(dat1$wt~dat1$country, vertical = T, pch = 16, cex = 0.5, add = T)

country_wt = tapply(dat1$wt, dat1$country, mean)
barplot(country_wt, ylim = c(0,140))


# make a new grouping variable that enables plotting of native & exotic countries together
dat1$range_country = paste(ifelse(dat1$range == "Exotic", "E", "N"), ":",dat1$country, sep = "")
boxplot(dat1$wt ~ dat1$range_country)
# now all exotic (E) countries are plotted first followed by all native "N" countries

# Make your boxplot more readable by reducing "chartjunk"
?par  # the help file for par gives you axis to lots of options for modifying the look of plots
par(bty = "l") # gets rid of the box around the plot area
par(las = 2) # puts axis group labels perpendicular to the axis making them more readable
boxplot(dat1$wt ~ dat1$range_country, pars = list(boxwex = 0.5, ylab = "Seed weight mg")) # makes boxes a bit narrower
stripchart(dat1$wt~dat1$range_country, vertical = T, pch = 16, cex = 0.5, add = T)


## Exploring how seed weight affects seed predator (beetle) weight

beetle_data = read.csv("seed_beetle_weight.csv")
plot(beetle_data$seed_size_mg,beetle_data$elytra_area)

plot(beetle_data$seed_size_mg,beetle_data$elytra_area, xlab = "seed size mg")

# set up axes and add some points but not all - you can play around with this to change colours of points by site or sex
plot(beetle_data$seed_size_mg,beetle_data$elytra_area, xlab = "seed size mg", type = "n")
points(beetle_data$seed_size_mg[beetle_data$site == "East Block"], beetle_data$elytra_area[beetle_data$site == "East Block"], col="green")



par(mfrow = c(1,1))
interaction.plot(succ, RGR_day33, Rel_fec)


#looking at Saoirse's logistic regression plotting problem
gh <- read.csv(file.choose() , header = TRUE, stringsAsFactors=TRUE)
head(gh)
color.dark <-as.factor(gh$colour.dark)
plot(gh$treat,gh$color.dark)
#come back to this some other time
ggplot2::aes(x=position, y=prob)