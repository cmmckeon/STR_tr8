
#****************************
#comparing model fit tutorial

install.packages("viridis")
library(viridis)
palette(viridis(8))
#test
plot(0,0, col = 4, pch = 16)
setwd("C:/Users/Caroline/OneDrive/Documents/trinity/SS/datahandling/datasets")
ff <- read.csv("curved-data.csv", header = T, stringsAsFactors = FALSE)
attach(ff) 
ls()
str(ff)
null.model <- glm(richness ~ 1, data = ff)

summary(null.model)
gives all your glm parameters
coef(null.model)
#gives intecept
plot(richness~effort, xlab = "Effort", ylab = "Richness", pch = 1, col = 4, main = "Fish species sampling effort", bty = "L", cex.lab = 1.2 )

abline(h = coef(null.model), lty = 2, lwd = 2, col = 6)
#lwd = line width
#lty = line type

rawres <-hist(resid(null.model), 15, col = 7)
plot(residuals.glm(null.model))
abline(0,0)

first.model <- glm(richness ~ effort, data = ff)

summary(first.model)
plot(richness~effort, xlab = "Effort", ylab = "Richness", pch = 1, col = 4, main = "Fish species sampling effort", bty = "L", cex.lab = 1.2 )
abline(16.526004, 0.088395)
graphics.off()
par(mfrow=c(1,2))
hist(resid(first.model), 15, col = 7)
#the number determins the number of bins used in hist
plot(residuals.glm(null.model))
abline (0,0, lty = 2, col = 4)

#add effort squared to the dataframe
ff$effort.sp <- ff$effort ^ 2
secondmodel <- glm(richness ~ effort + effort.sp, data = ff)
summary(secondmodel)


new.effort <- seq(0, 100, 1)
DF <- data.frame(effort = new.effort, effort.sp = new.effort ^ 2)
Whyhat <- predict(secondmodel, newdata = DF)
lines(Whyhat ~ new.effort, data = ff, col = 6, lty = 2, lwd =2)

par(mfrow=c(1,2))
plot(richness~effort, xlab = "Effort", ylab = "Richness", pch = 1, col = 4, main = "Fish species sampling effort", bty = "L", cex.lab = 1.2 )
lines(Whyhat ~ new.effort, data = ff, col = 6, lty = 2, lwd =2)
plot(resid(secondmodel))
abline(0,0)

effort.3 <- ff$effort ^ 3
thirdmodel <- glm(richness ~ effort + effort.sp + effort.3, data = ff)
summary(thirdmodel)


new.effort <- seq(0, 100, 1)
DF3 <- data.frame(effort = new.effort, effort.sp = new.effort ^ 2, effort.3 = new.effort ^ 3)
Whyhat.3 <- predict(thirdmodel, newdata = DF3)
lines(Whyhat ~ new.effort, data = ff, col = 6, lty = 2, lwd =2)
graphics.off()
par(mfrow=c(1,2))
plot(richness~effort, xlab = "Effort", ylab = "Richness", pch = 1, col = 4, main = "Fish species sampling effort", bty = "L", cex.lab = 1.2 )
lines(Whyhat.3 ~ new.effort, data = ff, col = 6, lty = 2, lwd =2)
plot(resid(thirdmodel))
abline(0,0)

#***********************
#modelfour

effort.4 <- ff$effort ^ 4
model4 <- glm(richness ~ effort + effort.sp + effort.3 + effort.4, data = ff)
summary(model4)

DF4 <- data.frame(effort = new.effort, effort.sp = new.effort ^ 2, effort.3 = new.effort ^ 3, effort.4 = new.effort ^ 4)
Whyhat.4 <- predict(model4, newdata = DF4)
graphics.off()
par(mfrow=c(1,2))
plot(richness~effort, xlab = "Effort", ylab = "Richness", pch = 1, col = 4, main = "Fish species sampling effort", bty = "L", cex.lab = 1.2 )
lines(Whyhat.4 ~ new.effort, data = ff, col = 6, lty = 2, lwd =2)
plot(resid(model4))
abline(0,0)

#***********************
#model 5

effort.5 <- ff$effort ^ 5
model5 <- glm(richness ~ effort + effort.sp + effort.3 + effort.4 + effort.5, data = ff)
summary(model5)

#**************************8
#model 6

effort.6 <- ff$effort ^ 6
model6 <- glm(richness ~ effort + effort.sp + effort.3 + effort.4 + effort.5 + effort.6, data = ff)
summary(model6)

#*********************88888
#model 7

effort.7 <- ff$effort ^ 7
model7 <- glm(richness ~ effort + effort.sp + effort.3 + effort.4 + effort.5 + effort.6 + effort.7, data = ff)
summary(model7)


#************************
# model 8

effort.8 <- ff$effort ^ 8
model8 <- glm(richness ~ effort + effort.sp + effort.3 + effort.4 + effort.5 + effort.6 + effort.7 + effort.8, data = ff)
summary(model8)



