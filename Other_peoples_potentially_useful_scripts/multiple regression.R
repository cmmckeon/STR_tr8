#multiple regression tutrial
#here we go

O <-read.csv(file.choose())
head(O)
str(O)
attach(O)
wind <-as.numeric(wind)
rad <-as.numeric(rad)
temp <-as.numeric(temp)
ozone <-as.numeric(ozone)

pairs(O, panel=panel.smooth)
install.packages("mgcy")
install.packages("tree")
library(tree)

m <-lm(ozone~temp*wind*rad+I(rad^2)+I(temp^2)+I(wind^2))
summary(m)

plot(m)
m2 <-update(m,~.-temp:wind:rad)
plot(m2)
summary(m2)
anova(m,m2,test="F")

#also not significant
m3 <-update(m2,~.-wind:rad)
anova(m2,m3,test="F")

m6 <-update(m2,~.-wind:rad-temp:rad-temp:wind)
par(mfrow=c(2,2))
plot(m6)
summary(m6)

nm <-lm(log(ozone)~temp+wind+rad+I(temp^2)+I(wind^2)+I(rad^2))
summary(nm)

nm2 <-update(nm,~.-I(temp^2))
summary(nm2)

nm3 <-update(nm2,~.-I(rad^2))
summary(nm3)
par(mfrow=c(2,2))
plot(nm3)

O2 <- O[-(17), ,drop=TRUE]
attach(O2)
nm4 <-lm(log(O2$ozone)~O2$temp+O2$wind+O2$rad+I(O2$wind^2))
plot(nm4)
