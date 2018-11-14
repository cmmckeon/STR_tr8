
#*************************************************
#Assignment Q1.

#finds the location you will be automatically brough to when reading in files
getwd()

#lets you dictate which folder you go to for reading in files (Windows uses backslashes; 
#must manually change them to forward slashes to be successfully read by R)
setwd("C:/Users/Caroline/Dropbox/MRes/R course")

#names the data you read in
q1 <- read.csv(file.choose() , header = TRUE, stringsAsFactors=FALSE)

#gives first few rows of data matrix (?)
head(q1)
#shows internal dataset structure 
str(q1)
#gives whole dataset 
print(q1)
#gives names of vectors within dataset
names(q1)
#allows R to interact directly with elements within the dataset, without having to specify what they are a subset of
attach(q1)
#gives length of vectors
length(Density)

#turns dataset into dataframe
Q1<-as.data.frame(q1)

#creates new vector made of entries from rows in Winged colume divided by entries from Total colume
Proportion <-(Winged/Total)

#checking how this vector looks
head(Proportion)
length(Proportion)

#adding new Proportion colume to dataframe
Q1 <- cbind(Q1,Proportion)

#making the graphics nice
par(bty = "l")
par(mfrow = c(1,1))

#visually assessing the data
plot(Proportion ~ log(Density), bty = "n", main ="Initial Density of Colony vs Final Proportion of Winged Aphids", cex.main = 0.7, xlab = "Log Inital Density", ylab = "Proportion of Winged Aphids")
abline(coef(m2), lwd = 2, col = "black")

#need to tell R that "Predator" colume is a factor
Pred <-as.factor(Predator)
plot(Proportion ~ Pred, main ="Predator Presence/Absence vs Final Proportion of Winged Aphids",
     cex.main = 0.7, xlab = "Predator", ylab = "Proportion of Winged Aphids")

#analysis of relationship between predator, density and proportion-with-wings
m1 <-glm(Proportion ~ Pred, family = "quasibinomial"(link="logit"), data = Q1)
hist(resid(m1), xlab = "Residuals", 15, cex.lab = 1.2, 15, main = "Histogram of errors")
summary(m1)
par(mfrow = c(2,2))
plot(m1)

m2 <-glm(Proportion ~ log(Density), family = "quasibinomial"(link="logit"), data = Q1)
hist(resid(m2), xlab = "Residuals", 15, cex.lab = 1.2, 15, main = "Histogram of errors")
summary(m2)
summary.lm(m2)
par(mfrow = c(2,2))
plot(m2)

m3 <-glm(Proportion ~ Pred*log(Density), family = "quasibinomial"(link="logit"), data = Q1)
summary(m3)
par(mfrow = c(2,2))
plot(m3)

#testing for normality in resiudals 
shapiro.test(resid)
shapiro.test(c(Proportion[Absent]-mean(Proportion[Absent]),Present-mean(Present)))

ls()
detach(Q1)


#*******************************************************
#Assignment Q2.

q2 <- lynx
head(q2)
tail(q2)
str(q2)
length(q2)

t<- (1821:1934)
head(t)
tail(t)

t2<-(q2[2:114])
length(t2)
head(t2)
T2 <-append(t2, 1, after = length(t2))
length(T2)

pgr <-log(T2/q2) 

#making a dataframe
Q2 <-(cbind.data.frame(q2, t, T2, pgr))

#visual exploration of the data
par(bty = "l")
par(mfrow = c(1,1))
plot(as.numeric(q2)~(t), xlab = "Time in years", ylab = "Lynx Popultion", main = "Lynx Population Cycles", data = Q2, type = "l")

m1 <-glm(pgr ~ log(q2), family = "gaussian"(link="identity"), data = Q2)
hist(resid(m1), xlab = "Residuals", 15, cex.lab = 1.2, 15, main = "Histogram of errors")
summary(m1)
par(mfrow = c(2,2))
plot(m1)

m2 <-glm(pgr ~ log(T2), family = "gaussian"(link="identity"), data = Q2)
summary(m2)
par(mfrow = c(2,2))
plot(m2)

#fix the final year mismatch issue
cheating <-Q2[-(114),, drop=TRUE]

#visual exploration of population vs growth-rate relationship
par(mfrow = c(1,1))
plot(pgr~log(q2), main = "Log of Population at time t vs Population Growth Rate", 
     cex.main = 0.8, xlab = "Log of lynx popultion", ylab = "Population growth rate", data = cheating)
abline(m1)
plot(pgr~log(T2), main = "Log of Population at time t+1 vs Population Growth Rate", 
     cex.main = 0.8, xlab = "Log of lynx popultion", ylab = "Population growth rate", data = cheating)
abline(m2)

#analysis of population effect on growth rate
m1 <-glm(pgr ~ log(q2), family = "gaussian"(link="identity"), data = cheating)
hist(resid(m1), xlab = "Residuals", 15, cex.lab = 1.2, 15, main = "Histogram of errors")
summary(m1)
summary.lm(m1)
par(mfrow = c(2,2))
plot(m1)

m2 <-glm(pgr ~ log(T2), family = "gaussian"(link="identity"), data = cheating)
hist(resid(m2), xlab = "Residuals", 15, cex.lab = 1.2, 15, main = "Histogram of errors")
summary(m2)
summary.lm(m2)
par(mfrow = c(2,2))
plot(m2)

m3 <-glm(pgr ~ log(T2)+log(q2), family = "gaussian"(link="identity"), data = cheating)
hist(resid(m3), xlab = "Residuals", 15, cex.lab = 1.2, 15, main = "Histogram of errors")
summary(m3)
par(mfrow = c(2,2))
plot(m3)

m4 <-glm(pgr ~ log(T2)*log(q2), family = "gaussian"(link="identity"), data = cheating)
hist(resid(m4), xlab = "Residuals", 15, cex.lab = 1.2, 15, main = "Histogram of errors")
summary(m4)
par(mfrow = c(2,2))
plot(m4)


#*********************************************
#Assignment Q3.

#reading in the data
q3 <- read.csv(file.choose() , header = TRUE, stringsAsFactors=FALSE)

#prilimiary exploration of data
head(q3)

attach(q3)
length(colony)
length(panel)
str(q3)

#gives vector name for colume x of a matrix 
names(q3[2])

#plotting all 4 growth rates against relative fecundifty
par(mfrow = c(2,2), bty = "l") 
plot(Rel_fec ~ RGR_day9, xlab = "RGR_day9", ylab = "Relative Fecundity")
m1 <- glm(Rel_fec ~ RGR_day9, family = "gaussian", data = q3)
abline(coef(m1), lwd = 2, col = "blue")

plot(Rel_fec ~ RGR_day17, ylab = "Relative Fecundity")
m2 <- glm(Rel_fec ~ RGR_day17, family = "gaussian", data = q3)
abline(coef(m2), lwd = 2, col = "red")

plot(Rel_fec ~ RGR_day25, ylab = "Relative Fecundity")
m3 <- glm(Rel_fec ~ RGR_day25, family = "gaussian", data = q3)
abline(coef(m3), lwd = 2, col = "yellow")

plot(Rel_fec ~ RGR_day33, ylab = "Relative Fecundity")
m4 <- glm(Rel_fec ~ RGR_day33, family = "gaussian", data = q3)
abline(coef(m4), lwd = 2, col = "green")

#plotting all growth rates together
par(mfrow = c(1,1), bty = "l") 
plot(Rel_fec ~ RGR_day9, , type = "n", main="Growth rates vs Relative fecundity", xlab = "RGR_day", ylab = "Relative Fecundity")
points(RGR_day9,Rel_fec, col = "blue", pch = 16)
points(RGR_day17,Rel_fec, col = "red", pch = 16)
points(RGR_day25,Rel_fec, col = "yellow", pch = 16)
points(RGR_day33,Rel_fec, col = "green", pch = 16)


m1 <- glm(Rel_fec ~ RGR_day9, family = "gaussian", data = q3)
abline(coef(m1), lwd = 2, col = "blue")

m2 <- glm(Rel_fec ~ RGR_day17, family = "gaussian", data = q3)
abline(coef(m2), lwd = 2, col = "red")

m3 <- glm(Rel_fec ~ RGR_day25, family = "gaussian", data = q3)
abline(coef(m3), lwd = 2, col = "yellow")

m4 <- glm(Rel_fec ~ RGR_day33, family = "gaussian", data = q3)
abline(coef(m4), lwd = 2, col = "green")

#so relative fecundidty does APPEAR to vary with Growth rate on different days. But is this statistically significant?
#and is there a significant difference between Relative fecundity and higher RGR on different days?

#multiple regression to analysis if the relationship between RGR and Relative fecundity differs between days
M <-glm(Rel_fec~RGR_day9+RGR_day17+RGR_day25+RGR_day33)
hist(resid(M), xlab = "Residuals", 15, cex.lab = 1.2, main = "Histogram of errors")
summary(M)
summary.lm(M)
par(mfrow = c(2,2))
plot(M)

#plotting Relative fecundity against succesional stage for visual exploration of data
succ <- factor(succ, levels = c("early","mid","late"))
par(mfrow=c(1,1))
plot(Rel_fec ~ succ, cex.main = 0.8, main="Successional stage vs Relative fecundity", xlab = "Successional Stage", ylab = "Relative Fecundity")

#Statistical investigation of whether succesional stage has an effect on Relative fecundity
M1 <-glm(Rel_fec ~ succ, family = "gaussian"(link="identity"), data = q3)
hist(resid(M1), xlab = "Residuals", 15, cex.lab = 1.2, main = "Histogram of errors")
summary(M1)
par(mfrow = c(2,2))
plot(M1)

#Looking at the normality of succesional data level "mid"
par(mfrow = c(1,1))
plot(Rel_fec[succ=="mid"] ~ RGR_day9[succ=="mid"])
M2 <-glm(Rel_fec[succ=="mid"] ~ RGR_day9[succ=="mid"])
hist(resid(M2), xlab = "Residuals", 15, cex.lab = 1.2, main = "Histogram of errors")
summary(M2)
summary.lm(M2)
par(mfrow = c(2,2))
plot(m1)

#Visually exploring growth rate on day 9 in relation to successional stage
par(mfrow = c(1,1))
plot(Rel_fec ~ RGR_day9, type = "n", cex.main = 0.8, main = "Day 9 Growth Rate vs Relative Fecundity")
points(RGR_day9[succ=="early"],Rel_fec[succ=="early"], col = "blue", pch = 16)
points(RGR_day9[succ=="mid"],Rel_fec[succ=="mid"], col = "red", pch = 16)
points(RGR_day9[succ=="late"],Rel_fec[succ=="late"], col = "yellow", pch = 16)

m1 <- glm(Rel_fec[succ=="early"]~RGR_day9[succ=="early"],family = "gaussian", data = q3)
abline(coef(m1), lwd = 2, col = "blue")
m2 <- glm(Rel_fec[succ=="mid"]~RGR_day9[succ=="mid"], family = "gaussian", data = q3)
abline(coef(m2), lwd = 2, col = "red")
m3 <- glm(Rel_fec[succ=="late"]~RGR_day9[succ=="late"], family = "gaussian", data = q3)
abline(coef(m3), lwd = 2, col = "yellow")

#looking at whether succesional stage interacts with growth rate to effect overall fecundity
#Models are run without, and then with an interation term
M2 <-glm(Rel_fec ~ succ+RGR_day9, family = "gaussian"(link = "identity"), data = q3)
hist(resid(M2), xlab = "Residuals", 15, cex.lab = 1.2, main = "Histogram of errors")
summary(M2)
par(mfrow = c(2,2))
plot(M2, bty ="l")

M2 <-glm(Rel_fec ~ succ * RGR_day9, family = "gaussian"(link = "identity"), data = q3)
hist(resid(M2), xlab = "Residuals", 15, cex.lab = 1.2, main = "Histogram of errors")
summary(M2)
par(mfrow = c(2,2))
plot(M2, bty ="l")

M3 <-glm(Rel_fec ~ succ+RGR_day17, family = "gaussian"(link = "identity"), data = q3)
hist(resid(M3), xlab = "Residuals", 15, cex.lab = 1.2, main = "Histogram of errors")
summary(M3)
par(mfrow = c(2,2))
plot(M3, bty ="l")

M3 <-glm(Rel_fec ~ succ*RGR_day17, family = "gaussian"(link = "identity"), data = q3)
hist(resid(M3), xlab = "Residuals", 15, cex.lab = 1.2, main = "Histogram of errors")
summary(M3)
par(mfrow = c(2,2))
plot(M3, bty ="l")

M4 <-glm(Rel_fec ~ succ+RGR_day25, family = "gaussian"(link = "identity"), data = q3)
hist(resid(M4), xlab = "Residuals", 15, cex.lab = 1.2, main = "Histogram of errors")
summary(M4)
par(mfrow = c(2,2))
plot(M4, bty ="l")

M4 <-glm(Rel_fec ~ succ*RGR_day25, family = "gaussian"(link = "identity"), data = q3)
hist(resid(M4), xlab = "Residuals", 15, cex.lab = 1.2, main = "Histogram of errors")
summary(M4)
par(mfrow = c(2,2))
plot(M4, bty ="l")

M5 <-glm(Rel_fec ~ succ+RGR_day33, family = "gaussian"(link = "identity"), data = q3)
hist(resid(M5), xlab = "Residuals", 15, cex.lab = 1.2, main = "Histogram of errors")
summary(M5)
par(mfrow = c(2,2))
plot(M5, bty ="l")

M5 <-glm(Rel_fec ~ succ*RGR_day33, family = "gaussian"(link = "identity"), data = q3)
hist(resid(M5), xlab = "Residuals", 15, cex.lab = 1.2, main = "Histogram of errors")
summary(M5)
par(mfrow = c(2,2))
plot(M5, bty ="l")

detach()

