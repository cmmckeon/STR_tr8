#housekeeping
setwd("C:/Users/Caroline/Dropbox/Thesis/")

install.packages("MASS")
library(MASS)
install.packages("plyr")
library(plyr)

new_df <-read.csv(file.choose())

ESM <-new_df[which(new_df$Model=="ESM_bin"),]
#write.csv(ESM, "ESM.csv", row.names = FALSE)

#dataset containing analysis variables only
ESM <-as.data.frame(c(ESM[5],ESM[60:70]))
#write.csv(ESM, "ESM.csv", row.names = FALSE)

#great function
table(sbank)

attach(ESM)
#***********************************
#test model - initial look at analysis
#canopy_height

par(mfrow=c(1,1), bty="l")
plot(log(total.area) ~ log(canopy_height), data = ESM)
m1 <- glm(log(total.area) ~ log(canopy_height), family = "gaussian", data = ESM)
abline(coef(m1), lwd = 2)
hist(resid(m1), xlab = "Residuals", 15, cex.lab = 1.2, main = "Histogram of errors")
summary(m1)
summary.lm(m1)
par(mfrow = c(2,2))
plot(m1)
length(which(!is.na(canopy_height)))


#****************************************
#seed_number_per_shoot

par(mfrow = c(1,1), bty = "l")
plot(log(total.area) ~ log(seed_number_per_shoot), data = ESM)
m1 <- glm(log(total.area) ~ log(seed_number_per_shoot), family = "gaussian", data = ESM)
abline(coef(m1), lwd = 2)
hist(resid(m1), xlab = "Residuals", 15, cex.lab = 1.2, main = "Histogram of errors")
summary(m1)
summary.lm(m1)
par(mfrow = c(2,2))
plot(m1)

length(which(!is.na(seed_number_per_shoot)))

#***************************
#seed_mass

par(mfrow = c(1,1), bty = "l")
plot(log(total.area) ~ log(seed_mass), data = ESM)
m1 <- glm(log(total.area) ~ log(seed_mass), family = "gaussian", data = ESM)
abline(coef(m1), lwd = 2)
hist(resid(m1), xlab = "Residuals", 15, cex.lab = 1.2, main = "Histogram of errors")
summary(m1)
summary.lm(m1)
par(mfrow = c(2,2))
plot(m1)

length(which(!is.na(seed_mass)))

#***********************
#sex_repod_fr

table(ESM$sex_reprod_fr)

#functioning code for renaming/handling levels
ESM$sex_reprod_fr <- revalue(ESM$sex_reprod_fr,c("hermaphrodite, Dioecious"="Dioecious"))
ESM$sex_reprod_fr <- revalue(ESM$sex_reprod_fr,c("Dioecious"="Gyno/Dioecious"))
ESM$sex_reprod_fr <- revalue(ESM$sex_reprod_fr,c("Gynodioecious"="Gyno/Dioecious"))
#write.csv(ESM, "ESM.csv", row.names = FALSE)

S <-revalue(ESM$sex_reprod_fr,c("Hermaphroditic"="Monoecious"))

par(mfrow=c(1,1), bty="l")
plot(log(total.area) ~ S, data = ESM)
m1 <- glm(log(total.area) ~ S, family = "gaussian", data = ESM)
hist(resid(m1), xlab = "Residuals", 15, cex.lab = 1.2, main = "Histogram of errors")
summary(m1)
summary.lm(m1)
par(mfrow = c(2,2))
plot(m1)
length(which(!is.na(sex_reprod_fr)))

#**************************************
#poll_vect_fr

table(ESM$poll_vect_fr)

#handling levels and lumping categories together
ESM$poll_vect_fr <- revalue(ESM$poll_vect_fr,c("insect, self" = "self, insect"))
ESM$poll_vect_fr <- revalue(ESM$poll_vect_fr,c("self, insect" = "self"))
ESM$poll_vect_fr <- revalue(ESM$poll_vect_fr,c("insect, wind" = "wind"))
ESM$poll_vect_fr <- revalue(ESM$poll_vect_fr,c("apogamy" = NA))
#write.csv(ESM, "ESM.csv", row.names = FALSE)
p <-factor(ESM$poll_vect_fr, levels = c("self", "water", "insect","wind"))

par(mfrow=c(1,1), bty="l")
plot(log(total.area)~p, data = ESM)
m1 <-glm(log(total.area)~p, family = "gaussian", data = ESM)
hist(resid(m1), xlab = "Residuals", 15, cex.lab = 1.2, main = "Histogram of errors")
summary(m1)
summary.lm(m1)
par(mfrow = c(2,2))
plot(m1)

length(which(!is.na(poll_vect_fr)))

#*****************************
#woodiness

table(ESM$woodiness)
plot(log(total.area)~woodiness, data = ESM)

#3 of 5 levels of woodiness are a variation of semi-woody
#changing 3 different levels to one level - "semi-woody"

ESM$woodiness <- revalue(ESM$woodiness,c("semi-woody + woody" = "semi-woody + non-woody"))
ESM$woodiness <- revalue(ESM$woodiness,c("non-woody + semi-woody" = "semi-woody + non-woody"))
ESM$woodiness <- revalue(ESM$woodiness,c("semi-woody + woody" = "semi-woody + non-woody"))
#write.csv(ESM, "ESM.csv", row.names = FALSE)

W <-factor(ESM$woodiness, levels = c("non-woody", "semi-woody + non-woody", "woody"))
W <- revalue(ESM$woodiness,c("semi-woody + non-woody" = "woody"))
par(mfrow = c(1,1))
plot(log(total.area)~W, data = ESM)
m1 <- glm(log(total.area) ~ W, family = "gaussian", data = ESM)
hist(resid(m1), xlab = "Residuals", 15, cex.lab = 1.2, main = "Histogram of errors")
summary(m1)
summary.lm(m1)
par(mfrow = c(2,2))
plot(m1)

length(which(!is.na(woodiness)))

#*****************************
#fruit_type_fr

table(ESM$fruit_type_fr)

par(mfrow=c(1,1), bty = "l")
plot(log(total.area)~fruit_type_fr, data = ESM)
ESM$fruit_type_fr <-revalue(ESM$fruit_type_fr,c("follicle"="silique","cone"=NA, "berry"=NA))
#write.csv(ESM, "ESM.csv", row.names = FALSE)
f <-factor(ESM$fruit_type_fr, levels = c("silique","capsule","achene"))

m1 <-glm(log(total.area)~fruit_type_fr, data = ESM)
hist(resid(m1), xlab = "Residuals", 15, cex.lab = 1.2, main = "Histogram of errors")
summary(m1)
summary.lm(m1)
par(mfrow=c(2,2))
plot(m1)

length(which(!is.na(fruit_type_fr)))

#**************************************
#dissemenation_fr 

table(ESM$dissemination_fr)

par(mfrow = c(1,1), bty="l")
plot(log(total.area)~dissemination_fr, data = ESM)
m1 <- glm(log(total.area) ~ dissemination_fr, family = "gaussian", data = ESM)
summary(m1)
par(mfrow = c(2,2))
plot(m1)

D <-revalue(ESM$dissemination_fr, c("endozoochore, dyszoochores"="endozoochores","dyszoochores"="endozoochores"))
D <-revalue(D, c("endozoochores"="zoochores","epizoochores"="zoochores"))
D <-revalue(D, c("autochores"="short","hydrochores"="short","barochores"="short"))
D <-revalue(D, c("anemochores"="medium","myrmecochores"="medium"))
D <-revalue(D, c("medium"="Mlong","zoochores"="Mlong"))

table(D)
par(mfrow = c(1,1), bty="l")
plot(log(total.area)~D, data = ESM)
m1 <- glm(log(total.area) ~ D, family = "gaussian", data = ESM)
hist(resid(m1), xlab = "Residuals", 15, cex.lab = 1.2, main = "Histogram of errors")
summary(m1)
summary.lm(m1)
par(mfrow = c(2,2))
plot(m1)

length(which(!is.na(dissemination_fr)))

#ordered by total.area, just for interest
D <-factor(dissemination_fr, levels = c("dyszoochores","endozoochores","hydrochores","myrmecochores",
                                        "autochores","barochores","anemochores","epizoochores",
                                        "endozoochore, dyszoochores"))

#***************************
#li_form_fr -sig

table(ESM$li_form_fr)

par(mfrow=c(1,1), bty = "l")
plot(log(total.area)~li_form_fr, data = ESM)

#lumping many categories into rankierian life forms
L <-revalue(ESM$li_form_fr, c("ccou-suc" = "Cham", "cfru" = "Cham", "Cfru" = "Cham",
                            "Cfru-cad" = "Cham", "csuf" = "Cham", "Csuf" = "Cham", "csuf-suc" = "Cham"))
L <-revalue(L, c("a-cad" = "Phan", "A-cad" = "Phan", "A-semp"= "Phan","b-cad"= "Phan",
                 "B-cad"= "Phan", "b-lia"= "Phan", "B-semp"= "Phan"))
L <-revalue(L, c("hbis"= "Hemi", "Hbis(Heri)"= "Hemi", "hces"= "Hemi", "heri"= "Hemi", "Heri"= "Hemi",
                 "heri-hpar"= "Hemi", "hros"= "Hemi", "Hros"= "Hemi", "hsto"= "Hemi", 
                 "Hsto-aqua"= "Hemi", "hsto-hpar"= "Hemi"))
L <-revalue(L, c("gbul"= "Geo", "grhi"= "Geo", "Grhi"= "Geo", "gtub"= "Geo"))
L <-revalue(L, c("test"= "Thero", "test-suc"= "Thero", "test(hbis)"= "Thero", "tver"= "Thero")) 
levels(L)
length(which(!is.na(L)))

#checking which are are sig-diff from each other
test <-factor(L, levels = c("Phan","Geo","Thero","Hemi","Cham"))
par(mfrow=c(1,1), bty = "l")
plot(log(total.area)~test, data = ESM)
m1 <-glm(log(total.area)~test, data = ESM)
hist(resid(m1), xlab = "Residuals", 15, cex.lab = 1.2, main = "Histogram of errors")
summary(m1)
summary.lm(m1)
par(mfrow=c(2,2),bty="l")
plot(m1)


#************************
#s_bank

table(ESM$sbank)

ESM$sbank <-revalue(ESM$sbank,c("present + transient"="transient"))
#write.csv(ESM, "ESM.csv", row.names = FALSE)
length(which(!is.na(sbank)))

S <-revalue(ESM$sbank,c("long-term persistent + present + short-term persistent + transient"="present"))
S <-revalue(S,c("long-term persistent + short-term persistent"="present"))
S <-revalue(S,c("long-term persistent + short-term persistent + transient"="present"))
S <-revalue(S,c("long-term persistent + transient"="present"))
S <-revalue(S,c("present + short-term persistent + transient"="present"))

table(S)

par(mfrow=c(1,1), bty = "l")
plot(log(1/total.area)~S, data = ESM)
m1 <-glm(log(1/total.area)~S, data = ESM)
hist(resid(m1), xlab = "Residuals", 15, cex.lab = 1.2, main = "Histogram of errors")
summary(m1)
summary.lm(m1)
par(mfrow=c(2,2),bty="l")
plot(m1)

#****************
detach(ESM)
#end

