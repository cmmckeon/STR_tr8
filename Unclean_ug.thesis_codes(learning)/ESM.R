#housekeeping
setwd("C:/Users/Caroline/Dropbox/Thesis/")

new_df <-read.csv(file.choose())

ESM <-new_df[which(new_df$Model=="ESM_bin"),]
#write.csv(ESM, "ESM.csv", row.names = FALSE)

#dataset containing analysis variables only
ESM <-as.data.frame(c(Occ[5],Occ[60:70]))
#write.csv(ESM, "ESM.csv", row.names = FALSE)

#great function: shows each level of a vector annontated with how many times it appears in the column
table(sbank)

O <-read.csv(file.choose())
attach(O)

#***********************************
#test model - initial look at analysis
#canopy_height
#sig
par(mfrow=c(1,1))
plot(log(total.area) ~ log(canopy_height), bty = "l", data = ESM)
abline(coef(m1), lwd = 2)
m1 <- glm(log(total.area) ~ log(canopy_height), family = "gaussian", data = ESM)
hist(resid(m1), xlab = "Residuals", cex.lab = 1.2, main = "(b) Histogram of errors")
summary(m1)
summary.lm(m1)
par(mfrow = c(2,2))
plot(m1)
length(which(!is.na(canopy_height)))
attach(ESM)

install.packages("MASS")
library(MASS)


#****************************************
#seed_number_per_shoot


par(mfrow = c(1,1), bty = "l")
plot(log(total.area) ~ log(seed_number_per_shoot), data = ESM)
m1 <- glm(log(total.area) ~ log(seed_number_per_shoot), family = "gaussian", data = ESM)
abline(coef(m1), lwd = 2)
hist(resid(m1), xlab = "Residuals", cex.lab = 1.2, main = "Histogram of errors")
summary(m1)
summary.lm(m1)
par(mfrow = c(2,2))
plot(m1)

length(which(!is.na(seed_number_per_shoot)))

#***************************
#seed_mass
#univartiate complete-sig

par(mfrow = c(1,1), bty = "l")
plot(log(total.area) ~ log(seed_mass), data = ESM)
m1 <- glm(log(total.area) ~ log(seed_mass), family = "gaussian", data = ESM)
abline(coef(m1), lwd = 2)
hist(resid(m1), xlab = "Residuals", cex.lab = 1.2, main = "Histogram of errors")
summary(m1)
summary.lm(m1)
par(mfrow = c(2,2))
plot(m1)

length(which(!is.na(seed_mass)))

#***********************
#sex_repod_fr
#univartiate complete- insig

table(ESM$sex_reprod_fr)

install.packages("plyr")
library(plyr)
#functioning code for renaming/handling levels
ESM$sex_reprod_fr <- revalue(ESM$sex_reprod_fr,c("hermaphrodite, Dioecious"="Dioecious"))
ESM$sex_reprod_fr <- revalue(ESM$sex_reprod_fr,c("Dioecious"="Gyno/Dioecious"))
ESM$sex_reprod_fr <- revalue(ESM$sex_reprod_fr,c("Gynodioecious"="Gyno/Dioecious"))
write.csv(ESM, "ESM.csv", row.names = FALSE)

S <-revalue(ESM$sex_reprod_fr,c("Hermaphroditic"="Monoecious"))

par(mfrow=c(1,1), bty="l")
plot(log(total.area) ~ S, data = ESM)
m1 <- glm(log(total.area) ~ S, family = "gaussian", data = ESM)
hist(resid(m1), xlab = "Residuals", cex.lab = 1.2, main = "Histogram of errors")
summary(m1)
summary.lm(m1)
par(mfrow = c(2,2))
plot(m1)
length(which(!is.na(sex_reprod_fr)))

#**************************************
#poll_vect_fr
#univartiate complete-sig, use the 4 level

table(ESM$poll_vect_fr)

#handling levels and lumping categories together
ESM$poll_vect_fr <- revalue(ESM$poll_vect_fr,c("insect, self" = "self, insect"))
ESM$poll_vect_fr <- revalue(ESM$poll_vect_fr,c("self, insect" = "self"))
ESM$poll_vect_fr <- revalue(ESM$poll_vect_fr,c("insect, wind" = "wind"))
ESM$poll_vect_fr <- revalue(ESM$poll_vect_fr,c("apogamy" = NA))
write.csv(ESM, "ESM.csv", row.names = FALSE)
C <-factor(V$poll_vect_fr, levels = c("insect","Wself","wind"))

par(mfrow=c(1,1), bty="l")
plot(log(total.area)~p, data = ESM)
m1 <-glm(log(total.area)~p, family = "gaussian", data = ESM)
hist(resid(m1), xlab = "Residuals", cex.lab = 1.2, main = "Histogram of errors")
summary(m1)
summary.lm(m1)
par(mfrow = c(2,2))
plot(m1)

length(which(!is.na(poll_vect_fr)))
#checking if level order changes r^2 values; it doesn't, thank God.
p <-factor(ESM$poll_vect_fr, levels = c("self", "water", "insect","wind"))


#*****************************
#woodiness
#univartiate complete-unsig

table(ESM$woodiness)
plot(log(total.area)~woodiness, data = O)

#3 of 5 levels of woodiness are a variation of semi-woody
#changing 3 different levels to one level - "semi-woody"

ESM$woodiness <- revalue(ESM$woodiness,c("semi-woody + woody" = "semi-woody + non-woody"))
ESM$woodiness <- revalue(ESM$woodiness,c("non-woody + semi-woody" = "semi-woody + non-woody"))
ESM$woodiness <- revalue(ESM$woodiness,c("semi-woody + woody" = "semi-woody + non-woody"))
write.csv(ESM, "ESM.csv", row.names = FALSE)

W <-factor(ESM$woodiness, levels = c("non-woody", "semi-woody + non-woody", "woody"))
W <- revalue(ESM$woodiness,c("semi-woody + non-woody" = "woody"))

par(mfrow = c(1,1))
plot(log(total.area)~W, data = ESM)
m1 <- glm(log(total.area) ~ W, family = "gaussian", data = ESM)
hist(resid(m1), xlab = "Residuals", cex.lab = 1.2, main = "Histogram of errors")
summary(m1)
summary.lm(m1)
par(mfrow = c(2,2))
plot(m1)

length(which(!is.na(woodiness)))
#*****************************
#fruit_type_fr
#univartiate complete-sig 
table(ESM$fruit_type_fr)

par(mfrow=c(1,1), bty = "l")
plot(log(total.area)~fruit_type_fr, data = ESM)
ESM$fruit_type_fr <-revalue(ESM$fruit_type_fr,c("follicle"="silique","cone"=NA, "berry"=NA))
f <-factor(O$fruit_type_fr, levels = c("silique","capsule","achene"))

m1 <-glm(log(total.area)~fruit_type_fr, data = ESM)
hist(resid(m1), xlab = "Residuals", cex.lab = 1.2, main = "Histogram of errors")
summary(m1)
summary.lm(m1)
par(mfrow=c(2,2),bty="l")
plot(m1)

length(which(!is.na(fruit_type_fr)))

write.csv(O, "ODF.csv", row.names = FALSE)
#**************************************
#dissemenation_fr 

table(ESM$dissemination_fr)
par(mfrow = c(1,1), bty="l")
plot(log(total.area)~dissemination_fr, data = O)
m1 <- glm(log(total.area) ~ dissemination_fr, family = "gaussian", data = O)
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
hist(resid(m1), xlab = "Residuals", cex.lab = 1.2, main = "Histogram of errors")
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
plot(log(total.area)~li_form_fr, data = O)
#lumping many categories into ranierian life forms
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

#contrasts to see which are sig-diff from eachother
test <-factor(O$li_form_fr, levels = c("Cham","Phan","Geo","Thero","Hemi"))
par(mfrow=c(1,1), bty = "l")
plot(log(total.area)~L, data = ESM)
m1 <-glm(log(total.area)~L, data = ESM)
hist(resid(m1), xlab = "Residuals", cex.lab = 1.2, main = "Histogram of errors")
summary(m1)
summary.lm(m1)
par(mfrow=c(2,2),bty="l")
plot(m1)



#************************
#s_bank
#can't trust insig results as errors not normal

table(ESM$sbank)

O$sbank <-revalue(O$sbank,c("present + transient"="transient"))
write.csv(V, "variablesDF.csv", row.names = FALSE)
length(which(!is.na(sbank)))

par(mfrow=c(1,1), bty = "l")
plot(log(total.area)~sbank, data = V)
m1 <-glm(log(total.area)~sbank, data = V)
summary(m1)
par(mfrow=c(2,2),bty="l")
plot(m1)

S <-revalue(ESM$sbank,c("long-term persistent + present + short-term persistent + transient"="present"))
S <-revalue(S,c("long-term persistent + short-term persistent"="present"))
S <-revalue(S,c("long-term persistent + short-term persistent + transient"="present"))
S <-revalue(S,c("long-term persistent + transient"="present"))
S <-revalue(S,c("present + short-term persistent + transient"="present"))
S <-revalue(S,c("present + transient"="present"))

table(S)

par(mfrow=c(1,1), bty = "l")
plot(log(1/total.area)~S, data = ESM)
m1 <-glm(log(1/total.area)~S, data = ESM)
hist(resid(m1), xlab = "Residuals", cex.lab = 1.2, main = "Histogram of errors")
summary(m1)
summary.lm(m1)
par(mfrow=c(2,2),bty="l")
plot(m1)

#****************
#unsuccessful tinkering

traits <- names(V)
cat(paste(shQuote(traits, type="cmd"), collapse=", "))
t <-c("total.area", "sbank", "seed_mass", "seed_number_per_shoot",
      "woodiness", "sex_reprod_fr", "poll_vect_fr", "fruit_type_fr",
      "dissemination_fr", "li_form_fr", "canopy_height")

bib(V)

length(which(!is.na(dissemination_fr)))

