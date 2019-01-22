#housekeeping
setwd("C:/Users/Caroline/Dropbox/Thesis/")

install.packages("MASS")
library(MASS)
install.packages("plyr")
library(plyr)

#new_df <-read.csv(file.choose())

full_df <-read.csv(file.choose())
attach(full_df)

#removing trait columns with insufficeint data
#checking number of column that I want to remove
names(full_df[82])
#following commands have been hashed out cus they are a huge hassle when run accidently
#new_df <- full_df[,-(82:110), drop=TRUE]
#new_df <- new_df[,-(67:71), drop=TRUE]
#new_df <- new_df[,-(61:65), drop=TRUE]
#new_df <- new_df[,-(66), drop=TRUE]
#write.csv(new_df, "new_df.csv", row.names = FALSE)

Occ <-new_df[which(new_df$Model=="occurrence"),]
#write.csv(Occ, "Occurence.csv", row.names = FALSE)

#dataset containing analysis variables only
O <-as.data.frame(c(Occ[5],Occ[60:70]))
#write.csv(O, "ODF.csv", row.names = FALSE)

#great function
table(sbank)

O <-read.csv(file.choose())
attach(O)

#***********************************
#test model - initial look at analysis
#canopy_height

par(mfrow=c(1,1), bty="l")
plot(log(total.area) ~ log(canopy_height), data = O)
m1 <- glm(log(total.area) ~ log(canopy_height), family = "gaussian", data = O)
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
plot(log(total.area) ~ log(seed_number_per_shoot), data = O)
m1 <- glm(log(total.area) ~ log(seed_number_per_shoot), family = "gaussian", data = O)
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
plot(log(total.area) ~ log(seed_mass), data = O)
m1 <- glm(log(total.area) ~ log(seed_mass), family = "gaussian", data = O)
abline(coef(m1), lwd = 2)
hist(resid(m1), xlab = "Residuals", 15, cex.lab = 1.2, main = "Histogram of errors")
summary(m1)
summary.lm(m1)
par(mfrow = c(2,2))
plot(m1)

length(which(!is.na(seed_mass)))

#***********************
#sex_repod_fr

table(O$sex_reprod_fr)

#functioning code for renaming/handling levels
O$sex_reprod_fr <- revalue(O$sex_reprod_fr,c("hermaphrodite, Dioecious"="Dioecious"))
O$sex_reprod_fr <- revalue(O$sex_reprod_fr,c("Dioecious"="Gyno/Dioecious"))
O$sex_reprod_fr <- revalue(O$sex_reprod_fr,c("Gynodioecious"="Gyno/Dioecious"))
#write.csv(O, "ODF.csv", row.names = FALSE)

S <-revalue(O$sex_reprod_fr,c("Hermaphroditic"="Monoecious"))

par(mfrow=c(1,1), bty="l")
plot(log(total.area) ~ S, data = O)
m1 <- glm(log(total.area) ~ S, family = "gaussian", data = O)
hist(resid(m1), xlab = "Residuals", 15, cex.lab = 1.2, main = "Histogram of errors")
summary(m1)
summary.lm(m1)
par(mfrow = c(2,2))
plot(m1)
length(which(!is.na(sex_reprod_fr)))

#**************************************
#poll_vect_fr

table(O$poll_vect_fr)

#handling levels and lumping categories together
O$poll_vect_fr <- revalue(O$poll_vect_fr,c("insect, self" = "self, insect"))
O$poll_vect_fr <- revalue(O$poll_vect_fr,c("self, insect" = "self"))
O$poll_vect_fr <- revalue(O$poll_vect_fr,c("insect, wind" = "wind"))
O$poll_vect_fr <- revalue(O$poll_vect_fr,c("apogamy" = NA))
#write.csv(O, "ODF.csv", row.names = FALSE)
p <-factor(O$poll_vect_fr, levels = c("self", "water", "insect","wind"))

par(mfrow=c(1,1), bty="l")
plot(log(total.area)~p, data = O)
m1 <-glm(log(total.area)~p, family = "gaussian", data = O)
hist(resid(m1), xlab = "Residuals", 15, cex.lab = 1.2, main = "Histogram of errors")
summary(m1)
summary.lm(m1)
par(mfrow = c(2,2))
plot(m1)

length(which(!is.na(poll_vect_fr)))

#*****************************
#woodiness

table(O$woodiness)
plot(log(total.area)~woodiness, data = O)

#3 of 5 levels of woodiness are a variation of semi-woody
#changing 3 different levels to one level - "semi-woody"

O$woodiness <- revalue(O$woodiness,c("semi-woody + woody" = "semi-woody + non-woody"))
O$woodiness <- revalue(O$woodiness,c("non-woody + semi-woody" = "semi-woody + non-woody"))
O$woodiness <- revalue(O$woodiness,c("semi-woody + woody" = "semi-woody + non-woody"))
#write.csv(O, "ODF.csv", row.names = FALSE)

W <-factor(O$woodiness, levels = c("non-woody", "semi-woody + non-woody", "woody"))
W <- revalue(O$woodiness,c("semi-woody + non-woody" = "woody"))
par(mfrow = c(1,1))
plot(log(total.area)~W, data = O)
m1 <- glm(log(total.area) ~ W, family = "gaussian", data = O)
hist(resid(m1), xlab = "Residuals", 15, cex.lab = 1.2, main = "Histogram of errors")
summary(m1)
summary.lm(m1)
par(mfrow = c(2,2))
plot(m1)

length(which(!is.na(woodiness)))

#*****************************
#fruit_type_fr

table(O$fruit_type_fr)

par(mfrow=c(1,1), bty = "l")
plot(log(total.area)~fruit_type_fr, data = O)
O$fruit_type_fr <-revalue(O$fruit_type_fr,c("follicle"="silique","cone"=NA, "berry"=NA))
#write.csv(O, "ODF.csv", row.names = FALSE)
f <-factor(O$fruit_type_fr, levels = c("silique","capsule","achene"))

m1 <-glm(log(total.area)~fruit_type_fr, data = O)
hist(resid(m1), xlab = "Residuals", 15, cex.lab = 1.2, main = "Histogram of errors")
summary(m1)
summary.lm(m1)
par(mfrow=c(2,2))
plot(m1)

length(which(!is.na(fruit_type_fr)))

#**************************************
#dissemenation_fr 

table(O$dissemination_fr)

par(mfrow = c(1,1), bty="l")
plot(log(total.area)~dissemination_fr, data = O)
m1 <- glm(log(total.area) ~ dissemination_fr, family = "gaussian", data = O)
summary(m1)
par(mfrow = c(2,2))
plot(m1)

D <-revalue(O$dissemination_fr, c("endozoochore, dyszoochores"="endozoochores","dyszoochores"="endozoochores"))
D <-revalue(D, c("endozoochores"="zoochores","epizoochores"="zoochores"))
D <-revalue(D, c("autochores"="short","hydrochores"="short","barochores"="short"))
D <-revalue(D, c("anemochores"="medium","myrmecochores"="medium"))
D <-revalue(D, c("medium"="Mlong","zoochores"="Mlong"))

table(D)
par(mfrow = c(1,1), bty="l")
plot(log(total.area)~D, data = O)
m1 <- glm(log(total.area) ~ D, family = "gaussian", data = O)
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

table(O$li_form_fr)

par(mfrow=c(1,1), bty = "l")
plot(log(total.area)~li_form_fr, data = O)

#lumping many categories into rankierian life forms
L <-revalue(O$li_form_fr, c("ccou-suc" = "Cham", "cfru" = "Cham", "Cfru" = "Cham",
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
plot(log(total.area)~test, data = O)
m1 <-glm(log(total.area)~test, data = O)
hist(resid(m1), xlab = "Residuals", 15, cex.lab = 1.2, main = "Histogram of errors")
summary(m1)
summary.lm(m1)
par(mfrow=c(2,2),bty="l")
plot(m1)


#************************
#s_bank

table(O$sbank)

O$sbank <-revalue(O$sbank,c("present + transient"="transient"))
#write.csv(O, "ODF.csv", row.names = FALSE)
length(which(!is.na(sbank)))

S <-revalue(O$sbank,c("long-term persistent + present + short-term persistent + transient"="present"))
S <-revalue(S,c("long-term persistent + short-term persistent"="present"))
S <-revalue(S,c("long-term persistent + short-term persistent + transient"="present"))
S <-revalue(S,c("long-term persistent + transient"="present"))
S <-revalue(S,c("present + short-term persistent + transient"="present"))

table(S)

par(mfrow=c(1,1), bty = "l")
plot(log(1/total.area)~S, data = O)
m1 <-glm(log(1/total.area)~S, data = O)
hist(resid(m1), xlab = "Residuals", 15, cex.lab = 1.2, main = "Histogram of errors")
summary(m1)
summary.lm(m1)
par(mfrow=c(2,2),bty="l")
plot(m1)

#****************
detach(O)

#unsuccessful tinkering
#trying to work out how to get references for citing software and databases

bib(V)

length(which(!is.na(dissemination_fr)))

