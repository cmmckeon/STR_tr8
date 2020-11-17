
#aggregation.index univariate models
#housekeeping
setwd("C:/Users/Caroline/Dropbox/Thesis/")

new_df <-read.csv(file.choose())
V <-read.csv(file.choose())

names(new_df[36])
#creating new small dataframe conatining agg.index and ncells to work on
A <-cbind(aggregation.index, V)
A <-cbind(ncells, A)

attach(A)

#***********************************
#canopy_height
#responce variable = log(aggregation.index)
#gaussian; not sig, diagnostics good
#gamma; not sig, better AIC

par(mfrow=c(1,1))
plot((aggregation.index) ~ log(canopy_height), bty = "l", data = A)
m1 <-glm((aggregation.index) ~ (canopy_height)+(ncells), family = "gaussian"(link="identity"),
         data = A)
hist(resid(m1), xlab = "Residuals", cex.lab = 1.2, main = "Histogram of errors")
summary(m1)
summary.lm(m1)
par(mfrow = c(2,2))
plot(m1)


#****************************************
#seed_number_per_shoot
#gaussian; not significant, diagnostic fine, 
#gamma: not sig, diagnostic fine, higher AIC (due to sample size issues?)

par(mfrow = c(1,1),bty="l")
plot(log(aggregation.index) ~ log(seed_number_per_shoot), data = A)
m1 <- glm(log(aggregation.index) ~ log(seed_number_per_shoot)+log(ncells), family = "Gamma", data = A)
hist(resid(m1), xlab = "Residuals", cex.lab = 1.2, main = "(b) Histogram of errors")
summary(m1)
par(mfrow = c(2,2))
plot(m1)
summary.lm(m1)

#***************************
#seed_mass
#gaussian: not sig, diagnostics fine
#gamma; not sig, AIC higher

par(mfrow = c(1,1),bty="l")
plot(log(aggregation.index) ~ log(seed_mass), data = A)
m1 <- glm(log(aggregation.index) ~ log(seed_mass)+log(ncells), family = "gaussian", data = A)
hist(resid(m1), xlab = "Residuals", cex.lab = 1.2, main = "(b) Histogram of errors")
summary(m1)
par(mfrow = c(2,2))
plot(m1)
summary.lm(m1)

#creating DF excluding agg.index less than 1, solves gamma non-positive problem with normal AICs
NZ <- A[which(aggregation.index > 0),]
CHECK <- A[which(A$aggregation.index < 1),]
attach(CHECK)
length(which(!is.na(NZ$sex_reprod_fr)))

#***********************
#sex_repod_fr
#Gaussian: monocecios barely sig, not the best plots
#Gamma: monoecious barely sig, not the best plots, worse AIC
#all fine though, r^2 is much improved by Gamma error distribution

table(NZ$sex_reprod_fr)

S <-revalue(NZ$sex_reprod_fr,c("Hermaphroditic"="Monoecious"))
table(S)
par(mfrow=c(1,1),bty="l")
plot(log(aggregation.index) ~ S, data = NZ)
m1 <- glm(log(aggregation.index) ~ S+log(ncells), family = "Gamma", data = NZ)
hist(resid(m1), xlab = "Residuals", cex.lab = 1.2, main = "(b) Histogram of errors")
summary(m1)
summary.lm(m1)
par(mfrow = c(2,2))
plot(m1)

#**************************************
#poll_vect_fr
#finished

table(NZ$poll_vect_fr)

par(mfrow=c(1,1), bty="l")
plot(log(aggregation.index)~log(ncells), data = NZ)
plot(log(aggregation.index)~poll_vect_fr, data = NZ)

plot(log(aggregation.index)~ log(ncells), type = "n")
points(log(aggregation.index)[poll_vect_fr=="insect"]~log(ncells)[poll_vect_fr=="insect"], col = "green", pch = 1)
points(log(aggregation.index)[poll_vect_fr=="wind"]~log(ncells)[poll_vect_fr=="wind"], col = "blue", pch = 1)
points(log(aggregation.index)[poll_vect_fr=="water"]~log(ncells)[poll_vect_fr=="water"], col = "red", pch = 1)
points(log(aggregation.index)[poll_vect_fr=="self"]~log(ncells)[poll_vect_fr=="self"], col = "purple", pch = 1)


m1 <-glm(log(aggregation.index) ~ p+log(ncells), family = "Gamma", data = NZ)
hist(resid(m1), xlab = "Residuals", cex.lab = 1.2, main = "Histogram of errors")
summary(m1)
summary.lm(m1)
par(mfrow = c(2,2))
plot(m1)

p <-factor(NZ$poll_vect_fr, levels = c("wind", "self", "insect","water"))


#*****************************
#woodiness
#finished

table(NZ$woodiness)
W <-revalue(NZ$woodiness, c("semi-woody + non-woody"="woody"))
table(W)
par(mfrow=c(1,1),bty="l")
plot(log(aggregation.index)~W, data = NZ)
m1 <- glm(log(aggregation.index) ~ W+log(ncells), family = "Gamma", data = NZ)
hist(resid(m1), xlab = "Residuals", cex.lab = 1.2, main = "Histogram of errors")
summary(m1)
summary.lm(m1)
par(mfrow = c(2,2))
plot(m1)

#*****************************
#fruit_type_fr
#finished

table(NZ$fruit_type_fr)

par(mfrow=c(1,1), bty = "l")
plot(log(aggregation.index)~fuit, data = NZ)
fuit <-revalue(NZ$fruit_type_fr,c("follicle"="silique","cone"=NA, "berry"=NA))
f <-factor(fuit, levels = c("silique","capsule","achene"))

m1 <-glm(log(aggregation.index)~fuit+log(ncells), family="Gamma", data = NZ)
hist(resid(m1), xlab = "Residuals", cex.lab = 1.2, main = "Histogram of errors")
summary(m1)
summary.lm(m1)
par(mfrow=c(2,2),bty="l")
plot(m1)


#**************************************
#dissemenation_fr
#gamma worse AIC

table(NZ$dissemination_fr)

D <-revalue(NZ$dissemination_fr, c("endozoochore, dyszoochores"="endozoochores","dyszoochores"="endozoochores"))
D <-revalue(D, c("endozoochores"="zoochores","epizoochores"="zoochores"))
D <-revalue(D, c("autochores"="short","hydrochores"="short","barochores"="short"))
D <-revalue(D, c("anemochores"="medium","myrmecochores"="medium"))

table(D)

par(mfrow=c(1,1), bty = "l")
plot(log(aggregation.index)~D, data = NZ)
m1 <-glm(log(aggregation.index)~D+log(ncells), family = "Gamma", data = NZ)
summary(m1)
summary.lm(m1)
hist(resid(m1), xlab = "Residuals", cex.lab = 1.2, main = "Histogram of errors")
par(mfrow=c(2,2),bty="l")
plot(m1)



#***************************
#li_form_fr

table(li_form_fr)
par(mfrow=c(1,1), bty = "l")
plot(log(aggregation.index)~li_form_fr, data = V)
#lumping many categories into ranierian life forms
L <-revalue(NZ$li_form_fr, c("ccou-suc" = "Cham", "cfru" = "Cham", "Cfru" = "Cham",
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

#test <-factor(V$li_form_fr, levels = c("Cham","Phan","Geo","Thero","Hemi"))

test <-factor(V$li_form_fr, levels = c("Cfru-cad","Hros","Hsto-aqua",
                                       "b-cad","gbul","hsto-hpar","heri-hpar","Heri","A-cad","test-suc","grhi","B-semp",
                                       "B-cad","b-lia","a-cad","test","csuf-suc","tver","A-semp","hces","ccou-suc",
                                       "cfru","heri","gtub","hsto","Grhi","Cfru","hros","csuf","hbis","test(hbis)",
                                       "Hbis(Heri)","Csuf"))
par(mfrow=c(1,1), bty = "l")
plot(log(aggregation.index)~L, data = NZ)
m1 <-glm(log(aggregation.index)~L+log(ncells), family = "Gamma", data = NZ)
hist(resid(m1), xlab = "Residuals", cex.lab = 1.2, main = "Histogram of errors")
summary(m1)
summary.lm(m1)
par(mfrow=c(2,2),bty="l")
plot(m1)

#************************
#s_bank
table(NZ$sbank)

S <-revalue(NZ$sbank,c("long-term persistent + present + short-term persistent + transient"="present"))
S <-revalue(S,c("long-term persistent + short-term persistent"="present"))
S <-revalue(S,c("long-term persistent + short-term persistent + transient"="present"))
S <-revalue(S,c("long-term persistent + transient"="present"))
S <-revalue(S,c("present + short-term persistent + transient"="present"))

table(S)


par(mfrow=c(1,1), bty = "l")
plot(log(aggregation.index)~S, data = NZ)
m1 <-glm(log(aggregation.index)~S+log(ncells), family = "Gamma", data = NZ)
hist(resid(m1), xlab = "Residuals", cex.lab = 1.2, main = "Histogram of errors")
summary(m1)
summary.lm(m1)
par(mfrow=c(2,2),bty="l")
plot(m1)


#****************
#unsuccessful tinkering


