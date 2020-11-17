#aggregation.index univariate models
#housekeeping
setwd("C:/Users/Caroline/Dropbox/Thesis/")

############################################
LOGIT SCRIPT
############################################

ESM <-read.csv(file.choose())
attach(ESM)

attach(fullESM)
#making dataframe to use in agg_occurence analysis
ESMA <-cbind(ESM,ncells,aggregation.index)

Occ <-read.csv(file.choose())
attach(Occ)
OA <-cbind(O,ncells,aggregation.index)

attach(ESMA)

#***********************************
#canopy_height
#responce variable = log(aggregation.index)

par(mfrow=c(1,1), mar=c(4.5,4.5,1,2))
plot(log(aggregation.index) ~ log(canopy_height), bty = "l", data = ESMA)
plot(log(aggregation.index) ~ log(ncells), bty = "l", data = ESMA,
     xlab = "Log number of cells", ylab = "Log Aggregation Index (%)", 
     cex.lab = 1.2, tcl=-0.25, las=1, pch =21, bg="grey", cex = 0.8)
plot(log(aggregation.index) ~ log(ncells), bty = "l", data = OA,
     xlab = "Log number of cells", ylab = "Log Aggregation Index (%)", 
     cex.lab = 1.2, tcl=-0.25, las=1, pch =21, bg="grey", cex = 0.8)
m1 <-glm((aggregation.index/100) ~ (canopy_height)+(ncells), 
         family = "gaussian"(link="logit"), data = ESMA)
m1 <-glm(log(aggregation.index/100) ~ log(ncells), 
         family = "gaussian"(link="identity"), data = OA)
m1 <-glm(log(aggregation.index/100) ~ log(canopy_height)+log(ncells), 
         family = "gaussian"(link="identity"), data = ESMA)
abline(coef(m1), lwd = 2)
hist(resid(m1), xlab = "Residuals", 20, cex.lab = 1.2, 15, main = "Histogram of errors")
summary(m1)
summary.lm(m1)
par(mfrow = c(2,2))
plot(m1)

#****************************************
#seed_number_per_shoot

par(mfrow = c(1,1),bty="l")
plot(log(aggregation.index) ~ log(seed_number_per_shoot), data = ESMA)
m1 <- glm(log(aggregation.index) ~ log(seed_number_per_shoot)+log(ncells), 
          family = "gaussian"(link = "identity"), data = ESMA)
abline(coef(m1), lwd = 2)
hist(resid(m1), xlab = "Residuals", 15, cex.lab = 1.2, main = "Histogram of errors")
summary(m1)
summary.lm(m1)
par(mfrow = c(2,2))
plot(m1)


#***************************
#seed_mass

par(mfrow = c(1,1),bty="l")
plot(log(aggregation.index) ~ log(seed_mass), data = ESMA)
m1 <- glm(log(aggregation.index) ~ log(seed_mass)+log(ncells), 
          family = "gaussian"(link="identity"), data = ESMA)
abline(coef(m1), lwd = 2)
hist(resid(m1), xlab = "Residuals", 15, cex.lab = 1.2, main = "Histogram of errors")
summary(m1)
par(mfrow = c(2,2))
plot(m1)
summary.lm(m1)

#***************************

#creating DF excluding agg.index less than 1, solves problem with non-positive values for Gamma
NZ <- ESMA[which(aggregation.index > 0),]
CHECK <- ESMA[which(ESMA$aggregation.index < 1),]
length(which(!is.na(NZ$sex_reprod_fr)))

#***********************
#sex_repod_fr

table(NZ$sex_reprod_fr)

S <-revalue(NZ$sex_reprod_fr,c("Hermaphroditic"="Monoecious"))

table(S)
par(mfrow=c(1,1),bty="l")
plot(log(aggregation.index) ~ S, data = NZ)
m1 <- glm(log(aggregation.index) ~ S+log(ncells), family = "gaussian"(link="identity"), data = NZ)
hist(resid(m1), xlab = "Residuals", cex.lab = 1.2, main = "(b) Histogram of errors")
summary(m1)
summary.lm(m1)
par(mfrow = c(2,2))
plot(m1)

#**************************************
#poll_vect_fr

table(NZ$poll_vect_fr)

par(mfrow=c(1,1), bty="l")
plot(log(aggregation.index)~poll_vect_fr, data = NZ)

#tring to visualise pollen vectors in association of number of cells
plot(log(aggregation.index)~ log(ncells), type = "n")
points(log(aggregation.index)[poll_vect_fr=="insect"]~log(ncells)[poll_vect_fr=="insect"], col = "green", pch = 1)
points(log(aggregation.index)[poll_vect_fr=="wind"]~log(ncells)[poll_vect_fr=="wind"], col = "blue", pch = 1)
points(log(aggregation.index)[poll_vect_fr=="water"]~log(ncells)[poll_vect_fr=="water"], col = "red", pch = 1)
points(log(aggregation.index)[poll_vect_fr=="self"]~log(ncells)[poll_vect_fr=="self"], col = "purple", pch = 1)

m1 <-glm(log(aggregation.index) ~ poll_vect_fr+log(ncells), 
         family = "gaussian"(link = "identity"), data = NZ)
hist(resid(m1), xlab = "Residuals", cex.lab = 1.2, main = "Histogram of errors")
summary(m1)
summary.lm(m1)
par(mfrow = c(2,2))
plot(m1)

#*****************************
#woodiness

table(NZ$woodiness)
W <-revalue(NZ$woodiness, c("semi-woody + non-woody"="woody"))

table(W)
par(mfrow=c(1,1),bty="l")
plot(log(aggregation.index)~W, data = NZ)
m1 <- glm(log(aggregation.index) ~ W+log(ncells), family = "gaussian"(link = "identity"), data = NZ)
hist(resid(m1), xlab = "Residuals", cex.lab = 1.2, main = "Histogram of errors")
summary(m1)
summary.lm(m1)
par(mfrow = c(2,2))
plot(m1)

#*****************************
#fruit_type_fr

table(NZ$fruit_type_fr)

par(mfrow=c(1,1), bty = "l")
plot(log(aggregation.index)~fuit, data = NZ)
fuit <-revalue(NZ$fruit_type_fr,c("follicle"="silique","cone"=NA, "berry"=NA))
f <-factor(fuit, levels = c("silique","capsule","achene"))

m1 <-glm(log(aggregation.index)~fuit+log(ncells), family="gaussian"(link = "identity"), data = NZ)
hist(resid(m1), xlab = "Residuals", cex.lab = 1.2, main = "Histogram of errors")
summary(m1)
summary.lm(m1)
par(mfrow=c(2,2),bty="l")
plot(m1)

#**************************************
#dissemenation_fr

table(NZ$dissemination_fr)

D <-revalue(NZ$dissemination_fr, c("endozoochore, dyszoochores"="endozoochores","dyszoochores"="endozoochores"))
D <-revalue(D, c("endozoochores"="zoochores","epizoochores"="zoochores"))
D <-revalue(D, c("autochores"="short","hydrochores"="short","barochores"="short"))
D <-revalue(D, c("anemochores"="medium","myrmecochores"="medium"))

table(D)

par(mfrow=c(1,1), bty = "l")
plot(log(aggregation.index)~D, data = NZ)
m1 <-glm(log(aggregation.index)~D+log(ncells), family = "gaussian"(link = "identity"), data = NZ)
summary(m1)
summary.lm(m1)
hist(resid(m1), xlab = "Residuals", cex.lab = 1.2, main = "Histogram of errors")
par(mfrow=c(2,2),bty="l")
plot(m1)

#***************************
#li_form_fr

table(li_form_fr)
par(mfrow=c(1,1), bty = "l")
plot(log(aggregation.index)~li_form_fr, data = NZ)

#lumping life forms
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

par(mfrow=c(1,1), bty = "l")
plot(log(aggregation.index)~L, data = NZ)
m1 <-glm(log(aggregation.index)~L+log(ncells), family = "gaussian"(link = "identity"), data = NZ)
hist(resid(m1), xlab = "Residuals", cex.lab = 1.2, main = "Histogram of errors")
summary(m1)
summary.lm(m1)
par(mfrow=c(2,2),bty="l")
plot(m1)

test <-factor(L, levels = c("Phan","Geo","Thero","Hemi","Cham"))
#ordered by agg_index out of interest
test <-factor(NZ$li_form_fr, levels = c("Cfru-cad","Hros","Hsto-aqua",
                                        "b-cad","gbul","hsto-hpar","heri-hpar","Heri","A-cad","test-suc","grhi","B-semp",
                                        "B-cad","b-lia","a-cad","test","csuf-suc","tver","A-semp","hces","ccou-suc",
                                        "cfru","heri","gtub","hsto","Grhi","Cfru","hros","csuf","hbis","test(hbis)",
                                        "Hbis(Heri)","Csuf"))
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
m1 <-glm(log(aggregation.index)~S+log(ncells), family = "gaussian"(link = "identity"), data = NZ)
hist(resid(m1), xlab = "Residuals", cex.lab = 1.2, main = "Histogram of errors")
summary(m1)
summary.lm(m1)
par(mfrow=c(2,2),bty="l")
plot(m1)


#****************
search()
detach()


