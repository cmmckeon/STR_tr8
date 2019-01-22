#exploring complete data set

full_df <-read.csv(file.choose())
attach(full_df)

#full_df <- full_df[which(full_df$synonyms != 'NA'),]

print(bud_bank_seasonality_soil)
#looking for traits with enough data points to analyse 
#bud_bank_seasonality_soil[which(bud_bank_seasonality_soil !="NA")]
#better way to do it 
length(which(!is.na(dispersal)))

#removing trait columns with insufficeint data
names(full_df[82])
#new_df <- full_df[,-(83:106), drop=TRUE]
names(new_df[73])
#new_df <- new_df[,-(73), drop=TRUE]
#above are hashed out cus they are a huge hassle when run 
#accidentally. Only to be used with extreme premeditation

str(new_df)

write.csv(new_df, "new_df.csv", row.names = FALSE)
attach(new_df)

#getting subset to explore with table
trait_block <-new_df[,60:83]
head(trait_block)
attach(trait_block)

#great function: shows each level of a vector annontated with how many times it appears in the column
table(sbank)

attach(new_df)

#***********************************
#test model - initial look at analysis
#canopy_height
#responce variable = total.area

plot(log(total.area) ~ log(canopy_height), data = new_df)
m1 <- glm(log(total.area) ~ log(canopy_height), family = "gaussian", data = new_df)
abline(coef(m1), lwd = 2)
summary(m1)
par(mfrow = c(2,2))
plot(m1)

m <-glm(log(total.area)~log(canopy_height)*woodiness, data = new_df)
plot(m)
summary(m)



#****************************************8
#seed_number_per_shoot

plot(log(total.area) ~ log(seed_number_per_shoot), data = new_df)
m1 <- glm(log(total.area) ~ log(seed_number_per_shoot), family = "gaussian", data = new_df)
abline(coef(m1), lwd = 2)
summary(m1)
par(mfrow = c(2,2))
plot(m1)

#***************************
#seed_mass

plot(log(total.area) ~ log(seed_mass), data = new_df)
m1 <- glm(log(total.area) ~ log(seed_mass), family = "gaussian", data = new_df)
abline(coef(m1), lwd = 2)
summary(m1)
par(mfrow = c(2,2))
plot(m1)


#***********************
#sex_repod_fr

table(sex_reprod_fr)

#making new object to let me analyse catagorical traits with tricky/irrevlant levels
names(new_df[76])
repro <-new_df[,c(5,76)]
#levels(repro$sex_reprod_fr)[3]<-levels(repro$sex_reprod_fr)[2]
repro <- repro[which(repro$sex_reprod_fr != "Gynodioecious"),]
levels(repro$sex_reprod_fr)

par(mfrow=c(1,1))
plot(log(total.area) ~ repro$sex_reprod_fr, data = repro)
m1 <- glm(log(total.area) ~ repro$sex_reprod_fr, family = "gaussian", data = repro)
summary(m1)
abline(coef(m1), lwd = 2)
hist(resid(m1), xlab = "Residuals", cex.lab = 1.2, main = "(b) Histogram of errors")
par(mfrow = c(2,2))
plot(m1)


#**************************************
table(new_df$poll_vect_fr)

names(new_df[77])
poll <-new_df[c(5,77)]
levels(poll_vect_fr)[levels(poll_vect_fr)=="self, insect"] <- "insect, self"
glm(log(total.area)~poll_vect_fr, data = new_df)
plot(log(total.area)~poll_vect_fr, data = new_df)

#levels(repro2$sex_reprod_fr)[3]<-levels(repro$sex_reprod_fr)[2]
#repro2 <- repro[which(repro$sex_reprod_fr != "Gynodioecious"),]
#levels(repro2$sex_reprod_fr)

#**************************************
#exploring contrasts for catagorical trait analysis
#dissemenation_fr
table(dissemination_fr)
par(mfrow = c(1,1))
plot(log(total.area)~dissemination_fr, data = new_df)

index<-order(tapply(log(total.area),dissemination_fr))
ordered<-factor(rep(index)(rep(16,9)))
boxplot(log(total.area)~index,notch=T,names=as.character(index),
        xlab="dissemenation_fr",ylab="log(total.area")


#*****************************
#woodiness   - unfinished
table(woodiness)
plot(total.area~woodiness, data = new_df)

m1 <- glm(total.area ~ woodiness, family = "gaussian", data = new_df)
summary(m1)
abline(coef(m1), lwd = 2)
hist(resid(m1), xlab = "Residuals", cex.lab = 1.2, main = "(b) Histogram of errors")
par(mfrow = c(2,2))
plot(m1)

table(woodiness)
W <-as.data.frame(c(new_df[5],new_df[74]))

#3 of 5 levels of woodiness are a variation of semi-woody
#changing 3 different levels to one level - "semi-woody"
levels(W$woodiness)[levels(W$woodiness)=="woody"] <- "semi/woody"
levels(W$woodiness)

par(mfrow=c(1,1)) 

plot(log(W$total.area) ~ W$woodiness)

m1 <- glm(log(total.area) ~ W$woodiness, family = "gaussian", data = W)
summary(m1)
abline(coef(m1), lwd = 2)
hist(resid(m1), xlab = "Residuals", cex.lab = 1.2, main = "(b) Histogram of errors")
par(mfrow = c(2,2))
plot(m1)





#****************
#unsuccessful tinkering


#************************
#dispersal- unfinished

par(mfrow = c(1,1))
plot(log(total.area) ~ dispersal, data = new_df)

#trying to rearranging dispersal data 
names(new_df[66])
new_df_trial <- as.factor(gsub(".+ ","", dispersalDF[,2]))


dispersalDF <-data.frame(Total.Area=total.area, Disp.=dispersal)
DisDF <- data.frame(Total.A=total.area, agochor=dispersal=="agochor", autochor=dispersal=="autochor", boleochor=dispersal=="boleochor", epizoochor=dispersal=="epizoochor", ethelochor=dispersal=="ethelochor",
                    nautochor=dispersal=="nautochor", other=dispersal=="other", zoochor=dispersal=="zoochor", dysochor=dispersal=="dysochor", hemerocho=dispersal=="hemerochor", ombrochor=dispersal=="ombrochor", endozoochor=dispersal=="endozoochor", 
                    meteorochor=dispersal=="meteorochor", speirochor=dispersal=="speirochor", ballochor=dispersal=="ballochor", chamaechor=dispersal=="chamaechor", blastochor=dispersal=="blastochor", bythisochor=dispersal=="bythisochor")                             
write.csv(dispersalDF, "disDF.csv", row.names = FALSE)
list(levels(dispersal))
head(sb)
str(sb)

SB <- data.frame(SPECIES=Species, T=seedbank == "transient", LTP=seedbank == "long-term persistent", P=seedbank == "present", STP=seedbank == "short-term persistent")
head(SB)
str(SB)
#****************************************************

#********************************
#unsuccessful tinkering


#trying to get rid of rows where everything but synonym column is NA 
#no luck
#trial <-cat(paste(shQuote(tbnames, type="cmd"), collapse=" & "))
#TB_goodrows <- trait_block[which(trait_block$tbnames != 'NA'),]


print(Seedling.Vigor)

tbnames <-(tbnames)
print(unique(synonyms))

#trying to remove rows with no results
#final <- final[!(is.na(final$rnor)) | !(is.na(rawdata$cfam)),]
#not working
#trial <- TB[!(is.na(final$rnor)) | !(is.na(rawdata$cfam)),]
#delete.na <- function(DF, n=0) {DF[rowSums(is.na(DF)) <= n,]}
#tbnames <-as.vector(names(TB))
#TB1 <- (tbnames, sep ="|", collapse = ",") 

