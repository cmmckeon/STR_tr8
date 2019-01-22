#### Homework (Q1) plotting models in 3-D
rm(list=ls())

setwd()			# Set the working directory appropriately
linreg.data <- read.table("BIOM25B_Prac3a.csv",header=T,sep=",")
attach(linreg.data)
names(linreg.data)

mreg1 <- lm(Diversity.Index ~ Sward.Height + Distance.Sea)
summary(mreg1)

mreg2 <- lm(Diversity.Index ~ Sward.Height + Distance.Sea + Sward.Height * Distance.Sea)
summary(mreg2)

# 3-d plotting: set up vectors of x- and y-axis values
x <- seq(min(Sward.Height),max(Sward.Height),length.out=11)
y <- seq(min(Distance.Sea),max(Distance.Sea),length.out=10)

# Convert these values to matrices, so that each x-value corresponds to all y-values, & vice-versa
xx <- t(matrix(1,10,1)%*%x)				# This uses matrix multiplication (algebra) to generate the matrix
yy <- matrix(1,11,1)%*%y				# This uses matrix multiplication (algebra) to generate the matrix

# Use the model coefficients to plot the model predicted values for mreg1
# You could also try using the predict.lm function, combined with as.vector(xx) and as.vector(yy)
zz <- coef(mreg1)[1] + coef(mreg1)[2]*xx + coef(mreg1)[3]*yy

persp(x,y,zz,xlab = "Sward Height (m), x",ylab="Distance to Sea (m), y",zlab="Diversity Index, z",ticktype="detailed",theta=300,phi=30) -> res

# Add the data points, as blue filled circles
points(trans3d(Sward.Height,Distance.Sea,Diversity.Index,pmat=res),pch=16,col="blue",bg="blue")


# Use the model coefficients to plot the model predicted values for mreg2
zz2 <- coef(mreg2)[1] + coef(mreg2)[2]*xx + coef(mreg2)[3]*yy + coef(mreg2)[4]*xx*yy

# Open a new plotting device and plot the mreg2 results
dev.new()
persp(x,y,zz2,xlab = "Sward Height (m), x",ylab="Distance to Sea (m), y",zlab="Diversity Index, z",ticktype="detailed",theta=300,phi=30) -> res
points(trans3d(Sward.Height,Distance.Sea,Diversity.Index,pmat=res),pch=16,col="blue",bg="blue")

# The interaction effect size (slope) is close to 0 (0.0087), so it has a very minor visual impact, 
# introducing some non-linearity to the 3-D plane. Try increasing that value manually (e.g., to ±0.5) 
# to see what could happen!

detach(linreg.data)

##############################
#### Homework (Q2) CO2 example
##############################
rm(list = ls())

anova(q2a <- lm(uptake ~ conc + Treatment + Type, data = CO2))			# All of the main effects are significant
anova(q2b <- lm(uptake ~ conc * Treatment * Type, data = CO2))			# ... and some (not all) of the interactions!
# could do some model simplification using the update() function {e.g. q2c <- update(q2b, .~ .-conc:Treatment:Type)}, 
# but I won't here. Feel free to try yourselves though!

shapiro.test(q2a$residuals)												# Not normal - be careful with further interpretation
shapiro.test(q2b$residuals)												# Just squeaks in as not deviating from normal...

AIC(q2a, q2b)														# q2b looks like a good model, with no interp issues

install.packages("AICcmodavg")											# Install the package
library("AICcmodavg")													# Load the library for use

cand.mods <- list("q2a" = q2a, "q2b" = q2b)								# Create a list of condidate models with labels
aictab(cand.mods)														# The aictab() function is found in AICcmodavg
																		# Confirms that q2b is a better model, time to interpret!
summary(q2b)			# None of the interaction terms differ significantly from the conc slope for 'nonchilled' Plants in Quebec
						# Howver, that doesn't mean that there aren't other differences between slopes. 
						# You can try using glht() function in the multcompare package, as a starting point.
						# But in general, interpreting ANCOVA interactions is tricky!

install.packages("multcomp")											# Install & load the multcomp package	
library("multcomp")
?glht

summary(tky <- glht(q2b, linfct = mcp(interaction(conc,Treatment,Type) = "Tukey")))

detach("package:multcomp")											# Remove package to avoid conflicts


###############################
#### Homework (Q3) Puma example
###############################

rm(list=ls())

# Data copy-pasted from Table 1 in Soria-Díaz et al (2017) Eur J Wildl Res
RAI_puma <- c(3.33, 1.22, 1.49, 1.42, 0.82, 0.95, 1.01, 2.74, 1.08, 7.32, 1.21, 2.82)
RAI_wtd <- c(1.85, 0.48, 1.55, 0.96, 0.57, 0.91, 1.21, 3.40, 2.43, 9.76, 3.86, 4.92)

n <- length(RAI_puma)														# How many data points are there?

puma_pca <- log(RAI_puma[2:n] / RAI_puma[1:(n-1)])							# Puma pca = ln(N_t+1/N_t)
puma_mdl <- lm(puma_pca ~ log(RAI_puma[1:11]) + log(RAI_wtd[1:11]))			# Fit the model, using log(RAI) values
summary(puma_mdl)															# Details for Table 3
AICc(puma_mdl)																# AICc from the AICcmodavge package
detach("package:AICcmodavg")												# Remove package
