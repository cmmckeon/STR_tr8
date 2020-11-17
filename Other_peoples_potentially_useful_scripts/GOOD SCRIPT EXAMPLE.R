#Script for Practical 4
#The GOOD way!!

#This script is for basic regression analysis with GLM models and scatter plots


Car<-read.csv(file.choose())

#--------SIMPLE BITS TO CHECK OUT THE DATA---------
#######--------------------------------------------


Car$Years<-as.numeric(Car$Years)#Make sure R reads Years as a numeric value

Car$Cost<-as.numeric(Car$Cost)

class(Car$Years)#Check that! 

class(Car$Years)


hist(Car$Years)#Histogram of Years 

hist(Car$Cost, col="steelblue4", xlab="Cost", ylab="Frequency", main="Histogram of Car Cost")#Histogram of Cost in blue with x and y lables and a plot header 
(main)
#Neither of these look too normal!

#--------GLM AND SCATTER PLOT---------
#######--------------------------------------------


plot(Car$Cost~Car$Years)#regresssion plot for how cost (y) varies according to Year (x)



glm.linear<-glm(Car$Cost~Car$Years)#general linear model of the effect of Year on the cost of cars. 


summary.lm(glm.linear)#using summary.lm here to get the slopes and p values and R-squared. 


#--------RESULTS:--------------
#-------------------------------


#Residuals:
#     Min       1Q   Median       3Q      Max 
#-132.436  -77.926    4.262   29.470  279.772 
#Coefficients:
#            
Estimate Std. Error t value Pr(>|t|)   

#(Intercept)    31.62      50.36   0.628  0.54284   

#Car$Years      46.30      11.35   4.080  0.00182 
**
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 109.6 on 11 degrees of freedom

#Multiple R-squared:  0.6021,	Adjusted R-squared:  0.566 

#F-statistic: 16.65 on 1 and 11 DF,  p-value: 0.00182


#-----HIGHIGHTS:-----------
#-------------------------

#So intercept is 31.62, slope is 46.30
#p- value is <0.001 so significant- there is a significant correlatio between car cost and years. 

#R- squared is 0.566. Strong Positive relationship. 


glm.linear.resids<-residuals(glm.linear)#gets residuals


hist(glm.linear.resids)#histogram of residuals

qqnorm(glm.linear.resids)qqline(glm.linear.resids)#QQ plot looks good- almost a straight line

glm.
linear.preds<-fitted.values(glm.linear)

plot(glm.linear.preds,glm.linear.resids)#esiduas plot. 
Looking for scatter- like snowflakes so this one looks pretty good. Want an even spread above and below the "zero" line. 


plot(Car$Cost~Car$Years, xlab="Years", ylab="Cost ($)", type="L", pch=16, cex=3, col="red")
#Same plot as before but with x and y labels, in an L shaped plot (type=L). 
Points are solid red circles (pch=16) of size 3(cex=3). 
abline (31.62,46.30)

#First4thyearScript
#opening data file in RStudio
mydata<-read.csv(file.choose() , header = TRUE, stringsAsFactors=FALSE)

head(mydata)

str(mydata)
mydata$brain_mass_g <- as.numeric(gsub(",","", mydata[,5]))
mydata$brain_mass_g <- as.numeric(gsub(",","", mydata[,6]))
mydata <- mydata[,1:6]
#finds blanks for you (NA's) gives you "FALSE" when NOT missing values (NA), so missing values come up as TRUE
idx <- is.na(mydata)
#gives you quick look at first few columns, to check if your previous line of code worked
head(idx)
#to select only subset of data, made of points with NO missing values
aa <- 