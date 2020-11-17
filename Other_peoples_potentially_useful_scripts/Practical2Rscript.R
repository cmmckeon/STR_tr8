dog<-read.csv(file.choose())
head(dog)#
names(dog)#
dog$Total.Worms<-as.numeric(dog$Total.Worms)
hist(dog$Total.Worms)
qqnorm(dog$Total.Worms)
hist(dog$Total.Worms[dog$Age=="Puppy"])
hist(dog$Total.Worms[dog$Sex=="Male"])
hist(dog$Total.Worms[dog$Sex=="Male"&dog$Age=="Puppy"])
boxplot(dog$Total.Worms)
boxplot(dog$Total.Worms[dog$Age=="Puppy"])
hist(dog$Total.Worms,col="green")
hist(dog$Total.Worms,col=rainbow(8))
hist(dog$Total.Worms,col="red",xlab="NumberofWorms",main="Wormburdeninmaledogs")
mean(dog$Total.Worms)
11.36
mean(dog$Total.Worms[dog$Sex=="Male"&dog$Age=="Puppy"])
36.91667
sd(dog$Total.Worms)
32.78812
var(dog$Total.Worms)
1075.061
summary(dog$Total.Worms)#minimum, mean, maximum
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   0.00    0.00    0.00   11.36    2.00  167.00 
Maleburden<-dog$Total.Worms[dog$Age=="Adult"&dog$Sex=="Male"]
Femaleburden<-dog$Total.Worms[dog$Age=="Adult"&dog$Sex=="Female"]
t.test(Maleburden,Femaleburden)
   Welch Two Sample t-test

data:  Maleburden and Femaleburden
t = 0.88318, df = 63.367, p-value = 0.3805
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -0.3197034  0.8261970
sample estimates:
mean of x mean of y 
0.5714286 0.3181818 
boxplot(Maleburden,Femaleburden)

Maleburden<-dog$Total.Worms[dog$Age=="Puppy"&dog$Sex=="Male"]
Femaleburden<-dog$Total.Worms[dog$Age=="Puppy"&dog$Sex=="Female"]
t.test(Maleburden,Femaleburden)

 Welch Two Sample t-test

data:  Maleburden and Femaleburden
t = -0.60851, df = 22.998, p-value = 0.5488
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -58.91450  32.13245
sample estimates:
mean of x mean of y 
 36.91667  50.30769 
boxplot(Maleburden,Femaleburden)


hist(dog$Total.Worms[dog$Sex=="Female"],col="yellow")
hist(dog$Total.Worms[dog$Age=="Puppy"],col="steelblue4",xlab="Number of Worms",main="Worm burden in Puppies")

