Doughnuts<-read.csv(file.choose())
head(Doughnuts)
names(Doughnuts)
Doughnuts
Doughnuts$Fat<-as.numeric(Doughnuts$Fat)
Doughnuts$Outlet<-as.factor(Doughnuts$Outlet)
class(Doughnuts$Fat)
hist(Doughnuts$Fat)
qqnorm(Doughnuts$Fat)
boxplot(Doughnuts$Fat~Doughnuts$Outlet)
mean(Doughnuts$Fat[Doughnuts$Outlet=="1"])
72
sd(Doughnuts$Fat[Doughnuts$Outlet=="1"])
13.34166
var(Doughnuts$Fat[Doughnuts$Outlet=="1"])
178
summary(Doughnuts)

       ID        Outlet      Fat       
 Min.   : 1.00   1:6    Min.   :49.00  
 1st Qu.: 6.75   2:6    1st Qu.:65.50  
 Median :12.50   3:6    Median :73.50  
 Mean   :12.50   4:6    Mean   :73.75  
 3rd Qu.:18.25          3rd Qu.:79.00  
 Max.   :24.00          Max.   :97.00 

aov.Doughnuts=aov(Fat~Outlet,data=Doughnuts)
summary(aov.Doughnuts)

            Df Sum Sq Mean Sq F value  Pr(>F)   
Outlet       3   1636   545.5   5.406 0.00688 **
Residuals   20   2018   100.9                   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

print(model.tables(aov.Doughnuts,"means"),digits=3)

Tables of means
Grand mean
      
73.75 

 Outlet 
Outlet
 1  2  3  4 
72 85 76 62 


