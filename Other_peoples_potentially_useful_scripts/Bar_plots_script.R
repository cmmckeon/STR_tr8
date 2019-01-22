p<-read.csv(file.choose())
head(p)
barplot(p$X, p$Publication_Year
        [p$Search_Place == "Topic" & p$Search_Term == "Ecosystem Service*"])

#par(mfrow = c(2,3), cex = 1.55)
#par(mfrow = c(1,1))

df = barplot(p$X
          [p$Search_Place == "Topic" & p$Search_Term == "Ecosystem Service*"]
          , p$Publication_Year
          [p$Search_Place == "Topic" & p$Search_Term == "Ecosystem Service*"]
          , border="white", main = "Ecosystem Service (Topic)"
          ,ylab="Articles published each year", xlab="Years"
          ,ylim=c(0,3000), axes=FALSE, space=0, col="grey50")
lab <- c(2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013
         ,2014,2015,2016,2017)
axis(2, las=2, pos=0)
axis(1, at=df, labels = lab, las = 2)
axis(3, at = c(0,40000), labels = FALSE, tick = TRUE, tck = 0)
axis(4, at = c(0,3000), labels = FALSE, tick = TRUE, tck = 0, xpd = NA)
axis(1, at = c(0,340000), labels = FALSE, tick = TRUE, tck = 0)


df = barplot(p$X
             [p$Search_Place == "Topic" & p$Search_Term == "Green Infrastructure*"]
             , p$Publication_Year
             [p$Search_Place == "Topic" & p$Search_Term == "Green Infrastructure*"]
             , border="white", main = "Green Infrastructure (Topic)"
             ,ylab="Articles published each year", xlab="Years"
             ,ylim=c(0,300), axes=FALSE, space=0, col="grey50")

lab <- c(2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013
         ,2014,2015,2016,2017)
axis(2, las=2, pos=0)
axis(1, at=df, labels = lab, las = 2)
axis(3, at = c(0,40000), labels = FALSE, tick = TRUE, tck = 0)
axis(4, at = c(0,3000), labels = FALSE, tick = TRUE, tck = 0)
axis(1, at = c(0,340000), labels = FALSE, tick = TRUE, tck = 0)


df = barplot(p$X
             [p$Search_Place == "Topic" & p$Search_Term == "Nature Based Solution*"]
             , p$Publication_Year
             [p$Search_Place == "Topic" & p$Search_Term == "Nature Based Solution*"]
             , border="white", main = "Nature Based Solution (Topic)"
             ,ylab="Articles published each year", xlab="Years"
             ,ylim=c(0,20), axes=FALSE, space=0, col="grey50")

lab <- c(2009,2015,2016,2017)
axis(2, las=2, pos=0)
axis(1, at=df, labels = lab, las = 2)
axis(3, at = c(0,40000), labels = FALSE, tick = TRUE, tck = 0)
axis(4, at = c(0,3000), labels = FALSE, tick = TRUE, tck = 0)
axis(1, at = c(0,340000), labels = FALSE, tick = TRUE, tck = 0)


###
#TITLE
###

df = barplot(p$X
             [p$Search_Place == "Title" & p$Search_Term == "Ecosystem Service*"]
             , p$Publication_Year
             [p$Search_Place == "Title" & p$Search_Term == "Ecosystem Service*"]
             , border="white", main = "Ecosystem Service (Title)"
             ,ylab="Articles published each year", xlab="Years"
             ,ylim=c(0,800), axes=FALSE, space=0, col="grey50")

lab <- c(2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013
         ,2014,2015,2016,2017)
axis(2, las=2, pos=0)
axis(1, at=df, labels = lab, las = 2)
axis(3, at = c(0,40000), labels = FALSE, tick = TRUE, tck = 0)
axis(4, at = c(0,3000), labels = FALSE, tick = TRUE, tck = 0)
axis(1, at = c(0,340000), labels = FALSE, tick = TRUE, tck = 0)


df = barplot(p$X
             [p$Search_Place == "Title" & p$Search_Term == "Green Infrastructure*"]
             , p$Publication_Year
             [p$Search_Place == "Title" & p$Search_Term == "Green Infrastructure*"]
             , border="white", main = "Green Infrastructure (Title)"
             ,ylab="Articles published each year", xlab="Years"
             ,ylim=c(0,100), axes=FALSE, space=0, col="grey50")

lab <- c(2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013
         ,2014,2015,2016,2017)
axis(2, las=2, pos=0)
axis(1, at=df, labels = lab, las = 2)
axis(3, at = c(0,40000), labels = FALSE, tick = TRUE, tck = 0)
axis(4, at = c(0,3000), labels = FALSE, tick = TRUE, tck = 0)
axis(1, at = c(0,340000), labels = FALSE, tick = TRUE, tck = 0)


df = barplot(p$X
             [p$Search_Place == "Title" & p$Search_Term == "Nature Based Solution*"]
             , p$Publication_Year
             [p$Search_Place == "Title" & p$Search_Term == "Nature Based Solution*"]
             , border="white", main = "Nature Based Solution (Title)"
             ,ylab="Articles published each year", xlab="Years"
             ,ylim=c(0,10), axes=FALSE, space=0, col="grey50")

lab <- c(2009,2015,2016,2017)
axis(2, las=2, pos=0)
axis(1, at=df, labels = lab, las = 2)
axis(3, at = c(0,40000), labels = FALSE, tick = TRUE, tck = 0)
axis(4, at = c(0,3000), labels = FALSE, tick = TRUE, tck = 0)
axis(1, at = c(0,340000), labels = FALSE, tick = TRUE, tck = 0)
