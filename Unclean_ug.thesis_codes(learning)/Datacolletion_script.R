#Thesis_script1

#***************************************************
#General notes and skill accumulation
#Ctrl L clears script from console

#to install R packages
#>install.packages("packagename")
#you can create a dat
#>hist(datasetname)
# display last 25 commands
#>history()
# display all previous commands
#>history(max.show=Inf) 

# cwd (where R goes to find files to read in when you ask it to open someting)
# print the current working directory
getwd() 

# list the objects in the current workspace
ls()    
setwd("C:/Users/Caroline/Dropbox/Thesis/") 


# dependncies means R also installs other packges that TR8 will need to opperate
install.packages("TR8", dependencies = TRUE)
install.packages("stringr")
install.packages("reshape")

# library loads and attaches add-on packages
library(stringr)

#rearrange trait dataframes
library(reshape)
library(TR8)

#read in dataset to R
sb<-read.csv(file.choose())
afe<-read.csv(file.choose())

attach(sb)
attach(afe)

# **************************************************
# trial on seedbank sample dataset

levels(seedbank)
str(sb)
seedbank
head(sb)

#shows internal dataset structure 
str(sb)
print(sb)

# look for NA entries in dataset
idx <- is.na(sb)
head(idx)

# tells R to read new_sb as a subset of sb dataset, made of only rows containing NA
new_sb <-subset(sb,sb$seedbank=="NA")
head(new_sb)
# result was no NA datapoints

# having issues with organisation of data
# grepl searches for matches to a pattern, within each element of a character vector
# here it searches for spaces in character strings in the seedbank vector
idx.seedbank <- grepl(" ", seedbank)
head(idx.seedbank)

#gsub will replace all the matches found by grepl
#sub will replace only the first one

#trying to remove spaces from seedbank columm in origanal dataset 
sb2 <- as.factor(gsub("_",",", sb[,1]))

# ***************************
# trying to re-organise the data from sb

SBtrial <- data.frame(SPECIES=Species, Transient=seedbank == "transient")

attach(SBtrial)
head(SBtrial)
print(SBtrial)

list(levels(seedbank))
head(sb)
str(sb)

SB <- data.frame(SPECIES=Species, T=seedbank == "transient", LTP=seedbank == "long-term persistent", P=seedbank == "present", STP=seedbank == "short-term persistent", data=sb)
head(SB)
str(SB)


#*********************************************
#having a look at the afe data
#*********************************************
attach(afe)
str(afe)
levels(Species.x)

#looking at the repeated species names Species.x vector
duplicated(Species.x, incomparables = FALSE)
print(unique(Species.x))

#gives only unique leves, but in quotations. 
levels(unique(Species.x))


#Anna's shortcut for creating species list vector that I can use in tr8 function
spl <-as.vector(Species.x, mode = "list")

splist<- as.vector(unique(Species.x))

fewsptrial <-splist[1:3]
fewsptrialNODASH <-gsub("_", " ", fewsptrial)

threesp <- tr8(fewsptrialNODASH, download_list = NULL, gui_config = TRUE, synonyms=TRUE)
threespDF <-extract_traits(threesp)

#tell you the class (type) of an object. Use to check if successfully created dataframe
class(threespDF)

#replace dashes so that dataframe contents will merge happily with afe dataframe
threespDF$original_names <-gsub(" ", "_", threespDF$original_names)

firstmerge<- merge(afe, threespDF, by.x="Species.x", by.y="original_names",all.x=TRUE)
#works perfectly 

spl1 <-splist[1:200]
spl1ND <-gsub("_", " ", spl1)
SP1 <- tr8(spl1ND, download_list = NULL, gui_config = TRUE, synonyms=TRUE)
SP1DF <-extract_traits(SP1)
SP1DF$original_names <-gsub(" ", "_", SP1DF$original_names)

 
completeDF <-merge(afe, allnewDF, by.x="Species.x", by.y="original_names",all.x=TRUE)




