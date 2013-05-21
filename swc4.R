gDat <- read.delim("gapminderDataFiveYear.txt")
str(gDat)
minYear <- min(gDat$year)
jFun <- function(df){
  jCoef <- coef(lm(lifeExp ~ I(year-minYear), df))
  names(jCoef) <- c("intercept", "slope")
  jCoef
}
jFun(gDat)
jFun(subset(gDat, country=="Canada"))
gCoef <- ddply(gDat, .(country), jFun)
str(gCoef)

#Now let's also add the continent that these countries are in.
gCoef <- ddply(gDat, .(country, continent), jFun)
str(gCoef)
head(gCoef) 

#Generate figures:
library(lattice)
bwplot(slope ~ continent, gCoef)

#Reordering levels of a factor:
gCoef <- reorder(gCoef$continent, gCoef$slope)
bwplot(slope ~ continent, gCoef)
levels(gCoef$continent)

foo <- subset(gCoef, continent!="Oceania")
str(foo)
subset(foo, country=="New Zealand")
levels(gCoef$continent)
droplevels(foo)

gCoef <- droplevels(subset(gCoef, continent!="Oceania"))
levels(gCoef$continent)

#Function for sending a data frame back out to a plain text file:
write.table(gCoef, "gCoef.txt", row.names=FALSE, quote=FALSE, sep="\t")
#This is easier to look at and send data to users of other software.

dput(gCoef, "gCoef_DPUT.txt") #Shorthand for recreating data frame and factor labels back into R.
rm(gCoef) #remove the data
gCoef <- dget("gCoef_DPUT.txt")
levels(gCoef$continent)
gCoef
#This is better for sending data to another R user.

saveRDS(gCoef, "gCoef.rds")
rm(gCoef)
gCoef <- readRDS("gCoef.rds")
levels(gCoef$continent)

#Write two small stand-alone scripts demonstrating analysis techniques