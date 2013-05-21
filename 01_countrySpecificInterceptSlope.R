library(plyr) #ddply()

gDat <- read.delim("gapminderDataFiveYear.txt")
str(gDat)

#Create a data frame, one row per country, with country, continent, intercept, and slope.
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
#Reorder the continent factor by slope.
gCoef$continent <-
  reorder(gCoef$continent, gCoef$slope)
#Drop Oceania.
gCoef <- droplevels(subset(gCoef, continent!="Oceania"))
levels(gCoef$continent)
#Write the data frame to plain text and RDS.
write.table(gCoef, "gCoef.txt", row.names=FALSE, quote=FALSE, sep="\t")
saveRDS(gCoef, "gCoef.rds")
#Save the script as 01_countrySpecificInterceptSlope.R