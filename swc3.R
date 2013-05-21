## useful bit at beginning of block03
gDat <- read.delim("gapminderDataFiveYear.txt")
str(gDat)

## don't store copies of little bits of your data.frame like this unless you
## have good reason
(snippet <- subset(gDat, country == "Cambodia"))

## do you want to make plots for each value of a factor? then use multi-panel
## conditioning in lattice or facetting in ggplot2
library(lattice)
xyplot(lifeExp ~ year | country, gDat,
       subset = continent == "Oceania")

## let's begin with the data aggregation functions built-in to R

## let's begin with a function that operates on a matrix (or higher dimensional
## arrays, actually)
## first, we must create a sensible matrix from some of the Gapminder data
(jCountries <-c("Canada", "Cambodia", "Rwanda"))
tinyDat <- subset(gDat, country %in% jCountries)
## safety first! making sure jCountries is in same
## order as the countries appear in tinyDat (and, therefore, gDat)
## important when we apply column names below
(jCountries <- as.character(sort(unique(tinyDat$country))))
tinyDat <- matrix(tinyDat$lifeExp, ncol = length(jCountries))
colnames(tinyDat) <- jCountries
rownames(tinyDat) <- sort(unique(gDat$year))
tinyDat

apply(tinyDat, 1, median) #apply function works on rows, columns of a data array
apply(tinyDat, 1, mean)
apply(tinyDat, 1, summary)
apply(tinyDat, 2, max)
#Obtaining multiple pieces of information (e.g. mean and standard deviation for each row)
apply(tinyDat, 1, 
      function(x){
        c(mean=mean(x), sd=sd(x))
        }
      )
#There are dedicated functions to take means of rows and columns:
rowMeans(tinyDat)

#Question: Which of these countries has the highest life expectency, stratified by year?
which.max(tinyDat["1957",])
which.min(tinyDat["1957",])
colnames(tinyDat)[apply(tinyDat, 1, which.min)] #This outputs the country with the lowest life 
#expectancy for each year for which data were collected.

#sapply and lapply operate on lists (e.g. each variable of a data frame).
sapply(gDat, summary)
sapply(gDat, is.numeric)
gDatNum <- subset(gDat, 
                  select=sapply(gDat, is.numeric))
str(gDatNum)
sapply(gDatNum, median)
sapply(gDatNum, range)
#sapply returns a matrix, and lapply returns a list, though they give the same information.
lapply(gDatNum, median)
#tapply will chop up a vector by a certain factor and apply a function for each factor.
tapply(gDat$lifeExp, gDat$continent, max)
with(gDat,
     tapply(lifeExp, continent, max))
tapply(gDat$lifeExp, gDat$continent, range)
leByCont <- tapply(gDat$lifeExp, gDat$continent, range)
rbind(leByCont[[1]], leByCont[[2]], leByCont[[3]], leByCont[[4]], leByCont[[5]])
do.call(rbind, leByCont) #Equivalent to the above rbind statement without hard coding numbers

install.packages("plyr")
library(plyr)
#The Splity-Apply-Combine Strategy for Data Analysis
ddply(gDat, .(continent), summarise, median=median(lifeExp))
leByCont <- ddply(gDat, .(continent), summarise, min=min(lifeExp), max=max(lifeExp))
leByCont

#Write a function that takes a data frame and returns estimated intercept and slope from a linear
#regression of lifeExp on I(year-yearMin). Use that function to compute estimated regression 
#parameters for all countries using ddply. The results should be a data frame with one row per
#country, giving the name of the country, the intercept, and the slope.
minYr <- min(gDat$year)
f <- function(x) coef(lm(lifeExp ~ I(year-minYr), x))
f(gDat)
ddply(gDat, .(country), f)

#Another example:
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