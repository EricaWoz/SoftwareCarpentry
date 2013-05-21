gDat <- read.delim("gapminderDataFiveYear.txt")
gDat <- read.delim("gapminderDataFiveYear.txt", header=TRUE, sep="\t") #Equivalent read-in

str(gDat) #"Proc contents" equivalent
head(gDat) #First 6 observations printed
tail(gDat) #Last 6 observations printed
sample(x=nrow(gDat), size=6) #randomly sample 6 rows
sort(sample(x=nrow(gDat), size=6)) #sort the randomly sampled rows
gDat[sort(sample(x=nrow(gDat), size=6)), ] #random sorted set of 6 observations

peek <- function(df) df[sort(sample(x=nrow(df), size=6)), ] #function that returns random sample
#of 6 observations from the data set

names(gDat)
dim(gDat)
nrow(gDat)
ncol(gDat)
length(gDat)
head(rownames(gDat))

summary(gDat)

library(lattice)
xyplot(lifeExp ~ year, gdpPercap)
xyplot(gDat$lifeExp ~ gDat$year)  #Equivalent

xyplot(lifeExp ~ gdpPercap, gDat, subset=country=="Colombia") #Subset this way to create self-
#documentation and reproducibility instead of generating a new data frame for Colombia alone.
xyplot(lifeExp ~ gdpPercap, gDat, subset=year==2007) 
xyplot(lifeExp ~ gdpPercap, gDat, group=continent, auto.key=TRUE, subset=year==2007) 
#Make a legend for a graph automatically

#Factors as stored as integers with an associated set of labels (usually character strings).
densityplot(~lifeExp, gDat)
table(gDat$continent) #useful for tabulating categorical variables
str(gDat) #note that country is listed as a factor with 142 levels and continent is a factor with 
#5 levels
levels(gDat$continent) #Get factor labels
nlevels(gDat$continent) #Get number of levels
barchart(table(gDat$continent))
dotplot(table(gDat$continent), type='h') #Same data as above, but different presentation
dotplot(table(gDat$continent), type=c("p","h"))

subset(gDat, subset=country=="Uruguay") #Generate a new data frame that is a subset of the data
subset(gDat, subset=country %in% c("Japan","Uruguay"))
subset(gDat, subset=country=="Uruguay", select=c(country, year, lifeExp))
subset(gDat, subset=country=="United States", select=c(year, lifeExp))
xyplot(lifeExp ~ year, gDat, subset=country=="United States") #Generate linear plot
lm(lifeExp ~ year, gDat, subset=country=="United States")

(minYear <- min(gDat$year)) #Use extra parentheses to print output as the code runs

lm(lifeExp ~ I(year-minYear), gDat, subset=country=="United States")


x <- 3*4
is.vector(x)
length(x)
x[2] <- 100 #Grow the vector by putting 100 in the second slot
x
x[5] <- 3 #Can also fill in a later slot, and R will fill in others with NA
x

x <- rnorm(25)
x
x^2 #Squares element by element

(y <- 1:3)
(z <- 3:7)
y+z #R recycles values from the smaller vector starting at the beginning
y <- 1:10
y+z

x <- c("cabbage", pi, TRUE, 4.3)
x
class(x)
x <- list("cabbage", pi, TRUE, 4.3)
x
class(x)

set.seed(1)
x <- round(rnorm(8), 2)
x
names(x) <- letters[seq_along(x)]
x
x[3] #Subset vectors
x[c(1,2,3)]
x[-c(1,2,3)]
x[-4]
x>0
x[x>0]
x[c(TRUE,FALSE)] #This vector is recycled, so this can be used to index by every other variable
x[!(x>0)]
x["a"]
x[c("a","d","e")]

ggDat <- gDat
ggDat$yearAlt  <- ggDat$year-minYear
str(ggDat)
ggDat$pop  <- ggDat$pop / 1000000 #Give population in millions
summary(ggDat$pop)
#Can equivalently use the transform function
