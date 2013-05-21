library(lattice)
install.packages("knitr")
library(knitr) #HTML as a simple option for creating dynamic reports.

gCoef <- readRDS("gCoef.rds")
str(gCoef)

#Only keep data from Asia and the Americas for comparison.
hDat <- droplevels(subset(gCoef, continent %in% c("Asia", "Americas")))
str(hDat)

dotplot(slope ~ continent, hDat)

t.test(slope ~ continent, hDat)