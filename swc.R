a <- 2
b <- 3
sigsq <- 0.5
n <- 400
x <- runif(n)
y <- a+b*x + rnorm(n,sd=sqrt(sigsq))
avgx <- mean(x)
plot(x,y)
abline(a,b,col="blue",lwd=3)
dev.print(pdf)
write(avgx, "avgx.txt")
