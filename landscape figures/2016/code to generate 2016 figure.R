
# code to generate 2016 fitness landscape

#Plot dist from source & days to germ, on fitness contours


dat2<- read.csv("C:/Users/Mason Kulbaba/Dropbox/git/geum-aster/landscape figures/2016/dat2.csv")


load("C:/Users/Mason Kulbaba/Dropbox/git/geum-aster/landscape figures/2016/aout.RData")

#check for coefficients

aout$coefficients


#extract coeff
a1 <- aout$coefficients["Dist.from.cg.km"]
a2 <- aout$coefficients["No.Days.to.Germ"]
a <- c(a1, a2)

A11 <- aout$coefficients["I(Dist.from.cg.km^2)"]
A22 <- aout$coefficients["I(No.Days.to.Germ^2)"]
A12 <- aout$coefficients["I(2 * Dist.from.cg.km * No.Days.to.Germ)"]
A <- matrix(c(A11, A12, A12, A22), 2, 2)

eigen(A, symmetric = TRUE, only.values = TRUE)$values


max8 <- (-solve(A, a)/2)
print(max8)



par(mar=c(5.5, 4.5, 4.5, 8.5), xpd=TRUE)

plot(dat2$Dist.from.cg.km, dat2$No.Days.to.Germ, xlab = "Dist.", 
     ylab = "Days to Germ", col=dat2$Region, ylim=c(5,25), pch=16)

legend("topright", inset=c(-.6,0), legend=c("Great Lakes Alvar", "Manitoba Alvar", "Prairie"),col=1:3, pch=16, title="Region", bty="n")


ufoo <- par("usr")
nx <- 3
ny <- 3
z <- matrix(NA, nx, ny)
x <- seq(ufoo[1], ufoo[2], length = nx)
y <- seq(ufoo[3], ufoo[4], length = ny)
points(max8[1], max8[2], pch = 17, col=4)
for (i in 1:nx) {
  for (j in 1:ny) {
    b <- c(x[i], y[j])
    z[i, j] <- sum(a * b) + as.numeric(t(b) %*% A %*%
                                         + b)
  }
}
b <- as.numeric(max8)
contour(x, y, z, add = TRUE)
contour(x, y, z, levels = c(0.325), add = TRUE)
