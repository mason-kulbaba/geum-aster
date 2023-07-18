
# code to generate 2016 fitness landscape

#flower number & PC1

setwd("C:/Users/mkulbaba/Dropbox/git/geum-aster/landscapes with PC/2016/2016 flower number")



dat2<- read.csv("dat2.csv")


aout<- load("aout3.RData")

#rename output to work with recycled code
aout<- aout3

#check for coefficients

aout3$coefficients



a1 <- aout$coefficients["PC1"]
a2 <- aout$coefficients["flw.no2016"]
a <- c(a1, a2)

A11 <- aout$coefficients["I(PC1^2)"]
A22 <- aout$coefficients["I(flw.no2016^2)"]
A12 <- aout$coefficients["I(2 * PC1 * flw.no2016)"]
A <- matrix(c(A11, A12, A12, A22), 2, 2)

eigen(A, symmetric = TRUE, only.values = TRUE)$values


max8 <- (-solve(A, a)/2)
print(max8)


#Plot dist from source & days to germ, on fitness contours


par(mar=c(6, 4.5, 4.5, 8.5), xpd=TRUE)

plot(dat2$PC1, dat2$flw.no2016, xlab = "PC1", 
     ylab = "Flower number 2016", col=as.factor(dat2$Region), pch=16)

legend("topright", inset=c(-.6,0), legend=c("Great Lakes Alvar", "Manitoba Alvar", "Prairie"),col=1:3, pch=16, title="Region", bty="n")

ufoo <- par("usr")
nx <- 101
ny <- 101
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
contour(x, y, z,levels=c(0.01, 0.02, 0.03, 0.04, 0.05), add = TRUE)

#contour(x, y, z, levels = c(0.325), add = TRUE)
