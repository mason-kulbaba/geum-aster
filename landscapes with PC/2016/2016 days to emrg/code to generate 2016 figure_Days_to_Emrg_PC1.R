
# code to generate 2016 fitness landscape

#days to emergence & PC1

setwd("C:/Users/mkulbaba/Dropbox/git/geum-aster/landscapes with PC/2016/2016 days to emrg")



dat2<- read.csv("dat2.csv")


aout<- load("aout.RData")

#rename output to work with recycled code
aout<- aout3

#check for coefficients

aout$coefficients


#extract coeff
a1 <- aout$coefficients["PC1"]
a2 <- aout$coefficients["No.Days.to.Germ"]
a <- c(a1, a2)

A11 <- aout$coefficients["I(PC1^2)"]
A22 <- aout$coefficients["I(No.Days.to.Germ^2)"]
A12 <- aout$coefficients["I(2 * PC1 * No.Days.to.Germ)"]
A <- matrix(c(A11, A12, A12, A22), 2, 2)

eigen(A, symmetric = TRUE, only.values = TRUE)$values


max8_2016 <- (-solve(A, a)/2)
print(max8_2016)

#write.table(max8_2016, file="C:/Users/Mason Kulbaba/Dropbox/git/geum-aster/landscape figures/max8_2016.csv", sep=",", quote=FALSE, row.names = FALSE)


#Plot PC1 & days to germ, on fitness contours



par(mar=c(6, 4.5, 4.5, 8.5), xpd=TRUE)

plot(dat2$PC1, dat2$No.Days.to.Germ, xlab = "PC1", 
     ylab = "Days to Emergence", col=as.factor(dat2$Region),pch=16)

legend("topright", inset=c(-.6,0), legend=c("Great Lakes Alvar", "Manitoba Alvar", "Prairie"),col=1:3, pch=16, title="Region", bty="n")


ufoo <- par("usr")
nx <- 101
ny <- 101
z <- matrix(NA, nx, ny)
x <- seq(ufoo[1], ufoo[2], length = nx)
y <- seq(ufoo[3], ufoo[4], length = ny)
points(max8_2016[1], max8_2016[2], pch = 17, col=4)
for (i in 1:nx) {
  for (j in 1:ny) {
    b <- c(x[i], y[j])
    z[i, j] <- sum(a * b) + as.numeric(t(b) %*% A %*%
                                         + b)
  }
}
b <- as.numeric(max8_2016)
contour(x, y, z, nlevels=3, levels= c(.001, .002, .003), add=T)

