<<<<<<< HEAD
#

dat<- read.csv("cleaned_data_for_aster_with_predictors.csv")


#subset data for 2017 analysis
dat2<- dat[c("Family.Unique",   "Block.ID", "HabitatType", "Region", "Population", "No.Days.to.Germ",
             "Dist.from.cg.km","Germination.Y.N","Survival.Y.N","Survival.Y.N.2017", 
             "Flower.Y.N.2016","Flower.Y.N.2017","No.Flowers.2016","Total.Flowers.2017",
             "Fruit.Y.N.2016","Fruit.Y.N.2017", "No.Fruit.2016","No.Fruit.2017",
             "sm", "sm.2", "Surv2017", "sm2017")]

####################################################################
#Now use Lande and Arnold (1983) method for comparison


dat2$relfit <- dat2$sm2017/mean(dat2$sm2017)
lout <- lm(relfit ~ Region + Dist.from.cg.km + No.Days.to.Germ + I(Dist.from.cg.km^2) +
             I(No.Days.to.Germ^2) + I(2*Dist.from.cg.km*No.Days.to.Germ), data = dat2)
summary(lout)


a1 <- lout$coefficients["Dist.from.cg.km"]
a2 <- lout$coefficients["No.Days.to.Germ"]
a <- c(a1, a2)

A11 <- lout$coefficients["I(Dist.from.cg.km^2)"]
A22 <- lout$coefficients["I(No.Days.to.Germ^2)"]
A12 <- lout$coefficients["I(2 * Dist.from.cg.km * No.Days.to.Germ)"]
A <- matrix(c(A11, A12, A12, A22), 2, 2)

eigen(A, symmetric = TRUE, only.values = TRUE)$values


max8 <- (-solve(A, a)/2)
print(max8)


#plot OLS (Lande and Arnold) way


par(mar=c(5.5, 4.5, 4.5, 8.5), xpd=TRUE)

plot(dat2$Dist.from.cg.km, dat2$No.Days.to.Germ, xlab = "Dist.", 
     ylab = "Days to Germ", col=dat2$Region, pch=16)
legend("topright", inset=c(-.5,0), legend=c("Great Lakes Alvar", "Manitoba Alvar", "Prairie"),col=1:3, pch=16, title="Region", bty="n")

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
contour(x, y, z, add = TRUE)
contour(x, y, z, levels = c(0.325), add = TRUE)
=======
#

dat<- read.csv("cleaned_data_for_aster_with_predictors.csv")


#subset data for 2017 analysis
dat2<- dat[c("Family.Unique",   "Block.ID", "HabitatType", "Region", "Population", "No.Days.to.Germ",
             "Dist.from.cg.km","Germination.Y.N","Survival.Y.N","Survival.Y.N.2017", 
             "Flower.Y.N.2016","Flower.Y.N.2017","No.Flowers.2016","Total.Flowers.2017",
             "Fruit.Y.N.2016","Fruit.Y.N.2017", "No.Fruit.2016","No.Fruit.2017",
             "sm", "sm.2", "Surv2017", "sm2017")]

####################################################################
#Now use Lande and Arnold (1983) method for comparison


dat2$relfit <- dat2$sm2017/mean(dat2$sm2017)
lout <- lm(relfit ~ Region + Dist.from.cg.km + No.Days.to.Germ + I(Dist.from.cg.km^2) +
             I(No.Days.to.Germ^2) + I(2*Dist.from.cg.km*No.Days.to.Germ), data = dat2)
summary(lout)


a1 <- lout$coefficients["Dist.from.cg.km"]
a2 <- lout$coefficients["No.Days.to.Germ"]
a <- c(a1, a2)

A11 <- lout$coefficients["I(Dist.from.cg.km^2)"]
A22 <- lout$coefficients["I(No.Days.to.Germ^2)"]
A12 <- lout$coefficients["I(2 * Dist.from.cg.km * No.Days.to.Germ)"]
A <- matrix(c(A11, A12, A12, A22), 2, 2)

eigen(A, symmetric = TRUE, only.values = TRUE)$values


max8 <- (-solve(A, a)/2)
print(max8)


#plot OLS (Lande and Arnold) way


par(mar=c(5.5, 4.5, 4.5, 8.5), xpd=TRUE)

plot(dat2$Dist.from.cg.km, dat2$No.Days.to.Germ, xlab = "Dist.", 
     ylab = "Days to Germ", col=dat2$Region, pch=16)
legend("topright", inset=c(-.5,0), legend=c("Great Lakes Alvar", "Manitoba Alvar", "Prairie"),col=1:3, pch=16, title="Region", bty="n")

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
contour(x, y, z, add = TRUE)
contour(x, y, z, levels = c(0.325), add = TRUE)
>>>>>>> e12372622c024867a908bcec09cc3820251104b8
