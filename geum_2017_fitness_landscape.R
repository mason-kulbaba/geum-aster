
setwd("C:/Users/Mason Kulbaba/Dropbox/git/geum-aster")


dat<- read.csv("cleaned_data_for_aster_with_predictors.csv")



#subset data for 2017 analysis
dat2<- dat[c("Family.Unique",   "Block.ID", "HabitatType", "Region", "Population", "No.Days.to.Germ",
             "Dist.from.cg.km","Germination.Y.N","Survival.Y.N","Survival.Y.N.2017", 
             "Flower.Y.N.2016","Flower.Y.N.2017","No.Flowers.2016","Total.Flowers.2017",
             "Fruit.Y.N.2016","Fruit.Y.N.2017", "No.Fruit.2016","No.Fruit.2017",
             "sm", "sm.2", "Surv2017", "sm2017")]



#load pca data

pca<- read.csv("pca.csv")


#merge PCA data into dat2

dat4<- merge(dat2, pca)

dat2<-dat4

#flower number wiht different name

dat2$flw.no2017<- dat2$Total.Flowers.2017


#remove rows with NAs in "days to germ." variable

dat3<- subset(dat2, No.Days.to.Germ >0)

dat3<- subset(dat3, Germination.Y.N==1 | Germination.Y.N==0)

dat2<-dat3


######################
vars<- c("Germination.Y.N", "Survival.Y.N","Surv2017",
         "No.Flowers.2016","Total.Flowers.2017","No.Fruit.2016", "No.Fruit.2017","sm", "sm.2", "sm2017")


#look into distributions for nodes

#recall nodes of graphical model
vars

#Isolate non bernoulli varibles, and prepare to test for 

flwno<- dat2$No.Flowers.2016


flwno2<- dat2$Total.Flowers.2017

frtno<- dat2$No.Fruit.2016

frtno2<- dat2$No.Fruit.2017

seeds<-dat2$sm2017

sm<-dat$sm

sm2<- dat$sm.2

sm2017<- dat$sm2017

library(MASS)

#library(fitdistrplus)
#library(gamlss) #to include ZIP etc.

#2016 flw no
fl.1<- fitdistr(flwno, "normal")
fl.2<- fitdistr(flwno, "negative binomial")#size: 0.156957321   
fl.3<- fitdistr(flwno, "poisson")

AIC(fl.1, fl.2, fl.3)
fl.2

#2017 flw no
fl2.1<- fitdistr(flwno2, "normal")
fl2.2<- fitdistr(flwno2, "negative binomial")#size: 1.13754614   
fl2.3<- fitdistr(flwno2, "poisson")

AIC(fl2.1, fl2.2, fl2.3)
fl2.2

#2016 fruit number
frt.1<- fitdistr(frtno, "normal")
frt.2<- fitdistr(frtno, "negative binomial")#size: 0.045627385   
frt.3<- fitdistr(frtno, "poisson")

AIC(frt.1, frt.2, frt.3)
frt.2

#2017 fruit number
frt2.1<- fitdistr(frtno2, "normal")
frt2.2<- fitdistr(frtno2, "negative binomial")#size: 0.64875354   
frt2.3<- fitdistr(frtno2, "poisson")

AIC(frt2.1, frt2.2, frt2.3)
frt2.2

#seeds set (2016 + 2017)
seed.1<- fitdistr(seeds, "normal")
seed.2<- fitdistr(seeds, "negative binomial")#size: 2.299916e-01
seed.3<- fitdistr(seeds, "poisson")

AIC(seed.1, seed.2, seed.3)
seed.2

#sm
sm.1<- fitdistr(sm, "normal")
sm.2<- fitdistr(sm, "negative binomial")#size: 0.0058024283
sm.3<- fitdistr(sm, "poisson")

AIC(sm.1, sm.2, sm.3)
sm.2

#sm.2
sm.2.1<- fitdistr(sm2, "normal")
sm.2.2<- fitdistr(sm2, "negative binomial")#size: 0.08457787
sm.2.3<- fitdistr(sm2, "poisson")

AIC(sm.2.1, sm.2.2, sm.2.3)
sm.2.2


#sm2017
sm2017.2.1<- fitdistr(sm2017, "normal")
sm2017.2.2<- fitdistr(sm2017, "negative binomial")#size: 8.544092e-02
sm2017.2.3<- fitdistr(sm2017, "poisson")

AIC(sm2017.2.1, sm2017.2.2, sm2017.2.3)
sm2017.2.2

#reshape data so that all response variables are located in a single vector in a new data
#set called "redata"
redata2017 <- reshape(dat2, varying = list(vars), direction = "long",timevar = "varb", times = as.factor(vars), v.names = "resp")


#Designation of fitness variable for 2016 data
fit <- grepl("sm2017", as.character(redata2017$varb))
fit<- as.numeric(fit)

redata2017$fit <- fit

#check
with(redata2017, sort(unique(as.character(varb)[fit == 0])))
with(redata2017, sort(unique(as.character(varb)[fit == 1])))


#add a variable "root" to redata files, where value is 1
redata2017<- data.frame(redata2017, root=1)

#check classes of redata2017
sapply(redata2017, class)

#make block.id a factor

redata2017$Block.ID<- as.factor(redata2017$Block.ID)


#load aster package
library(aster)


#set up custom family list

famlist <- list(fam.bernoulli(),
                fam.negative.binomial(0.156957321),
                fam.negative.binomial(1.13754614),
                fam.negative.binomial(0.045627385),
                fam.negative.binomial(0.64875354), 
                fam.negative.binomial(0.0058024283),
                fam.negative.binomial(0.08457787),
                fam.negative.binomial(8.544092e-02))





pred<- c(0,1,2,2,3,4,5,6,7,1)

fam<- c(1,1,1,2,3,4,5,6,7,8)
#sapply(fam.default(), as.character)[fam]

#fixed effect model for 2017 with only fitness: note the use of 'famlist'
aouta<- aster(resp~varb, pred, fam, varb, id, root, data=redata2017,famlist = famlist)

summary(aouta, show.graph=T, info.tol = 1e-11)

#include HabitatType in model
aout<- aster(resp~varb + fit:(Region), pred, fam, varb, id, root, data=redata2017, famlist=famlist)


summary(aout, show.graph = TRUE, info.tol=1e-13)

anova(aouta, aout)#Region is significant

aout3<- aster(resp~varb + fit:Region + PC1 + dat2$Total.Flowers.2017 + I(PC1^2) + I(dat2$Total.Flowers.2017^2) + I(2*PC1*dat2$Total.Flowers.2017), pred, fam, varb, id, root, 
              data=redata2017, famlist = famlist)

summary(aout3, show.graph=T, info.tol=1e-12)

aout3$coefficients

aout<- aout3

#save(aout, file="C:/Users/Mason Kulbaba/Dropbox/git/geum-aster/landscape figures/2017/aout.RData")



######################################################################
# Estimate Selection Gradient (distance from source)

pout <- predict(aout)
pout <- matrix(pout, nrow = nrow(aout$x), ncol = ncol(aout$x))
colnames(pout) <- colnames(aout$x)
mufit <- pout[, grep("sm2017", colnames(pout))]


#only needed when >1 year of data
#mufit <- apply(mufit, 1, "sum")

#calcualte mean fitness
wmu <- mufit/mean(mufit)

#perform linear analysis
wmout <- lm(wmu ~ dat2$PC1)

pre_w<- predict(wmout)

summary(wmout)


#Now try with two predictors dist to source and days to germination

#see new aout model on line 125

#extract two coeff

a1 <- aout$coefficients["PC1"]
a2 <- aout$coefficients["No.Days.to.Germ"]
a <- c(a1, a2)

A11 <- aout$coefficients["I(PC1^2)"]
A22 <- aout$coefficients["I(No.Days.to.Germ^2)"]
A12 <- aout$coefficients["I(2 * PC1 * No.Days.to.Germ)"]
A <- matrix(c(A11, A12, A12, A22), 2, 2)

eigen(A, symmetric = TRUE, only.values = TRUE)$values


max8 <- (-solve(A, a)/2)
print(max8)


#Plot dist from source & days to germ, on fitness contours

par(mar=c(5.5, 4.5, 4.5, 8.5), xpd=TRUE)

plot(dat2$PC1, dat2$No.Days.to.Germ, xlab = "PC1", 
     ylab = "Days to Germ", col=dat2$Region, pch=16)

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
contour(x, y, z, add = TRUE)
contour(x, y, z, levels = c(0.325), add = TRUE)


####################################################################
# PC1 and PC2
aout4<- aster(resp~varb + fit:Region + PC1 + PC2 + I(PC1^2) + I(PC2^2) + I(2*PC1*PC2), pred, fam, varb, id, root, 
              data=redata2017, famlist = famlist)

summary(aout4, show.graph=T, info.tol=1e-12)

aout4$coefficients

aout<- aout4


a1 <- aout$coefficients["PC1"]
a2 <- aout$coefficients["PC2"]
a <- c(a1, a2)

A11 <- aout$coefficients["I(PC1^2)"]
A22 <- aout$coefficients["I(PC2^2)"]
A12 <- aout$coefficients["I(2 * PC1 * PC2)"]
A <- matrix(c(A11, A12, A12, A22), 2, 2)

eigen(A, symmetric = TRUE, only.values = TRUE)$values


max8 <- (-solve(A, a)/2)
print(max8)

####################################################

# Flower Number Analsyis

aout3<- aster(resp~varb + fit:Region + PC1 + flw.no2017 + I(PC1^2) + I(flw.no2017^2) + I(2*PC1*flw.no2017), pred, fam, varb, id, root, 
              data=redata2017, famlist = famlist)

summary(aout3, show.graph=T, info.tol=1e-12)

aout3$coefficients

aout<- aout3

#save(aout, file="C:/Users/Mason Kulbaba/Dropbox/git/geum-aster/landscape figures/2017/aout.RData")



######################################################################
# Estimate Selection Gradient (distance from source)

#pout <- predict(aout)
#pout <- matrix(pout, nrow = nrow(aout$x), ncol = ncol(aout$x))
#colnames(pout) <- colnames(aout$x)
#mufit <- pout[, grep("sm2017", colnames(pout))]


#only needed when >1 year of data
#mufit <- apply(mufit, 1, "sum")

#calcualte mean fitness
#wmu <- mufit/mean(mufit)

#perform linear analysis
#wmout <- lm(wmu ~ dat2$PC1)

#pre_w<- predict(wmout)

#summary(wmout)


#Now try with two predictors dist to source and days to germination

#see new aout model on line 125

#extract two coeff

a1 <- aout$coefficients["PC1"]
a2 <- aout$coefficients["flw.no2017"]
a <- c(a1, a2)

A11 <- aout$coefficients["I(PC1^2)"]
A22 <- aout$coefficients["I(flw.no2017^2)"]
A12 <- aout$coefficients["I(2 * PC1 * flw.no2017)"]
A <- matrix(c(A11, A12, A12, A22), 2, 2)

eigen(A, symmetric = TRUE, only.values = TRUE)$values


max8 <- (-solve(A, a)/2)
print(max8)

par(mar=c(5.5, 4.5, 4.5, 8.5), xpd=TRUE)

plot(dat2$PC1, dat2$flw.no2017, xlab = "PC1", 
     ylab = "Flower number 2017", col=dat2$Region, pch=16, ylim=c(0,60))

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
contour(x, y, z, add = TRUE)
contour(x, y, z, levels = c(0.325), add = TRUE)

####################################################

# Flower Number Analysis with PC2

aout3<- aster(resp~varb + fit:Region + PC2 + flw.no2017 + I(PC2^2) + I(flw.no2017^2) + I(2*PC2*flw.no2017), pred, fam, varb, id, root, 
              data=redata2017, famlist = famlist)

summary(aout3, show.graph=T, info.tol=1e-12)

aout3$coefficients

aout<- aout3

#save(aout, file="C:/Users/Mason Kulbaba/Dropbox/git/geum-aster/landscape figures/2017/aout.RData")





#extract two coeff

a1 <- aout$coefficients["PC2"]
a2 <- aout$coefficients["flw.no2017"]
a <- c(a1, a2)

A11 <- aout$coefficients["I(PC2^2)"]
A22 <- aout$coefficients["I(flw.no2017^2)"]
A12 <- aout$coefficients["I(2 * PC2 * flw.no2017)"]
A <- matrix(c(A11, A12, A12, A22), 2, 2)

eigen(A, symmetric = TRUE, only.values = TRUE)$values


max8 <- (-solve(A, a)/2)
print(max8)

par(mar=c(5.5, 4.5, 4.5, 8.5), xpd=TRUE)

plot(dat2$PC2, dat2$flw.no2017, xlab = "PC2", 
     ylab = "Flower number 2017", col=dat2$Region, pch=16, ylim=c(0,60))

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
contour(x, y, z, add = TRUE)
contour(x, y, z, levels = c(0.325), add = TRUE)
