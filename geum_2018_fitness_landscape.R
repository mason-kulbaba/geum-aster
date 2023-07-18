#Load data and run code that cleans 2016 & 2017 data.

setwd("C:/Users/Mason Kulbaba/Dropbox/git/geum-aster")

#load data
dat<- read.csv("cleaned_data_for_aster_with_predictors.csv")

#sum seed mass across all three years
dat$sm.tot<- dat$sm + dat$sm.2 + dat$sm.3


#subset data for 2017 analysis
dat2<- dat[c("Family.Unique",   "Block.ID", "HabitatType", "Region", "Population",
                          "Dist.from.cg.km","Germination.Y.N","Survival.Y.N","Surv2017", "Surv2018", 
                          "Flower.Y.N.2016","Flower.Y.N.2017","No.Flowers.2016","Total.Flowers.2017",
                          "Fruit.Y.N.2016","Fruit.Y.N.2017", "No.Fruit.2016","No.Fruit.2017",
                          "sm", "sm.2", "sm.3",  "Flowering.Y.N.2018",
             "Total.Flowers.2018", "Fruit.Y.N.2018", 
             "No.Fruit.2018","sm.tot","No.Days.to.Germ",
             "Dist.from.cg.km" )]

#load pca data

pca<- read.csv("pca.csv")


#merge PCA data into dat2

dat4<- merge(dat2, pca)

dat2<-dat4

#make redundant variable for flower number 2018

dat2$flw.no2018<- dat2$Total.Flowers.2018


#remove rows with NAs in "days to germ." variable

dat3<- subset(dat2, No.Days.to.Germ >0)

dat3<- subset(dat3, Germination.Y.N==1 | Germination.Y.N==0)

dat2<-dat3




#set response variables -> these represent variables in graphical model
#vars<- c( "Germination.Y.N","Survival.Y.N","Surv2017", "Surv2018","Flower.Y.N.2016", 
 #         "Flower.Y.N.2017", "Flowering.Y.N.2018","No.Flowers.2016","Total.Flowers.2017",
  #        "Total.Flowers.2018","No.Fruit.2016","No.Fruit.2017", "No.Fruit.2018","sm", "sm.2",
   #       "sm.3", "sm.tot")



vars<- c( "Germination.Y.N","Survival.Y.N","Surv2017", "Surv2018",
          "No.Flowers.2016","Total.Flowers.2017",
          "Total.Flowers.2018","No.Fruit.2016","No.Fruit.2017", 
          "No.Fruit.2018", "sm.tot")


#reshape data so that all response variables are located in a single vector in a new data
#set called "redata"
redata <- reshape(dat2, varying = list(vars), direction = "long",timevar = "varb", times = as.factor(vars), v.names = "resp")

#Designation of fitness variable for 2016 data
fit <- grepl("sm.tot", as.character(redata$varb))
fit<- as.numeric(fit)

redata$fit <- fit

#check
with(redata, sort(unique(as.character(varb)[fit == 0])))
with(redata, sort(unique(as.character(varb)[fit == 1])))


#add a variable "root" to redata files, where value is 1
redata<- data.frame(redata, root=1)

#check class of each variable
sapply(redata, class)

#make sure Block.ID is a factor
redata$Block.ID <- as.factor(redata$Block.ID)

##########################################

# Estimate distribtuions for data

flwno1<- dat2$No.Flowers.2016

flwno2<- dat2$Total.Flowers.2017

frt1<- dat2$No.Fruit.2016

frt2<- dat2$No.Fruit.2017

flwno<- dat2$Total.Flowers.2018

frtno<- dat2$No.Fruit.2018

sm<- dat2$sm

sm2<- dat2$sm.2

sm3<- dat2$sm.3

seeds<-dat2$sm.tot



library(MASS)

#2016 flower number
fl1.1<- fitdistr(flwno1, "normal")
fl1.2<- fitdistr(flwno1, "negative binomial")#size: 0.087611463
fl1.3<- fitdistr(flwno1, "poisson")

AIC(fl1.1, fl1.2, fl1.3)
fl1.2


#2017 flower number
fl2.1<- fitdistr(flwno2, "normal")
fl2.2<- fitdistr(flwno2, "negative binomial")#size: 0.30514117
fl2.3<- fitdistr(flwno2, "poisson")

AIC(fl2.1, fl2.2, fl2.3)
fl2.2


#flower number
fl.1<- fitdistr(flwno, "normal")
fl.2<- fitdistr(flwno, "negative binomial")#size: 0.32113855
fl.3<- fitdistr(flwno, "poisson")

AIC(fl.1, fl.2, fl.3)
fl.2


#2016 fruit number
frt1.1<- fitdistr(frt1, "normal")
frt1.2<- fitdistr(frt1, "negative binomial")#size: 0.027821465
frt1.3<- fitdistr(frt1, "poisson")

AIC(frt1.1, frt1.2, frt1.3)
frt1.2


#2017 fruit number
frt2.1<- fitdistr(frt2, "normal")
frt2.2<- fitdistr(frt2, "negative binomial")#size: 0.23720330
frt2.3<- fitdistr(frt2, "poisson")

AIC(frt2.1, frt2.2, frt2.3)
frt2.2


#2018 fruit number
frt.1<- fitdistr(frtno, "normal")
frt.2<- fitdistr(frtno, "negative binomial")#size: 0.22983979
frt.3<- fitdistr(frtno, "poisson")

AIC(frt.1, frt.2, frt.3)
frt.2


#2016 seed mass
sm.1<- fitdistr(sm, "normal")
sm.2<- fitdistr(sm, "negative binomial")#size: 0.0058024283
sm.3<- fitdistr(sm, "poisson")

AIC(sm.1, sm.2, sm.3)
sm.2


#2017 seed mass
sm2.1<- fitdistr(sm2, "normal")
sm2.2<- fitdistr(sm2, "negative binomial")#size: 0.08457787
sm2.3<- fitdistr(sm2, "poisson")

AIC(sm2.1, sm2.2, sm2.3)
sm2.2


#2018 seed mass
sm3.1<- fitdistr(sm3, "normal")
sm3.2<- fitdistr(sm3, "negative binomial")#size: 6.633839e-02
sm3.3<- fitdistr(sm3, "poisson")

AIC(sm3.1, sm3.2, sm3.3)
sm3.2

#total seed mass
seeds.1<- fitdistr(seeds, "normal")
seeds.2<- fitdistr(seeds, "negative binomial")#size: 9.216314e-02
seeds.3<- fitdistr(seeds, "poisson")

AIC(seeds.1, seeds.2, seeds.3)
seeds.2

#NOTE: sm, sm.2 size estimates for neg.binomial dist from code: geum_aster_2017b.R

#load aster package
library(aster)


#Note: right below is "full" graphical model, that we could not avoid direction of
#       recession/constancy. Therefore, Flowering.Y.N (2016 - 2018) removed, and 
#       analysis works correctly.

#Node:   1  2  3  4  5  6  7  8  9  10  11  12  13   14   15   16   17
#pred<- c(0, 1, 2, 3, 2, 3, 4, 5, 6, 7,  8,  9,  10,  11,  12,  13,   1)
#fam<-  c(1, 1, 1, 1, 1, 1, 1, 2, 3, 4,  5,  6,  7,   8,   9,   10,  11)


############################################################
#Below graphical model excludes Flowering.Y.N variables


vars<- c( "Germination.Y.N","Survival.Y.N","Surv2017", "Surv2018",
          "No.Flowers.2016","Total.Flowers.2017",
          "Total.Flowers.2018","No.Fruit.2016","No.Fruit.2017", 
          "No.Fruit.2018","sm", "sm.2", "sm.3", "sm.tot")


#reshape data so that all response variables are located in a single vector in a new data
#set called "redata"
redata <- reshape(dat2, varying = list(vars), direction = "long",timevar = "varb", times = as.factor(vars), v.names = "resp")

#Designation of fitness variable
fit <- grepl("sm.tot", as.character(redata$varb))
fit<- as.numeric(fit)

redata$fit <- fit

#check
with(redata, sort(unique(as.character(varb)[fit == 0])))
with(redata, sort(unique(as.character(varb)[fit == 1])))


#add a variable "root" to redata files, where value is 1
redata<- data.frame(redata, root=1)

#check class of each variable
sapply(redata, class)

#make sure Block.ID is a factor
redata$Block.ID <- as.factor(redata$Block.ID)



famlist <- list(fam.bernoulli(),
                #2
                fam.negative.binomial(0.087611463),#2016 flower number
                #3
                fam.negative.binomial(0.30514117),#2017 flower number
                #4
                fam.negative.binomial(0.32113855),#2018 flower number
                #5
                fam.negative.binomial(0.027821465),#2016 fruit number
                #6
                fam.negative.binomial(0.23720330), #2017 fruit number
                #7
                fam.negative.binomial(0.22983979), #2018 fruit number
                #8
                fam.negative.binomial(0.0058024283),#sm
                #9
                fam.negative.binomial(0.08457787),#sm.2
                #10
                fam.negative.binomial(6.633839e-02),#sm.3 
                #11
                fam.negative.binomial(9.216314e-02))#sm.tot

pred<- c(0,1,2,3,2,3,4,5,6,7,8,9,10,1)
fam<- c(1,1,1,1,2,3,4,5,6,7,8,9,10,11)


aout1<- aster(resp~varb, pred, fam, varb, id, root, data=redata, famlist=famlist)

summary(aout1, show.graph=TRUE,info.tol = 1e-11)


aout3<- aster(resp~varb  +fit:Region +PC1 + No.Days.to.Germ + I(PC1^2) + I(No.Days.to.Germ^2) + I(2*PC1*No.Days.to.Germ), pred, fam, varb, id, root, 
             maxiter=5000, data=redata, famlist = famlist)

summary(aout3, show.graph=T, info.tol=1e-13)

aout3$coefficients

aout<- aout3


######################################################################
# Estimate Selection Gradient (distance from source)

pout <- predict(aout1)
pout <- matrix(pout, nrow = nrow(aout$x), ncol = ncol(aout$x))
colnames(pout) <- colnames(aout1$x)
mufit <- pout[, grep("sm.tot", colnames(pout))]


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

legend("topright", inset=c(-.351,0),col=1:3, pch=16, title="Region", bty="n")


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

####################################################################
# PC1 and PC2
aout4<- aster(resp~varb + fit:Region + PC1 + PC2 + I(PC1^2) + I(PC2^2) + I(2*PC1*PC2), pred, fam, varb, id, root, 
              data=redata, famlist = famlist)

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


#plot 


par(mar=c(5.5, 4.5, 4.5, 8.5), xpd=TRUE)

plot(dat2$PC1, dat2$PC2, xlab = "PC1", 
     ylab = "PC2", col=dat2$Region, pch=16)
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


######################################################################################
# Flower Number Analsyis

aout3<- aster(resp~varb + fit:Region + PC1 + flw.no2018 + I(PC1^2) + I(flw.no2018^2) + I(2*PC1*flw.no2018), pred, fam, varb, id, root, 
              data=redata, famlist = famlist)

summary(aout3, show.graph=T, info.tol=1e-12)

aout3$coefficients

aout<- aout3

#save(aout, file="C:/Users/Mason Kulbaba/Dropbox/git/geum-aster/landscape figures/2017/aout.RData")


#extract two coeff

a1 <- aout$coefficients["PC1"]
a2 <- aout$coefficients["flw.no2018"]
a <- c(a1, a2)

A11 <- aout$coefficients["I(PC1^2)"]
A22 <- aout$coefficients["I(flw.no2018^2)"]
A12 <- aout$coefficients["I(2 * PC1 * flw.no2018)"]
A <- matrix(c(A11, A12, A12, A22), 2, 2)

eigen(A, symmetric = TRUE, only.values = TRUE)$values


max8 <- (-solve(A, a)/2)
print(max8)

par(mar=c(5.5, 4.5, 4.5, 8.5), xpd=TRUE)

plot(dat2$PC1, dat2$flw.no2018, xlab = "PC1", 
     ylab = "Flower number 2018", col=dat2$Region, pch=16, ylim=c(0,60))

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



######################################################################################
# Flower Number Analsyis with PC2

aout3<- aster(resp~varb + fit:Region + PC2 + flw.no2018 + I(PC2^2) + I(flw.no2018^2) + I(2*PC2*flw.no2018), pred, fam, varb, id, root, 
              data=redata, famlist = famlist)

summary(aout3, show.graph=T, info.tol=1e-12)

aout3$coefficients

aout<- aout3

#save(aout, file="C:/Users/Mason Kulbaba/Dropbox/git/geum-aster/landscape figures/2017/aout.RData")


#extract two coeff

a1 <- aout$coefficients["PC2"]
a2 <- aout$coefficients["flw.no2018"]
a <- c(a1, a2)

A11 <- aout$coefficients["I(PC2^2)"]
A22 <- aout$coefficients["I(flw.no2018^2)"]
A12 <- aout$coefficients["I(2 * PC2 * flw.no2018)"]
A <- matrix(c(A11, A12, A12, A22), 2, 2)

eigen(A, symmetric = TRUE, only.values = TRUE)$values


max8 <- (-solve(A, a)/2)
print(max8)

par(mar=c(5.5, 4.5, 4.5, 8.5), xpd=TRUE)

plot(dat2$PC2, dat2$flw.no2018, xlab = "PC2", 
     ylab = "Flower number 2018", col=dat2$Region, pch=16, ylim=c(0,60))

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
