
# Begin working with Fitness Surface Work. Starting with 2016 Data


setwd("C:/Users/Mason Kulbaba/Dropbox/git/geum-aster")

#load data
#dat<- read.csv("NV_CG_Experiment2.csv")

dat<- read.csv("cleaned_data_for_aster_with_predictors.csv")


#subset data for 2016 analysis
dat2<- dat[c( "Family.Unique",   "Block.ID", "HabitatType","Dist.from.cg.km", "No.Days.to.Germ",
             "Region", "Population", "Germination.Y.N","Survival.Y.N", "Flower.Y.N.2016",
             "No.Flowers.2016", "Fruit.Y.N.2016", "No.Fruit.2016", "sm")]

#remove rows with NAs in "days to germ." variable

dat3<- subset(dat2, No.Days.to.Germ >0)

dat3<- subset(dat3, Germination.Y.N==1 | Germination.Y.N==0)

dat2<-dat3

#set response variables -> these represent variables in graphical model
vars<- c("Germination.Y.N","Survival.Y.N", "Flower.Y.N.2016", "No.Flowers.2016", "No.Fruit.2016", "sm")


#reshape data so that all response variables are located in a single vector in a new data
#set called "redata"
redata2016 <- reshape(dat3, varying = list(vars), direction = "long",timevar = "varb", times = as.factor(vars), v.names = "resp")

#write.csv(redata2016, file="redata2016.csv", row.names = FALSE, quote = FALSE)

#Designation of fitness variable for 2016 data
fit <- grepl("sm", as.character(redata2016$varb))
fit<- as.numeric(fit)

redata2016$fit <- fit

#check
with(redata2016, sort(unique(as.character(varb)[fit == 0])))
with(redata2016, sort(unique(as.character(varb)[fit == 1])))


#add a variable "root" to redata files, where value is 1
redata2016<- data.frame(redata2016, root=1)


#check class of redata columns

sapply(redata2016, class)

#make block.id a factor

redata2016$Block.ID<- as.factor(redata2016$Block.ID)

###############################################################
#look into distributions for nodes

#recall nodes of graphical model
vars

#Germ, Survival, Flower.Y.N, all bernoulli

flwno<- dat2$No.Flowers.2016

frtno<- dat2$No.Fruit.2016

sm<- dat2$sm


library(MASS)

fl.1<- fitdistr(flwno, "normal")
fl.2<- fitdistr(flwno, "negative binomial")#size: 0.165422825
fl.3<- fitdistr(flwno, "poisson")

AIC(fl.1, fl.2, fl.3)
fl.2


frt.1<- fitdistr(frtno, "normal")
frt.2<- fitdistr(frtno, "negative binomial")#size:  0.04617859
frt.3<- fitdistr(frtno, "poisson")

AIC(frt.1, frt.2, frt.3)

frt.2

sm.1<- fitdistr(sm, "normal")
sm.2<- fitdistr(sm, "negative binomial")#size: 0.009421762 
sm.3<- fitdistr(sm, "poisson")

AIC(sm.1, sm.2, sm.3)

sm.2

#####################################
# Overall Model work

#load aster package
library(aster)

#set family list

famlist<- list(fam.bernoulli(), 
               fam.negative.binomial(0.165422825), 
               fam.negative.binomial(0.04617859),
               fam.negative.binomial(0.009421762))

#set graphical mode and dist. for fitness nodes
pred<- c(0,1,2,3,4,5)
fam<- c(1,1,1,2,3,4) 

#describe dist. of preds.
sapply(fam.default(), as.character)[fam]

#fixed effect model for 2016 with only fitness variable: this is the "basic" model
# that we compare with later models, to determine significance of facotrs (e.g. Habitat Type, etc.)


#add distance from seed source
aout1<- aster(resp~varb, pred, fam, varb, id, root, data=redata2016, famlist = famlist)

summary(aout1, show.graph=T)


aout3<- aster(resp~varb +fit:Region+Dist.from.cg.km + No.Days.to.Germ + I(Dist.from.cg.km^2) + I(No.Days.to.Germ^2) + I(2*Dist.from.cg.km*No.Days.to.Germ), pred, fam, varb, id, root, 
              data=redata2016, famlist = famlist)

summary(aout3, show.graph=T, info.tol=1e-13)

aout3$coefficients

aout<- aout3


######################################################################
# Estimate Selection Gradient (distance from source)

pout <- predict(aout)
pout <- matrix(pout, nrow = nrow(aout1$x), ncol = ncol(aout1$x))
colnames(pout) <- colnames(aout1$x)
mufit <- pout[, grep("sm", colnames(pout))]


#only needed when >1 year of data
#mufit <- apply(mufit, 1, "sum")

#calcualte mean fitness
wmu <- mufit/mean(mufit)

#perform linear analysis
wmout <- lm(wmu ~ dat2$Dist.from.cg.km)

pre_w<- predict(wmout)

summary(wmout)


#Now try with two predictors dist to source and days to germination

#see new aout model on line 125

#extract two coeff

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


#Plot dist from source & days to germ, on fitness contours

par(mar=c(5.5, 4.5, 4.5, 8.5), xpd=TRUE)

plot(dat2$Dist.from.cg.km, dat2$No.Days.to.Germ, xlab = "Dist.", 
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

####################################################################################

#Divide into region-specific data set and fit node distributions

#Start with GL_alvar

dat.gla<- subset(dat2, Region=="GL_alvar")
dat.gla<- droplevels(dat.gla)


flwno<- dat.gla$No.Flowers.2016

frtno<- dat.gla$No.Fruit.2016

sm<- dat.gla$sm


library(MASS)

fl.1<- fitdistr(flwno, "normal")
fl.2<- fitdistr(flwno, "negative binomial")#size: 0.23784984
fl.3<- fitdistr(flwno, "poisson")

AIC(fl.1, fl.2, fl.3)
fl.2


frt.1<- fitdistr(frtno, "normal")
frt.2<- fitdistr(frtno, "negative binomial")#size: 0.06728733
frt.3<- fitdistr(frtno, "poisson")

AIC(frt.1, frt.2, frt.3)

frt.2

sm.1<- fitdistr(sm, "normal")
sm.2<- fitdistr(sm, "negative binomial")#size: 0.013494317
sm.3<- fitdistr(sm, "poisson")

AIC(sm.1, sm.2, sm.3)

sm.2

#set gla famlist

famlist.gla<- list(fam.bernoulli(), 
               fam.negative.binomial(0.23784984), 
               fam.negative.binomial(0.06728733),
               fam.negative.binomial(0.013494317))

redata.gla<- subset(redata2016, Region=="GL_alvar")
redata.gla<- droplevels(redata.gla)



aout1<- aster(resp~varb, pred, fam, varb, id, root, data=redata.gla, famlist = famlist.gla)

summary(aout1, show.graph=TRUE, info.tol=1e-16)



#Add dist to seed source and Number of days to germ
aout3<- aster(resp~varb+0+Dist.from.cg.km + No.Days.to.Germ + I(Dist.from.cg.km^2) + I(No.Days.to.Germ^2) + I(2*Dist.from.cg.km*No.Days.to.Germ), pred, fam, varb, id, root, 
            maxiter=8000,data=redata.gla, famlist = famlist.gla)

summary(aout3, show.graph=T, info.tol=1e-16)


#check for coefficients of above model

aout3$coefficients

aout<- aout3


######################################################################
# Estimate Selection Gradient (distance from source)

pout <- predict(aout)
pout <- matrix(pout, nrow = nrow(aout1$x), ncol = ncol(aout1$x))
colnames(pout) <- colnames(aout1$x)
mufit <- pout[, grep("sm", colnames(pout))]


#only needed when >1 year of data
#mufit <- apply(mufit, 1, "sum")

#calcualte mean fitness
wmu <- mufit/mean(mufit)

#perform linear analysis
wmout <- lm(wmu ~ dat.gla$Dist.from.cg.km)

pre_w<- predict(wmout)

summary(wmout)


#Now try with two predictors dist to source and days to germination

#see new aout model on line 125

#extract two coeff

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


#Plot dist from source & days to germ, on fitness contours
plot(dat.gla$Dist.from.cg.km, dat.gla$No.Days.to.Germ, xlab = "Dist.", ylab = "Days to Germ")
 ufoo <- par("usr")
 nx <- 101
 ny <- 101
 z <- matrix(NA, nx, ny)
 x <- seq(ufoo[1], ufoo[2], length = nx)
 y <- seq(ufoo[3], ufoo[4], length = ny)
 points(max8[1], max8[2], pch = 19)
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
 
 
 
 
 
 #OK, cool. But let's be thorough and compare this with the Lande and Arnold (1984) way:
 
 dat.gla$relfit <- dat.gla$sm/mean(dat.gla$sm)
  lout <- lm(relfit ~ Dist.from.cg.km + No.Days.to.Germ + I(Dist.from.cg.km^2) +
                I(No.Days.to.Germ^2) + I(2*Dist.from.cg.km*No.Days.to.Germ), data = dat.gla)
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
 plot(dat.gla$Dist.from.cg.km, dat.gla$No.Days.to.Germ, xlab = "Dist.", ylab = "Days to Germ")
  ufoo <- par("usr")
  nx <- 101
  ny <- 101
  z <- matrix(NA, nx, ny)
  x <- seq(ufoo[1], ufoo[2], length = nx)
  y <- seq(ufoo[3], ufoo[4], length = ny)
  points(max8[1], max8[2], pch = 19)
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
  

######################################################################