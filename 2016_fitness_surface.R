
# Begin working with Fitness Surface Work. Starting with 2016 Data


setwd("C:/Users/mkulbaba/Dropbox/git/geum-aster")

#load data
#dat<- read.csv("NV_CG_Experiment2.csv")

dat<- read.csv("cleaned_data_for_aster_with_predictors.csv")



#subset data for 2016 analysis
dat2<- dat[c( "Family.Unique",   "Block.ID", "HabitatType","Dist.from.cg.km", "No.Days.to.Germ",
             "Region", "Population", "Germination.Y.N","Survival.Y.N", "Flower.Y.N.2016",
             "No.Flowers.2016", "Fruit.Y.N.2016", "No.Fruit.2016", "sm")]




#write.table(dat2, file="C:/Users/Mason Kulbaba/Dropbox/git/geum-aster/landscape figures/dat2.csv", sep=",", quote=FALSE, row.names = FALSE)

#remove rows with NAs in "days to germ." variable

dat3<- subset(dat2, No.Days.to.Germ >0)

dat3<- subset(dat3, Germination.Y.N==1 | Germination.Y.N==0)

dat2<-dat3

#load pca data

pca<- read.csv("pca.csv")


#merge PCA data into dat2

dat4<- merge(dat2, pca)

dat2<-dat4

#add flower number in 2016 with different name than in 'vars'

dat2$flw.no2016<- dat2$No.Flowers.2016

######################################################################
#
# Dec. 15 - relative seed mass

#obtain seed mass for each region
mean.sm<- aggregate(dat2$sm,by=list(dat2$Region), mean)

#make rel.sm

p<- subset(dat2, dat2$Region=="Prairie")
gl<- subset(dat2, dat2$Region=="GL_alvar")
mb<- subset(dat2, dat2$Region=="MB_alvar")

p$rel.sm<- p$sm 
gl$rel.sm<- gl$sm
mb$rel.sm<- mb$sm

p$rel.sm<- p$rel.sm/0.1077441
gl$rel.sm<- gl$rel.sm/2.8985006
mb$rel.sm<- mb$rel.sm/0.5303867

dat4<- rbind(p, gl, mb)

dat2<- dat4

#set response variables -> these represent variables in graphical model
vars<- c("Germination.Y.N","Survival.Y.N", "Flower.Y.N.2016", "No.Flowers.2016", "No.Fruit.2016", "rel.sm")


#reshape data so that all response variables are located in a single vector in a new data
#set called "redata"
redata2016 <- reshape(dat2, varying = list(vars), direction = "long",timevar = "varb", times = as.factor(vars), v.names = "resp")

#write.csv(redata2016, file="redata2016.csv", row.names = FALSE, quote = FALSE)

#Designation of fitness variable for 2016 data
fit <- grepl("rel.sm", as.character(redata2016$varb))
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
fl.2<- fitdistr(flwno, "negative binomial")#size: 0.156957321   
fl.3<- fitdistr(flwno, "poisson")

AIC(fl.1, fl.2, fl.3)
fl.2


frt.1<- fitdistr(frtno, "normal")
frt.2<- fitdistr(frtno, "negative binomial")#size:  0.045627385   
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
               fam.negative.binomial(0.156957321), 
               fam.negative.binomial(0.045627385),
               fam.negative.binomial(0.009421762))

#set graphical mode and dist. for fitness nodes
pred<- c(0,1,2,3,4,5)
fam<- c(1,1,1,2,3,4) 

#describe dist. of preds.
sapply(fam.default(), as.character)[fam]

#fixed effect model for 2016 with only fitness variable: this is the "basic" model
# that we compare with later models, to determine significance of facotrs (e.g. Habitat Type, etc.)


#add distance from seed source
aout1<- aster(resp~varb + fit:Region, pred, fam, varb, id, root, data=redata2016, famlist = famlist)

summary(aout1, show.graph=T)

aout2<- aster(resp~varb + fit:Region + No.Days.to.Germ + I(No.Days.to.Germ^2), pred, fam, varb, id, root, data=redata2016, famlist = famlist)

summary(aout2, show.graph=T, info.tol=1e-11)

aout3<- aster(resp~varb + fit:Region + PC1 + No.Days.to.Germ + I(PC1^2) + I(No.Days.to.Germ^2) + I(2*PC1*No.Days.to.Germ), pred, fam, varb, id, root, 
              data=redata2016, famlist = famlist)

summary(aout3, show.graph=T, info.tol=1e-13)


anova(aout1, aout2, aout3)


aout3$coefficients

aout4<- aster(resp~varb +PC1 + PC2 + I(PC1^2) + I(PC2^2) + I(2*PC1*PC2), pred, fam, varb, id, root, 
              data=redata2016, famlist = famlist)

summary(aout4, show.graph=T, info.tol=1e-13)

aout<- aout3

#save(aout, file="C:/Users/Mason Kulbaba/Dropbox/git/geum-aster/landscape figures/aout2016_landscap.RData")


######################################################################
# Estimate Selection Gradient (distance from source)

pout <- predict(aout)
pout <- matrix(pout, nrow = nrow(aout1$x), ncol = ncol(aout1$x))
colnames(pout) <- colnames(aout3$x)
mufit <- pout[, grep("sm", colnames(pout))]


#only needed when >1 year of data
#mufit <- apply(mufit, 1, "sum")

#calcualte mean fitness
wmu <- mufit/mean(mufit)

#perform linear analysis
wmout <- lm(wmu ~ dat2$PC1)

wmout2 <- lm(wmu ~ dat2$PC2)

summary(wmout)

summary(wmout2)

pre_w<- predict(wmout)




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
contour(x, y, z, add = TRUE)
contour(x, y, z, levels = c(0.325), add = TRUE)

####################################################################################



# PC1 & PC2 analysis plot
aout<- aout4

#save(aout, file="C:/Users/Mason Kulbaba/Dropbox/git/geum-aster/landscape figures/aout2016_landscap.RData")


######################################################################
# Estimate Selection Gradient (distance from source)

pout <- predict(aout)
pout <- matrix(pout, nrow = nrow(aout$x), ncol = ncol(aout$x))
colnames(pout) <- colnames(aout$x)
mufit <- pout[, grep("sm", colnames(pout))]


#only needed when >1 year of data
#mufit <- apply(mufit, 1, "sum")

#calcualte mean fitness
wmu <- mufit/mean(mufit)

#perform linear analysis
wmout <- lm(wmu ~ dat2$PC1)

wmout2 <- lm(wmu ~ dat2$PC2)

summary(wmout)

summary(wmout2)

pre_w<- predict(wmout)




#Now try with two predictors dist to source and days to germination

#see new aout model on line 125

#extract two coeff



a1 <- aout4$coefficients["PC1"]
a2 <- aout4$coefficients["PC2"]
a <- c(a1, a2)

A11 <- aout4$coefficients["I(PC1^2)"]
A22 <- aout4$coefficients["I(PC2^2)"]
A12 <- aout4$coefficients["I(2 * PC1 * PC2)"]
A <- matrix(c(A11, A12, A12, A22), 2, 2)

eigen(A, symmetric = TRUE, only.values = TRUE)$values


max8_2016 <- (-solve(A, a)/2)
print(max8_2016)

#write.table(max8_2016, file="C:/Users/Mason Kulbaba/Dropbox/git/geum-aster/landscape figures/max8_2016.csv", sep=",", quote=FALSE, row.names = FALSE)


#Plot PC1 & days to germ, on fitness contours



par(mar=c(6, 4.5, 4.5, 8.5), xpd=TRUE)

plot(dat2$PC1, dat2$PC2, xlab = "PC1", 
     ylab = "PC2", col=dat2$Region, pch=16)

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
contour(x, y, z, add = TRUE)
contour(x, y, z, levels = c(0.325), add = TRUE)


 ####################################################################
 #Now use flower number
 
 aout3<- aster(resp~varb + fit:Region + PC1 + flw.no2016 + I(PC1^2) + I(flw.no2016^2) + I(2*PC1*flw.no2016), pred, fam, varb, id, root, 
               data=redata2016, famlist = famlist)
 
 summary(aout3, show.graph=T, info.tol=1e-13)
 
 aout3$coefficients
 
 aout<- aout3
 
 
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
      ylab = "Flower number 2016", col=dat2$Region, pch=16)
 
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
 
 #######################################################################################
 #
 # Flower number and PC2
 #
 
 ####################################################################
 #Now use flower number
 
 aout3<- aster(resp~varb + fit:Region + PC2 + flw.no2016 + I(PC2^2) + I(flw.no2016^2) + I(2*PC2*flw.no2016), pred, fam, varb, id, root, 
               data=redata2016, famlist = famlist)
 
 summary(aout3, show.graph=T, info.tol=1e-13)
 
 aout3$coefficients
 
 aout<- aout3
 
 
 a1 <- aout$coefficients["PC2"]
 a2 <- aout$coefficients["flw.no2016"]
 a <- c(a1, a2)
 
 A11 <- aout$coefficients["I(PC2^2)"]
 A22 <- aout$coefficients["I(flw.no2016^2)"]
 A12 <- aout$coefficients["I(2 * PC2 * flw.no2016)"]
 A <- matrix(c(A11, A12, A12, A22), 2, 2)
 
 eigen(A, symmetric = TRUE, only.values = TRUE)$values
 
 
 max8 <- (-solve(A, a)/2)
 print(max8)
 
 
 #Plot dist from source & days to germ, on fitness contours
 
 
 par(mar=c(6, 4.5, 4.5, 8.5), xpd=TRUE)
 
 plot(dat2$PC2, dat2$flw.no2016, xlab = "PC2", 
      ylab = "Flower number 2016", col=dat2$Region, pch=16)
 
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
 