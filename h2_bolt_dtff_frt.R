setwd("C:/Users/Mason Kulbaba/Dropbox/git/geum-aster/")

dat<- read.csv("C:/Users/Mason Kulbaba/Dropbox/git/geum-aster/final_data.csv")

library(MASS)

library(lme4)

#####################################
#
# Questions to resolve:

#   1. h2 / CVa for Germination.Y.N (a 0/1 variable). 
#

##################
#Begin with Germination

#subset by region
dat.gla<- subset(dat, Region=="GL_alvar")
dat.mba<- subset(dat, Region=="MB_alvar")
dat.pr<- subset(dat, Region=="Prairie")

#black as factor
dat.gla$Block.ID<- as.factor(dat.gla$Block.ID)
dat.mba$Block.ID<- as.factor(dat.mba$Block.ID)
dat.pr$Block.ID<- as.factor(dat.pr$Block.ID)

#GLA

gla_germ<- glmer(Germination.Y.N ~ Block.ID +Population + (1|Family.Unique)  
                 + Block.ID : (1|Family.Unique), family='binomial',
                 data=dat.gla, 
                 control=glmerControl(check.conv.grad     = .makeCC("warning", tol = 1e-1, relTol = NULL),
                optCtrl=list(maxfun=2e5)))

summary(gla_germ)

#extract variance components
print(VarCorr(gla_germ), comp=c("Variance", "Std.Dev."))

#calcualte residual variance
var(resid(gla_germ))

h2<- (0.50553)/ (0.50553 + 0.63713  + 0.8189673)
h2 #0.258

CVa<- sqrt(0.50553)/mean(dat.gla$Germination.Y.N)
CVa #0.959


#calculate standard error from standard deviation (sd / sqrt(n))
se<- 0.71101 / sqrt(nrow(dat.gla))
se # 0.019



#MBA
mba_germ<- glmer(Germination.Y.N ~ Block.ID +Population + (1|Family.Unique)  
                + Block.ID : (1|Family.Unique), family='binomial',
                data=dat.mba,
                control=glmerControl(nAGQ = 1,optCtrl=list(maxfun=2e5)))
summary(mba_germ)

print(VarCorr(mba_germ), comp=c("Variance", "Std.Dev."))


#calcualte residual variance
var(resid(mba_germ))

h2<- (0.59219)/ (0.59219 + 1.00430 + 0.8084166)
h2 #0.246

CVa<- sqrt(0.59219)/mean(dat.mba$Germination.Y.N)
CVa #1.021777

#calculate standard error from standard deviation (sd / sqrt(n))
se<- 0.76954 / sqrt(nrow(dat.mba))
se # 0.049

#PRA
pr_germ<- glmer(Germination.Y.N ~ Block.ID +Population + (1|Family.Unique)  
                + Block.ID : (1|Family.Unique), family='binomial',
                data=dat.pr,
                control=glmerControl(check.conv.grad     = .makeCC("warning", tol = 1, relTol = NULL),
                                     optCtrl=list(maxfun=2e5)))
isSingular(pr_germ)

summary(pr_germ)

print(VarCorr(pr_germ), comp=c("Variance", "Std.Dev."))

#calcualte residual variance
var(resid(pr_germ))

h2<- ( 1.1641  )/ ( 1.1641   + 2.0214 + 0.4846457)
h2 #0.317

CVa<- sqrt( 1.1641  )/mean(dat.pr$Germination.Y.N)
CVa #2.860


#calculate standard error from standard deviation (sd / sqrt(n))
se<- 1.0790 / sqrt(nrow(dat.pr))
se # 0.038


# Below measures require germination, so use only plants that germinated
dat2<- subset(dat, No.Flowers.2016 > 0)

dat2$tot.seeds<- dat2$sm + dat2$sm.2 + dat2$sm.3

dat.gla<- subset(dat2, Region=="GL_alvar")
dat.mba<- subset(dat2, Region=="MB_alvar")
dat.pr<- subset(dat2, Region=="Prairie")



#make block a factor

dat.gla$Block.ID<- as.factor(dat.gla$Block.ID)
dat.mba$Block.ID<- as.factor(dat.mba$Block.ID)
dat.pr$Block.ID<- as.factor(dat.pr$Block.ID)



##########################################################
# Number of Flowers 2016: GL, MB, and prairie

#models for : GL_alvar
gla_flw2016<- glmer.nb(No.Flowers.2016 ~ Block.ID +Population + (1|Family.Unique)  
                     + Block.ID : (1|Family.Unique),
                     data=dat.gla)

isSingular(gla_flw2016)

summary(gla_flw2016)


print(VarCorr(gla_flw2016), comp=c("Variance", "Std.Dev."))

#calculate variance of residuals (i.e. residual variance component)
var(resid(gla_flw2016))

h2<- (2.5 * 0.037760)/ (0.037760 + 0.018973  + 0.9305964)
h2 #0.196

dat2.gla<-subset(dat.gla, No.Flowers.2016 >0)
CVa<- sqrt(2.5*0.037760)/mean(dat.gla$No.Flowers.2016)
CVa #0.034

#calculate standard error from standard deviation (sd / sqrt(n))
se<- 0.19432  / sqrt(nrow(dat.gla))
se # 0.009


#models for : MB_alvar
#
# 180 mba plants flowered
#
mba_flw2016<- glmer.nb(No.Flowers.2016 ~ Block.ID + Population + (1| Family.Unique)
                    + Block.ID:(1|Family.Unique),
                  data=dat.mba)


isSingular(mba_flw2016)

summary(mba_flw2016)

#calculate variance of residuals (i.e. residual variance component)
var(resid(mba_flw2016))

print(VarCorr(mba_flw2016), comp=c("Variance", "Std.Dev."))

h2<- (0.23985)/ (0.23985 + 0.26323  + 0.450509)
h2 #0.251

CVa<- sqrt(2.5*0.23985)/mean(dat.mba$No.Flowers.2016)
CVa #0.176

#calculate standard error from standard deviation (sd / sqrt(n))
se<- 0.48975  / sqrt(nrow(dat.mba))
se # 0.083



nor<- fitdistr(dat.pr$No.Flowers.2016, "normal")
poi<- fitdistr(dat.pr$No.Flowers.2016, "poisson")
nb<- fitdistr(dat.pr$No.Flowers.2016, "negative binomial")


AIC(nor, poi, nb)

#models for germination: Prairie
pr_flw2016<- glmer.nb(No.Flowers.2016 ~Block.ID +Population+ (1|Family.Unique)  
                    + Block.ID : (1|Family.Unique),
                    data=dat.pr,
                    control=glmerControl(check.conv.grad     = .makeCC("warning", tol = 1e-1, relTol = NULL),
                                         optCtrl=list(maxfun=2e5)))
 isSingular(pr_flw2016)#TRUE  therefore =0               

summary(pr_flw2016)

#calculate variance of residuals (i.e. residual variance component)
var(resid(pr_flw2016))


print(VarCorr(pr_flw2016), comp=c("Variance", "Std.Dev."))

h2<- (2.5*3.8360e-13 )/ (3.8360e-13  + 1.0407e-14 + 0.8406185)
h2 #0.000

CVa<- sqrt(2.5*3.8360e-13)/mean(dat.pr$No.Flowers.2016)
CVa #0.00

#calculate standard error from standard deviation (sd / sqrt(n))
se<- 6.1935e-07  / sqrt(nrow(dat.pr))
se # 0.00


###################################################################################
# Number of flowers in 2017


#following reqire  survival to 2017 
dat2<- subset(dat, No.Flowers.2017 > 0)

dat2$tot.seeds<- dat2$sm + dat2$sm.2 + dat2$sm.3

dat.gla<- subset(dat2, Region=="GL_alvar")
dat.mba<- subset(dat2, Region=="MB_alvar")
dat.pr<- subset(dat2, Region=="Prairie")

#make block a factor

dat.gla$Block.ID<- as.factor(dat.gla$Block.ID)
dat.mba$Block.ID<- as.factor(dat.mba$Block.ID)
dat.pr$Block.ID<- as.factor(dat.pr$Block.ID)



# GL_alvar


gla_flw2017<- glmer.nb(Total.Flowers.2017 ~ Block.ID + Population+ (1|Family.Unique)  
                     + Block.ID : (1|Family.Unique), tol=1e-2,
                     data=dat.gla,
                     
                     control=glmerControl(optimizer="Nelder_Mead",
                                          optCtrl=list(maxfun=2e5)))

isSingular(gla_flw2017)#False

summary(gla_flw2017)

print(VarCorr(gla_flw2017), comp=c("Variance", "Std.Dev."))

#calculate variance of residuals (i.e. residual variance component)
var(resid(gla_flw2017))

h2<- (2.5*0.017522)/ (0.017522 + 0.019139  + 1.029903)
h2 #0.041

CVa<- sqrt(2.5*0.017522)/mean(dat.gla$Total.Flowers.2017)
CVa #0.014

#calculate standard error from standard deviation (sd / sqrt(n))
se<- 0.13237   / sqrt(nrow(dat.gla))
se # 0.006


#models for : MB_alvar
mba_flw2017<- glmer.nb(Total.Flowers.2017 ~ Block.ID +Population+ (1|Family.Unique)  
                     + Block.ID : (1|Family.Unique),
                     data=dat.mba)

summary(mba_flw2017)

print(VarCorr(mba_flw2017), comp=c("Variance", "Std.Dev."))

#calculate variance of residuals (i.e. residual variance component)
var(resid(mba_flw2017))

h2<- (2.5*0.29371)/ (0.29371 + 0.25130 + 2.5207798)
h2 #0.240

CVa<- sqrt(2.5*0.29371)/mean(dat.mba$Total.Flowers.2017)
CVa #0.115

#calculate standard error from standard deviation (sd / sqrt(n))
se<- 0.54195  / sqrt(nrow(dat.mba))
se # 0.091



#models for germination: Prairie
pr_flw2017<- glmer.nb(Total.Flowers.2017 ~Block.ID +Population+ (1|Family.Unique)  
                    + Block.ID : (1|Family.Unique),
                    data=dat.pr, tol=1e-1,
                    
                    control=glmerControl(optimizer="Nelder_Mead",
                                         optCtrl=list(maxfun=2e5)))

isSingular(pr_flw2017)#False

summary(pr_flw2017)

print(VarCorr(pr_flw2017), comp=c("Variance", "Std.Dev."))

#calculate variance of residuals (i.e. residual variance component)
var(resid(pr_flw2017))

h2<- (2.5*5.7529e-08)/ (5.7529e-08 + 3.6263e-07 + 1.069585)
h2 #0.000

CVa<- sqrt(2.5*5.7529e-08)/mean(dat.pr$Total.Flowers.2017)
CVa #0.000

#calculate standard error from standard deviation (sd / sqrt(n))
se<- 0.00023985  / sqrt(nrow(dat.pr))
se # 0.000

###################################################################################
# Number of flowers in 2018


#following reqire  survival to 2018
dat2<- subset(dat, Total.Flowers.2018 > 0)

dat2$tot.seeds<- dat2$sm + dat2$sm.2 + dat2$sm.3

dat.gla<- subset(dat2, Region=="GL_alvar")
dat.mba<- subset(dat2, Region=="MB_alvar")
dat.pr<- subset(dat2, Region=="Prairie")

#make block a factor

dat.gla$Block.ID<- as.factor(dat.gla$Block.ID)
dat.mba$Block.ID<- as.factor(dat.mba$Block.ID)
dat.pr$Block.ID<- as.factor(dat.pr$Block.ID)



# GL_alvar # false positive of failure to converge - Var estimates are identical
# with different opimizers

gla_flw2018<- glmer.nb(Total.Flowers.2018 ~ Block.ID +Population+ (1|Family.Unique)  
                       + Block.ID : (1|Family.Unique),
                       data=dat.gla, tol=1e-1,
                       
                       control=glmerControl(optimizer="Nelder_Mead",
                                            optCtrl=list(maxfun=2e5)))
isSingular(gla_flw2018)

summary(gla_flw2018)

print(VarCorr(gla_flw2018), comp=c("Variance", "Std.Dev."))

#calculate variance of residuals (i.e. residual variance component)
var(resid(gla_flw2018))

h2<- (2.5*0.035914)/ (0.035914 + 0.010768 + 0.9159448)
h2 #0.093

CVa<- sqrt(2.5*0.035914)/mean(dat.gla$Total.Flowers.2018)
CVa #0.053

#calculate standard error from standard deviation (sd / sqrt(n))
se<- 0.18951 / sqrt(nrow(dat.gla))
se # 0.007


#models for : MB_alvar
mba_flw2018<- glmer.nb(Total.Flowers.2018 ~ Block.ID +Population+ (1|Family.Unique)  
                   + Block.ID : (1|Family.Unique),
                   data=dat.mba, tol=1e-1,
                   
                   control=glmerControl(optimizer="Nelder_Mead",
                                        optCtrl=list(maxfun=2e5)))
isSingular(mba_flw2018)#False

summary(mba_flw2018)

print(VarCorr(mba_flw2018), comp=c("Variance", "Std.Dev."))

#calculate variance of residuals (i.e. residual variance component)
var(resid(mba_flw2018))

h2<- (2.5*0.054705)/ (0.054705 + 0.018412 + 0.7758769)
h2 #0.161

CVa<- sqrt(2.5 * 0.054705)/mean(dat.mba$Total.Flowers.2018)
CVa #0.126

#calculate standard error from standard deviation (sd / sqrt(n))
se<- 0.23389   / sqrt(nrow(dat.mba))
se # 0.021



#models for: Prairie
pr_flw2018<- glmer.nb(Total.Flowers.2018 ~Block.ID +Population+ (1|Family.Unique)  
                  + Block.ID : (1|Family.Unique),
                  data=dat.pr, tol=1e-1,
                  
                  control=glmerControl(optimizer="Nelder_Mead",
                                       optCtrl=list(maxfun=2e5)))

isSingular(pr_flw2018)#False

summary(pr_flw2018)

print(VarCorr(pr_flw2018), comp=c("Variance", "Std.Dev."))

#calculate variance of residuals (i.e. residual variance component)
var(resid(pr_flw2018))

h2<- (2.5*0.00076322)/ (0.00076322 + 0.00119966 + 0.8699691)
h2 #002

CVa<- sqrt(2.5*0.00076322)/mean(dat.pr$Total.Flowers.2018)
CVa #0.011

#calculate standard error from standard deviation (sd / sqrt(n))
se<- 0.027626  / sqrt(nrow(dat.pr))
se # 0.002

##########################################################################
# Number of Fruit 2016

#for fruit set, need individuals that at least flowered 
dat2<- subset(dat, No.Fruit.2016 > 0)

dat2$tot.seeds<- dat2$sm + dat2$sm.2 + dat2$sm.3

dat.gla<- subset(dat2, Region=="GL_alvar")
dat.mba<- subset(dat2, Region=="MB_alvar")
dat.pr<- subset(dat2, Region=="Prairie")

#make block a factor

dat.gla$Block.ID<- as.factor(dat.gla$Block.ID)
dat.mba$Block.ID<- as.factor(dat.mba$Block.ID)
dat.pr$Block.ID<- as.factor(dat.pr$Block.ID)


#models for : GL_alvar
gla_frt2016<- glmer.nb(No.Fruit.2016 ~ Block.ID + Population+ (1|Family.Unique)  
                       + Block.ID : (1|Family.Unique),
                       data=dat.gla, tol=1e-2,
                       control=glmerControl(optimizer="Nelder_Mead",optCtrl=list(maxfun=1e5)))

isSingular(gla_frt2016)

summary(gla_frt2016)

print(VarCorr(gla_frt2016), comp=c("Variance", "Std.Dev."))

#calculate variance of residuals (i.e. residual variance component)
var(resid(gla_frt2016))

h2<- (2.5*0.023463 )/ (0.023463  + 0.070622 + 0.6308709)
h2 #0.081

CVa<- sqrt(2.5*0.023463 )/mean(dat.gla$No.Fruit.2016)
CVa #0.099

#calculate standard error from standard deviation (sd / sqrt(n))
se<- 0.15318   / sqrt(nrow(dat.gla))
se # 0.016


#models for : MB_alvar: only 2 with >0 fruits
mba_frt2016<- glmer.nb(No.Fruit.2016 ~ Block.ID +Population+ (1|Family.Unique)  
                       + Block.ID : (1|Family.Unique),
                       data=dat.mba)

summary(mba_frt2016)

print(VarCorr(mba_frt2016), comp=c("Variance", "Std.Dev."))

#calculate variance of residuals (i.e. residual variance component)
var(resid(mba_frt2016))

h2<- NA
h2 #na

CVa<- sqrt(na)/mean(dat.mba$No.Fruit.2016)
CVa #na

#calculate standard error from standard deviation (sd / sqrt(n))
se<- na  / sqrt(nrow(dat.mba))
se # na



#models for: Prairie on 2 with >0 fruits
pr_frt2016<- glmer.nb(No.Fruit.2016 ~Block.ID +Population+ (1|Family.Unique)  
                      + Block.ID : (1|Family.Unique),
                      data=dat.pr)

summary(pr_frt2016)

print(VarCorr(pr_frt2016), comp=c("Variance", "Std.Dev."))

#calculate variance of residuals (i.e. residual variance component)
var(resid(pr_frt2016))

h2<- #NA
h2 #000

CVa<- sqrt(5.4564e-08)/mean(dat.pr$No.Fruit.2016)
CVa #0.000

#calculate standard error from standard deviation (sd / sqrt(n))
se<- 0.00023359  / sqrt(nrow(dat.pr))
se # 0


##########################################################################
# Number of Fruit 2017

#for fruit set, need individuals that at least flowered 
dat2<- subset(dat, No.Fruit.2017 > 0)

dat2$tot.seeds<- dat2$sm + dat2$sm.2 + dat2$sm.3

dat.gla<- subset(dat2, Region=="GL_alvar")
dat.mba<- subset(dat2, Region=="MB_alvar")
dat.pr<- subset(dat2, Region=="Prairie")

#make block a factor

dat.gla$Block.ID<- as.factor(dat.gla$Block.ID)
dat.mba$Block.ID<- as.factor(dat.mba$Block.ID)
dat.pr$Block.ID<- as.factor(dat.pr$Block.ID)


#models for : GL_alvar
gla_frt2017<- glmer.nb(No.Fruit.2017 ~ Block.ID +Population+ (1|Family.Unique)  
                     + Block.ID : (1|Family.Unique),
                     data=dat.gla)

isSingular(gla_frt2017)#False

summary(gla_frt2017)

print(VarCorr(gla_frt2017), comp=c("Variance", "Std.Dev."))

#calculate variance of residuals (i.e. residual variance component)
var(resid(gla_frt2017))

h2<- (2.5*0.025367)/ (0.025367 + 0.013596 + 0.9613593)
h2 #0.063

CVa<- sqrt(2.5*0.025367)/mean(dat.gla$No.Fruit.2017)
CVa #0.029

#calculate standard error from standard deviation (sd / sqrt(n))
se<- 0.15927   / sqrt(nrow(dat.gla))
se # 0.006



#models for germination: MB_alvar
mba_frt2017<- glmer.nb(No.Fruit.2017 ~ Block.ID +Population+ (1|Family.Unique)  
                     + Block.ID : (1|Family.Unique),
                     data=dat.mba,  tol=1e-2,
                     control=glmerControl(optimizer="Nelder_Mead",
                                          optCtrl=list(maxfun=1e5)))


isSingular(mba_frt2017)#False

summary(mba_frt2017)

print(VarCorr(mba_frt2017), comp=c("Variance", "Std.Dev."))

#calculate variance of residuals (i.e. residual variance component)
var(resid(mba_frt2017))

h2<- (2.5*0.00023116)/ (0.00023116 + 0.00063778 +  0.9267133)
h2 #000

CVa<- sqrt(2.5*0.00023116)/mean(dat.mba$No.Fruit.2017)
CVa #0.006

#calculate standard error from standard deviation (sd / sqrt(n))
se<- 0.015204  / sqrt(nrow(dat.mba))
se # 0.002


#models for Prairie
pr_frt2017<- glmer.nb(No.Fruit.2017 ~ Block.ID +Population+ (1|Family.Unique)  
                    + Block.ID : (1|Family.Unique),
                    data=dat.pr,  tol=1e-1,
                    control=glmerControl(optimizer="Nelder_Mead",
                                         optCtrl=list(maxfun=1e5)))

isSingular(pr_frt2017)#False

summary(pr_frt2017)

print(VarCorr(pr_frt2017), comp=c("Variance", "Std.Dev."))

#calculate variance of residuals (i.e. residual variance component)
var(resid(pr_frt2017))

h2<- (2.5*0.0107414)/ (0.0107414 + 0.0053198 + 0.9142678)
h2 #0.029

CVa<- sqrt(2.5*0.0107414)/mean(dat.pr$No.Fruit.2017)
CVa #0.038

#calculate standard error from standard deviation (sd / sqrt(n))
se<- 0.103641  / sqrt(nrow(dat.pr))
se # 0.008




##########################################################################
# Number of Fruit 2018

#for fruit set, need individuals that at least flowered 
dat2<- subset(dat, No.Fruit.2018 > 0)

dat2$tot.seeds<- dat2$sm + dat2$sm.2 + dat2$sm.3

dat.gla<- subset(dat2, Region=="GL_alvar")
dat.mba<- subset(dat2, Region=="MB_alvar")
dat.pr<- subset(dat2, Region=="Prairie")

#make block a factor

dat.gla$Block.ID<- as.factor(dat.gla$Block.ID)
dat.mba$Block.ID<- as.factor(dat.mba$Block.ID)
dat.pr$Block.ID<- as.factor(dat.pr$Block.ID)

#models for : GL_alvar
gla_frt2018<- glmer.nb(No.Fruit.2018 ~ Block.ID +Population+ (1|Family.Unique)  
                     + Block.ID : (1|Family.Unique),
                     data=dat.gla,  tol=1e-1,
                     control=glmerControl(optimizer="Nelder_Mead",
                                          optCtrl=list(maxfun=1e5)))

isSingular(gla_frt2018)#False

summary(gla_frt2018)

#calculate variance of residuals (i.e. residual variance component)
var(resid(gla_frt2018))

print(VarCorr(gla_frt2018), comp=c("Variance", "Std.Dev."))

h2<- (2.5*0.0105278)/ (0.0105278 + 0.0045763 + 0.9333743)
h2 # 0.028

CVa<- sqrt(2.5*0.0105278)/mean(dat.gla$No.Fruit.2018)
CVa #0.032

#calculate standard error from standard deviation (sd / sqrt(n))
se<- 0.102605  / sqrt(nrow(dat.gla))
se # 0.004

####
#models for germination: MB_alvar
mba_frt2018<- glmer.nb(No.Fruit.2018 ~ Block.ID +Population+ (1|Family.Unique)  
                     + Block.ID : (1|Family.Unique),
                     data=dat.mba,  tol=1e-1,
                     control=glmerControl(optimizer="Nelder_Mead",
                                          optCtrl=list(maxfun=1e5)))

isSingular(mba_frt2018)#False

summary(mba_frt2018)

print(VarCorr(mba_frt2018), comp=c("Variance", "Std.Dev."))

var(resid(mba_frt2018))

h2<- (2.5*0.05059321)/ (0.05059321 + 0.00080764 + 0.807145)
h2 # 0.147

CVa<- sqrt(2.5*0.05059321)/mean(dat.mba$No.Fruit.2018)
CVa #0.113

#calculate standard error from standard deviation (sd / sqrt(n))
se<- 0.224929  / sqrt(nrow(dat.mba))
se # 0.025

#models for germination: Prairie
pr_frt2018<- glmer.nb(No.Fruit.2018 ~ Block.ID +Population+ (1|Family.Unique)  
                    + Block.ID : (1|Family.Unique),
                    data=dat.pr,  tol=1e-2,
                  control=glmerControl(optimizer="bobyqa",
                                       optCtrl=list(maxfun=1e5)))

isSingular(pr_frt2018)#True -  approx. to 0

summary(pr_frt2018)

print(VarCorr(pr_frt2018), comp=c("Variance", "Std.Dev."))

var(resid(pr_frt2018))

h2<- (2.5*2.4078e-15)/ (2.4078e-15 + 2.2828e-15 + 0.8559392)
h2 # 0.000

#calculate standard error from standard deviation (sd / sqrt(n))
se<- 4.9069e-08  / sqrt(nrow(dat.pr))
se # 0.000

#CVa = 0


######################################################################################
######################################################################################
#
# Phenology Data: no.Planting.to.DTFF, Planting.to.DTFF.2017, Planting.to.DTFF.2018,
#                                Planting.to.DatetoFrt.2017, Planting.to.DatetoFrt.2018,
#                                        Planting.to.DTB.2017, Planting.to.DTB.2018
#
######################################################################################
######################################################################################

#no.Planting.to.DTFF


dat.gla<- subset(dat, Region=="GL_alvar")
dat.mba<- subset(dat, Region=="MB_alvar")
dat.pr<- subset(dat, Region=="Prairie")

#make block a factor

dat.gla$Block.ID<- as.factor(dat.gla$Block.ID)
dat.mba$Block.ID<- as.factor(dat.mba$Block.ID)
dat.pr$Block.ID<- as.factor(dat.pr$Block.ID)



#dist test

#remove na i.e., omit if did not germinate
dat.gla<- dat.gla[!is.na(dat.gla$no.Planting.to.DTFF), ]

nor<- fitdistr(dat.gla$no.Planting.to.DTFF, "normal")
nb<- fitdistr(dat.gla$no.Planting.to.DTFF, "negative binomial")
po<- fitdistr(dat.gla$no.Planting.to.DTFF, "poisson")
ga<- fitdistr(dat.gla$no.Planting.to.DTFF, "gamma")

AIC(nor, nb, po, ga)#nb

#models for : GL_alvar
gla_DTFF2016<- glmer.nb(no.Planting.to.DTFF ~ Block.ID +Population+ (1|Family.Unique)  
                      + Block.ID : (1|Family.Unique),
                      data=dat.gla, tol=1e-1,
                     control=glmerControl(optimizer="Nelder_Mead",
                                          optCtrl=list(maxfun=1e5)))

isSingular(gla_DTFF2016)#False

summary(gla_DTFF2016)# false pos for "fail to converge", identical estimates with diff optim

print(VarCorr(gla_DTFF2016), comp=c("Variance", "Std.Dev."))

var(resid(gla_DTFF2016))

h2<- (2.5*0.00058278)/ (0.00058278 + 0.00021499 + 0.9682206)
h2 # 0.002

CVa<- sqrt(2.5*0.00058278)/(mean(dat.gla$no.Planting.to.DTFF))
CVa #0

#calculate standard error from standard deviation (sd / sqrt(n))
se<- 0.024141  / sqrt(nrow(dat.gla))
se # 0.001


#models for germination: MB_alvar

dat.mba<- dat.mba[!is.na(dat.mba$no.Planting.to.DTFF), ]

nor<- fitdistr(dat.mba$no.Planting.to.DTFF, "normal")
nb<- fitdistr(dat.mba$no.Planting.to.DTFF, "negative binomial")
po<- fitdistr(dat.mba$no.Planting.to.DTFF, "poisson")
ga<- fitdistr(dat.mba$no.Planting.to.DTFF, "gamma")

AIC(nor, nb, po, ga)#nb

#note: only 35 individuals - is singular

mba_DTFF2016<- glmer.nb(no.Planting.to.DTFF ~ Block.ID +Population+ (1|Family.Unique)  
                      + Block.ID : (1|Family.Unique),
                      data=dat.mba, tol=1e-2,
                      control=glmerControl(optimizer="bobyqa",
                                           optCtrl=list(maxfun=1e5)))

isSingular(mba_DTFF2016)# TRUE - only 35 individuals (=0)

summary(mba_DTFF2016)

var(resid(mba_DTFF2016))

print(VarCorr(mba_DTFF2016), comp=c("Variance", "Std.Dev."))

h2<- (2.5*0)/ (0 + 0 + 1.012859)
h2 # 0.0

#calculate standard error from standard deviation (sd / sqrt(n))
se<- 0  / sqrt(nrow(dat.mba))
se # 0


#models for germination: Prairie

dat.pr<- dat.pr[!is.na(dat.pr$no.Planting.to.DTFF), ]

nor<- fitdistr(dat.pr$no.Planting.to.DTFF, "normal")
nb<- fitdistr(dat.pr$no.Planting.to.DTFF, "negative binomial")
po<- fitdistr(dat.pr$no.Planting.to.DTFF, "poisson")
ga<- fitdistr(dat.pr$no.Planting.to.DTFF, "gamma")

AIC(nor, nb, po, ga)#nb



pr_DTFF2016<- glmer.nb(no.Planting.to.DTFF ~ Block.ID +Population+ (1|Family.Unique)  
                     + Block.ID : (1|Family.Unique),
                     data=dat.pr, tol=1e-2,
                     control=glmerControl(optimizer="bobyqa",
                                          optCtrl=list(maxfun=1e5)))

isSingular(pr_DTFF2016)#TRUE - only 34 individuals (=0)

summary(pr_DTFF2016)

var(resid(pr_DTFF2016))

print(VarCorr(pr_DTFF2016), comp=c("Variance", "Std.Dev."))

h2<- (2.5*0)/ (0 + 0 + 1.02496)
h2 # 0.0

#calculate standard error from standard deviation (sd / sqrt(n))
se<- 0  / sqrt(nrow(dat.pr))
se # 0



#################################################################################
#Planting.to.DTFF.2017

dat.gla<- subset(dat, Region=="GL_alvar")
dat.mba<- subset(dat, Region=="MB_alvar")
dat.pr<- subset(dat, Region=="Prairie")


dat.gla<- dat.gla[!is.na(dat.gla$Planting.to.DTFF.2017), ]

nor<- fitdistr(dat.gla$Planting.to.DTFF.2017, "normal")
nb<- fitdistr(dat.gla$Planting.to.DTFF.2017, "negative binomial")
po<- fitdistr(dat.gla$Planting.to.DTFF.2017, "poisson")
ga<- fitdistr(dat.gla$Planting.to.DTFF.2017, "gamma")

AIC(nor, nb, po, ga)#normal


#models for : GL_alvar
gla_DTFF2017<- lmer(Planting.to.DTFF.2017 ~ Block.ID +Population+ (1|Family.Unique)  
                      + Block.ID : (1|Family.Unique),
                      data=dat.gla,control=lmerControl(
                        optimizer="bobyqa",
                                                       optCtrl=list(maxfun=1e5)))

isSingular(gla_DTFF2017)

summary(gla_DTFF2017)

print(VarCorr(gla_DTFF2017), comp=c("Variance", "Std.Dev."))

h2<- (2.5*0.0058254)/ (0.0058254 + 1.3730980 + 14.9471687)
h2 # 0.202


CVa<- sqrt(2.5*0.0058254)/mean(dat.gla$Planting.to.DTFF.2017)
CVa # 0.000

#calculate standard error from standard deviation (sd / sqrt(n))
se<- 0.076324  / sqrt(nrow(dat.gla))
se # 0.003


#models for germination: MB_alvar

dat.mba<- dat.mba[!is.na(dat.mba$Planting.to.DTFF.2017), ]

nor<- fitdistr(dat.mba$Planting.to.DTFF.2017, "normal")
nb<- fitdistr(dat.mba$Planting.to.DTFF.2017, "negative binomial")
po<- fitdistr(dat.mba$Planting.to.DTFF.2017, "poisson")
ga<- fitdistr(dat.mba$Planting.to.DTFF.2017, "gamma")

AIC(nor, nb, po, ga)



mba_DTFF2017<- lmer(Planting.to.DTFF.2017 ~ Block.ID +Population+ (1|Family.Unique)  
                      + Block.ID : (1|Family.Unique),
                      data=dat.mba)


isSingular(mba_DTFF2017)

summary(mba_DTFF2017)

print(VarCorr(mba_DTFF2017), comp=c("Variance", "Std.Dev."))

h2<- (2.5*0.46193)/ (0.46193 + 0.10652 + 31.28296)
h2 # 0.13625

CVa<- sqrt(2.5*0.46193)/mean(dat.mba$Planting.to.DTFF.2017)
CVa # 0.002

#calculate standard error from standard deviation (sd / sqrt(n))
se<- 0.67966  / sqrt(nrow(dat.mba))
se # 0.054


#models for germination: Prairie
dat.pr<- dat.pr[!is.na(dat.pr$Planting.to.DTFF.2017), ]

nor<- fitdistr(dat.pr$Planting.to.DTFF.2017, "normal")
nb<- fitdistr(dat.pr$Planting.to.DTFF.2017, "negative binomial")
po<- fitdistr(dat.pr$Planting.to.DTFF.2017, "poisson")
ga<- fitdistr(dat.pr$Planting.to.DTFF.2017, "gamma")

AIC(nor, nb, po, ga)


pr_DTFF2017<- lmer(Planting.to.DTFF.2017 ~ Block.ID +Population+ (1|Family.Unique)  
                     + Block.ID : (1|Family.Unique),
                     data=dat.pr)

summary(pr_DTFF2017)

isSingular(pr_DTFF2017)

print(VarCorr(pr_DTFF2017), comp=c("Variance", "Std.Dev."))

h2<- (2.5*0)/ (0 + 0 + 1.0501e+02)
h2 # 0

#calculate standard error from standard deviation (sd / sqrt(n))
se<- 0  / sqrt(nrow(dat.pr))
se # 0

#################################################################################
#Planting.to.DTFF.2018

dat.gla<- subset(dat, Region=="GL_alvar")
dat.mba<- subset(dat, Region=="MB_alvar")
dat.pr<- subset(dat, Region=="Prairie")


dat.gla<- dat.gla[!is.na(dat.gla$Planting.to.DTFF.2018), ]

nor<- fitdistr(dat.gla$Planting.to.DTFF.2018, "normal")
nb<- fitdistr(dat.gla$Planting.to.DTFF.2018, "negative binomial")
po<- fitdistr(dat.gla$Planting.to.DTFF.2018, "poisson")
ga<- fitdistr(dat.gla$Planting.to.DTFF.2018, "gamma")

AIC(nor, nb, po, ga)

#models for : GL_alvar
gla_DTFF2018<- lmer(Planting.to.DTFF.2018 ~ Block.ID +Population+ (1|Family.Unique)  
                      + Block.ID : (1|Family.Unique),
                      data=dat.gla)

summary(gla_DTFF2018)

print(VarCorr(gla_DTFF2018), comp=c("Variance", "Std.Dev."))

h2<- (2.5*2.73792)/ (2.73792 + 0.16825 + 31.75423)
h2 # 0.197

CVa<- sqrt(2.5*2.73792)/ mean(dat.gla$Planting.to.DTFF.2018)
CVa# 0.003

#calculate standard error from standard deviation (sd / sqrt(n))
se<- 1.65467   / sqrt(nrow(dat.gla))
se # 0.057


#models for germination: MB_alvar
dat.mba<- dat.mba[!is.na(dat.mba$Planting.to.DTFF.2018), ]

nor<- fitdistr(dat.mba$Planting.to.DTFF.2018, "normal")
nb<- fitdistr(dat.mba$Planting.to.DTFF.2018, "negative binomial")
po<- fitdistr(dat.mba$Planting.to.DTFF.2018, "poisson")
ga<- fitdistr(dat.mba$Planting.to.DTFF.2018, "gamma")

AIC(nor, nb, po, ga)


mba_DTFF2018<- lmer(Planting.to.DTFF.2018 ~  Block.ID +Population+ (1|Family.Unique)  
                      + Block.ID : (1|Family.Unique),
                      data=dat.mba)

summary(mba_DTFF2018)

print(VarCorr(mba_DTFF2018), comp=c("Variance", "Std.Dev."))

h2<- (2.5*2.2026)/ (2.2026 + 10.1286 + 52.0275)
h2 # 0.086

CVa<- sqrt(2.5*2.2026)/mean(dat.mba$Planting.to.DTFF.2018)
CVa #0.003

#calculate standard error from standard deviation (sd / sqrt(n))
se<- 1.4841  / sqrt(nrow(dat.mba))
se # 0.130


#models for germination: Prairie
dat.pr<- dat.pr[!is.na(dat.pr$Planting.to.DTFF.2018), ]

nor<- fitdistr(dat.mba$Planting.to.DTFF.2018, "normal")
nb<- fitdistr(dat.mba$Planting.to.DTFF.2018, "negative binomial")
po<- fitdistr(dat.mba$Planting.to.DTFF.2018, "poisson")
ga<- fitdistr(dat.mba$Planting.to.DTFF.2018, "gamma")

AIC(nor, nb, po, ga)

pr_DTFF2018<- lmer(Planting.to.DTFF.2018 ~ Block.ID +Population+ (1|Family.Unique)  
                     + Block.ID : (1|Family.Unique),
                     data=dat.pr)

summary(pr_DTFF2018)

isSingular(pr_DTFF2018)#TRUE therefore, =0

print(VarCorr(pr_DTFF2018), comp=c("Variance", "Std.Dev."))

h2<- (2.5*0)/ (0 + 0 + 53.662)
h2 # 0

#calculate standard error from standard deviation (sd / sqrt(n))
se<- 0  / sqrt(nrow(dat.pr))
se # 0.070



#################################################################################
#Planting.to.DatetoFrt.2017

dat.gla<- subset(dat, Region=="GL_alvar")
dat.mba<- subset(dat, Region=="MB_alvar")
dat.pr<- subset(dat, Region=="Prairie")


dat.gla<- dat.gla[!is.na(dat.gla$Planting.to.DatetoFrt.2017), ]

nor<- fitdistr(dat.gla$Planting.to.DatetoFrt.2017, "normal")
nb<- fitdistr(dat.gla$Planting.to.DatetoFrt.2017, "negative binomial")
po<- fitdistr(dat.gla$Planting.to.DatetoFrt.2017, "poisson")
ga<- fitdistr(dat.gla$Planting.to.DatetoFrt.2017, "gamma")

AIC(nor, nb, po, ga)

#models for : GL_alvar
gla_DTFrt2017<- lmer(Planting.to.DatetoFrt.2017 ~ Block.ID +Population+ (1|Family.Unique)  
                       + Block.ID : (1|Family.Unique),
                       data=dat.gla)

summary(gla_DTFrt2017)

isSingular(gla_DTFrt2017)#TRUE


print(VarCorr(gla_DTFrt2017), comp=c("Variance", "Std.Dev."))

h2<- (2.5*0)/ (0 + 0 + 43.454)
h2 # 0.000

#calculate standard error from standard deviation (sd / sqrt(n))
se<- 0  / sqrt(nrow(dat.gla))
se # 0.00



#models for germination: MB_alvar
dat.mba<- dat.mba[!is.na(dat.mba$Planting.to.DatetoFrt.2017), ]

nor<- fitdistr(dat.mba$Planting.to.DatetoFrt.2017, "normal")
nb<- fitdistr(dat.mba$Planting.to.DatetoFrt.2017, "negative binomial")
po<- fitdistr(dat.mba$Planting.to.DatetoFrt.2017, "poisson")
ga<- fitdistr(dat.mba$Planting.to.DatetoFrt.2017, "gamma")

AIC(nor, nb, po, ga)



mba_DTFrt2017<- lmer(Planting.to.DatetoFrt.2017 ~ Block.ID +Population+ (1|Family.Unique)  
                       + Block.ID : (1|Family.Unique),
                       data=dat.mba)

isSingular(mba_DTFrt2017)#false

summary(mba_DTFrt2017)

print(VarCorr(mba_DTFrt2017), comp=c("Variance", "Std.Dev."))

h2<- (2.5*0.18676)/ (0.18676 + 2.08811 + 43.34328)
h2 # 0.010

CVa<- sqrt(2.5*0.18676)/ mean(dat.mba$Planting.to.DatetoFrt.2017)
CVa # 0.001

#calculate standard error from standard deviation (sd / sqrt(n))
se<- 0.43216  / sqrt(nrow(dat.mba))
se # 0.042


#models for germination: Prairie
dat.pr<- dat.pr[!is.na(dat.pr$Planting.to.DatetoFrt.2017), ]

nor<- fitdistr(dat.pr$Planting.to.DatetoFrt.2017, "normal")
nb<- fitdistr(dat.pr$Planting.to.DatetoFrt.2017, "negative binomial")
po<- fitdistr(dat.pr$Planting.to.DatetoFrt.2017, "poisson")
ga<- fitdistr(dat.pr$Planting.to.DatetoFrt.2017, "gamma")

AIC(nor, nb, po, ga)

pr_DTFrt2017<- lmer(Planting.to.DatetoFrt.2017 ~ Block.ID +Population+ (1|Family.Unique)  
                      + Block.ID : (1|Family.Unique),
                      data=dat.pr)

isSingular(pr_DTFrt2017)#true, approx to 0

summary(pr_DTFrt2017)

print(VarCorr(pr_DTFrt2017), comp=c("Variance", "Std.Dev."))

h2<- (2.5*0)/ (0 + 0 + 61.363)
h2 # 0

#calculate standard error from standard deviation (sd / sqrt(n))
se<- 0  / sqrt(nrow(dat.pr))
se # 0


#################################################################################
#Planting.to.DatetoFrt.2018

dat.gla<- subset(dat, Region=="GL_alvar")
dat.mba<- subset(dat, Region=="MB_alvar")
dat.pr<- subset(dat, Region=="Prairie")


dat.gla<- dat.gla[!is.na(dat.gla$Planting.to.DatetoFrt.2018), ]

nor<- fitdistr(dat.gla$Planting.to.DatetoFrt.2018, "normal")
nb<- fitdistr(dat.gla$Planting.to.DatetoFrt.2018, "negative binomial")
po<- fitdistr(dat.gla$Planting.to.DatetoFrt.2018, "poisson")
ga<- fitdistr(dat.gla$Planting.to.DatetoFrt.2018, "gamma")

AIC(nor, nb, po, ga)

#models for : GL_alvar
gla_DTFrt2018<- lmer(Planting.to.DatetoFrt.2018 ~ Block.ID +Population+ (1|Family.Unique)  
                       + Block.ID : (1|Family.Unique),
                       data=dat.gla)

summary(gla_DTFrt2018)

print(VarCorr(gla_DTFrt2018), comp=c("Variance", "Std.Dev."))

h2<- (2.5*0.20540 )/ (0.20540  + 0.55267 + 7.97468)
h2 # 0.059

CVa<- sqrt(2.5*0.20540 )/ mean(dat.gla$Planting.to.DatetoFrt.2018)
CVa #0.001

#calculate standard error from standard deviation (sd / sqrt(n))
se<- 0.45321 / sqrt(nrow(dat.gla))
se # 0.018


#models for germination: MB_alvar
dat.mba<- dat.mba[!is.na(dat.mba$Planting.to.DatetoFrt.2018), ]

nor<- fitdistr(dat.mba$Planting.to.DatetoFrt.2018, "normal")
nb<- fitdistr(dat.mba$Planting.to.DatetoFrt.2018, "negative binomial")
po<- fitdistr(dat.mba$Planting.to.DatetoFrt.2018, "poisson")
ga<- fitdistr(dat.mba$Planting.to.DatetoFrt.2018, "gamma")

AIC(nor, nb, po, ga)


mba_DTFrt2018<- lmer(Planting.to.DatetoFrt.2018 ~ Block.ID +Population+ (1|Family.Unique)  
                       + Block.ID : (1|Family.Unique),
                       data=dat.mba)

summary(mba_DTFrt2018)

print(VarCorr(mba_DTFrt2018), comp=c("Variance", "Std.Dev."))

h2<- (2.5*0.056991)/ (0.056991 + 0.011481 + 10.859547)
h2 # 0.013

CVa<- sqrt(2.5*0.056991)/mean(dat.mba$Planting.to.DatetoFrt.2018)
CVa #0.0004

#calculate standard error from standard deviation (sd / sqrt(n))
se<- 0.23873 / sqrt(nrow(dat.mba))
se # 0.026


#models for germination: Prairie
dat.pr<- dat.pr[!is.na(dat.pr$Planting.to.DatetoFrt.2018), ]

nor<- fitdistr(dat.pr$Planting.to.DatetoFrt.2018, "normal")
nb<- fitdistr(dat.pr$Planting.to.DatetoFrt.2018, "negative binomial")
po<- fitdistr(dat.pr$Planting.to.DatetoFrt.2018, "poisson")
ga<- fitdistr(dat.pr$Planting.to.DatetoFrt.2018, "gamma")

AIC(nor, nb, po, ga)


pr_DTFrt2018<- lmer(Planting.to.DatetoFrt.2018 ~ Block.ID +Population+ (1|Family.Unique)  
                      + Block.ID : (1|Family.Unique),
                      data=dat.pr)

summary(pr_DTFrt2018)

print(VarCorr(pr_DTFrt2018), comp=c("Variance", "Std.Dev."))

h2<- (2.5*0.053798 )/ (0.053798  + 0.437690 + 9.471916)
h2 # 0.013

CVa<- sqrt(2.5*0.053798)/mean(dat.pr$Planting.to.DatetoFrt.2018)
CVa #0.001

#calculate standard error from standard deviation (sd / sqrt(n))
se<- 0.23194 / sqrt(nrow(dat.pr))
se # 0.021


##############################################################################
#Planting.to.DTB.2017

dat.gla<- subset(dat, Region=="GL_alvar")
dat.mba<- subset(dat, Region=="MB_alvar")
dat.pr<- subset(dat, Region=="Prairie")


dat.gla<- dat.gla[!is.na(dat.gla$Planting.to.DTB.2017), ]

nor<- fitdistr(dat.gla$Planting.to.DTB.2017, "normal")
nb<- fitdistr(dat.gla$Planting.to.DTB.2017, "negative binomial")
po<- fitdistr(dat.gla$Planting.to.DTB.2017, "poisson")
ga<- fitdistr(dat.gla$Planting.to.DTB.2017, "gamma")

AIC(nor, nb, po, ga)

#models for : GL_alvar
gla_DTB2017<- lmer(Planting.to.DTB.2017 ~ Block.ID +Population+ (1|Family.Unique)  
                     + Block.ID : (1|Family.Unique),
                     data=dat.gla, tol=1e-2,
                   control=lmerControl(optCtrl=list(maxfun=100000)))

summary(gla_DTB2017)

print(VarCorr(gla_DTB2017), comp=c("Variance", "Std.Dev."))

h2<- (2.5*1.7968)/ (1.7968 + 8.205 + 66.3385)
h2 # 0.059

CVa<- sqrt(2.5*1.7968)/mean(dat.gla$Planting.to.DTB.2017)
CVa# 0.004

#calculate standard error from standard deviation (sd / sqrt(n))
se<- 1.3404 / sqrt(nrow(dat.gla))
se # 0.045


#models for germination: MB_alvar
dat.mba<- dat.mba[!is.na(dat.mba$Planting.to.DTB.2017), ]

nor<- fitdistr(dat.mba$Planting.to.DTB.2017, "normal")
nb<- fitdistr(dat.mba$Planting.to.DTB.2017, "negative binomial")
po<- fitdistr(dat.mba$Planting.to.DTB.2017, "poisson")
ga<- fitdistr(dat.mba$Planting.to.DTB.2017, "gamma")

AIC(nor, nb, po, ga)


mba_DTB2017<- lmer(Planting.to.DTB.2017 ~ Block.ID +Population+ (1|Family.Unique)  
                     + Block.ID : (1|Family.Unique),
                     data=dat.mba,tol=1e-2,
                   control=lmerControl(optCtrl=list(maxfun=100000)))

summary(mba_DTB2017)

print(VarCorr(mba_DTB2017), comp=c("Variance", "Std.Dev."))

h2<- (2.5*0.036165)/ (0.036165 + 8.018868 + 99.342344)
h2 # 0.001

CVa<- sqrt(2.5*0.036165)/mean(dat.mba$Planting.to.DTB.2017)
CVa# 0.001

#calculate standard error from standard deviation (sd / sqrt(n))
se<- 0.19017 / sqrt(nrow(dat.mba))
se # 0.015


#models for germination: Prairie
dat.pr<- dat.pr[!is.na(dat.pr$Planting.to.DTB.2017), ]

nor<- fitdistr(dat.pr$Planting.to.DTB.2017, "normal")
nb<- fitdistr(dat.pr$Planting.to.DTB.2017, "negative binomial")
po<- fitdistr(dat.pr$Planting.to.DTB.2017, "poisson")
ga<- fitdistr(dat.pr$Planting.to.DTB.2017, "gamma")

AIC(nor, nb, po, ga)

pr_DTB2017<- lmer(Planting.to.DTB.2017 ~ Block.ID +Population+ (1|Family.Unique)  
                    + Block.ID : (1|Family.Unique),
                    data=dat.pr)

summary(pr_DTB2017)

print(VarCorr(pr_DTB2017), comp=c("Variance", "Std.Dev."))

h2<- (2.5*0.028423 )/ (0.028423  + 2.76406 + 114.352542)
h2 # 0.001

CVa<- sqrt(2.5*0.028423)/mean(dat.pr$Planting.to.DTB.2017)
CVa# 0.000

#calculate standard error from standard deviation (sd / sqrt(n))
se<- 0.16859 / sqrt(nrow(dat.pr))
se # 0.012



##############################################################################
#Planting.to.DTB.2018

dat.gla<- subset(dat, Region=="GL_alvar")
dat.mba<- subset(dat, Region=="MB_alvar")
dat.pr<- subset(dat, Region=="Prairie")


dat.gla<- dat.gla[!is.na(dat.gla$Planting.to.DTB.2018), ]

nor<- fitdistr(dat.gla$Planting.to.DTB.2018, "normal")
nb<- fitdistr(dat.gla$Planting.to.DTB.2018, "negative binomial")
po<- fitdistr(dat.gla$Planting.to.DTB.2018, "poisson")
ga<- fitdistr(dat.gla$Planting.to.DTB.2018, "gamma")

AIC(nor, nb, po, ga)

#models for : GL_alvar
gla_DTB2018<- lmer(Planting.to.DTB.2018 ~ Block.ID +Population+ (1|Family.Unique)  
                     + Block.ID : (1|Family.Unique),
                     data=dat.gla)

summary(gla_DTB2018)

print(VarCorr(gla_DTB2018), comp=c("Variance", "Std.Dev."))

h2<- (2.5*0.11693)/ (0.11693 + 1.07690 + 26.01082)
h2 # 0.011

CVa<- sqrt(2.5*0.11693)/mean(dat.gla$Planting.to.DTB.2018)
CVa# 0.001

#calculate standard error from standard deviation (sd / sqrt(n))
se<- 0.34195 / sqrt(nrow(dat.gla))
se # 0.012


#models for germination: MB_alvar
dat.mba<- dat.mba[!is.na(dat.mba$Planting.to.DTB.2018), ]

nor<- fitdistr(dat.mba$Planting.to.DTB.2018, "normal")
nb<- fitdistr(dat.mba$Planting.to.DTB.2018, "negative binomial")
po<- fitdistr(dat.mba$Planting.to.DTB.2018, "poisson")
ga<- fitdistr(dat.mba$Planting.to.DTB.2018, "gamma")

AIC(nor, nb, po, ga)

mba_DTB2018<- lmer(Planting.to.DTB.2018 ~ Block.ID +Population+ (1|Family.Unique)  
                     + Block.ID : (1|Family.Unique),
                     data=dat.mba)

summary(mba_DTB2018)

print(VarCorr(mba_DTB2018), comp=c("Variance", "Std.Dev."))

h2<- (2.5*0.1449)/ (0.1449 + 1.2190 + 26.9313)
h2 # 0.013

CVa<- sqrt(2.5*0.1449)/mean(dat.mba$Planting.to.DTB.2018)
CVa #0.001

#calculate standard error from standard deviation (sd / sqrt(n))
se<- 0.38066  / sqrt(nrow(dat.mba))
se # 0.037


#models for germination: Prairie
dat.pr<- dat.pr[!is.na(dat.pr$Planting.to.DTB.2018), ]

nor<- fitdistr(dat.pr$Planting.to.DTB.2018, "normal")
nb<- fitdistr(dat.pr$Planting.to.DTB.2018, "negative binomial")
po<- fitdistr(dat.pr$Planting.to.DTB.2018, "poisson")
ga<- fitdistr(dat.pr$Planting.to.DTB.2018, "gamma")

AIC(nor, nb, po, ga)


pr_DTB2018<- lmer(Planting.to.DTB.2018 ~ Block.ID +Population+ (1|Family.Unique)  
                    + Block.ID : (1|Family.Unique),
                    data=dat.pr)

summary(pr_DTB2018)

print(VarCorr(pr_DTB2018), comp=c("Variance", "Std.Dev."))

h2<- (2.5*0.010145)/ (0.010145 + 1.246974 + 16.761657)
h2 # 0.001

CVa<- sqrt(2.5*0.010145)/mean(dat.pr$Planting.to.DTB.2018)
CVa #0.001

#calculate standard error from standard deviation (sd / sqrt(n))
se<- 0.10072 / sqrt(nrow(dat.pr))
se # 0.008


############################################################
# seed mass
#
#
# sm - seed mass in 2016
# sm.2 - seed mass in 2017
# sm.3 - seed mass in 2018
# sm2017 - combined seed mass from 2016 and 2017
# sm2018 - combined seed mass from 2016, 2017, and 2018
#
#
#
#
# for 2016 data, use only those that made >0 fruits
#
 dat2<- subset(dat, No.Fruit.2016 > 0)
#
##subset by region
dat.gla<- subset(dat2, Region=="GL_alvar")
dat.mba<- subset(dat2, Region=="MB_alvar")
dat.pr<- subset(dat2, Region=="Prairie")

#black as factor
dat.gla$Block.ID<- as.factor(dat.gla$Block.ID)
dat.mba$Block.ID<- as.factor(dat.mba$Block.ID)
dat.pr$Block.ID<- as.factor(dat.pr$Block.ID)



#GLA
dat.gla<- dat.gla[!is.na(dat.gla$sm), ]

nor<- fitdistr(dat.gla$sm, "normal")
nb<- fitdistr(dat.gla$sm, "negative binomial")
po<- fitdistr(dat.gla$sm, "poisson")
ga<- fitdistr(dat.gla$sm, "gamma")

AIC(nor, nb, po, ga)




gla_sm<- lmer(sm ~ Block.ID +Population + (1|Family.Unique)  
                + Block.ID : (1|Family.Unique),
                data=dat.gla)
summary(gla_sm)

print(VarCorr(gla_sm), comp=c("Variance", "Std.Dev."))

h2<- (2.5*31.407)/ (31.407 + 342.961 + 2834.575)
h2 #0.024

CVa<- sqrt(2.5*31.407)/mean(dat.gla$sm)
CVa # 0.246

#calculate standard error from standard deviation (sd / sqrt(n))
se<- 5.6042  / sqrt(nrow(dat.gla))
se # 0.581


#MBA - only two flowering idividuals, with only one setting seed, h2 = NA
mba_sm<- lmer(sm ~ Block.ID +Population + (1|Family.Unique)  
                + Block.ID : (1|Family.Unique),
                data=dat.mba)
summary(mba_germ)

print(VarCorr(mba_germ), comp=c("Variance", "Std.Dev."))

#PRA - only two individuals in data, h2=NA

pr_sm<- lmer(sm ~ Block.ID +Population + (1|Family.Unique)  
               + Block.ID : (1|Family.Unique),
               data=dat.pr)


#################################################################
#
#2017 seed set

#for 2017 data, use only those that set >0 fruit in 2017
#
dat2<- subset(dat, No.Fruit.2017 > 0)
#
##subset by region
dat.gla<- subset(dat2, Region=="GL_alvar")
dat.mba<- subset(dat2, Region=="MB_alvar")
dat.pr<- subset(dat2, Region=="Prairie")

#black as factor
dat.gla$Block.ID<- as.factor(dat.gla$Block.ID)
dat.mba$Block.ID<- as.factor(dat.mba$Block.ID)
dat.pr$Block.ID<- as.factor(dat.pr$Block.ID)

#GLA
dat.gla<- dat.gla[!is.na(dat.gla$No.Fruit.2017), ]

nor<- fitdistr(dat.gla$sm.2, "normal")
nb<- fitdistr(dat.gla$sm.2, "negative binomial")
po<- fitdistr(dat.gla$sm.2, "poisson")
ga<- fitdistr(dat.gla$sm.2, "gamma")

AIC(nor, nb, po, ga)


gla_sm<- lmer(sm.2 ~ Block.ID +Population + (1|Family.Unique)  
              + Block.ID : (1|Family.Unique),
              data=dat.gla)
summary(gla_sm)

print(VarCorr(gla_sm), comp=c("Variance", "Std.Dev."))

h2<- (2.5*4084.6)/ (4084.6 + 68857.9 + 592155.4)
h2 #0.015

CVa<- sqrt(2.5*4084.6)/ mean(dat.gla$sm.2)
CVa# 0.106

#calculate standard error from standard deviation (sd / sqrt(n))
se<- 63.911 / sqrt(nrow(dat.gla))
se # 2.268

#MBA
dat.mba<- dat.mba[!is.na(dat.mba$No.Fruit.2017), ]

nor<- fitdistr(dat.gla$sm.2, "normal")
nb<- fitdistr(dat.gla$sm.2, "negative binomial")
po<- fitdistr(dat.gla$sm.2, "poisson")
ga<- fitdistr(dat.gla$sm.2, "gamma")

AIC(nor, nb, po, ga)

mba_sm<- lmer(sm.2 ~ Block.ID +Population + (1|Family.Unique)  
              + Block.ID : (1|Family.Unique),
              data=dat.mba)
summary(mba_sm)

print(VarCorr(mba_sm), comp=c("Variance", "Std.Dev."))

h2<- (2.5*1638.7)/ (1638.7 + 9123.4 + 73194.7)
h2 #0.049

CVa<- sqrt(2.5*1638.7)/mean(dat.mba$sm.2)
CVa# 0.235

#calculate standard error from standard deviation (sd / sqrt(n))
se<- 40.481 / sqrt(nrow(dat.mba))
se # 4.048


#PRA
dat.pr<- dat.pr[!is.na(dat.pr$No.Fruit.2017), ]

nor<- fitdistr(dat.pr$sm.2, "normal")
nb<- fitdistr(dat.pr$sm.2, "negative binomial")
po<- fitdistr(dat.pr$sm.2, "poisson")
ga<- fitdistr(dat.pr$sm.2, "gamma")

AIC(nor, nb, po, ga)#nb

pra_sm<- glmer.nb(sm.2 ~ Block.ID +Population + (1|Family.Unique)  
              + Block.ID : (1|Family.Unique),
              data=dat.pr, tol=1e-2,
              control=glmerControl(optimizer="Nelder_Mead",
                                   optCtrl=list(maxfun=1e5)))
summary(pra_sm)

isSingular(pra_sm)#False

var(resid(pra_sm))

print(VarCorr(pra_sm), comp=c("Variance", "Std.Dev."))

h2<- (2.5*0.011270)/ (0.011270 + 0.014726 + 1.008947)
h2 #0.027

CVa<- sqrt(2.5*0.011270) / mean(dat.pr$sm.2)
CVa# 0.001

#calculate standard error from standard deviation (sd / sqrt(n))
se<- 0.10616 / sqrt(nrow(dat.pr))
se # 0.008

###################################################################################
#
# 2018 seed set

################################################################
#
# for 2087 data must have set fruit in 2018
#
#
dat2<- subset(dat, No.Fruit.2018 > 0)
#
##subset by region
dat.gla<- subset(dat2, Region=="GL_alvar")
dat.mba<- subset(dat2, Region=="MB_alvar")
dat.pr<- subset(dat2, Region=="Prairie")

#black as factor
dat.gla$Block.ID<- as.factor(dat.gla$Block.ID)
dat.mba$Block.ID<- as.factor(dat.mba$Block.ID)
dat.pr$Block.ID<- as.factor(dat.pr$Block.ID)

#GLA
dat.gla<- dat.gla[!is.na(dat.gla$No.Fruit.2018), ]

nor<- fitdistr(dat.gla$sm.3, "normal")
nb<- fitdistr(dat.gla$sm.3, "negative binomial")
po<- fitdistr(dat.gla$sm.3, "poisson")
ga<- fitdistr(dat.gla$sm.3, "gamma")

AIC(nor, nb, po, ga)


gla_sm<- glmer.nb(sm.3 ~ Block.ID +Population + (1|Family.Unique)  
              + Block.ID : (1|Family.Unique),
              data=dat.gla)

isSingular(gla_sm)#False

summary(gla_sm)

var(resid(gla_sm))

print(VarCorr(gla_sm), comp=c("Variance", "Std.Dev."))

h2<- (2.5*0.0065694)/ (0.0065694 + 0.0288352 + 1.004482)
h2 #0.016

CVa<- sqrt(2.5*0.0065694)/mean(dat.gla$sm.3)
CVa#0.000

#calculate standard error from standard deviation (sd / sqrt(n))
se<- 0.081052 / sqrt(nrow(dat.gla))
se # 0.003

#MBA
dat.mba<- dat.mba[!is.na(dat.mba$No.Fruit.2018), ]

nor<- fitdistr(dat.mba$sm.3, "normal")
nb<- fitdistr(dat.mba$sm.3, "negative binomial")
po<- fitdistr(dat.mba$sm.3, "poisson")
ga<- fitdistr(dat.mba$sm.3, "gamma")

AIC(nor, nb, po, ga)


mba_sm<- glmer.nb(sm.3 ~ Block.ID +Population + (1|Family.Unique)  
              + Block.ID : (1|Family.Unique),
              data=dat.mba)

isSingular(mba_sm)#False

summary(mba_sm)

print(VarCorr(mba_sm), comp=c("Variance", "Std.Dev."))

var(resid(mba_sm))

h2<- (2.5*0.0416427)/ (0.0416427 + 0.0043986 + 0.9940254)
h2 #0.100

CVa<- sqrt(2.5*0.0416427)/mean(dat.mba$sm.3)
CVa#0.001

#calculate standard error from standard deviation (sd / sqrt(n))
se<- 0.204065 / sqrt(nrow(dat.mba))
se # 0.023


#PRA
dat.pr<- dat.pr[!is.na(dat.pr$No.Fruit.2018), ]

nor<- fitdistr(dat.pr$sm.3, "normal")
nb<- fitdistr(dat.pr$sm.3, "negative binomial")
po<- fitdistr(dat.pr$sm.3, "poisson")
ga<- fitdistr(dat.pr$sm.3, "gamma")

AIC(nor, nb, po, ga)

pra_sm<- glmer.nb(sm.3 ~ Block.ID +Population + (1|Family.Unique)  
              + Block.ID : (1|Family.Unique),
              data=dat.pr, tol=1e-2,
              control=glmerControl(optimizer="Nelder_Mead",
                                   optCtrl=list(maxfun=1e5)))

isSingular(pra_sm)#False

summary(pra_sm)

print(VarCorr(pra_sm), comp=c("Variance", "Std.Dev."))

var(resid(pra_sm))

h2<- (2.5*0.002623)/ (0.002623 + 0.037266 + 0.9956004)
h2 #0.006

CVa<- sqrt(2.5*0.002623)/mean(dat.pr$sm.3)
CVa#0.000

#calculate standard error from standard deviation (sd / sqrt(n))
se<- 0.051215 / sqrt(nrow(dat.pr))
se # 0.004

###########################################################################
#
# combined 2016 and 2017 seed mass (sm2017)
#
# must have set fruit in 2016 or 2017
#
dat2<- subset(dat, No.Fruit.2016 > 0 | No.Fruit.2017 >0)
#
##subset by region
dat.gla<- subset(dat2, Region=="GL_alvar")
dat.mba<- subset(dat2, Region=="MB_alvar")
dat.pr<- subset(dat2, Region=="Prairie")

#black as factor
dat.gla$Block.ID<- as.factor(dat.gla$Block.ID)
dat.mba$Block.ID<- as.factor(dat.mba$Block.ID)
dat.pr$Block.ID<- as.factor(dat.pr$Block.ID)

#GLA
dat.gla<- dat.gla[!is.na(dat.gla$sm2017), ]

nor<- fitdistr(dat.gla$sm2017, "normal")
nb<- fitdistr(dat.gla$sm2017, "negative binomial")
po<- fitdistr(dat.gla$sm2017, "poisson")
ga<- fitdistr(dat.gla$sm2017, "gamma")

AIC(nor, nb, po, ga)

gla_sm<- glmer.nb(sm2017 ~ Block.ID +Population + (1|Family.Unique)  
              + Block.ID : (1|Family.Unique),
              data=dat.gla)

isSingular(gla_sm)#False

summary(gla_sm)

print(VarCorr(gla_sm), comp=c("Variance", "Std.Dev."))

var(resid(gla_sm))

h2<- (2.5*0.023417)/ (0.023417 + 0.047723 + 0.9916497)
h2 #0.055

CVa<- sqrt(2.5*0.023417)/mean(dat.gla$sm2017)
CVa#0.0002

#calculate standard error from standard deviation (sd / sqrt(n))
se<- 0.15302  / sqrt(nrow(dat.gla))
se # 0.005

#MBA
dat.mba<- dat.mba[!is.na(dat.mba$sm2017), ]

nor<- fitdistr(dat.mba$sm2017, "normal")
nb<- fitdistr(dat.mba$sm2017, "negative binomial")
po<- fitdistr(dat.mba$sm2017, "poisson")
ga<- fitdistr(dat.mba$sm2017, "gamma")

AIC(nor, nb, po, ga)


mba_sm<- glmer.nb(sm2017 ~ Block.ID +Population + (1|Family.Unique)  
              + Block.ID : (1|Family.Unique),
              data=dat.mba)

isSingular(mba_sm)#False

summary(mba_sm)

print(VarCorr(mba_sm), comp=c("Variance", "Std.Dev."))

var(resid(mba_sm))

h2<- (2.5*0.00062882)/ (0.00062882 + 0.05794090 + 0.9946775)
h2 #0.001

CVa<- sqrt(2.5*0.00062882)/mean(dat.mba$sm2017)
CVa#0.000

#calculate standard error from standard deviation (sd / sqrt(n))
se<- 0.025076 / sqrt(nrow(dat.mba))
se # 0.002


#PRA
dat.pr<- dat.pr[!is.na(dat.pr$sm2017), ]

nor<- fitdistr(dat.pr$sm2017, "normal")
nb<- fitdistr(dat.pr$sm2017, "negative binomial")
po<- fitdistr(dat.pr$sm2017, "poisson")
ga<- fitdistr(dat.pr$sm2017, "gamma")

AIC(nor, nb, po, ga)

pra_sm<- glmer.nb(sm2017 ~ Block.ID +Population + (1|Family.Unique)  
              + Block.ID : (1|Family.Unique),
              data=dat.pr)

isSingular(pra_sm)#False

summary(pra_sm)

print(VarCorr(pra_sm), comp=c("Variance", "Std.Dev."))

var(resid(pra_sm))

h2<- (2.5*0.011633)/ (0.011633 + 0.013781 + 1.009794)
h2 #0.028

CVa<- sqrt(2.5*0.011633)/mean(dat.pr$sm2017)
CVa#0.001

#calculate standard error from standard deviation (sd / sqrt(n))
se<- 0.10786 / sqrt(nrow(dat.pr))
se # 0.008

###########################################################################
#
# combined 2016, 2017, and 2018 seed mass (sm2018)
#
# must have set fruit in 2016, 2017, or 2018
#
dat2<- subset(dat, No.Fruit.2016 > 0 | No.Fruit.2017 >0 | No.Fruit.2018 >0 )
#
##subset by region
dat.gla<- subset(dat2, Region=="GL_alvar")
dat.mba<- subset(dat2, Region=="MB_alvar")
dat.pr<- subset(dat2, Region=="Prairie")

#black as factor
dat.gla$Block.ID<- as.factor(dat.gla$Block.ID)
dat.mba$Block.ID<- as.factor(dat.mba$Block.ID)
dat.pr$Block.ID<- as.factor(dat.pr$Block.ID)

#GLA
dat.gla<- dat.gla[!is.na(dat.gla$sm2018), ]

nor<- fitdistr(dat.gla$sm2018, "normal")
nb<- fitdistr(dat.gla$sm2018, "negative binomial")
po<- fitdistr(dat.gla$sm2018, "poisson")
ga<- fitdistr(dat.gla$sm2018, "gamma")

AIC(nor, nb, po, ga)


gla_sm<- glmer.nb(sm2018 ~ Block.ID +Population + (1|Family.Unique)  
              + Block.ID : (1|Family.Unique),
              data=dat.gla)

isSingular(gla_sm)#False

summary(gla_sm)

print(VarCorr(gla_sm), comp=c("Variance", "Std.Dev."))

var(resid(gla_sm))

h2<- (2.5*0.054539)/ (0.054539 + 0.040467 + 0.9817235)
h2 #0.126

CVa<- sqrt(2.5*0.054539)/mean(dat.gla$sm2018)
CVa#0.000

#calculate standard error from standard deviation (sd / sqrt(n))
se<- 0.23353 / sqrt(nrow(dat.gla))
se # 0.008

#MBA
dat.mba<- dat.mba[!is.na(dat.mba$sm2018), ]

nor<- fitdistr(dat.mba$sm2018, "normal")
nb<- fitdistr(dat.mba$sm2018, "negative binomial")
po<- fitdistr(dat.mba$sm2018, "poisson")
ga<- fitdistr(dat.mba$sm2018, "gamma")

AIC(nor, nb, po, ga)


mba_sm<- glmer.nb(sm2018 ~ Block.ID +Population + (1|Family.Unique)  
              + Block.ID : (1|Family.Unique),
              data=dat.mba)

isSingular(mba_sm)#False

summary(mba_sm)

print(VarCorr(mba_sm), comp=c("Variance", "Std.Dev."))

var(resid(mba_sm))

h2<- (2.5*0.016616)/ (0.016616 + 0.016863 + 1.012803)
h2 #0.040

CVa<- sqrt(2.5*0.016616)/mean(dat.mba$sm2018)
CVa#0.0004

#calculate standard error from standard deviation (sd / sqrt(n))
se<- 0.12890 / sqrt(nrow(dat.mba))
se # 0.012


#PRA
dat.pr<- dat.pr[!is.na(dat.pr$sm2018), ]

nor<- fitdistr(dat.pr$sm2018, "normal")
nb<- fitdistr(dat.pr$sm2018, "negative binomial")
po<- fitdistr(dat.pr$sm2018, "poisson")
ga<- fitdistr(dat.pr$sm2018, "gamma")

AIC(nor, nb, po, ga)


pra_sm<- glmer.nb(sm2018 ~ Block.ID +Population + (1|Family.Unique)  
              + Block.ID : (1|Family.Unique),
              data=dat.pr, tol=1e-2,
              control=glmerControl(optimizer="Nelder_Mead",
                                   optCtrl=list(maxfun=1e5)))

isSingular(pra_sm)#False

summary(pra_sm)

print(VarCorr(pra_sm), comp=c("Variance", "Std.Dev."))

var(resid(pra_sm))

h2<- (2.5*0.00049744)/ (0.00049744 + 0.00865605 + 1.019885)
h2 #0.001

CVa<- sqrt(2.5*0.00049744)/mean(dat.pr$sm2018)
CVa#0.000

#calculate standard error from standard deviation (sd / sqrt(n))
se<- 0.022303 / sqrt(nrow(dat.pr))
se # 0.002
