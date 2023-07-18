#This is a test code to use the Hamilton Lab Server

#set Thelio working directory
setwd("/home/mkulbaba/heritability/")

dat<- read.csv("final_data.csv")

#install.packages("MASS")

library(MASS)

dat2<- subset(dat, Germination.Y.N== 1)

dat2$tot.seeds<- dat2$sm + dat2$sm.2 + dat2$sm.3

dat.gla<- subset(dat2, Region=="GL_alvar")
dat.mba<- subset(dat2, Region=="MB_alvar")
dat.pr<- subset(dat2, Region=="Prairie")


#install.packages("lme4")

library(lme4)

#make block a factor

dat.gla$Block.ID<- as.factor(dat.gla$Block.ID)
dat.mba$Block.ID<- as.factor(dat.mba$Block.ID)
dat.pr$Block.ID<- as.factor(dat.pr$Block.ID)

############################
# Start with Germination

datG.gla<- subset(dat, Region=="GL_alvar")
datG.mba<- subset(dat, Region=="MB_alvar")
datG.pr<- subset(dat, Region=="Prairie")

datG.gla$Block.ID<- as.factor(datG.gla$Block.ID)
datG.mba$Block.ID<- as.factor(datG.mba$Block.ID)
datG.pr$Block.ID<- as.factor(datG.pr$Block.ID)

##########################################################
# No.Days.to.Germ

#models for germination: GL_alvar
gla_germ1<- glmer(No.Days.to.Germ ~ Block.ID + Population + (1| Family.Unique),
                 data=datG.gla)

gla_germ2<- glmer(No.Days.to.Germ ~ Block.ID + Population + (1| Family.Unique),
                 data=datG.gla, family="poisson", maxiter=500)

gla_germ3<- glmer.nb(No.Days.to.Germ ~ Block.ID + Population + (1| Family.Unique),
                 data=datG.gla, maxiter=500)

summary(gla_germ1)
summary(gla_germ2)
summary(gla_germ3)

AIC(gla_germ1)
AIC(gla_germ2)
AIC(gla_germ3)


#models for germination: MB_alvar
mba_germ1<- glmer(No.Days.to.Germ ~ Block.ID + Population + (1| Family.Unique),
                  data=datG.mba)

mba_germ2<- glmer(No.Days.to.Germ ~ Block.ID + Population + (1| Family.Unique),
                  data=datG.mba, family="poisson", maxiter=500)

mba_germ3<- glmer.nb(No.Days.to.Germ ~ Block.ID + Population + (1| Family.Unique),
                     data=datG.mba, maxiter=500)

summary(mba_germ1)
summary(mba_germ2)
summary(mba_germ3)

AIC(mba_germ1)
AIC(mba_germ2)
AIC(mba_germ3)


#models for germination: Prairie
pr_germ1<- glmer(No.Days.to.Germ ~ Block.ID + Population + (1| Family.Unique),
                  data=datG.pr)

pr_germ2<- glmer(No.Days.to.Germ ~ Block.ID + Population + (1| Family.Unique),
                  data=datG.pr, family="poisson", maxiter=500)

pr_germ3<- glmer.nb(No.Days.to.Germ ~ Block.ID + Population + (1| Family.Unique),
                     data=datG.pr, maxiter=500)

summary(pr_germ1)
summary(pr_germ2)
summary(pr_germ3)

AIC(pr_germ1)
AIC(pr_germ2)
AIC(pr_germ3)

#estract family-level variance in seed mass

#GL_alvar 
#extract variance component - gaussian dist
glavars.germ <- as.data.frame(VarCorr(gla_germ1))
glavars.germ
print(VarCorr(gla_germ1), comp = "Variance")

va.gla.germ <-glavars.germ[1,4]
va.gla.germ

#convert to response scale - is identity
Va.gla.germ<- va.gla.germ

Va.gla.germ

#remove NAs for calc of var

days.gla <- datG.gla$No.Days.to.Germ[!is.na(datG.gla$No.Days.to.Germ)]

vp.gla.germ<- var(days.gla)
4*vp.gla.germ

h2.gla.germ<- Va.gla.germ/vp.gla.germ
h2.gla.germ #0.2281384




#MB_alvar - poisson
mba.germ <- as.data.frame(VarCorr(mba_germ2))
mba.germ
print(VarCorr(mba_germ2), comp = "Variance")

va.mba.germ <-mba.germ[1,4]
va.mba.germ

#convert to response scale- link funciton log
Va.mba.germ<- log(va.mba.germ)

Va.mba.germ

days.mba <- datG.mba$No.Days.to.Germ[!is.na(datG.mba$No.Days.to.Germ)]

vp.mba.germ<- var(days.mba)
vp.mba.germ

h2.mba.germ<- Va.mba.germ/vp.mba.germ
h2.mba.germ # approx. to 0


#Prairie - Poisson
pravars.germ <- as.data.frame(VarCorr(pr_germ2))
pravars.germ
print(VarCorr(pr_germ2), comp = "Variance")

pravars.germ <-pravars.germ[1,4]
pravars.germ

pravars.germ<- exp(pravars.germ)

Va.pra.germ<-4*pravars.germ

days.pra <- datG.pr$No.Days.to.Germ[!is.na(datG.pr$No.Days.to.Germ)]

vp.pra.germ<- var(days.pra)
vp.pra.germ

h2.pra.germ<- Va.pra.germ/vp.pra.germ
h2.pra.germ # 0.001348861

##########################################################
# No.Days to True Leaf

##########################################################

#models for germination: GL_alvar
gla_germ1<- lmer(No.Days.to.TrueLeaf ~ Block.ID + Population + (1| Family.Unique),
                  data=datG.gla)

gla_germ2<- glmer(No.Days.to.TrueLeaf ~ Block.ID + Population + (1| Family.Unique),
                  data=datG.gla, family="poisson", maxiter=500)

gla_germ3<- glmer.nb(No.Days.to.TrueLeaf ~ Block.ID + Population + (1| Family.Unique),
                     data=datG.gla, maxiter=500)

summary(gla_germ1)
summary(gla_germ2)
summary(gla_germ3)

AIC(gla_germ1)
AIC(gla_germ2)
AIC(gla_germ3)


#models for germination: MB_alvar
mba_germ1<- lmer(No.Days.to.TrueLeaf ~ Block.ID + Population + (1| Family.Unique),
                  data=datG.mba)

mba_germ2<- glmer(No.Days.to.TrueLeaf ~ Block.ID + Population + (1| Family.Unique),
                  data=datG.mba, family="poisson", maxiter=500)

mba_germ3<- glmer.nb(No.Days.to.TrueLeaf ~ Block.ID + Population + (1| Family.Unique),
                     data=datG.mba, maxiter=500)

summary(mba_germ1)
summary(mba_germ2)
summary(mba_germ3)

AIC(mba_germ1)
AIC(mba_germ2)
AIC(mba_germ3)


#models for germination: Prairie
pr_germ1<- lmer(No.Days.to.TrueLeaf ~ Block.ID + Population + (1| Family.Unique),
                 data=datG.pr)

pr_germ2<- glmer(No.Days.to.TrueLeaf ~ Block.ID + Population + (1| Family.Unique),
                 data=datG.pr, family="poisson", maxiter=500)

pr_germ3<- glmer.nb(No.Days.to.TrueLeaf ~ Block.ID + Population + (1| Family.Unique),
                    data=datG.pr, maxiter=500)

summary(pr_germ1)
summary(pr_germ2)
summary(pr_germ3)

AIC(pr_germ1)
AIC(pr_germ2)
AIC(pr_germ3)

#estract family-level variance in seed mass

#GL_alvar 
#extract variance component - gaussian dist
glavars.germ <- as.data.frame(VarCorr(gla_germ1))
glavars.germ
print(VarCorr(gla_germ1), comp = "Variance")

va.gla.germ <-glavars.germ[1,4]
va.gla.germ

#convert to response scale - is identity
Va.gla.germ<- va.gla.germ

Va.gla.germ

#remove NAs for calc of var

days.gla <- datG.gla$No.Days.to.TrueLeaf[!is.na(datG.gla$No.Days.to.TrueLeaf)]

vp.gla.germ<- var(days.gla)
vp.gla.germ

h2.gla.germ<- Va.gla.germ/vp.gla.germ
h2.gla.germ #0.254046


#MB_alvar - gaussian
mba.germ <- as.data.frame(VarCorr(mba_germ1))
mba.germ
print(VarCorr(mba_germ2), comp = "Variance")

va.mba.germ <-mba.germ[1,4]
va.mba.germ

#convert to response scale- identity
Va.mba.germ<- va.mba.germ

Va.mba.germ

days.mba <- datG.mba$No.Days.to.TrueLeaf[!is.na(datG.mba$No.Days.to.TrueLeaf)]

vp.mba.germ<- var(days.mba)
vp.mba.germ

h2.mba.germ<- Va.mba.germ/vp.mba.germ
h2.mba.germ # 0.127


#Prairie - Poisson
pravars.germ <- as.data.frame(VarCorr(pr_germ2))
pravars.germ
print(VarCorr(pr_germ2), comp = "Variance")

pravars.germ <-pravars.germ[1,4]
pravars.germ

va.par<- exp(pravars.germ)

Va.pra.germ<-va.par
Va.pra.germ

days.pra <- datG.pr$No.Days.to.TrueLeaf[!is.na(datG.pr$No.Days.to.TrueLeaf)]

vp.pra.germ<- var(days.pra)
vp.pra.germ

h2.pra.germ<- Va.pra.germ/vp.pra.germ
h2.pra.germ # 0.0345

###########################################
# Germ.to.TrueLeaf
###########################################

#models for germination: GL_alvar
gla_germ1<- lmer(Germ.to.TrueLeaf ~ Block.ID + Population + (1| Family.Unique),
                 data=datG.gla)

gla_germ2<- glmer(Germ.to.TrueLeaf ~ Block.ID + Population + (1| Family.Unique),
                  data=datG.gla, family="poisson", maxiter=500)

gla_germ3<- glmer.nb(Germ.to.TrueLeaf ~ Block.ID + Population + (1| Family.Unique),
                     data=datG.gla, maxiter=500)

summary(gla_germ1)
summary(gla_germ2)
summary(gla_germ3)

AIC(gla_germ1)
AIC(gla_germ2)
AIC(gla_germ3)


#models for germination: MB_alvar
mba_germ1<- lmer(Germ.to.TrueLeaf ~ Block.ID + Population + (1| Family.Unique),
                 data=datG.mba)

mba_germ2<- glmer(Germ.to.TrueLeaf ~ Block.ID + Population + (1| Family.Unique),
                  data=datG.mba, family="poisson", maxiter=500)

mba_germ3<- glmer.nb(Germ.to.TrueLeaf ~ Block.ID + Population + (1| Family.Unique),
                     data=datG.mba, maxiter=500)

summary(mba_germ1)
summary(mba_germ2)
summary(mba_germ3)

AIC(mba_germ1)
AIC(mba_germ2)
AIC(mba_germ3)


#models for germination: Prairie
pr_germ1<- lmer(Germ.to.TrueLeaf ~ Block.ID + Population + (1| Family.Unique),
                data=datG.pr)

pr_germ2<- glmer(Germ.to.TrueLeaf ~ Block.ID + Population + (1| Family.Unique),
                 data=datG.pr, family="poisson", maxiter=500)

pr_germ3<- glmer.nb(Germ.to.TrueLeaf ~ Block.ID + Population + (1| Family.Unique),
                    data=datG.pr, maxiter=500)

summary(pr_germ1)
summary(pr_germ2)
summary(pr_germ3)

AIC(pr_germ1)
AIC(pr_germ2)
AIC(pr_germ3)

#estract family-level variance in seed mass

#GL_alvar 
#extract variance component - gaussian dist
glavars.germ <- as.data.frame(VarCorr(gla_germ1))
glavars.germ
print(VarCorr(gla_germ1), comp = "Variance")

va.gla.germ <-glavars.germ[1,4]
va.gla.germ

#convert to response scale - is identity
Va.gla.germ<- va.gla.germ

Va.gla.germ

#remove NAs for calc of var

days.gla <- datG.gla$Germ.to.TrueLeaf[!is.na(datG.gla$Germ.to.TrueLeaf)]

vp.gla.germ<- var(days.gla)
vp.gla.germ

h2.gla.germ<- Va.gla.germ/vp.gla.germ
h2.gla.germ #0.099


#MB_alvar - gaussian
mba.germ <- as.data.frame(VarCorr(mba_germ1))
mba.germ
print(VarCorr(mba_germ2), comp = "Variance")

va.mba.germ <-mba.germ[1,4]
va.mba.germ

#convert to response scale- identity
Va.mba.germ<- va.mba.germ

Va.mba.germ

days.mba <- datG.mba$Germ.to.TrueLeaf[!is.na(datG.mba$Germ.to.TrueLeaf)]

vp.mba.germ<- var(days.mba)
vp.mba.germ

h2.mba.germ<- Va.mba.germ/vp.mba.germ
h2.mba.germ # 0.023


#Prairie - Poisson
pravars.germ <- as.data.frame(VarCorr(pr_germ1))
pravars.germ
print(VarCorr(pr_germ1), comp = "Variance")

pravars.germ <-pravars.germ[1,4]
pravars.germ

va.par<- pravars.germ

Va.pra.germ<-va.par
Va.pra.germ

days.pra <- datG.pr$No.Days.to.TrueLeaf[!is.na(datG.pr$No.Days.to.TrueLeaf)]

vp.pra.germ<- var(days.pra)
vp.pra.germ

h2.pra.germ<- Va.pra.germ/vp.pra.germ
h2.pra.germ # 0.025

#####################################################################
# Y.N. Germination

#models for germination: GL_alvar
gla_germ1<- glmer(Germination.Y.N ~ Block.ID + Population + (1| Family.Unique),
                  data=datG.gla, family="binomial", maxiter=500)

mba_germ1<- glmer(Germination.Y.N ~ Block.ID + Population + (1| Family.Unique),
                  data=datG.mba, family="binomial", maxiter=500)

pr_germ1<- glmer(Germination.Y.N ~ Block.ID + Population + (1| Family.Unique),
                  data=datG.pr, family="binomial", maxiter=500)


summary(gla_germ1)
summary(mba_germ1)
summary(pr_germ1)

#models for germination: MB_alvar
mba_germ1<- lmer(Germ.to.TrueLeaf ~ Block.ID + Population + (1| Family.Unique),
                 data=datG.mba)

mba_germ2<- glmer(Germ.to.TrueLeaf ~ Block.ID + Population + (1| Family.Unique),
                  data=datG.mba, family="poisson", maxiter=500)

mba_germ3<- glmer.nb(Germ.to.TrueLeaf ~ Block.ID + Population + (1| Family.Unique),
                     data=datG.mba, maxiter=500)

summary(mba_germ1)
summary(mba_germ2)
summary(mba_germ3)

AIC(mba_germ1)
AIC(mba_germ2)
AIC(mba_germ3)


#models for germination: Prairie
pr_germ1<- lmer(Germ.to.TrueLeaf ~ Block.ID + Population + (1| Family.Unique),
                data=datG.pr)

pr_germ2<- glmer(Germ.to.TrueLeaf ~ Block.ID + Population + (1| Family.Unique),
                 data=datG.pr, family="poisson", maxiter=500)

pr_germ3<- glmer.nb(Germ.to.TrueLeaf ~ Block.ID + Population + (1| Family.Unique),
                    data=datG.pr, maxiter=500)

summary(pr_germ1)
summary(pr_germ2)
summary(pr_germ3)

AIC(pr_germ1)
AIC(pr_germ2)
AIC(pr_germ3)

#estract family-level variance in seed mass

#GL_alvar 
#extract variance component - gaussian dist
glavars.germ <- as.data.frame(VarCorr(gla_germ1))
glavars.germ
print(VarCorr(gla_germ1), comp = "Variance")

va.gla.germ <-glavars.germ[1,4]
va.gla.germ

#convert to response scale - is identity
Va.gla.germ<- exp(va.gla.germ)



Va.gla.germ<- Va.gla.germ/(Va.gla.germ + 1)

Va.gla.germ

#remove NAs for calc of var

days.gla <- datG.gla$Germ.to.TrueLeaf[!is.na(datG.gla$Germ.to.TrueLeaf)]

vp.gla.germ<- var(days.gla)
vp.gla.germ

h2.gla.germ<- Va.gla.germ/vp.gla.germ
h2.gla.germ #0.099


#MB_alvar - gaussian
mba.germ <- as.data.frame(VarCorr(mba_germ1))
mba.germ
print(VarCorr(mba_germ2), comp = "Variance")

va.mba.germ <-mba.germ[1,4]
va.mba.germ

#convert to response scale- identity
Va.mba.germ<- va.mba.germ

Va.mba.germ

days.mba <- datG.mba$Germ.to.TrueLeaf[!is.na(datG.mba$Germ.to.TrueLeaf)]

vp.mba.germ<- var(days.mba)
vp.mba.germ

h2.mba.germ<- Va.mba.germ/vp.mba.germ
h2.mba.germ # 0.023


#Prairie - Poisson
pravars.germ <- as.data.frame(VarCorr(pr_germ1))
pravars.germ
print(VarCorr(pr_germ1), comp = "Variance")

pravars.germ <-pravars.germ[1,4]
pravars.germ

va.par<- pravars.germ

Va.pra.germ<-va.par
Va.pra.germ

days.pra <- datG.pr$No.Days.to.TrueLeaf[!is.na(datG.pr$No.Days.to.TrueLeaf)]

vp.pra.germ<- var(days.pra)
vp.pra.germ

h2.pra.germ<- Va.pra.germ/vp.pra.germ
h2.pra.germ # 0.025


#Note, use glmer with appropriate family (or lmer if normal...hahaha), and 
#glmer.nb for negative binomial model

#To develope a 'pipeline' perform seeds mass measures for 2016, 2017, 2018, and total
#seed mass for GL_alvar region

m2016<- glmer.nb(sm ~ Block.ID + Population + (1| Family.Unique),
                 data=dat.gla)

m2017<- glmer.nb(sm.2 ~ Block.ID + Population + (1| Family.Unique),
                 data=dat.gla)

m2018<- glmer.nb(sm.3 ~ Block.ID + Population + (1| Family.Unique),
                 data=dat.gla)

m.tot<- glmer.nb(tot.seeds ~ Block.ID + Population + (1| Family.Unique),
                  data=dat.gla)

summary(m2016)
summary(m2017)
summary(m2018)
summary(m.tot)

#estract family-level variance in seed mass

#2016
#extract variance component
vars2016 <- as.data.frame(VarCorr(m2016))
vars2016
print(VarCorr(m2016), comp = "Variance")

va2016.gla <-vars2016[1,4]
va2016.gla



Va2016<- log(4*va2016.gla)
Va2016

vp2016<- var(dat.gla$sm)
vp2016

h2.2016<- Va2016/vp2016
h2.2016 #0.0103

#2017
vars2017 <- as.data.frame(VarCorr(m2017))
vars2017
print(VarCorr(m2017), comp = "Variance")

va2017.gla <-vars2017[1,4]
va2017.gla

#2018
vars2018 <- as.data.frame(VarCorr(m2018))
vars2018
print(VarCorr(m2018), comp = "Variance")

va2018.gla <-vars2018[1,4]
va2018.gla

#2019
varstot <- as.data.frame(VarCorr(m.tot))
varstot
print(VarCorr(m.tot), comp = "Variance")

vatot.gla <-varstot[1,4]
vatot.gla


#multiply by 4 for Va (half-sib design/assumption),
#and 10^x for negative binomial link-function (log)

#2016
Va2016<- log(4*va2016.gla)
Va2016

vp2016<- var(dat.gla$sm)
vp2016

h2.2016<- Va2016/vp2016
h2.2016 

#2017
Va2017<- log(4*va2017.gla)
Va2017

vp2017<- var(dat.gla$sm.2)
vp2017

h2.2017<- Va2017/vp2017
h2.2017

#2018
Va2018<- log(4*va2018.gla)
Va2018

vp2018<- var(dat.gla$sm.3)
vp2018

h2.2018<- Va2018/vp2018
h2.2018

#total
Vatot<- log(4*vatot.gla)
Vatot

vptot<- var(dat.gla$tot.seeds)
vptot

h2.tot<- Vatot/vptot
h2.tot 

save.image("test_output.Rout")
