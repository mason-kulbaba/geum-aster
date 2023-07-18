setwd("C:/Users/Mason Kulbaba/Dropbox/git/geum-aster/")

dat<- read.csv("C:/Users/Mason Kulbaba/Dropbox/git/geum-aster/final_data.csv")

library(MASS)

dat2<- subset(dat, Germination.Y.N== 1)

# combine 2016 + 2017 sm to reflect how fitness was estiamted in 2017
dat2$sm.2b<- dat2$sm + dat2$sm.2

dat2$tot.seeds<- dat2$sm + dat2$sm.2 + dat2$sm.3

dat.gla<- subset(dat2, Region=="GL_alvar")
dat.mba<- subset(dat2, Region=="MB_alvar")
dat.pr<- subset(dat2, Region=="Prairie")


#dists for p->Germ

library(MASS)

norm<- fitdistr(dat.gla$Planting.to.DTFF.2017, "normal")
poi<- fitdistr(dat.gla$Planting.to.DTFF.2017, "Poisson")
negb<- fitdistr(dat.gla$Planting.to.DTFF.2017, "negative binomial")


AIC(norm, poi, negb)



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
# Number of tot.seeds: GL, MB, and prairie

#models for : GL_alvar
gla_tot.seeds.1<- lmer(tot.seeds ~ Block.ID + Population + (1| Family.Unique),
                     data=dat.gla)

gla_tot.seeds.2<- glmer(tot.seeds ~ Block.ID + Population + (1| Family.Unique),
                      data=dat.gla, family="poisson", maxiter=500)

gla_tot.seeds.3<- glmer.nb(tot.seeds ~ Block.ID + Population + (1| Family.Unique),
                         data=dat.gla, maxiter=500)

summary(gla_tot.seeds.1)
summary(gla_tot.seeds.2)
summary(gla_tot.seeds.3)

AIC(gla_tot.seeds.1)
AIC(gla_tot.seeds.2)
AIC(gla_tot.seeds.3)


#models for germination: MB_alvar
mba_tot.seeds.1<- lmer(tot.seeds ~ Block.ID + Population + (1| Family.Unique),
                     data=dat.mba)

mba_tot.seeds.2<- glmer(tot.seeds ~ Block.ID + Population + (1| Family.Unique),
                      data=dat.mba, family="poisson", maxiter=500)

mba_tot.seeds.3<- glmer.nb(tot.seeds ~ Block.ID + Population + (1| Family.Unique),
                         data=dat.mba, maxiter=500)

summary(mba_tot.seeds.1)
summary(mba_tot.seeds.2)
summary(mba_tot.seeds.3)

AIC(mba_tot.seeds.1)
AIC(mba_tot.seeds.2)
AIC(mba_tot.seeds.3)


#models for germination: Prairie
pr_tot.seeds.1<- lmer(tot.seeds ~ Block.ID + Population + (1| Family.Unique),
                    data=dat.pr)

pr_tot.seeds.2<- glmer(tot.seeds ~ Block.ID + Population + (1| Family.Unique),
                     data=dat.pr, family="poisson", maxiter=500)

pr_tot.seeds.3<- glmer.nb(tot.seeds ~ Block.ID + Population + (1| Family.Unique),
                        data=dat.pr, maxiter=500)

summary(pr_tot.seeds.1)
summary(pr_tot.seeds.2)
summary(pr_tot.seeds.3)

AIC(pr_tot.seeds.1)
AIC(pr_tot.seeds.2)
AIC(pr_tot.seeds.3)

#estract family-level variance in seed mass

#GL_alvar 
#extract variance component - gaussian dist
glavars.tot.seeds <- as.data.frame(VarCorr(gla_tot.seeds.3))
glavars.tot.seeds
print(VarCorr(gla_tot.seeds.3), comp = "Variance")

va.gla.tot.seeds <-glavars.tot.seeds[1,4]
va.gla.tot.seeds

#convert to response scale -
Va.gla.tot.seeds<- log(va.gla.tot.seeds/(1-va.gla.tot.seeds))
Va.gla.tot.seeds

#remove NAs for calc of var

days.tot.seeds <- dat.gla$tot.seeds[!is.na(dat.gla$tot.seeds)]

vp.gla.tot.seeds<- var(days.tot.seeds)
vp.gla.tot.seeds

h2.gla.tot.seeds<- Va.gla.tot.seeds/vp.gla.tot.seeds
h2.gla.tot.seeds #0


#MB_alvar - poisson
mbavars.tot.seeds <- as.data.frame(VarCorr(mba_tot.seeds.3))
mbavars.tot.seeds
print(VarCorr(mba_tot.seeds.3), comp = "Variance")

va.mba.tot.seeds <-mbavars.tot.seeds[1,4]
va.mba.tot.seeds

#convert to response scale -
Va.mba.tot.seeds<- log(va.mba.tot.seeds/(1-va.mba.tot.seeds))
Va.mba.tot.seeds

#remove NAs for calc of var

days.tot.seeds <- dat.mba$tot.seeds[!is.na(dat.mba$tot.seeds)]

vp.mba.tot.seeds<- var(days.tot.seeds)
vp.mba.tot.seeds

h2.mba.tot.seeds<- Va.mba.tot.seeds/vp.mba.tot.seeds
h2.mba.tot.seeds #0


#Prairie - Poisson
prvars.tot.seeds <- as.data.frame(VarCorr(pr_tot.seeds.3))
prvars.tot.seeds
print(VarCorr(pr_tot.seeds.3), comp = "Variance")

va.pr.tot.seeds <-mbavars.tot.seeds[1,4]
va.pr.tot.seeds

#convert to response scale - antilog
Va.pr.tot.seeds<- 10^(va.pr.tot.seeds)
Va.pr.tot.seeds

#remove NAs for calc of var

days.tot.seeds <- dat.pr$tot.seeds[!is.na(dat.pr$tot.seeds)]

vp.pr.tot.seeds<- var(days.tot.seeds)
vp.pr.tot.seeds

h2.pr.tot.seeds<- Va.pr.tot.seeds/vp.pr.tot.seeds
h2.pr.tot.seeds #0

##################################################################################
# 2016 Seed Mass: sm

#models for : GL_alvar
gla_sm.1<- lmer(sm ~ Block.ID + Population + (1| Family.Unique),
                       data=dat.gla)

gla_sm.2<- glmer(sm ~ Block.ID + Population + (1| Family.Unique),
                        data=dat.gla, family="poisson", maxiter=500)

gla_sm.3<- glmer.nb(sm ~ Block.ID + Population + (1| Family.Unique),
                           data=dat.gla, maxiter=500)

summary(gla_sm.1)
summary(gla_sm.2)
summary(gla_sm.3)

AIC(gla_sm.1)
AIC(gla_sm.2)
AIC(gla_sm.3)


#models for germination: MB_alvar
mba_sm.1<- lmer(sm ~ Block.ID + Population + (1| Family.Unique),
                       data=dat.mba)

mba_sm.2<- glmer(sm ~ Block.ID + Population + (1| Family.Unique),
                        data=dat.mba, family="poisson", maxiter=500)

mba_sm.3<- glmer.nb(sm ~ Block.ID + Population + (1| Family.Unique),
                           data=dat.mba, maxiter=500)

summary(mba_sm.1)
summary(mba_sm.2)
summary(mba_sm.3)

AIC(mba_sm.1)
AIC(mba_sm.2)
AIC(mba_sm.3)


#models for germination: Prairie
pr_sm.1<- lmer(sm ~ Block.ID + Population + (1| Family.Unique),
                      data=dat.pr)

pr_sm.2<- glmer(sm ~ Block.ID + Population + (1| Family.Unique),
                       data=dat.pr, family="poisson", maxiter=500)

pr_sm.3<- glmer.nb(sm ~ Block.ID + Population + (1| Family.Unique),
                          data=dat.pr, maxiter=500)

summary(pr_sm.1)
summary(pr_sm.2)
summary(pr_sm.3)

AIC(pr_sm.1)
AIC(pr_sm.2)
AIC(pr_sm.3)

#estract family-level variance in seed mass

#GL_alvar 
#extract variance component - gaussian dist
glavars.sm <- as.data.frame(VarCorr(gla_sm.1))
glavars.sm
print(VarCorr(gla_sm.1), comp = "Variance")

va.gla.sm <-glavars.sm[1,4]
va.gla.sm

#convert to response scale - identity
Va.gla.sm<- va.gla.sm

Va.gla.sm

#remove NAs for calc of var

days.sm <- dat.gla$sm[!is.na(dat.gla$sm)]

vp.gla.sm<- var(days.sm)
vp.gla.sm

h2.gla.sm<- Va.gla.sm/vp.gla.sm
h2.gla.sm #0.012


#MB_alvar - poisson
mbavars.sm <- as.data.frame(VarCorr(mba_sm.1))
mbavars.sm
print(VarCorr(mba_sm.1), comp = "Variance")

va.mba.sm <-mbavars.sm[1,4]
va.mba.sm

#convert to response scale -
Va.mba.sm<- log10(va.mba.sm)
Va.mba.sm

#remove NAs for calc of var

days.sm <- dat.mba$sm[!is.na(dat.mba$sm)]

vp.mba.sm<- var(days.sm)
vp.mba.sm

h2.mba.sm<- Va.mba.sm/vp.mba.sm
h2.mba.sm #0


#Prairie - Poisson
prvars.sm <- as.data.frame(VarCorr(pr_sm.1))
prvars.sm
print(VarCorr(pr_sm.1), comp = "Variance")

va.pr.sm <-prvars.sm[1,4]
va.pr.sm

#convert to response scale - identity
Va.pr.sm<- va.pr.sm
Va.pr.sm

#remove NAs for calc of var

days.sm <- dat.pr$sm[!is.na(dat.pr$sm)]

vp.pr.sm<- var(days.sm)
vp.pr.sm

h2.pr.sm<- Va.pr.sm/vp.pr.sm
h2.pr.sm#0

##################################################################################
# 2017 Seed Mass: sm.2

#models for : GL_alvar
gla_sm.2.1<- lmer(sm.2 ~ Block.ID + Population + (1| Family.Unique),
                data=dat.gla)

gla_sm.2.2<- glmer(sm.2 ~ Block.ID + Population + (1| Family.Unique),
                 data=dat.gla, family="poisson", maxiter=500)

gla_sm.2.3<- glmer.nb(sm.2 ~ Block.ID + Population + (1| Family.Unique),
                    data=dat.gla, maxiter=500)

summary(gla_sm.2.1)
summary(gla_sm.2.2)
summary(gla_sm.2.3)

AIC(gla_sm.2.1)
AIC(gla_sm.2.2)
AIC(gla_sm.2.3)


#models for germination: MB_alvar
mba_sm.2.1<- lmer(sm.2 ~ Block.ID + Population + (1| Family.Unique),
                data=dat.mba)

mba_sm.2.2<- glmer(sm.2 ~ Block.ID + Population + (1| Family.Unique),
                 data=dat.mba, family="poisson", maxiter=500)

mba_sm.2.3<- glmer.nb(sm.2 ~ Block.ID + Population + (1| Family.Unique),
                    data=dat.mba, maxiter=500)

summary(mba_sm.2.1)
summary(mba_sm.2.2)
summary(mba_sm.2.3)

AIC(mba_sm.2.1)
AIC(mba_sm.2.2)
AIC(mba_sm.2.3)


#models for germination: Prairie
pr_sm.2.1<- lmer(sm.2 ~ Block.ID + Population + (1| Family.Unique),
               data=dat.pr)

pr_sm.2.2<- glmer(sm.2 ~ Block.ID + Population + (1| Family.Unique),
                data=dat.pr, family="poisson", maxiter=500)

pr_sm.2.3<- glmer.nb(sm.2 ~ Block.ID + Population + (1| Family.Unique),
                   data=dat.pr, maxiter=500)

summary(pr_sm.2.1)
summary(pr_sm.2.2)
summary(pr_sm.2.3)

AIC(pr_sm.2.1)
AIC(pr_sm.2.2)
AIC(pr_sm.2.3)

#estract family-level variance in seed mass

#GL_alvar 
#extract variance component - gaussian dist
glavars.sm.2 <- as.data.frame(VarCorr(gla_sm.2.3))
glavars.sm.2
print(VarCorr(gla_sm.2.3), comp = "Variance")

va.gla.sm.2 <-glavars.sm.2[1,4]
va.gla.sm.2

#convert to response scale -
Va.gla.sm.2<- 10^va.gla.sm.2

Va.gla.sm.2

#remove NAs for calc of var

days.sm.2 <- dat.gla$sm.2[!is.na(dat.gla$sm.2)]

vp.gla.sm.2<- var(days.sm.2)
vp.gla.sm.2

h2.gla.sm.2<- Va.gla.sm.2/vp.gla.sm.2
h2.gla.sm.2 #0


#MB_alvar - poisson
mbavars.sm.2 <- as.data.frame(VarCorr(mba_sm.2.1))
mbavars.sm.2
print(VarCorr(mba_sm.2.3), comp = "Variance")

va.mba.sm.2 <-mbavars.sm.2[1,4]
va.mba.sm.2

#convert to response scale -
Va.mba.sm.2<- log(va.mba.sm.2)
Va.mba.sm.2

#remove NAs for calc of var

days.sm.2 <- dat.mba$sm.2[!is.na(dat.mba$sm.2)]

vp.mba.sm.2<- var(days.sm.2)
vp.mba.sm.2

h2.mba.sm.2<- Va.mba.sm.2/vp.mba.sm.2
h2.mba.sm.2 #0


#Prairie - Poisson
prvars.sm.2 <- as.data.frame(VarCorr(pr_sm.2.3))
prvars.sm.2
print(VarCorr(pr_tot.sm.2.3), comp = "Variance")

va.pr.sm.2 <-prvars.sm.2[1,4]
va.pr.sm.2

#convert to response scale - identity
Va.pr.sm.2<- va.pr.sm.2
Va.pr.sm.2

#remove NAs for calc of var

days.sm.2 <- dat.pr$sm.2[!is.na(dat.pr$sm.2)]

vp.pr.sm.2<- var(days.sm.2)
vp.pr.sm.2

h2.pr.sm.2<- Va.pr.sm.2/vp.pr.sm.2
h2.pr.sm.2#0

##################################################################################
# 2018 Seed Mass: sm.3

#models for : GL_alvar
gla_sm.3.1<- lmer(sm.3 ~ Block.ID + Population + (1| Family.Unique),
                  data=dat.gla)

gla_sm.3.2<- glmer(sm.3 ~ Block.ID + Population + (1| Family.Unique),
                   data=dat.gla, family="poisson", maxiter=500)

gla_sm.3.3<- glmer.nb(sm.3 ~ Block.ID + Population + (1| Family.Unique),
                      data=dat.gla, maxiter=500)

summary(gla_sm.3.1)
summary(gla_sm.3.2)
summary(gla_sm.3.3)

AIC(gla_sm.3.1)
AIC(gla_sm.3.2)
AIC(gla_sm.3.3)


#models for germination: MB_alvar
mba_sm.3.1<- lmer(sm.3 ~ Block.ID + Population + (1| Family.Unique),
                  data=dat.mba)

mba_sm.3.2<- glmer(sm.3 ~ Block.ID + Population + (1| Family.Unique),
                   data=dat.mba, family="poisson", maxiter=500)

mba_sm.3.3<- glmer.nb(sm.3 ~ Block.ID + Population + (1| Family.Unique),
                      data=dat.mba, maxiter=500)

summary(mba_sm.3.1)
summary(mba_sm.3.2)
summary(mba_sm.3.3)

AIC(mba_sm.3.1)
AIC(mba_sm.3.2)
AIC(mba_sm.3.3)


#models for germination: Prairie
pr_sm.3.1<- lmer(sm.3 ~ Block.ID + Population + (1| Family.Unique),
                 data=dat.pr)

pr_sm.3.2<- glmer(sm.3 ~ Block.ID + Population + (1| Family.Unique),
                  data=dat.pr, family="poisson", maxiter=500)

pr_sm.3.3<- glmer.nb(sm.3 ~ Block.ID + Population + (1| Family.Unique),
                     data=dat.pr, maxiter=500)

summary(pr_sm.3.1)
summary(pr_sm.3.2)
summary(pr_sm.3.3)

AIC(pr_sm.3.1)
AIC(pr_sm.3.2)
AIC(pr_sm.3.3)

#estract family-level variance in seed mass

#GL_alvar 
#extract variance component - gaussian dist
glavars.sm.3 <- as.data.frame(VarCorr(gla_sm.3.3))
glavars.sm.3
print(VarCorr(gla_sm.3.3), comp = "Variance")

va.gla.sm.3 <-glavars.sm.3[1,4]
va.gla.sm.3

#convert to response scale -
Va.gla.sm.3<- 10^va.gla.sm.3

Va.gla.sm.3

#remove NAs for calc of var

days.sm.3 <- dat.gla$sm.3[!is.na(dat.gla$sm.3)]

vp.gla.sm.3<- var(days.sm.3)
vp.gla.sm.3

h2.gla.sm.3<- Va.gla.sm.3/vp.gla.sm.3
h2.gla.sm.3 #


#MB_alvar - poisson
mbavars.sm.3 <- as.data.frame(VarCorr(mba_sm.3.3))
mbavars.sm.3
print(VarCorr(mba_sm.3.3), comp = "Variance")

va.mba.sm.3 <-mbavars.sm.3[1,4]
va.mba.sm.3

#convert to response scale -
Va.mba.sm.3<- 10^(va.mba.sm.3)
Va.mba.sm.3

#remove NAs for calc of var

days.sm.3 <- dat.mba$sm.3[!is.na(dat.mba$sm.3)]

vp.mba.sm.3<- var(days.sm.3)
vp.mba.sm.3

h2.mba.sm.3<- Va.mba.sm.3/vp.mba.sm.3
h2.mba.sm.3 #0


#Prairie - Poisson
prvars.sm.3 <- as.data.frame(VarCorr(pr_sm.3.3))
prvars.sm.3
print(VarCorr(pr_sm.3.3), comp = "Variance")

va.pr.sm.3 <-mbavars.sm.3[1,4]
va.pr.sm.3

#convert to response scale - identity
Va.pr.sm.3<-10^ va.pr.sm.3
Va.pr.sm.3

#remove NAs for calc of var

days.sm.3 <- dat.pr$sm.3[!is.na(dat.pr$sm.3)]

vp.pr.sm.3<- var(days.sm.3)
vp.pr.sm.3

h2.pr.sm.3<- Va.pr.sm.3/vp.pr.sm.3
h2.pr.sm.3#0

##################################################################################
# 2017 Seed Mass: sm.2b

# NOTE: sm.2b = sm + sm.2. This better reflets how fitness was estiamted

#models for : GL_alvar
gla_sm.2b.1<- lmer(sm.2b ~ Block.ID + Population + (1| Family.Unique),
                  data=dat.gla)

gla_sm.2b.2<- glmer(sm.2b ~ Block.ID + Population + (1| Family.Unique),
                   data=dat.gla, family="poisson", maxiter=500)

gla_sm.2b.3<- glmer.nb(sm.2b ~ Block.ID + Population + (1| Family.Unique),
                      data=dat.gla, maxiter=500)

summary(gla_sm.2b.1)
summary(gla_sm.2b.2)
summary(gla_sm.2b.3)

AIC(gla_sm.2b.1)
AIC(gla_sm.2b.2)
AIC(gla_sm.2b.3)


#models for germination: MB_alvar
mba_sm.2b.1<- lmer(sm.2b ~ Block.ID + Population + (1| Family.Unique),
                  data=dat.mba)

mba_sm.2b.2<- glmer(sm.2b ~ Block.ID + Population + (1| Family.Unique),
                   data=dat.mba, family="poisson", maxiter=500)

mba_sm.2b.3<- glmer.nb(sm.2b ~ Block.ID + Population + (1| Family.Unique),
                      data=dat.mba, maxiter=500)

summary(mba_sm.2b.1)
summary(mba_sm.2b.2)
summary(mba_sm.2b.3)

AIC(mba_sm.2b.1)
AIC(mba_sm.2b.2)
AIC(mba_sm.2b.3)


#models for germination: Prairie
pr_sm.2b.1<- lmer(sm.2b ~ Block.ID + Population + (1| Family.Unique),
                 data=dat.pr)

pr_sm.2b.2<- glmer(sm.2b ~ Block.ID + Population + (1| Family.Unique),
                  data=dat.pr, family="poisson", maxiter=500)

pr_sm.2b.3<- glmer.nb(sm.2b ~ Block.ID + Population + (1| Family.Unique),
                     data=dat.pr, maxiter=500)

summary(pr_sm.2b.1)
summary(pr_sm.2b.2)
summary(pr_sm.2b.3)

AIC(pr_sm.2b.1)
AIC(pr_sm.2b.2)
AIC(pr_sm.2b.3)

#estract family-level variance in seed mass

#GL_alvar 
#extract variance component - gaussian dist
glavars.sm.2b <- as.data.frame(VarCorr(gla_sm.2b.3))
glavars.sm.2b
print(VarCorr(gla_sm.2b.3), comp = "Variance")

va.gla.sm.2b <-glavars.sm.2b[1,4]
va.gla.sm.2b

#convert to response scale -
Va.gla.sm.2b<- 10^(va.gla.sm.2b)

Va.gla.sm.2b

#remove NAs for calc of var

days.sm.2b <- dat.gla$sm.2b[!is.na(dat.gla$sm.2b)]

vp.gla.sm.2b<- var(days.sm.2b)
vp.gla.sm.2b

h2.gla.sm.2b<- Va.gla.sm.2b/vp.gla.sm.2b
h2.gla.sm.2b #0


#MB_alvar -
mbavars.sm.2b <- as.data.frame(VarCorr(mba_sm.2b.3))
mbavars.sm.2b
print(VarCorr(mba_sm.2b.3), comp = "Variance")

va.mba.sm.2b <-mbavars.sm.2b[1,4]
va.mba.sm.2b

#convert to response scale -
Va.mba.sm.2b<- 10^(va.mba.sm.2b)
Va.mba.sm.2b

#remove NAs for calc of var

days.sm.2b <- dat.mba$sm.2b[!is.na(dat.mba$sm.2b)]

vp.mba.sm.2b<- var(days.sm.2b)
vp.mba.sm.2b

h2.mba.sm.2b<- Va.mba.sm.2b/vp.mba.sm.2b
h2.mba.sm.2b #0.001 (0.0006)


#Prairie - Poisson
prvars.sm.2b <- as.data.frame(VarCorr(pr_sm.2b.3))
prvars.sm.2b
print(VarCorr(pr_sm.2b.3), comp = "Variance")

va.pr.sm.2b <-prvars.sm.2b[1,4]
va.pr.sm.2b

#convert to response scale - identity
Va.pr.sm.2b<-10^( va.pr.sm.2b)
Va.pr.sm.2b

#remove NAs for calc of var

days.sm.2b <- dat.pr$sm.2b[!is.na(dat.pr$sm.2b)]

vp.pr.sm.2b<- var(days.sm.2b)
vp.pr.sm.2b

h2.pr.sm.2b<- Va.pr.sm.2b/vp.pr.sm.2b
h2.pr.sm.2b#0.00
