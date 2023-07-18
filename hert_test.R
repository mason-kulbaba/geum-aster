#This is a test code to use the Hamilton Lab Server

#set Thelio working directory
setwd("C:/Users/mason/Dropbox/git/geum-aster")

dat<- read.csv("final_data.csv")

#install.packages("MASS")

library(MASS)

dat2<- subset(dat, Germination.Y.N== 1)

dat2$tot.seeds<- dat2$sm + dat2$sm.2 + dat2$sm.3

dat.gla<- subset(dat2, Region=="GL_alvar")

#install.packages("lme4")
library(lme4)

#what dist for 2016 seedmass?

sm.p<- fitdistr(dat.gla$sm, "Poisson")
sm.n<- fitdistr(dat.gla$sm, "normal")
sm.nb<- fitdistr(dat.gla$sm, "Negative Binomial")

AIC(sm.p, sm.n, sm.nb)

#what dist for 2017 seedmass?
sm.p<- fitdistr(dat.gla$sm.2, "Poisson")
sm.n<- fitdistr(dat.gla$sm.2, "normal")
sm.nb<- fitdistr(dat.gla$sm.2, "Negative Binomial")

AIC(sm.p, sm.n, sm.nb)

#make block a factor

dat.gla$Block.ID<- as.factor(dat.gla$Block.ID)

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
                 REML=TRUE, maxiter=5000, data=dat.gla)

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
