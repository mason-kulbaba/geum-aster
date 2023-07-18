setwd("C:/Users/mason/Dropbox/git/geum-aster/")

dat<- read.csv("C:/Users/mason/Dropbox/git/geum-aster/final_data.csv")


library(MASS)

ste <- function(x) sd(x)/sqrt(length(x))

dat2<- subset(dat, Germination.Y.N== 1)

dat2$tot.seeds<- dat2$sm + dat2$sm.2 + dat2$sm.3

# great lakes alvars
dat.gla<- subset(dat2, Region=="GL_alvar")

mean(dat.gla$No.Days.to.Germ) # 10.8
ste(dat.gla$No.Days.to.Germ) # 0.08

gla.tl<- na.omit(dat.gla$No.Days.to.TrueLeaf)

mean(gla.tl) # 18.4
ste(gla.tl) # 0.09

gla.ff<- na.omit(dat.gla$no.Planting.to.DTFF)

mean(gla.ff) # 265.21
ste(gla.ff) # 1.83


gla.tf<- na.omit(dat.gla$Total.Flowers.2017)

mean(gla.tf) # 11.5
ste(gla.tf) # 0.28

gla.seed<- na.omit(dat.gla$sm2018)

mean(gla.seed) # 1212.9
ste(gla.seed) # 42.05

#MB alvar
dat.mb<- subset(dat2, Region=="MB_alvar")

mean(dat.mb$No.Days.to.Germ) # 11.6
ste(dat.mb$No.Days.to.Germ) # 0.30

mb.tl<- na.omit(dat.mb$No.Days.to.TrueLeaf)

mean(mb.tl) # 19
ste(mb.tl) # 0.31

mb.ff<- na.omit(dat.mb$no.Planting.to.DTFF)

mean(gla.ff) # 271.1
ste(gla.ff) # 6.61

mb.tf<- na.omit(dat.mb$Total.Flowers.2017)

mean(mb.tf) # 5.2
ste(mb.tf) # 0.37

mb.seed<- na.omit(dat.mb$sm2018)

mean(mb.seed) # 311.8
ste(mb.seed) # 35.53

#Prairie regions
dat.pra<- subset(dat2, Region=="Prairie")

dat.pra<- na.omit(dat.pra$No.Days.to.Germ)

mean(dat.pra) # 14.4
ste(dat.pra) # 0.33

dat.pra<- subset(dat2, Region=="Prairie")

pra.tl<- na.omit(dat.pra$No.Days.to.TrueLeaf)

mean(pra.tl) # 21.46
ste(pra.tl) # 0.34

pra.ff<- na.omit(dat.pra$no.Planting.to.DTFF)

mean(pra.ff) # 266.1
ste(pra.ff) # 5.44

pra.tf<- na.omit(dat.pra$Total.Flowers.2017)

mean(pra.tf) # 311.8
ste(pra.tf) # 35.53

pra.seed<- na.omit(dat.pra$sm2018)

mean(pra.seed) # 367.3
ste(pra.seed) # 32.74


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


m2016<- lmer(sm ~ Block.ID +(1|Population) + (1|Family.Unique)  
             + Block.ID : (1|Family.Unique), 
           data=dat.gla)

summary(m2016)

print(VarCorr(m2016), comp="Variance")


m2017<- glmer.nb(sm.2 ~ Block.ID + Population + (1| Family.Unique),
                 data=dat.gla)

m2018<- glmer.nb(sm.3 ~ Block.ID + Population + (1| Family.Unique),
                 data=dat.gla)

m.tot<- lmer(tot.seeds ~ Block.ID +(1|Population) + (1|Family.Unique)  
             + Block.ID : (1|Family.Unique),
             data=dat.gla)

print(VarCorr(m.tot), comp="Variance")

summary(m2016)
summary(m2017)
summary(m2018)
summary(m.tot)

#estract family-level variance in seed mass

print(VarCorr(m2016), comp="Variance")


#2016
#extract variance component
vars2016 <- as.data.frame(VarCorr(m2016), comp="Variance")
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
