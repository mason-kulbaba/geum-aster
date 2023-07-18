#Load data and run code that cleans 2016 & 2017 data.

setwd("C:/Users/Mason Kulbaba/Dropbox/git/geum-aster")

#load data
dat<- read.csv("cleaned_data_for_aster_with_predictors.csv")

#sum seed mass across all three years
dat$sm.tot<- dat$sm + dat$sm.2 + dat$sm.3


#load pca data
pca<- read.csv("pca.csv")


#merge PCA data into dat2
dat4<- merge(dat, pca)

dat<-dat4


library(MASS)
library(emmeans)
#####################################################################

# Relationship b/w PC1 & traits?


t<- glm.nb(No.Days.to.Germ ~ PC1, data=dat)

t2<- glm.nb(No.Days.to.Germ ~ PC1 + PC2, data=dat)

AIC(t, t2)

summary(t2)

#####################################################################

# General analysis of flower number

flw.mod<- glm.nb(No.Flowers.2016 ~ Region + Population + Region*Population, data=dat)

summary(flw.mod)

emmeans(flw.mod,"Region",type="response")

pairs(emmeans(flw.mod,"Region",type="response"))
test(emmeans(flw.mod,"Region",type="response"))

plot(emmeans(flw.mod,"Region",type="response"))


## 2017
flw.mod2<- glm.nb(Total.Flowers.2017 ~ Region + Population + Region*Population, data=dat)


summary(flw.mod2)

emmeans(flw.mod2,"Region",type="response")

pairs(emmeans(flw.mod2,"Region",type="response"))
test(emmeans(flw.mod2,"Region",type="response"))

plot(emmeans(flw.mod2,"Region",type="response"))

## 2018
flw.mod3<- glm.nb(Total.Flowers.2018 ~ Region + Population + Region*Population, data=dat)


summary(flw.mod3)

emmeans(flw.mod3,"Region",type="response")

pairs(emmeans(flw.mod3,"Region",type="response"))
test(emmeans(flw.mod3,"Region",type="response"))

plot(emmeans(flw.mod3,"Region",type="response"))

###################################################################################
#
# GLA analysis
#


dat.gla<- subset(dat, Region=="GL_alvar")

flw.mod.gla<- glm.nb(No.Flowers.2016 ~  Population, data=dat.gla)

summary(flw.mod.gla)

gla.2016<-emmeans(flw.mod.gla,"Population",type="response")

pairs(emmeans(flw.mod.gla,"Population",type="response"))
test(emmeans(flw.mod.gla,"Population",type="response"))

plot(emmeans(flw.mod.gla,"Population",type="response"))

write.table(gla.2016, file="C:/Users/Mason Kulbaba/Dropbox/git/geum-aster/flw_results/gla_2016.csv",
                               sep=",", quote=F, row.names=F)

# 2017

flw.mod.gla2<- glm.nb(Total.Flowers.2017 ~  Population, data=dat.gla)

summary(flw.mod.gla2)

gla.2017<-emmeans(flw.mod.gla2,"Population",type="response")

pairs(emmeans(flw.mod.gla2,"Population",type="response"))
test(emmeans(flw.mod.gla2,"Population",type="response"))

plot(emmeans(flw.mod.gla2,"Population",type="response"))


write.table(gla.2017, file="C:/Users/Mason Kulbaba/Dropbox/git/geum-aster/flw_results/gla_2017.csv",
            sep=",", quote=F, row.names=F)

# 2018

flw.mod.gla3<- glm.nb(Total.Flowers.2018 ~  Population, data=dat.gla)

summary(flw.mod.gla3)

gla.2018<- emmeans(flw.mod.gla3,"Population",type="response")

pairs(emmeans(flw.mod.gla3,"Population",type="response"))
test(emmeans(flw.mod.gla3,"Population",type="response"))

plot(emmeans(flw.mod.gla3,"Population",type="response"))


write.table(gla.2018, file="C:/Users/Mason Kulbaba/Dropbox/git/geum-aster/flw_results/gla_2018.csv",
            sep=",", quote=F, row.names=F)

###################################################################################
#
# prairie analysis
#


dat.pra<- subset(dat, Region=="Prairie")

flw.mod.pra<- glm.nb(No.Flowers.2016 ~  Population, data=dat.pra)

summary(flw.mod.pra)

pra.2016<-emmeans(flw.mod.pra,"Population",type="response")

pairs(emmeans(flw.mod.pra,"Population",type="response"))
test(emmeans(flw.mod.pra,"Population",type="response"))

plot(emmeans(flw.mod.pra,"Population",type="response"))

write.table(pra.2016, file="C:/Users/Mason Kulbaba/Dropbox/git/geum-aster/flw_results/pra_2016.csv",
            sep=",", quote=F, row.names=F)

# 2017

flw.mod.pra2<- glm.nb(Total.Flowers.2017 ~  Population, data=dat.pra)

summary(flw.mod.pra2)

pra.2017<-emmeans(flw.mod.pra2,"Population",type="response")

pairs(emmeans(flw.mod.pra2,"Population",type="response"))
test(emmeans(flw.mod.pra2,"Population",type="response"))

plot(emmeans(flw.mod.pra2,"Population",type="response"))


write.table(pra.2017, file="C:/Users/Mason Kulbaba/Dropbox/git/geum-aster/flw_results/pra_2017.csv",
            sep=",", quote=F, row.names=F)

# 2018

flw.mod.pra3<- glm.nb(Total.Flowers.2018 ~  Population, data=dat.pra)

summary(flw.mod.pra3)

pra.2018<- emmeans(flw.mod.pra3,"Population",type="response")

pairs(emmeans(flw.mod.pra3,"Population",type="response"))
test(emmeans(flw.mod.pra3,"Population",type="response"))

plot(emmeans(flw.mod.pra3,"Population",type="response"))


write.table(pra.2018, file="C:/Users/Mason Kulbaba/Dropbox/git/geum-aster/flw_results/pra_2018.csv",
            sep=",", quote=F, row.names=F)

###################################################################################
#
# MB alvar analysis
#


dat.mb<- subset(dat, Region=="MB_alvar")

flw.mod.mba<- glm.nb(No.Flowers.2016 ~  Population, data=dat.mb)

summary(flw.mod.mba)

mb.2016<-emmeans(flw.mod.mba,"Population",type="response")

pairs(emmeans(flw.mod.mba,"Population",type="response"))
test(emmeans(flw.mod.mba,"Population",type="response"))

plot(emmeans(flw.mod.mba,"Population",type="response"))

write.table(mb.2016, file="C:/Users/Mason Kulbaba/Dropbox/git/geum-aster/flw_results/mba_2016.csv",
            sep=",", quote=F, row.names=F)

# 2017

flw.mod.mb2<- glm.nb(Total.Flowers.2017 ~  Population, data=dat.mb)

summary(flw.mod.mb2)

mb.2017<-emmeans(flw.mod.mb2,"Population",type="response")

pairs(emmeans(flw.mod.mb2,"Population",type="response"))
test(emmeans(flw.mod.mb2,"Population",type="response"))

plot(emmeans(flw.mod.mb2,"Population",type="response"))


write.table(mb.2017, file="C:/Users/Mason Kulbaba/Dropbox/git/geum-aster/flw_results/mba_2017.csv",
            sep=",", quote=F, row.names=F)

# 2018

flw.mod.mb3<- glm.nb(Total.Flowers.2018 ~  Population, data=dat.mb)

summary(flw.mod.pra3)

mb.2018<- emmeans(flw.mod.mb3,"Population",type="response")

pairs(emmeans(flw.mod.mb3,"Population",type="response"))
test(emmeans(flw.mod.mb3,"Population",type="response"))

plot(emmeans(flw.mod.mb3,"Population",type="response"))


write.table(mb.2018, file="C:/Users/Mason Kulbaba/Dropbox/git/geum-aster/flw_results/mba_2018.csv",
            sep=",", quote=F, row.names=F)
