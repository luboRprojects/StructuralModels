# Hair: CFA & lavaan
library(lavaan)
setwd("D:/user/Desktop/FactorAnalysis")

data.sem <- read.table("hair_sem.csv", header=TRUE);str(data.sem)

cfa.model <- "
job =~ OC1 + OC2 + OC3 + OC4
cow =~ AC1 + AC2 + AC3 + AC4
env =~ EP1 + EP2 + EP3 + EP4
int =~ SI1 + SI2 + SI3 + SI4
com =~ JS1 + JS2 + JS3 + JS4
"

cfa.fit <- cfa(cfa.model, data = data.sem)
	
library(semPlot)
sem.lav.plot <- semPlotModel_lavaanModel(cfa.model)
semPaths(sem.lav.plot)
summary(cfa.fit, fit.measures=TRUE)

inspect(cfa.fit,"cov.lv")
inspect(cfa.fit, "rsquare" ) #AVE

library(semTools)
reliability(cfa.fit)
ave.const

ave.dat <- data.frame(matrix(ave, ncol=4, byrow=TRUE))
ave.const <- data.frame(AVE=apply(ave.dat, 1, mean))
rownames(ave.const) <- c("Commitment","Coworkers","Environment","Intensions","Satisfaction")

#---------- Nepoužito
cov.mat <- cor(data.sem[, -c(1,18)],use="complete.obs")
cfa.fit <- cfa(cfa.model, sample.cov=cov.mat, sample.nobs=398,std.lv=FALSE)

library(psych)
fa(cov.mat,nfactors=5)