# CFA z knihovny sem
setwd("D:/user/Desktop/FactorAnalysis")
data.in <- read.csv("data.csv");str(data.in)
library(sem)

model.school <- specifyModel()
BIO -> prirodni, p1, NA
GEO -> prirodni, p2, NA
CHEM -> prirodni, p3, NA
ALG -> technicke, t1, NA
STAT -> technicke, t2, NA
CALC -> technicke, t3, NA
prirodni <-> technicke, psi, NA


cov.mat <- cov(data.in)

## CFA ##
model.cfa <- cfa()
prirodni: BIO, GEO, CHEM
technicke: ALG, STAT, CALC

n <- nrow(data.in)
cfa.fit <- sem(model.cfa, cov.mat, n)
plot.model <- semPlotModel(cfa.fit)
semPaths(plot.model)
summary(cfa.fit)


#--------- Hair CFA:
library(sem)
library(semPlot)
data.sem <- read.table("hair_sem.csv", header=TRUE);str(data.sem)
cfa.hair.model <- cfa()
job: OC1 , OC2 , OC3 , OC4
cow: AC1 , AC2 , AC3 , AC4
env: EP1 , EP2 , EP3 , EP4
int: SI1 , SI2 , SI3 , SI4
com: JS1 , JS2 , JS3 , JS4

cfa.hair.fit <- sem(cfa.hair.model, data=data.sem)
plot.hair <- semPlotModel(cfa.hair.fit)
semPaths(plot.hair, whatLabels="est", layout="spring", edge.label.cex=1)
summary(cfa.hair.fit)
pdf()
dev.off()










#---------- Nepoužito
hair.cov <- cov(data.sem[, -c(1,18)],use="complete.obs")
dim(hair.cov)
hair.cov%*%solve(hair.cov)
hair.cov <- cor(data.sem, use="complete.obs")
cfa.hair.fit <- sem(cfa.hair.model, hair.cov, N=399)

sem.school <- sem(model=model.school, data=data.in)
 
S=cov.mat, N=n

library(semPlot)
help(package=semPlot)

plot.model <- semPlotModel(cfa.fit)
semPaths(plot.model)


def.model <- semPlotModel_lavaanModel(str.1)
semPaths(def.model)
model.1