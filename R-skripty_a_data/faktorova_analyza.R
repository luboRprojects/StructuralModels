setwd("D:/user/Desktop/FactorAnalysis")
data.in <- read.csv("data.csv");str(data.in)

library(psych)
cor.mat <- cor(data.in)
pca <- principal(cor.mat, nfactors=3)
plot(pca)

factor.analysis <- fa(data.in, nfactors=2)
factor.analysis
plot(factor.analysis)

summary(princomp(cor.mat))

EFA.3f <- factanal(data.in ,factors=3)
EFA.3f

EFA.2f <- factanal(data.in ,factors=2)
EFA.2f


factanal(data.in ,factors=2)

# Confirmatory Factor Analysis
library(lavaan)

str.1 <- "prirodni =~ BIO + GEO + CHEM
	technicke =~ ALG  + STAT  + CALC"
model.1 <- cfa(str.1, data=data.in, std.lv=FALSE)
summary(model.1, fit.measures = TRUE)
plot(model.1)