rm(list = ls())
library(tseries)
library(quadprog)

datafile = read.csv("StockData.csv")
datafile = datafile[,-1]

assets = 30
last = nrow(datafile)

returns = (datafile[-1,]/datafile[-last,]) - 1 #Computes simple monthly returns

asset.returns = returns[1:70,-1]
benchmark.returns = returns[1:70,1]
mean.asset.returns = apply(asset.returns,2,mean)
#exp.benchmark = mean(benchmark.returns)
exp.benchmark = mean(mean.asset.returns)
Dmat1 = cov(asset.returns)

Dmat = matrix(Dmat1[1:assets, 1:assets], nrow = assets, ncol = assets)  
dvec = matrix(mean.asset.returns[1:assets], nrow = assets, ncol = 1)
# #Constraint matrix formation
# A.Equality <- matrix(rep(1,assets), ncol=1)
# Amat <- cbind(A.Equality, dvec, diag(assets), -diag(assets))
# bvec <- c(1, exp.benchmark, rep(0, assets), rep(-1/assets, assets))
# qp <- solve.QP(Dmat, dvec, Amat, bvec, meq=1)
# qp$solution
################
A.Equality <- matrix(rep(1,assets), ncol=1)
Amat <- cbind(A.Equality, dvec, diag(assets), -diag(assets))
bvec <- c(1, exp.benchmark, rep(0, assets),rep(-2/assets, assets))
qp <- solve.QP(Dmat, dvec, Amat, bvec, meq=1)
qp$solution

betas = qp$solution
beta1 = ifelse(abs(betas)<10^(-3),0,betas)
beta1
length(beta1[beta1!=0])
