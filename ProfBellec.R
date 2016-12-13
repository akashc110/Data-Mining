
rm(list=ls())
library(ggplot2)
library(glmnet)
library(quadprog)
library(tseries)

smallcap <- read.csv("Small50.csv")[,-c(1,2)]
midcap <- read.csv("Mid50.csv")[,-c(1,2)]
largecap <- read.csv("Large50.csv")[,-c(1,2)]
benchmark <- read.csv("Benchmark.csv")[-1]
names(benchmark) <- "SP500"

universe <- cbind(smallcap,midcap,largecap)

assets <- ncol(universe)

cap <- cbind(benchmark,universe)
last = nrow(cap)
returns = (cap[-1,]/cap[-last,]) - 1 #Computes simple monthly returns

#Split training and testing data
returns.train <- returns[1:59,]
returns.test <- returns[60:71,]

asset.returns = returns.train[,-1]
benchmark.returns = returns.train[,1]               #Benchmark returns

mean.asset.returns = apply(asset.returns,2,mean)  #Calculate mean of every asset
exp.benchmark = mean(benchmark.returns)           #Calculate benchmark mean

Dmat1 = cov(asset.returns)                        #Compute covariance matrix of assets

#Tuning parameter lambda. Smaller the value more the zeros

lambda = 0.01
Dmat = matrix(Dmat1[1:assets, 1:assets], nrow = assets, ncol = assets)  + lambda*(diag(assets))

#Each assets expected return
dvec = matrix(mean.asset.returns[1:assets], nrow = assets, ncol = 1)  
#Constraint for sum of weights =1
A.Equality <- matrix(rep(1,assets), ncol=1)
#Amat constraints: sum=1, dvec == benchmark returns, weights>=0, weights <= 2/(*assets)
Amat <- cbind(A.Equality, dvec, diag(assets), -diag(assets))
bvec <- c(1, exp.benchmark, rep(10^(-12), assets),rep(-2/assets, assets))

qp <- solve.QP(Dmat, dvec, Amat, bvec, meq=1) 
weights <- qp$solution
range(qp$solution)

ptf_weights <- function(bin, weights){
  x = sample(1:150,size = bin, replace = T, prob =  weights)
  print(x)
  wt = tabulate(x)/length(x)
  return(data.frame(index = which(wt!=0), weights =wt[which(wt!=0)] )) 
}

#Code to calculate portfolio returns for 12 months of year 2015
d = matrix(0,12,3)
for (i in 1:12) {
  portfolio = ptf_weights(30,weights)
  returns.T <- returns.test[i,1]
  returns.predict = returns.test[i,-1]
  portfolio.returns <- t(as.matrix(portfolio$weights))%*%t(returns.predict[portfolio$index])
  result <- ifelse(portfolio.returns>=returns.T,"Good","Poor")
  d[i,] <- t(c(returns.T,portfolio.returns, result))
  
}

df= as.data.frame(d)
names(df) <- c("Benchmark","Portfolio","Performance")



