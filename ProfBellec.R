
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
  bin =30
  x = sample(1:150,size = bin, replace = T, prob =  weights)
  wt = tabulate(x)/length(x)
  table(wt)
  
  return(data.frame(index = which(wt!=0), weights =wt[which(wt!=0)] )) 
}

#Code to calculate portfolio returns for 12 months of year 2015
size =500
Parameters = matrix(ncol = 2, nrow = size)

for (j in 1:size) {
  
  d = matrix(0,12,4)  
  
  
  for (i in 1:12) {
      if(j<=100){portfolio = ptf_weights(30,weights)}
      if(j>100){portfolio = ptf_weights(60,weights)}
      if(j>200){portfolio = ptf_weights(90,weights)}
      if(j>300){portfolio = ptf_weights(120,weights)}
      if(j>400){portfolio = ptf_weights(135,weights)}
      
      companies <- nrow(portfolio)
      returns.T <- returns.test[i,1]
      returns.predict = returns.test[i,-1]
      portfolio.returns <- t(as.matrix(portfolio$weights))%*%t(returns.predict[portfolio$index])
      result <- ifelse(portfolio.returns>=returns.T,"Good","Poor")
      d[i,] <- t(c(companies  ,returns.T,portfolio.returns, result))
      
  }
  
  df= as.data.frame(d)
  names(df) <- c("Firms","Benchmark","Portfolio","Performance")
  
  Firms = as.numeric(as.character(df$Firms))
  BM = as.numeric(as.character(df$Benchmark))
  Ptf = as.numeric(as.character(df$Portfolio))
  
  Transaction.Cost = (mean(Firms))  
  Tracking.Error =(mean(Ptf - BM)^2)  
  
  Parameters[j,] =  c(Transaction.Cost, Tracking.Error)

}

Parameters = data.frame(Parameters)
names(Parameters) = c("TransactionCost", "TrackingError")
#plot(scale(Parameters$TransactionCost) ,scale(Parameters$TrackingError) )

plot(Parameters$TrackingError, col = 'green', pch = 19,
     main = "Transaction cost", 
     xlab =  "Transaction Cost", ylab = "Tracking Error")
lines(lowess(1:500,Parameters$TrackingError) , col='red')

boxplot((Parameters$TransactionCost[1:100]),col = 'green',
     main = 'Trade off between transaction cost and tracking error', 
     xlab =  'Transaction Cost', ylab = 'Tracking Error')

ggplot(Parameters, aes(factor(rbind(rep(1,100),rep(2,100),rep(3,100),rep(4,100),rep(5,100))),TrackingError)) + geom_boxplot() + xlab('portfolio sizes') +
  ylab('Tracking Error') +  ggtitle("Variation of Tracking Error for 100 portfolio samples")

ggplot(Parameters, aes(factor(rbind(rep(1,100),rep(2,100),rep(3,100),rep(4,100),rep(5,100))),Transaction.Cost)) + geom_boxplot() + xlab('portfolio sizes') +
  ylab('Transaction Cost') +  ggtitle("Variation of Transaction Cost for 100 portfolio samples")











