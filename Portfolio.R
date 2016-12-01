rm(list=ls())
library(ggplot2)
library(glmnet)
library(quadprog)
library(tseries)

smallcap <- read.csv("Small50.csv")[,-1]
midcap <- read.csv("Mid50.csv")[,-1]
largecap <- read.csv("Large50.csv")[,-1]
benchmark <- read.csv("Benchmark.csv")[,-1]
  
#ustart= "2010-1-1"
#uend = "2015-12-31"
#benchmark <- (get.hist.quote(instrument = "GSPC", start=ustart, end=uend, quote = c("Adj"),provider = "yahoo", compression = "m") )
#write.csv(benchmark, file = "Benchmark.csv")


#5 Portfolios each of sizes 30(10 each), 60(20 each), 90(30 each), 120(40 each), 150(45 each)

size =  function(x) {sample(2:51,x)}

portfolio = function(x)  {
  
  return(cbind(smallcap[,size(x)],midcap[,size(x)],largecap[,size(x)]))
  }

bin = 1:5
for (i in bin) {
  assign(paste('P30_',i,sep ='' ), portfolio(10))
  assign(paste('P60_',i,sep ='' ), portfolio(20))
  assign(paste('P90_',i,sep ='' ), portfolio(30))
  assign(paste('P120_',i,sep ='' ), portfolio(40))
  assign(paste('P135_',i,sep ='' ), portfolio(45))
}

ptf = list(P30_1,P30_2,P30_3,P30_4,P30_5,
           P60_1,P60_2,P60_3,P60_4,P60_5,
           P90_1,P90_2,P90_3,P90_4,P90_5,
           P120_1,P120_2,P120_3,P120_4,P120_5,
           P135_1,P135_2,P135_3,P135_4,P135_5)

beta.30 = data.frame()
Track <- data.frame()

for (i in ptf) {
  
  cap <- i
  assets <- ncol(cap)
  cap <- cbind(benchmark,cap)
  last = nrow(cap)
  returns = (cap[-1,]/cap[-last,]) - 1 #Computes simple monthly returns
  
  #Split training and testing data
  returns.train <- returns[1:59,]
  returns.test <- returns[60:71,]
  
  asset.returns = returns.train[,-1]
  benchmark.returns = returns.train[,1]            #Benchmark returns
  #benchmark.returns = apply(asset.returns, 1, mean)            #Benchmark returns
  #returns.test[,1] = apply(returns.test[,-1], 1, mean)
  
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
  bvec <- c(1, exp.benchmark, rep(0, assets),rep(-2/assets, assets))
  
  qp <- solve.QP(Dmat, dvec, Amat, bvec, meq=1) #Comment this when running equal weight above snippet
  qp$solution
  
  betas = qp$solution
  beta1 = ifelse(abs(betas)<10^(-3),0,betas)
  transaction.cost <- length(beta1[beta1!=0])
  
  #if(assets == 30){beta.30 = rbind(beta.30,t(beta1))}
  #Code to calculate portfolio returns for 12 months of year 2015
  d = matrix(0,12,3)
  for (i in 1:12) {
    returns.T <- returns.test[i,1]
    portfolio.returns <- t(as.matrix(beta1))%*%t(returns.test[i,-1])
    result <- ifelse(portfolio.returns>=returns.T,"Good","Poor")
    d[i,] <- t(c(returns.T,portfolio.returns, result))
    
  }
  d <- as.data.frame(d)     #Test year (2015) returns
  names(d) <- c("Benchmark_Returns", "Portfolio_Returns","Performance")
  
  #z = table(d$Performance)
  #Performance = rbind(Performance, z)
  
  Tracking.Error = mean((as.numeric(as.character(d$Portfolio_Returns) )  -  as.numeric(as.character(d$Benchmark_Returns)))^2 )*100
  Tracking.Error1 <- paste(as.character(round(Tracking.Error,3) ),"%")
  #sd(as.numeric(as.character(d$Portfolio_Returns) )  -  as.numeric(as.character(d$Benchmark_Returns)))
  Track = rbind(Track,cbind(Tracking.Error, transaction.cost))
  
  #table(beta1) 
  #length(beta1[beta1!=0])
  #rm(returns.test, beta1 )


#portfolio.name <- c("12 Assets", "24 Assets", "36 Assets","48 Assets","60 Assets")
#tab <- data.frame(Portfolio_Type = portfolio.name, Tracking_Error = Track,Performance)

}

names(Track)<- c('Tracking.Error', 'Transaction.Cost')
#names(Track)<- c('x', 'y')
ggplot(Track, aes(x = Tracking.Error,y=Transaction.Cost)) + geom_point() + geom_smooth() +  ggtitle("Transaction Cost vs Tracking Error for 25 Portfolios")

View(Track)

