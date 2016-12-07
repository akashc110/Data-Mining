rm(list=ls())
library(ggplot2)
library(glmnet)
library(quadprog)
library(tseries)
library(RCurl)

smallcap <- read.csv("Small50.csv")[,-1]
midcap <- read.csv("Mid50.csv")[,-1]
largecap <- read.csv("Large50.csv")[,-1]
benchmark <- read.csv("Benchmark.csv")[,-1]
#benchmark <-  cbind(smallcap[,-1],midcap[,-1],largecap[,-1])
#benchmark <- apply(benchmark, 1, mean)
  
#5 Portfolios each of sizes 30(10 each), 60(20 each), 90(30 each), 120(40 each), 150(45 each)

size =  function(x) {sample(2:51,x)}

portfolio = function(x)  {
  #data1 = cbind(smallcap[,size(x)],midcap[,size(x)],largecap[,size(x)])
  #Lasso.model  <- model.matrix(Benchmark~.-1, data = data1[,-1])
  #fit.lasso <- cv.glmnet(Lasso.model,Benchmark)
  #values <- ifelse(coef(fit.lasso)[-1] !=0, 1, 0)
  
  return(cbind(smallcap[,size(x)],midcap[,size(x)],largecap[,size(x)]))
  }

bin = 1:100
portfolio.names1 <- list()
for (i in bin) {
  name1 <- paste('A30_',i,sep ='')
  name2 <- paste('B60_',i,sep ='')
  name3 <- paste('C90_',i,sep ='')
  name4 <- paste('D120_',i,sep ='')
  name5 <- paste('E135_',i,sep ='')
  
  assign(name1, portfolio(10))
  assign(name2, portfolio(20))
  assign(name3, portfolio(30))
  assign(name4, portfolio(40))
  assign(name5, portfolio(45))
  
  portfolio.names1[[(name1)]] = get(name1)
  portfolio.names1[[(name2)]] = get(name2)
  portfolio.names1[[(name3)]] = get(name3)
  portfolio.names1[[(name4)]] = get(name4)
  portfolio.names1[[(name5)]] = get(name5)
  
}

#sorting the portfolios
z = sort(names(portfolio.names1))
ptf = list()
for (u in 1:length(z)) {
  
  for (l in 1:length(z)) {
      if((z[u]) == names(portfolio.names1[l])){
        ptf[[z[u]]] =  portfolio.names1[l]
      }
  }

}
names(ptf) = z
beta.30 = data.frame()
Track <- data.frame()

for (i in ptf) {
  
  cap <-as.data.frame(i)
  assets <- ncol(cap)
  cap <- cbind(benchmark,cap)
  last = nrow(cap)
  returns = (cap[-1,]/cap[-last,]) - 1 #Computes simple monthly returns
  
#Split training and testing data
  returns.train <- returns[1:59,]
  returns.test <- returns[60:71,]
  
  asset.returns = returns.train[,-1]
  benchmark.returns = returns.train[,1]               #Benchmark returns
  #benchmark.returns = apply(asset.returns, 1, mean)      
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
  
  #Comment this when running equal weight above snippet
  qp <- solve.QP(Dmat, dvec, Amat, bvec, meq=1) 
  qp$solution
  
  betas = qp$solution
  beta1 = ifelse(abs(betas)<10^(-3),0,betas)
  transaction.cost <- length(beta1[beta1!=0])
  
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
  
  Tracking.Error = mean((as.numeric(as.character(d$Portfolio_Returns) )  -  
                           as.numeric(as.character(d$Benchmark_Returns)))^2 )*100
  Track = rbind(Track,cbind(Tracking.Error, transaction.cost))
  
}
chunk <- function(x,n) split(x, factor(sort(rank(x)%%n)))
index =  chunk(1:length(Track$transaction.cost),5)  
Optimized = c(round(mean(Track$transaction.cost[index$`0`]),0),
              round(mean(Track$transaction.cost[index$`1`]),0),
              round(mean(Track$transaction.cost[index$`2`]),0),
              round(mean(Track$transaction.cost[index$`3`]),0),
              round(mean(Track$transaction.cost[index$`4`]),0))
count = c(rep(30,Optimized[1]),rep(60,Optimized[2]),rep(90,Optimized[3]),
          rep(120,Optimized[4]),rep(135,Optimized[5]))
tab = c(rep(Optimized[1],Optimized[1]) ,rep(Optimized[2],Optimized[2]),
        rep(Optimized[3],Optimized[3]),rep(Optimized[4],Optimized[4]),
        rep(Optimized[5],Optimized[5]))
#count <- data.frame(Original = c(rep(30,Optimized[1]),60,90,120,135),Optimized)

t1 = (Optimized[1]/30)*100
t2 = (Optimized[2]/60)*100
t3 = (Optimized[3]/90)*100
t4 = (Optimized[4]/120)*100
t5 = (Optimized[5]/135)*100

count = c(t1,t2,t3,t4,t5)
names(count) <- c('30 Assets','60 Assets','90 Assets','120 Assets','135 Assets')
j= barplot(count,col = 'lightgreen',horiz = FALSE, ylim=c(0,100),
           main="Optimization Performance",xlab = "Portfolio Sizes",
           ylab = "Percentage of original size")
lab = paste(as.character(round(count,0)),'%','(',as.character(Optimized) ,' Assets)',sep = '')

text(x = j, y = count, label =lab,
     pos = 3, cex = 0.8, col = "red")


names(Track)<- c('Tracking.Error', 'Transaction.Cost')
Portfolio.Size =as.factor(Track$Transaction.Cost)
ggplot(Track, aes(x = Tracking.Error,y=Transaction.Cost/100,col=Portfolio.Size)) +
  geom_point(size = 2) + ggtitle("Transaction Cost vs Tracking Error for 500 Portfolios") + 
  geom_text(aes(label=Track$Transaction.Cost),hjust=0, vjust=0)


# chunk <- function(x,n) split(x, factor(sort(rank(x)%%n)))
# index =  chunk(x,n)  
# 
# par(mfrow=c(1,5))
# boxplot(Track$Tracking.Error[index$`0`])
# boxplot(Track$Tracking.Error[index$`1`])
# boxplot(Track$Tracking.Error[index$`2`])
# boxplot(Track$Tracking.Error[index$`3`])
# boxplot(Track$Tracking.Error[index$`4`])

df <- data.frame(
  value1 = Track$Tracking.Error,
  value2 = Track$Transaction.Cost,
  group = c(rep('Set1_30',length(bin)),rep('Set2_60',length(bin)),rep('Set3_90',length(bin)),
            rep('Set4_120',length(bin)),rep('Set5_135',length(bin)))
)

ggplot(df, aes(factor(group),value1)) + geom_boxplot() + xlab('portfolio sizes') +
  ylab('Tracking Error') +  ggtitle("Variation of Tracking Error for 100 portfolio samples")
  
ggplot(df, aes(factor(group),value2)) + geom_boxplot() + xlab('portfolio sizes') +
  ylab('Transaction Cost') +  ggtitle("Variation of Transaction Cost for 100 portfolio samples")






# y = c(mean(Track$Transaction.Cost[1:5]),mean(Track$Transaction.Cost[6:10]),
#       mean(Track$Transaction.Cost[11:15]),mean(Track$Transaction.Cost[16:20]),
#       mean(Track$Transaction.Cost[21:25]))/100
# 
# x = c(mean(Track$Tracking.Error[1:5]),mean(Track$Tracking.Error[6:10]),
#       mean(Track$Tracking.Error[11:15]),mean(Track$Tracking.Error[16:20]),
#       mean(Track$Tracking.Error[21:25]))
# 
# plot(x,y)
# lines(x,y)
