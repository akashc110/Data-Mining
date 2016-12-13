rm(list=ls())
library(ggplot2)
library(glmnet)
library(quadprog)
library(tseries)
library(RCurl)
library(calibrate)

smallcap <- read.csv("Small50.csv")[,-1]
midcap <- read.csv("Mid50.csv")[,-1]
largecap <- read.csv("Large50.csv")[,-1]
benchmark <- read.csv("Benchmark.csv")[,-1]
#benchmark <-  cbind(smallcap[,-1],midcap[,-1],largecap[,-1])
#benchmark <- apply(benchmark, 1, mean)
  
#5 Portfolios each of sizes 30(10 each), 60(20 each), 90(30 each), 120(40 each), 150(45 each)

size =  function(x) {sample(2:51,x)}

portfolio = function(x)  {
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
  #lambda = 0.01
  lambda = 0.01
  Dmat = matrix(Dmat1[1:assets, 1:assets], nrow = assets, ncol = assets)  + lambda*(diag(assets))  
  
  #Each assets expected return
  dvec = matrix(mean.asset.returns[1:assets], nrow = assets, ncol = 1)  
  #Constraint for sum of weights =1
  A.Equality <- matrix(rep(1,assets), ncol=1)
  #Amat constraints: sum=1, dvec == benchmark returns, weights>=0, weights <= 2/(*assets)
  Amat <- cbind(A.Equality, dvec, diag(assets), -diag(assets))
  bvec <- c(1, exp.benchmark, rep(0, assets),rep(-2/assets, assets))
  
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
# ggplot(Track, aes(x = Tracking.Error,y=Transaction.Cost/100,col=Portfolio.Size)) +
#   geom_point(size = 2) + ggtitle("Transaction Cost vs Tracking Error for 500 Portfolios") + 
#   geom_text(aes(label=Track$Transaction.Cost),hjust=0, vjust=0)

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



fact.track <- scale(Track$Tracking.Error) 
fact.tran <- scale(Track$Transaction.Cost)
plot(fact.track)
plot(fact.tran)
points(fact.track, col='red')

lambda1 = 1.6
#loss_function = (1-lambda1)*fact.track + (lambda1)*fact.tran
loss_function = fact.track + (lambda1)*fact.tran
plot(loss_function)
abline(h=0,  col = 'red', lty=2)

variance_function = c(var(Track$Tracking.Error[1:100]),var(Track$Tracking.Error[101:200]),
                      var(Track$Tracking.Error[201:300]),var(Track$Tracking.Error[301:400]),
                      var(Track$Tracking.Error[401:500]))

plot(variance_function,  xaxt = "n", main = 'Variance of Tracking Error', ylab = 'Variance', xlab = 'Portfolio Sizes')
axis(1, at=1:5, labels=c('Set1_30','Set1_60','Set1_90','Set1_120','Set1_135'))
lines(variance_function, col='red', lwd=2)
  
linMap <- function(x, from, to)
  (x - min(x)) / max(x - min(x)) * (to - from) + from

trck_function = c(median(Track$Tracking.Error[1:100]),median(Track$Tracking.Error[101:200]),
                  median(Track$Tracking.Error[201:300]),median(Track$Tracking.Error[301:400]),
                  median(Track$Tracking.Error[401:500]))

trans_function = linMap(Track$Transaction.Cost, min(Track$Tracking.Error), max(Track$Tracking.Error))

trans_function = c(median(Track$Transaction.Cost[1:100]),median(Track$Transaction.Cost[101:200]),
                   median(Track$Transaction.Cost[201:300]),median(Track$Transaction.Cost[301:400]),
                   median(Track$Transaction.Cost[401:500]))



# trck_function = c(median(fact.track[1:100]),median(fact.track[101:200]),
#                   median(fact.track[201:300]),median(fact.track[301:400]),
#                   median(fact.track[401:500]))
# 
# trans_function = c(median(fact.tran[1:100]),median(fact.tran[101:200]),
#                    median(fact.tran[201:300]),median(fact.tran[301:400]),
#                    median(fact.tran[401:500]))


plot(trck_function, col = 'red',pch =19,xaxt = "n", main = 'Error-Cost Trade-off for Portfolio Selection', xlab = 'Portfolio Sizes',
     ylab = 'Standard error/cost values')
axis(1, at=1:5, labels=c('Set1_30','Set1_60','Set1_90','Set1_120','Set1_135'))
legend("topright", c("Tracking Error","Transaction Cost"),
       lty=c(1,1),lwd=c(2,2),col=c("blue","orange"),cex=0.8) 
#par(new = TRUE)
lines(trck_function, col='darkblue', lwd=2)
# second plot
par(new = TRUE)
plot(trans_function,col = 'green',pch =19,xaxt = "n", yaxt = "n",xlab = NA, ylab = NA)
lines(trans_function, col='orange', lwd=2)
#locator()
#textxy(locator()$x,locator()$y,'~67Assets',cex = 1.1)
#dev.off()

