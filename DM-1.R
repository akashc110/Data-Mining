#Data screening link
#http://www.nasdaq.com/screening/companies-by-industry.aspx?exchange=NASDAQ&marketcap=Small-cap&sortname=marketcap&sorttype=1

#INTRODUCTION
#Benchmark is taken to be S&P500 index.
#A five year period from Jan 2010 to Dec 2014 is considered to compute returns for the year 2015
#Portfolio is rebalanced at the end of every month and transaction costs are taken into account
#Four different portfolios are constructed based on market capitalization
#Small, Medium, Big and a combination of these
#12 month period is considered to study the trade-off between tracking error and portfolio size
  #for these portfolios
#Each portfolio is constructed using top 12 stocks by market capitalization in those segments

#To see the download code script refer Symb_download.R file in directory

rm(list  = ls())
library(quadprog)
smallcap <- read.csv("SmallCap.csv")[,-1]
midcap <- read.csv("MidCap.csv")[,-1]
largecap <- read.csv("LargeCap.csv")[,-1]
mixcap <- cbind(smallcap[,1:5],midcap[,2:5],largecap[,2:5])

#Caps <- c(smallcap,midcap,largecap)
Track = NULL

#Small cap portfolio calculations
for (i in 1:4) {

assets = 12
#cap <- smallcap
if(i==1){cap <- smallcap}
if(i==2){cap <- midcap}
if(i==3){cap <- largecap}
if(i==4){cap <- mixcap}

last = nrow(cap)

returns = (cap[-1,]/cap[-last,]) - 1 #Computes simple monthly returns

#Split training and testing data
returns.train <- returns[1:59,]
returns.test <- returns[60:71,]

asset.returns = returns.train[,-1]               #12 asset returns
benchmark.returns = returns.train[,1]            #Benchmark returns

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

#CODE SNIPPET TO CHECK FOR EQUAL WEIGHTS
#######
#Dmat = (diag(assets))  
#bvec <- c(1, mean(mean.asset.returns[1:assets]), rep(0, assets),rep(-2/assets, assets))
#qp <- solve.QP(Dmat, dvec, Amat, bvec, meq=2)
#######

qp <- solve.QP(Dmat, dvec, Amat, bvec, meq=1) #Comment this when running equal weight above snippet
qp$solution

betas = qp$solution
beta1 = ifelse(abs(betas)<10^(-3),0,betas)
table(beta1) 
length(beta1[beta1!=0])

d = matrix(0,12,3)
for (i in 1:12) {
  returns.T <- returns.test[i,1]
  portfolio.returns <- t(as.matrix(beta1))%*%t(returns.test[i,-1])
  result <- ifelse(portfolio.returns>=returns.T,"Good","Poor")
  d[i,] <- t(c(returns.T,portfolio.returns, result))
  
}
d <- as.data.frame(d)
table(d$V3)

Tracking.Error = mean((as.numeric(as.character(d$V2) )  -  as.numeric(as.character(d$V1)))^2 )*100
Tracking.Error <- paste(as.character(round(Tracking.Error,3) ),"%")
#sd(as.numeric(as.character(d$V2) )  -  as.numeric(as.character(d$V1)))
Track = rbind(Track,Tracking.Error)

table(beta1) 
 length(beta1[beta1!=0])
}

portfolio.name <- c("12 Small Cap", "12 Mid Cap", "12 Large Cap","12 Mix Cap")
tab <- data.frame(Portfolio_Type = portfolio.name, Tracking_Error = Track)
