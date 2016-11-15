#Data screening link
#http://www.nasdaq.com/screening/companies-by-industry.aspx?exchange=NASDAQ&marketcap=Small-cap&sortname=marketcap&sorttype=1

#INTRODUCTION
#Benchmark is taken to be S&P500 index.
#A five year period from Jan 2010 to Dec 2014 is considered to compute returns for the year 2015
#Portfolio is rebalanced at the end of every month and transaction costs are taken into account (not yet included in the script)
#Five different portfolios are constructed based on the size
#12,24,36,48,60 are the respective number of assets in each portfolio
#12 month period is considered to study the trade-off between tracking error and portfolio size
  #for these portfolios
#Each portfolio is constructed using top n stocks by market capitalization in small,mid & large segments

#To see the download code script refer Symb_download.R file in directory

rm(list  = ls())
library(quadprog)
smallcap <- read.csv("SmallCap.csv")[,-1]
midcap <- read.csv("MidCap.csv")[,-1]
largecap <- read.csv("LargeCap.csv")[,-1]

Portfolio12 <- cbind(smallcap[,1:5],midcap[,2:5],largecap[,2:5])
Portfolio24 <- cbind(smallcap[,1:9],midcap[,2:9],largecap[,2:9])
Portfolio36 <- cbind(smallcap[,1:13],midcap[,2:13],largecap[,2:13])
Portfolio48 <- cbind(smallcap[,1:17],midcap[,2:17],largecap[,2:17]) 
Portfolio60 <- cbind(smallcap[,1:21],midcap[,2:21],largecap[,2:21])
  
Track = NULL
Performance = NULL
for (i in 1:5) {

assets = 12*i

if(i==1){cap <- Portfolio12}
if(i==2){cap <- Portfolio24}
if(i==3){cap <- Portfolio36}
if(i==4){cap <- Portfolio48}
if(i==5){cap <- Portfolio60}

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

z = table(d$Performance)
Performance = rbind(Performance, z)

Tracking.Error = mean((as.numeric(as.character(d$Portfolio_Returns) )  -  as.numeric(as.character(d$Benchmark_Returns)))^2 )*100
Tracking.Error <- paste(as.character(round(Tracking.Error,3) ),"%")
#sd(as.numeric(as.character(d$Portfolio_Returns) )  -  as.numeric(as.character(d$Benchmark_Returns)))
Track = rbind(Track,Tracking.Error)

table(beta1) 
 length(beta1[beta1!=0])
 rm(returns.test, beta1 )
}

portfolio.name <- c("12 Assets", "24 Assets", "36 Assets","48 Assets","60 Assets")
tab <- data.frame(Portfolio_Type = portfolio.name, Tracking_Error = Track,Performance)

#Open tab from Environment to see the results