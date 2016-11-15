rm(list=ls())
# Tickers <- read.csv("Data.csv") 
# str(Tickers$Date.first.added)
# Tickers$Date.first.added = as.character.Date(Tickers$Date.first.added)
# 
# symbs <- Tickers[Tickers$Date.first.added != Tickers$Date.first.added[1],]
# dim(symbs)
# #cate = strsplit(symbs$Date.first.added[1], split='[/]')[[1]][3]
# symbs$Year_start = sapply(symbs$Date.first.added, FUN=function(x) {strsplit(x, split='[/]')[[1]][3]})
# 
# symbs1 <- symbs[order(symbs$Year_start),]
# 
# ticks <-as.character(symbs1[symbs1$Year_start<2005,1])  
# 
# 
# 
# 
# require(tseries)
# 
#  symb = c("GSPC",ticks)
#  ustart= "2009-1-1"
#  uend = "2015-12-31"
#  d = NULL
# for (i in 1:length(symb)){
# 
#     if(i != 34){
#     zeta = as.vector(get.hist.quote(instrument = symb[i], start=ustart, end=uend, quote = c("Adj"),provider = "yahoo", compression = "m") )
#     d = data.frame(cbind(d,zeta))
#     }
#  }
# names(d) = symb[-34]
# 
# write.csv(d,file = "StockData.csv")


#########################################################################
#DM-1.R data download script
require(tseries)
small_tick <- c("CALM","ROLL","SYNA","EXAS","TSRA","UFPI","POWI","SAFM","WERN","ARLP",
                "NXST","IIVI","STMP","MNRO","NWBI","HTLD","SKYW","NXTM","NTGR","AHGP")
mid_tick <- c("IDXX","SGEN","NTAP","TSCO","FFIV","SNPS","WYNN","VRSN","LULU","STLD",
              "ANSS","SBNY","OTEX","ALGN","SEIC","TRMB","ODFL","BBBY","JKHY","AGNC")
large_tick <- c("CSCO","AMGN","GILD","QCOM","CELG","SBUX","PCLN","ADBE","NFLX","NVDA",
                "ESRX","REGN","CME","YHOO","CTSH","EBAY","AMAT","INTU","DISH","ALXN")

symb = c("GSPC",_tick)
ustart= "2010-1-1"
uend = "2015-12-31"
d = NULL
for (i in 1:length(symb)){
  
  quote = as.vector(get.hist.quote(instrument = symb[i], start=ustart, end=uend, quote = c("Adj"),provider = "yahoo", compression = "m") )
  d = data.frame(cbind(d,quote))
}

names(d) <- symb

write.csv(d,file = "LargeCap.csv")

