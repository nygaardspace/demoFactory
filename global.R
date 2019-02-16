#################################################################################
###########             LOAD STOCK INFO                                  ########
###########             See file downloadRT.R                            ########
###########             for details                                      ########
#################################################################################
load("mainEnv.RData")
data <- mainEnv
load(file = "stock_data.RData")
load(file = "etf_data.RData")
load(file = "etf_ts.RData")
stock_data <- stock_data[!(stock_data$`Country of Exchange` == ""),]

#ALL is the global variable that contains all stock info.
ALL <- stock_data
ALL$universe <- "index"
ALL$Mean <- ""
ALL$Weight <- NULL

#Define color palette
color_palette <- c("#B2EBF2","#4DD0E1","#00ACC1","#00838F","#006064","#A5D6A7","#66BB6A","#43A047","#2E7D32","#1B5E20","#FFCC80","#FFA726","#FB8C00","#EF6C00","#E65100","#2E7D32","#1B5E20","#FFCC80","#FFA726","#FB8C00","#EF6C00","#E65100")

########################################################################
# FUNCTIONS
########################################################################
#Comute Mean of ranking
foo2 <- function(n, sektors, countries, mom, vol, value, size, liquidity, quality) {

  if(!is.integer(n)) n <- 1
  stockSelect <- subset(ALL, ALL$`GICS Sector Name` %in% sektors)
  stockSelect <- subset(stockSelect, stockSelect$`Country of Exchange` %in% countries)
  
  stockSelect$Mean <- ( mom*stockSelect$`52 Week Total Return` + vol*stockSelect$`Volatility - 250 days`  
                        + value*stockSelect$`Return On Assets - Mean` + size*stockSelect$`Company Market Cap` 
                        + liquidity*stockSelect$`Liquidity Ten Day Volume` + quality*stockSelect$`Price To Book Value Per Share (Daily Time Series Ratio)` ) / (mom+vol+value+size+liquidity+quality) 
  stockSelect <- stockSelect[order(stockSelect$Mean),]
  stockSelect[1:n,]
}
#Filter on sector
foo3 <- function(etf_data, instruments) {
  subset(etf_data, etf_data$Instrument %in% instruments)
}

long_to_xts <- function(master_tibble){
  close_all <- reshape2::dcast(master_tibble, TIMESTAMP ~ Instrument, value.var = "CLOSE",fun.aggregate = mean)
  prices_daily <- xts::xts(close_all[-c(1)], order.by = close_all$TIMESTAMP)
  prices_daily <- zoo::na.locf(prices_daily)
  return(prices_daily)
}

momentum <- function(prices, lag1, lag2) {
  momentum12 = ( (xts::lag.xts(prices, lag1) -  xts::lag.xts(prices, lag2)) )/ xts::lag.xts(prices, lag2)
  last12 <- as.vector(momentum12[nrow(momentum12),])
  last12[is.na(last12)] <- median(na.omit(last12))
  return(last12)
}

######################## BACKTEST CODE #############################
backtest <- function(tickers, data,startdate, top_n,monthsMom, daysVol){
print(ls(data))
print(tickers)
print(startdate)
print(top_n)
print(monthsMom)
print(daysVol)
bt.prep(data, align='keep.all', dates=paste0(lubridate::year(startdate),":",lubridate::month(startdate),"::")) 
bt.prep.remove.symbols(data,setdiff(data$symbolnames, tickers))
print("hello")
prices = data$prices
n = ncol(prices)
prices4mom = data$prices
prices4vol = data$prices
models = list()
period.ends = endpoints(prices, 'months')
period.ends = period.ends[period.ends > 0]
n.top = top_n
n.mom = monthsMom*21
n.vol = 1*daysVol
print("hello11")
data$weight[] = NA
data$weight[period.ends[period.ends > 189],] = ntop(prices[period.ends[period.ends > 189],], n)
print("hell2o")
models$equal.weight = bt.run.share(data, clean.signal=F,trade.summary = TRUE)
print("hello3")
ret.log = bt.apply.matrix(prices4vol, ROC, type='continuous')
momentum = prices4mom / mlag(prices4mom, n.mom)
print("hello")
weight = NA * prices
weight[period.ends,] = ntop(momentum[period.ends,], n.top)
for( i in period.ends[period.ends >= n.mom] ) {
  hist = ret.log[ (i - n.vol + 1):i, ]
  include.index = count(hist)== n.vol
  index = ( weight[i,] > 0 ) & include.index
  n = sum(index)
  if(n > 0) {
    hist = hist[ , index]
    ia = create.ia(hist)
    s0 = apply(coredata(hist),2,sd)
    ia$cov = cor(coredata(hist), use='complete.obs',method='pearson') * (s0 %*% t(s0))
    constraints = new.constraints(n, lb = 0, ub = 1)
    constraints = add.constraints(rep(1, n), 1, type = '=', constraints)
    weight[i,] = 0
    weight[i,index] = min.risk.portfolio(ia, constraints)
  }
}
data$weight[] = NA
data$weight[period.ends,] = weight[period.ends,]
models$aaa = bt.run.share(data, clean.signal=F,trade.summary = TRUE)

data$weight[] = NA
data$weight[period.ends,] = target.vol.strategy(models$aaa,weight, 4.3/100, 21, 100/100)[period.ends,]
models$trygg = bt.run.share(data, clean.signal=F,trade.summary = TRUE)

data$weight[] = NA
data$weight[period.ends,] = target.vol.strategy(models$aaa,weight, 30/100, 21, 500/100)[period.ends,]
models$modigplus = bt.run.share(data, clean.signal=F,trade.summary = TRUE)

print("hello")
models$aaa <- NULL
models <- bt.trim(models)
models
}

# ##############TRANSITION MAP TRYGG#############
#general_plots(models)
#strategy_plots(models, "trygg")
#strategy_plots(models, "balansert")
#strategy_plots(models, "modig")
#strategy_plots(models, "modigplus")
