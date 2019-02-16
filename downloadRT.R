options(max.print=100000,stringsAsFactors = FALSE)

library(curl)
library(eikonapir)
library(tidyverse)
library(methods)
library(plyr)
library(quantmod)
library(xlsx)
set_app_id('f5f95348c835468eb31cee1aa8979f12c7a277ee')

set_proxy_port(9000L)
stock_data1 <- get_data(paste0('0#.OMXC20'),     list("TR.TURNOVER","TR.Employees(Period=FY0)","CF_CLOSE","CF_VOLUME", "PCTCHNG", "TR.CommonName","TR.ExchangeName", "TR.FiIssuerSPLongRating", "TR.GICSSector","TR.GICSSubIndustry","CF_DATE", "TR.CompanyMarketCap","CURRENCY","TR.Revenue(Period=FY0)", "TR.ExchangeCountry","TR.TotalReturn1Wk","TR.TotalReturn1Mo","TR.TotalReturn3Mo","TR.TotalReturn6Mo","TR.TotalReturn52Wk","TR.Volatility25D","TR.Volatility50D","TR.Volatility100D","TR.Volatility150D","TR.Volatility200D","TR.Volatility250D","TR.ROAMean(Period=FY1)","TR.PE","TR.PriceToBVPerShare","TR.PriceToCFPerShare","TR.PriceToSalesPerShare","TR.EVToEBIT","TR.BetaFiveYear","TR.BetaWkly3Y","TR.BetaWkly2Y","TR.BetaDaily180D","TR.BetaDaily90D","TR.Liquidity10DVol", TR_Field('TR.EventTitle', list('EventType'= 'RES')),TR_Field('TR.EventStartDate', list('EventType'= 'RES'))))
stock_data2 <- get_data(paste0('0#.OMXS30'),     list("TR.TURNOVER","TR.Employees(Period=FY0)","CF_CLOSE","CF_VOLUME", "PCTCHNG", "TR.CommonName","TR.ExchangeName", "TR.FiIssuerSPLongRating", "TR.GICSSector","TR.GICSSubIndustry","CF_DATE", "TR.CompanyMarketCap","CURRENCY","TR.Revenue(Period=FY0)", "TR.ExchangeCountry","TR.TotalReturn1Wk","TR.TotalReturn1Mo","TR.TotalReturn3Mo","TR.TotalReturn6Mo","TR.TotalReturn52Wk","TR.Volatility25D","TR.Volatility50D","TR.Volatility100D","TR.Volatility150D","TR.Volatility200D","TR.Volatility250D","TR.ROAMean(Period=FY1)","TR.PE","TR.PriceToBVPerShare","TR.PriceToCFPerShare","TR.PriceToSalesPerShare","TR.EVToEBIT","TR.BetaFiveYear","TR.BetaWkly3Y","TR.BetaWkly2Y","TR.BetaDaily180D","TR.BetaDaily90D","TR.Liquidity10DVol", TR_Field('TR.EventTitle', list('EventType'= 'RES')),TR_Field('TR.EventStartDate', list('EventType'= 'RES'))))
stock_data3 <- get_data(paste0('0#.OMXH25'),     list("TR.TURNOVER","TR.Employees(Period=FY0)","CF_CLOSE","CF_VOLUME", "PCTCHNG", "TR.CommonName","TR.ExchangeName", "TR.FiIssuerSPLongRating", "TR.GICSSector","TR.GICSSubIndustry","CF_DATE", "TR.CompanyMarketCap","CURRENCY","TR.Revenue(Period=FY0)", "TR.ExchangeCountry","TR.TotalReturn1Wk","TR.TotalReturn1Mo","TR.TotalReturn3Mo","TR.TotalReturn6Mo","TR.TotalReturn52Wk","TR.Volatility25D","TR.Volatility50D","TR.Volatility100D","TR.Volatility150D","TR.Volatility200D","TR.Volatility250D","TR.ROAMean(Period=FY1)","TR.PE","TR.PriceToBVPerShare","TR.PriceToCFPerShare","TR.PriceToSalesPerShare","TR.EVToEBIT","TR.BetaFiveYear","TR.BetaWkly3Y","TR.BetaWkly2Y","TR.BetaDaily180D","TR.BetaDaily90D","TR.Liquidity10DVol", TR_Field('TR.EventTitle', list('EventType'= 'RES')),TR_Field('TR.EventStartDate', list('EventType'= 'RES'))))
stock_data4 <- get_data(paste0('0#.OSEAX'),      list("TR.TURNOVER","TR.Employees(Period=FY0)","CF_CLOSE","CF_VOLUME", "PCTCHNG", "TR.CommonName","TR.ExchangeName", "TR.FiIssuerSPLongRating", "TR.GICSSector","TR.GICSSubIndustry","CF_DATE", "TR.CompanyMarketCap","CURRENCY","TR.Revenue(Period=FY0)", "TR.ExchangeCountry","TR.TotalReturn1Wk","TR.TotalReturn1Mo","TR.TotalReturn3Mo","TR.TotalReturn6Mo","TR.TotalReturn52Wk","TR.Volatility25D","TR.Volatility50D","TR.Volatility100D","TR.Volatility150D","TR.Volatility200D","TR.Volatility250D","TR.ROAMean(Period=FY1)","TR.PE","TR.PriceToBVPerShare","TR.PriceToCFPerShare","TR.PriceToSalesPerShare","TR.EVToEBIT","TR.BetaFiveYear","TR.BetaWkly3Y","TR.BetaWkly2Y","TR.BetaDaily180D","TR.BetaDaily90D","TR.Liquidity10DVol", TR_Field('TR.EventTitle', list('EventType'= 'RES')),TR_Field('TR.EventStartDate', list('EventType'= 'RES'))))
stock_data5 <- get_data(paste0('0#.N500'),       list("TR.TURNOVER","TR.Employees(Period=FY0)","CF_CLOSE","CF_VOLUME", "PCTCHNG", "TR.CommonName","TR.ExchangeName", "TR.FiIssuerSPLongRating", "TR.GICSSector","TR.GICSSubIndustry","CF_DATE", "TR.CompanyMarketCap","CURRENCY","TR.Revenue(Period=FY0)", "TR.ExchangeCountry","TR.TotalReturn1Wk","TR.TotalReturn1Mo","TR.TotalReturn3Mo","TR.TotalReturn6Mo","TR.TotalReturn52Wk","TR.Volatility25D","TR.Volatility50D","TR.Volatility100D","TR.Volatility150D","TR.Volatility200D","TR.Volatility250D","TR.ROAMean(Period=FY1)","TR.PE","TR.PriceToBVPerShare","TR.PriceToCFPerShare","TR.PriceToSalesPerShare","TR.EVToEBIT","TR.BetaFiveYear","TR.BetaWkly3Y","TR.BetaWkly2Y","TR.BetaDaily180D","TR.BetaDaily90D","TR.Liquidity10DVol", TR_Field('TR.EventTitle', list('EventType'= 'RES')),TR_Field('TR.EventStartDate', list('EventType'= 'RES'))))
stock_data6 <- get_data(paste0('0#.SPX'),        list("TR.TURNOVER","TR.Employees(Period=FY0)","CF_CLOSE","CF_VOLUME", "PCTCHNG", "TR.CommonName","TR.ExchangeName", "TR.FiIssuerSPLongRating", "TR.GICSSector","TR.GICSSubIndustry","CF_DATE", "TR.CompanyMarketCap","CURRENCY","TR.Revenue(Period=FY0)", "TR.ExchangeCountry","TR.TotalReturn1Wk","TR.TotalReturn1Mo","TR.TotalReturn3Mo","TR.TotalReturn6Mo","TR.TotalReturn52Wk","TR.Volatility25D","TR.Volatility50D","TR.Volatility100D","TR.Volatility150D","TR.Volatility200D","TR.Volatility250D","TR.ROAMean(Period=FY1)","TR.PE","TR.PriceToBVPerShare","TR.PriceToCFPerShare","TR.PriceToSalesPerShare","TR.EVToEBIT","TR.BetaFiveYear","TR.BetaWkly3Y","TR.BetaWkly2Y","TR.BetaDaily180D","TR.BetaDaily90D","TR.Liquidity10DVol", TR_Field('TR.EventTitle', list('EventType'= 'RES')),TR_Field('TR.EventStartDate', list('EventType'= 'RES'))))
stock_data7 <- get_data(paste0('0#.GDAXI'),      list("TR.TURNOVER","TR.Employees(Period=FY0)","CF_CLOSE","CF_VOLUME", "PCTCHNG", "TR.CommonName","TR.ExchangeName", "TR.FiIssuerSPLongRating", "TR.GICSSector","TR.GICSSubIndustry","CF_DATE", "TR.CompanyMarketCap","CURRENCY","TR.Revenue(Period=FY0)", "TR.ExchangeCountry","TR.TotalReturn1Wk","TR.TotalReturn1Mo","TR.TotalReturn3Mo","TR.TotalReturn6Mo","TR.TotalReturn52Wk","TR.Volatility25D","TR.Volatility50D","TR.Volatility100D","TR.Volatility150D","TR.Volatility200D","TR.Volatility250D","TR.ROAMean(Period=FY1)","TR.PE","TR.PriceToBVPerShare","TR.PriceToCFPerShare","TR.PriceToSalesPerShare","TR.EVToEBIT","TR.BetaFiveYear","TR.BetaWkly3Y","TR.BetaWkly2Y","TR.BetaDaily180D","TR.BetaDaily90D","TR.Liquidity10DVol", TR_Field('TR.EventTitle', list('EventType'= 'RES')),TR_Field('TR.EventStartDate', list('EventType'= 'RES'))))
stock_data8 <- get_data(paste0('0#.FTSE'),       list("TR.TURNOVER","TR.Employees(Period=FY0)","CF_CLOSE","CF_VOLUME", "PCTCHNG", "TR.CommonName","TR.ExchangeName", "TR.FiIssuerSPLongRating", "TR.GICSSector","TR.GICSSubIndustry","CF_DATE", "TR.CompanyMarketCap","CURRENCY","TR.Revenue(Period=FY0)", "TR.ExchangeCountry","TR.TotalReturn1Wk","TR.TotalReturn1Mo","TR.TotalReturn3Mo","TR.TotalReturn6Mo","TR.TotalReturn52Wk","TR.Volatility25D","TR.Volatility50D","TR.Volatility100D","TR.Volatility150D","TR.Volatility200D","TR.Volatility250D","TR.ROAMean(Period=FY1)","TR.PE","TR.PriceToBVPerShare","TR.PriceToCFPerShare","TR.PriceToSalesPerShare","TR.EVToEBIT","TR.BetaFiveYear","TR.BetaWkly3Y","TR.BetaWkly2Y","TR.BetaDaily180D","TR.BetaDaily90D","TR.Liquidity10DVol", TR_Field('TR.EventTitle', list('EventType'= 'RES')),TR_Field('TR.EventStartDate', list('EventType'= 'RES'))))
stock_data9 <- get_data(paste0('0#.N100'),       list("TR.TURNOVER","TR.Employees(Period=FY0)","CF_CLOSE","CF_VOLUME", "PCTCHNG", "TR.CommonName","TR.ExchangeName", "TR.FiIssuerSPLongRating", "TR.GICSSector","TR.GICSSubIndustry","CF_DATE", "TR.CompanyMarketCap","CURRENCY","TR.Revenue(Period=FY0)", "TR.ExchangeCountry","TR.TotalReturn1Wk","TR.TotalReturn1Mo","TR.TotalReturn3Mo","TR.TotalReturn6Mo","TR.TotalReturn52Wk","TR.Volatility25D","TR.Volatility50D","TR.Volatility100D","TR.Volatility150D","TR.Volatility200D","TR.Volatility250D","TR.ROAMean(Period=FY1)","TR.PE","TR.PriceToBVPerShare","TR.PriceToCFPerShare","TR.PriceToSalesPerShare","TR.EVToEBIT","TR.BetaFiveYear","TR.BetaWkly3Y","TR.BetaWkly2Y","TR.BetaDaily180D","TR.BetaDaily90D","TR.Liquidity10DVol", TR_Field('TR.EventTitle', list('EventType'= 'RES')),TR_Field('TR.EventStartDate', list('EventType'= 'RES'))))
stock_data10 <- get_data(paste0('0#.HSI'),       list("TR.TURNOVER","TR.Employees(Period=FY0)","CF_CLOSE","CF_VOLUME", "PCTCHNG", "TR.CommonName","TR.ExchangeName", "TR.FiIssuerSPLongRating", "TR.GICSSector","TR.GICSSubIndustry","CF_DATE", "TR.CompanyMarketCap","CURRENCY","TR.Revenue(Period=FY0)", "TR.ExchangeCountry","TR.TotalReturn1Wk","TR.TotalReturn1Mo","TR.TotalReturn3Mo","TR.TotalReturn6Mo","TR.TotalReturn52Wk","TR.Volatility25D","TR.Volatility50D","TR.Volatility100D","TR.Volatility150D","TR.Volatility200D","TR.Volatility250D","TR.ROAMean(Period=FY1)","TR.PE","TR.PriceToBVPerShare","TR.PriceToCFPerShare","TR.PriceToSalesPerShare","TR.EVToEBIT","TR.BetaFiveYear","TR.BetaWkly3Y","TR.BetaWkly2Y","TR.BetaDaily180D","TR.BetaDaily90D","TR.Liquidity10DVol", TR_Field('TR.EventTitle', list('EventType'= 'RES')),TR_Field('TR.EventStartDate', list('EventType'= 'RES'))))
stock_data11 <- get_data(paste0('0#.NDX'),       list("TR.TURNOVER","TR.Employees(Period=FY0)","CF_CLOSE","CF_VOLUME", "PCTCHNG", "TR.CommonName","TR.ExchangeName", "TR.FiIssuerSPLongRating", "TR.GICSSector","TR.GICSSubIndustry","CF_DATE", "TR.CompanyMarketCap","CURRENCY","TR.Revenue(Period=FY0)", "TR.ExchangeCountry","TR.TotalReturn1Wk","TR.TotalReturn1Mo","TR.TotalReturn3Mo","TR.TotalReturn6Mo","TR.TotalReturn52Wk","TR.Volatility25D","TR.Volatility50D","TR.Volatility100D","TR.Volatility150D","TR.Volatility200D","TR.Volatility250D","TR.ROAMean(Period=FY1)","TR.PE","TR.PriceToBVPerShare","TR.PriceToCFPerShare","TR.PriceToSalesPerShare","TR.EVToEBIT","TR.BetaFiveYear","TR.BetaWkly3Y","TR.BetaWkly2Y","TR.BetaDaily180D","TR.BetaDaily90D","TR.Liquidity10DVol", TR_Field('TR.EventTitle', list('EventType'= 'RES')),TR_Field('TR.EventStartDate', list('EventType'= 'RES'))))
stock_data12 <- get_data(paste0('0#.GSPTSE'),    list("TR.TURNOVER","TR.Employees(Period=FY0)","CF_CLOSE","CF_VOLUME", "PCTCHNG", "TR.CommonName","TR.ExchangeName", "TR.FiIssuerSPLongRating", "TR.GICSSector","TR.GICSSubIndustry","CF_DATE", "TR.CompanyMarketCap","CURRENCY","TR.Revenue(Period=FY0)", "TR.ExchangeCountry","TR.TotalReturn1Wk","TR.TotalReturn1Mo","TR.TotalReturn3Mo","TR.TotalReturn6Mo","TR.TotalReturn52Wk","TR.Volatility25D","TR.Volatility50D","TR.Volatility100D","TR.Volatility150D","TR.Volatility200D","TR.Volatility250D","TR.ROAMean(Period=FY1)","TR.PE","TR.PriceToBVPerShare","TR.PriceToCFPerShare","TR.PriceToSalesPerShare","TR.EVToEBIT","TR.BetaFiveYear","TR.BetaWkly3Y","TR.BetaWkly2Y","TR.BetaDaily180D","TR.BetaDaily90D","TR.Liquidity10DVol", TR_Field('TR.EventTitle', list('EventType'= 'RES')),TR_Field('TR.EventStartDate', list('EventType'= 'RES'))))
stock_data13 <- get_data(paste0('0#.TRXFLDITP'), list("TR.TURNOVER","TR.Employees(Period=FY0)","CF_CLOSE","CF_VOLUME", "PCTCHNG", "TR.CommonName","TR.ExchangeName","TR.FiIssuerSPLongRating", "TR.GICSSector","TR.GICSSubIndustry","CF_DATE", "TR.CompanyMarketCap","CURRENCY","TR.Revenue(Period=FY0)", "TR.ExchangeCountry","TR.TotalReturn1Wk","TR.TotalReturn1Mo","TR.TotalReturn3Mo","TR.TotalReturn6Mo","TR.TotalReturn52Wk","TR.Volatility25D","TR.Volatility50D","TR.Volatility100D","TR.Volatility150D","TR.Volatility200D","TR.Volatility250D","TR.ROAMean(Period=FY1)","TR.PE","TR.PriceToBVPerShare","TR.PriceToCFPerShare","TR.PriceToSalesPerShare","TR.EVToEBIT","TR.BetaFiveYear","TR.BetaWkly3Y","TR.BetaWkly2Y","TR.BetaDaily180D","TR.BetaDaily90D","TR.Liquidity10DVol", TR_Field('TR.EventTitle', list('EventType'= 'RES')),TR_Field('TR.EventStartDate', list('EventType'= 'RES'))))
stock_data14 <- get_data(paste0('0#.SSMI'),      list("TR.TURNOVER","TR.Employees(Period=FY0)","CF_CLOSE","CF_VOLUME", "PCTCHNG", "TR.CommonName","TR.ExchangeName","TR.FiIssuerSPLongRating", "TR.GICSSector","TR.GICSSubIndustry","CF_DATE", "TR.CompanyMarketCap","CURRENCY","TR.Revenue(Period=FY0)", "TR.ExchangeCountry","TR.TotalReturn1Wk","TR.TotalReturn1Mo","TR.TotalReturn3Mo","TR.TotalReturn6Mo","TR.TotalReturn52Wk","TR.Volatility25D","TR.Volatility50D","TR.Volatility100D","TR.Volatility150D","TR.Volatility200D","TR.Volatility250D","TR.ROAMean(Period=FY1)","TR.PE","TR.PriceToBVPerShare","TR.PriceToCFPerShare","TR.PriceToSalesPerShare","TR.EVToEBIT","TR.BetaFiveYear","TR.BetaWkly3Y","TR.BetaWkly2Y","TR.BetaDaily180D","TR.BetaDaily90D","TR.Liquidity10DVol", TR_Field('TR.EventTitle', list('EventType'= 'RES')),TR_Field('TR.EventStartDate', list('EventType'= 'RES'))))
stock_data15 <- get_data(paste0('0#.BVSP'),      list("TR.TURNOVER","TR.Employees(Period=FY0)","CF_CLOSE","CF_VOLUME", "PCTCHNG", "TR.CommonName","TR.ExchangeName","TR.FiIssuerSPLongRating", "TR.GICSSector","TR.GICSSubIndustry","CF_DATE", "TR.CompanyMarketCap","CURRENCY","TR.Revenue(Period=FY0)", "TR.ExchangeCountry","TR.TotalReturn1Wk","TR.TotalReturn1Mo","TR.TotalReturn3Mo","TR.TotalReturn6Mo","TR.TotalReturn52Wk","TR.Volatility25D","TR.Volatility50D","TR.Volatility100D","TR.Volatility150D","TR.Volatility200D","TR.Volatility250D","TR.ROAMean(Period=FY1)","TR.PE","TR.PriceToBVPerShare","TR.PriceToCFPerShare","TR.PriceToSalesPerShare","TR.EVToEBIT","TR.BetaFiveYear","TR.BetaWkly3Y","TR.BetaWkly2Y","TR.BetaDaily180D","TR.BetaDaily90D","TR.Liquidity10DVol", TR_Field('TR.EventTitle', list('EventType'= 'RES')),TR_Field('TR.EventStartDate', list('EventType'= 'RES'))))
stock_data16 <- get_data(paste0('0#.TRXFLDAFPU'),list("TR.TURNOVER","TR.Employees(Period=FY0)","CF_CLOSE","CF_VOLUME", "PCTCHNG", "TR.CommonName","TR.ExchangeName","TR.FiIssuerSPLongRating", "TR.GICSSector","TR.GICSSubIndustry","CF_DATE", "TR.CompanyMarketCap","CURRENCY","TR.Revenue(Period=FY0)", "TR.ExchangeCountry","TR.TotalReturn1Wk","TR.TotalReturn1Mo","TR.TotalReturn3Mo","TR.TotalReturn6Mo","TR.TotalReturn52Wk","TR.Volatility25D","TR.Volatility50D","TR.Volatility100D","TR.Volatility150D","TR.Volatility200D","TR.Volatility250D","TR.ROAMean(Period=FY1)","TR.PE","TR.PriceToBVPerShare","TR.PriceToCFPerShare","TR.PriceToSalesPerShare","TR.EVToEBIT","TR.BetaFiveYear","TR.BetaWkly3Y","TR.BetaWkly2Y","TR.BetaDaily180D","TR.BetaDaily90D","TR.Liquidity10DVol", TR_Field('TR.EventTitle', list('EventType'= 'RES')),TR_Field('TR.EventStartDate', list('EventType'= 'RES'))))
stock_data17 <- get_data(paste0('0#.TRXFLDRUP'), list("TR.TURNOVER","TR.Employees(Period=FY0)","CF_CLOSE","CF_VOLUME", "PCTCHNG", "TR.CommonName","TR.ExchangeName","TR.FiIssuerSPLongRating", "TR.GICSSector","TR.GICSSubIndustry","CF_DATE", "TR.CompanyMarketCap","CURRENCY","TR.Revenue(Period=FY0)", "TR.ExchangeCountry","TR.TotalReturn1Wk","TR.TotalReturn1Mo","TR.TotalReturn3Mo","TR.TotalReturn6Mo","TR.TotalReturn52Wk","TR.Volatility25D","TR.Volatility50D","TR.Volatility100D","TR.Volatility150D","TR.Volatility200D","TR.Volatility250D","TR.ROAMean(Period=FY1)","TR.PE","TR.PriceToBVPerShare","TR.PriceToCFPerShare","TR.PriceToSalesPerShare","TR.EVToEBIT","TR.BetaFiveYear","TR.BetaWkly3Y","TR.BetaWkly2Y","TR.BetaDaily180D","TR.BetaDaily90D","TR.Liquidity10DVol", TR_Field('TR.EventTitle', list('EventType'= 'RES')),TR_Field('TR.EventStartDate', list('EventType'= 'RES'))))
stock_data18 <- get_data(paste0('0#.MCX10'),     list("TR.TURNOVER","TR.Employees(Period=FY0)","CF_CLOSE","CF_VOLUME", "PCTCHNG", "TR.CommonName","TR.ExchangeName","TR.FiIssuerSPLongRating", "TR.GICSSector","TR.GICSSubIndustry","CF_DATE", "TR.CompanyMarketCap","CURRENCY","TR.Revenue(Period=FY0)", "TR.ExchangeCountry","TR.TotalReturn1Wk","TR.TotalReturn1Mo","TR.TotalReturn3Mo","TR.TotalReturn6Mo","TR.TotalReturn52Wk","TR.Volatility25D","TR.Volatility50D","TR.Volatility100D","TR.Volatility150D","TR.Volatility200D","TR.Volatility250D","TR.ROAMean(Period=FY1)","TR.PE","TR.PriceToBVPerShare","TR.PriceToCFPerShare","TR.PriceToSalesPerShare","TR.EVToEBIT","TR.BetaFiveYear","TR.BetaWkly3Y","TR.BetaWkly2Y","TR.BetaDaily180D","TR.BetaDaily90D","TR.Liquidity10DVol", TR_Field('TR.EventTitle', list('EventType'= 'RES')),TR_Field('TR.EventStartDate', list('EventType'= 'RES'))))
stock_data19 <- get_data(paste0('0#.HNXI'),      list("TR.TURNOVER","TR.Employees(Period=FY0)","CF_CLOSE","CF_VOLUME", "PCTCHNG", "TR.CommonName","TR.ExchangeName","TR.FiIssuerSPLongRating", "TR.GICSSector","TR.GICSSubIndustry","CF_DATE", "TR.CompanyMarketCap","CURRENCY","TR.Revenue(Period=FY0)", "TR.ExchangeCountry","TR.TotalReturn1Wk","TR.TotalReturn1Mo","TR.TotalReturn3Mo","TR.TotalReturn6Mo","TR.TotalReturn52Wk","TR.Volatility25D","TR.Volatility50D","TR.Volatility100D","TR.Volatility150D","TR.Volatility200D","TR.Volatility250D","TR.ROAMean(Period=FY1)","TR.PE","TR.PriceToBVPerShare","TR.PriceToCFPerShare","TR.PriceToSalesPerShare","TR.EVToEBIT","TR.BetaFiveYear","TR.BetaWkly3Y","TR.BetaWkly2Y","TR.BetaDaily180D","TR.BetaDaily90D","TR.Liquidity10DVol", TR_Field('TR.EventTitle', list('EventType'= 'RES')),TR_Field('TR.EventStartDate', list('EventType'= 'RES'))))
stock_data <- rbind(stock_data1,stock_data2,stock_data3,stock_data4,stock_data5,stock_data6,stock_data7,stock_data8,stock_data9,stock_data10,stock_data11,stock_data12,stock_data13,stock_data14,stock_data15,stock_data16,stock_data17,stock_data18,stock_data19)

stock_data[, c(2,3,4,5,6,13,15,17:39)] <- round(sapply(stock_data[, c(2,3,4,5,6,13,15,17:39)], as.numeric),1)

stock_data <- as.tibble(stock_data)
mutate(stock_data, CF_DATE= as.Date(CF_DATE, format= "%Y-%m-%d"))
mutate(stock_data, `Event Start Date`= as.Date(`Event Start Date`, format= "%Y-%m-%d"))


tickers = list("SPY","QQQ.O","IWM","IYR","EEM","EFA","TLT.O","GLD")
etfList <- read.xlsx("GlobalETF.xlsx",1)
tickersLip = as.list(head(etfList$Lipper.RIC))

etf_data <- get_data(tickersLip, list("TR.FundHoldingRIC(StartNum=1,EndNum=10)","TR.FundHoldingName(StartNum=1,EndNum=10)","TR.FundNumberOfShares(StartNum=1,EndNum=10)","TR.FundPercentageOfFundAssets(StartNum=1,EndNum=10)","TR.FundGeographicFocus", "TR.TURNOVER","CF_CLOSE","CF_VOLUME", "PCTCHNG", "TR.CommonName", "CF_DATE", "TR.CompanyMarketCap","CURRENCY","TR.Revenue(Period=FY0)", "TR.ExchangeCountry","TR.TotalReturn1Wk","TR.TotalReturn1Mo","TR.TotalReturn3Mo","TR.TotalReturn6Mo","TR.TotalReturn52Wk","TR.Volatility25D","TR.Volatility50D","TR.Volatility100D","TR.Volatility150D","TR.Volatility200D","TR.Volatility250D"))
etf_data[, c(4,5,7:10,13,15,17:27)] <- round(sapply(etf_data[, c(4,5,7:10,13,15,17:27)], as.numeric),1)

etf_ts <- data.frame()
end_time <- paste0(Sys.Date(),"T15:04:05")
mainEnv <- new.env()
data <- new.env()
models <- new.env()
for(i in tickers){
  result = tryCatch({
    print(i)
    df = get_timeseries(i,list("TIMESTAMP","HIGH","CLOSE","LOW","OPEN","VOLUME"),"2000-01-01T15:04:05",end_time,"daily")
    if(ncol(df) == 7){
      names(df)[c(7)] <- "Instrument"
      #df <- as.tibble(df)
      df <- mutate(df, TIMESTAMP= as.Date(TIMESTAMP, format= "%Y-%m-%d"))
      
      etf_ts <- rbind(etf_ts,df)
      mainEnv[[i]] <- as.xts(df, order.by = df$TIMESTAMP)[, c(2:6)]
      storage.mode(mainEnv[[i]]) <- "numeric"
      names(mainEnv[[i]]) <- c(paste0(i,".Open"), paste0(i,".High"), paste0(i, ".Close"), paste0(i,".Low"),paste0(i,".Volume"))
    } 
    if(ncol(df) != 7) paste(i, "not included")
  }, warning = function(w) {
    print(w)
  }, error = function(e) {
    print(e)
  })
}

etf_ts[, c(2,3,4,5,6)] <- round(sapply(etf_ts[, c(2,3,4,5,6)], as.numeric),1)
etf_ts <- as.tibble(etf_ts)
etf_ts <- mutate(etf_ts, TIMESTAMP= as.Date(TIMESTAMP, format= "%Y-%m-%d"))

save(mainEnv, file = "mainEnv.RData")
save(stock_data, file = "stock_data.RData")
save(etf_data, file = "etf_data.RData")
save(etf_ts, file = "etf_ts.RData")
data <- load("mainEnv.RData")

#eikon_data_xts <- long_to_xts(eikon_data)
#eikon_data_xts_returns <- TTR::ROC(eikon_data_xts, type = 'discrete')










#load(url("https://www.norquant.no/data/norge_eod.RData"))
#eod$Code <- paste0(eod$Code,".OL")
#eod_norge <- eod
#load(url("https://www.norquant.no/data/sverige_eod.RData"))
#eod$Code <- paste0(eod$Code,".ST")
#eod_all <- rbind(eod_norge, eod)



#https://eodhistoricaldata.com/api/fundamentals/ZAL.OL?&api_token=5aad812c21367&filter=General::Name
#https://eodhistoricaldata.com/api/fundamentals/ZAL.OL?&api_token=5aad812c21367&filter=General::Exchange
#https://eodhistoricaldata.com/api/fundamentals/ZAL.OL?&api_token=5aad812c21367&filter=General::CountryName