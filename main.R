source('data.R')
source('backtester_v3.1.R')

####################################################################################
# Choices are made by commenting the current choice and uncommenting your choice.
####################################################################################

####################################################################################
# DATA 
####################################################################################

#dataList 	<- getData(part=1) 
#dataList 	<- getData(part=2) 
dataList 	<- getData(part=3) 

####################################################################################
# STRATEGY
####################################################################################

#####################################
# Strategies using only market orders
#####################################


#strategyFile <-'strategies/bbands.R'
#strategyFile <-'strategies/BbandsRsi.R'
#strategyFile <-'strategies/PairsTrading.R'
#strategyFile <-'strategies/macd.R'
#strategyFile <-'strategies/macd_v2.R'
#strategyFile <-'strategies/rsi.R'
#strategyFile <-'strategies/stochastic.R'
#strategyFile <-'strategies/simple_limit_with_SMA.R'
strategyFile <-'strategies/team9.R'

###############################
# Strategies using Limit orders
###############################

#strategyFile <-'strategies/simple_limit_1.R'
#strategyFile <-'strategies/simple_limit_with_position_sizing.R'
#strategyFile <-'strategies/cheating_limit.R'

cat("Sourcing",strategyFile,"\n")
source(strategyFile) # load in getOrders

####################################################################################
# STRATEGY PARAMETERS
####################################################################################



#params     <- list(lookback=50,sdParam=1.5,series=1:10) # params for bbands strategy
#params		<- list(spreadPercentage=0.5,inventoryLimits=rep(10000,10)) # params for limit strategy
#params		<- list(spreadPercentage=0.025,inventoryLimits=rep(1000,10),maxPositionSize=50) # params for limit strategy with position sizing
#params   <-list(lookback=180, series=1:10) #params for pairs trading
#params <- list(lookbackB=20,sdParam=1.5,lookbackR=20,threshold=30,series=1:10)#bbands&rsi
params   <-list(series=1:10,lookback1=26,lookback2=12,lookback3=40, #params for pairs trading
                spreadPercentage=0.8,maxPositionSize=70,                 #params for limit order
                lookbackB=11,sdParam=1.2,seriesB=1:10,     #params for bbands
                lookbackM=37,seriesM=c(2,3,9),                           #macd
                lookbackR=15,threshold=10,seriesR=1:10)    #params for rsi
####################################################################################
# BACKTEST PARAMETERS
####################################################################################

#numOfDays 	<- 200 # don't use all available days to start with!
#dataList	<- lapply(dataList, function(x) x[1:numOfDays])
#dataList	<- lapply(dataList, function(x) x[(numOfDays+1):nrow(dataList[[1]])])

sMult		<- 0.20 # slippage multiplier

####################################################################################
# DO BACKTEST
####################################################################################

results 	<- backtest(dataList, getOrders, params, sMult)
pfolioPnL 	<- plotResults(dataList,results$pnlList)
cat("proportion of time in the market: ", results$k, "\n")

