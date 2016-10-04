source('data.R')
source('backtester_v3.1.R')
source('strategies/stochastic.R') 

numOfDays   <- 1100
#dataList    <- getData(part=1) 
dataList    <- getData(part=2)
dataList    <- lapply(dataList, function(x) x[1:numOfDays])

sMult       <- 0.2 # slippage multiplier

lookbackSeq <- seq(from=20,to=800,by=20)
# many options you want for this param
paramsList  <- list(lookbackSeq)

numberComb <- prod(sapply(paramsList,length))

resultsMatrix   <- matrix(nrow=numberComb,ncol=2)
colnames(resultsMatrix) <- c("lookback","SharpeRatio")
pfolioPnLList   <- vector(mode="list",length=numberComb)
count           <- 1

for (lookback in lookbackSeq) {
  params <- list(lookback=lookback,series=1:10)
  results <- backtest(dataList, getOrders, params, sMult)
  pfolioPnL <- plotResults(dataList,results$pnlList)
  pfolioSharpe <- pfolioPnL$sharpeAgg
  resultsMatrix[count,] <- c(lookback,pfolioSharpe)
  pfolioPnLList[[count]]<- pfolioPnL
  cat("Just completed",count,"out of",numberComb,"\n")
  print(resultsMatrix[count,])
  count <- count + 1      
}

print(resultsMatrix[order(resultsMatrix[,"SharpeRatio"]),])
