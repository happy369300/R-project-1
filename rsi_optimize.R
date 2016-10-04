source('data.R')
source('backtester_v3.1.R')
source('strategies/rsi.R') 

numOfDays   <- 1100
#dataList   <- getData(part=1) 
dataList    <- getData(part=2) 
dataList    <- lapply(dataList, function(x) x[1:numOfDays])

sMult       <- 0.2 # slippage multiplier

lookbackSeq <- seq(from=8,to=20,by=1)
thresholdSeq <- seq(from=4,to=20,by=1)

paramsList  <- list(lookbackSeq,thresholdSeq)

numberComb <- prod(sapply(paramsList,length))

resultsMatrix   <- matrix(nrow=numberComb,ncol=3)
colnames(resultsMatrix) <- c("lookback","threshold","SharpeRatio")
pfolioPnLList   <- vector(mode="list",length=numberComb)
count           <- 1

for (lookback in lookbackSeq) {
    for (threshold in thresholdSeq) {
        params <- list(lookback=lookback,threshold=threshold,series=1:10)
        results <- backtest(dataList, getOrders, params, sMult)
        pfolioPnL <- plotResults(dataList,results$pnlList)
        pfolioSharpe <- pfolioPnL$sharpeAgg
        resultsMatrix[count,] <- c(lookback,threshold,pfolioSharpe)
        pfolioPnLList[[count]]<- pfolioPnL
        cat("Just completed",count,"out of",numberComb,"\n")
        print(resultsMatrix[count,])
        count <- count + 1      
    }
}

print(resultsMatrix[order(resultsMatrix[,"SharpeRatio"]),])
