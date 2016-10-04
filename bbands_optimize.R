source('data.R')
source('backtester_v3.1.R')
source('strategies/bbands.R') 


#numOfDays   <- 200
dataList    <- getData(part=1) 
#dataList    <- lapply(dataList, function(x) x[1:numOfDays])
#dataList  <- lapply(dataList, function(x) x[(numOfDays+1):nrow(dataList[[1]])])

sMult       <- 0.2 # slippage multiplier

lookbackSeq <- seq(from=7,to=19,by=1)
sdParamSeq  <- seq(from=0.9,to=1.5,by=0.1)
# sdParamSeq  <- seq(from=1.5,to=2,length.out=100) # the way to specify how
# many options you want for this param
paramsList  <- list(lookbackSeq,sdParamSeq)

numberComb <- prod(sapply(paramsList,length))

resultsMatrix   <- matrix(nrow=numberComb,ncol=3)
colnames(resultsMatrix) <- c("lookback","sdParam","SharpeRatio")
pfolioPnLList   <- vector(mode="list",length=numberComb)
count           <- 1

for (lookback in lookbackSeq) {
    for (sdParam in sdParamSeq) {
        params <- list(lookback=lookback,sdParam=sdParam,series7=7,series=10) #added series7=7 to input series7 data
        results <- backtest(dataList, getOrders, params, sMult)
        pfolioPnL <- plotResults(dataList,results$pnlList)
        pfolioSharpe <- pfolioPnL$sharpeAgg
        resultsMatrix[count,] <- c(lookback,sdParam,pfolioSharpe)
        pfolioPnLList[[count]]<- pfolioPnL
        cat("Just completed",count,"out of",numberComb,"\n")
        print(resultsMatrix[count,])
        count <- count + 1      
    }
}

print(resultsMatrix[order(resultsMatrix[,"SharpeRatio"]),])
