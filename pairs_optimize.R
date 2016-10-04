source('data.R')
source('backtester_v3.1.R')
source('strategies/pairstest_1.R') 
maxRows  <-1100
#numOfDays   <- 200
dataList    <- getData(part=1) ###
#dataList    <- lapply(dataList, function(x) x[1:numOfDays])

sMult       <- 0.2 # slippage multiplier

lookback1Seq <-seq(from=10,to=100,by=10)
lookback2Seq <-seq(from=10,to=100,by=10)
lookback3Seq <-seq(from=10,to=100,by=10)
# the way to specify how many options you want for this param

paramsList  <- list(lookback1Seq,lookback2Seq,lookback3Seq)
numberComb <- prod(sapply(paramsList,length))

resultsMatrix   <- matrix(nrow=numberComb,ncol=4)
colnames(resultsMatrix) <- c("lookback1","lookback2","lookback3","SharpeRatio")

pfolioPnLList   <- vector(mode="list",length=numberComb)
count           <- 1

for (lookback1 in lookback1Seq) {
  for (lookback2 in lookback2Seq){
    for(lookback3 in lookback3Seq){
  
        params <- list(lookback1=lookback1,lookback2=lookback2,lookback3=lookback3,series7=7,series=1:10) 
        
        results <- backtest(dataList, getOrders, params, sMult)
        pfolioPnL <- plotResults(dataList,results$pnlList)
        pfolioSharpe <- pfolioPnL$sharpeAgg
        
        resultsMatrix[count,] <- c(lookback1,lookback2,lookback3,pfolioSharpe)
        
        pfolioPnLList[[count]]<- pfolioPnL
        cat("Just completed",count,"out of",numberComb,"\n")
        print(resultsMatrix[count,])
        count <- count + 1      
    }
  }
}
print(head(resultsMatrix[order(resultsMatrix[,"SharpeRatio"]),],100))
