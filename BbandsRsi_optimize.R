source('data.R')
source('backtester_v3.1.R')
dataList <- getData(part=1)

strategyFile <-'strategies/BbandsRsi.R'

cat("Sourcing",strategyFile,"\n")
source(strategyFile) # load in getOrders

params <- list(lookbackb=50,sdParam=1.5,lookbackr=10,threshold=20,series=5:8)#overbuy 70,oversell 30
#params <- list(lookbackb=50,sdParam=1.5,lookbackr=10,threshold=30,series=5:8)#overbuy 80,oversell 20


sMult <- 0.20 # slippage multiplier

####################################################################################
# DO BACKTEST
####################################################################################

results <- backtest(dataList, getOrders, params, sMult)
pfolioPnL <- plotResults(dataList,results$pnlList)


####################################################################################
# Optimize Parameters
####################################################################################
sMult <- 0.1 # slippage multiplier

lookbackbSeq <- seq(from=20,to=50,by=10) #bbands's lookback
sdParamSeq <- seq(from=1,to=2,by=0.5)
lookbackrSeq <- seq(from=5,to=20,by=5) #rsi's lookback
thresholdSeq <- seq(from=10,to=30,by = 5)

paramsList <- list(lookbackbSeq, sdParamSeq, lookbackrSeq, thresholdSeq)
numberComb <- prod(sapply(paramsList,length))


for (series in 5:8) {
    resultsMatrix   <- matrix(nrow=numberComb,ncol=5)
    colnames(resultsMatrix) <- c("lookbackb","sdParam","lookbackr","threshold","SharpeRatio")
    pfolioPnLList <- vector(mode="list",length=numberComb)
    count <- 1

    for (lookbackb in lookbackbSeq) {
        for (sdParam in sdParamSeq) {
            for (lookbackr in lookbackrSeq) {
                for (threshold in thresholdSeq) {
                    params <- list(lookbackb=lookbackb,sdParam=sdParam,lookbackr=lookbackr,threshold=threshold,series=series)
                    results <- backtest(dataList, getOrders, params, sMult)
                    pfolioPnL <- plotResults(dataList,results$pnlList)
                    pfolioSharpe <- pfolioPnL$sharpeAgg
                    resultsMatrix[count,] <- c(lookbackb,sdParam,lookbackr,threshold,pfolioSharpe)
                    pfolioPnLList[[count]]<- pfolioPnL
                    cat("Just completed the ",series,"th series",count,"out of",numberComb,"\n")
                    print(resultsMatrix[count,])
                    count <- count + 1
                }
            }
        }
    }
    cat("***** series",series,"'s backtest result *****\n")
    print(resultsMatrix[order(resultsMatrix[,"SharpeRatio"]),])
    write.csv(resultsMatrix[order(resultsMatrix[,"SharpeRatio"]),],paste("series",series,"results of parameters optimize.csv"))
}

