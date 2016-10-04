maxRows <- 1100 # used to initialize a matrix to store closing prices
#params is a list,like params <- list(lookbackb=50,sdParam=1.5,lookbackr=10,threshold=25,series=1:10)
#lookbackb and sdParam are the parameters of bbands, lookbackr and threshold are the parameters of rsi,and series are the markets.
getOrders <- function(store, newRowList, currentPos, params) {

    allzero  <- rep(0,length(newRowList))
    if (is.null(store)) {
        checkParams(params)
        store <- initStore(newRowList,params$series)
    }
    else
	store <- updateStore(store, newRowList, params$series)

    marketOrders <- -currentPos
    pos <- allzero
    if (store$iter > max(params$lookbackb,params$lookbackr)) {
        startIndex <- store$iter - max(params$lookbackb,params$lookbackr)
        for (i in 1:length(params$series)) {
            cl <- newRowList[[params$series[i]]]$Close
            bbands <- last(BBands(store$cl[startIndex:store$iter,i],n=params$lookbackb,sd=params$sdParam))[c("dn","mavg","up")]
            rsi <- last(RSI(store$cl[startIndex:store$iter,i],n=params$lookbackr))
            if (cl > bbands["up"] && rsi > (50 + params$threshold))
                pos[params$series[i]] <- -1 #short
            if (cl < bbands["dn"] && rsi < (50 - params$threshold))
                pos[params$series[i]] <- 1 #long
       }
    }

    marketOrders <- marketOrders + pos
    return(list(store=store,marketOrders=marketOrders,
                            limitOrders1=allzero,
	                    limitPrices1=allzero,
	                    limitOrders2=allzero,
	                    limitPrices2=allzero))

}


checkParams <- function(params) { # make sure params are correct
    if (!"lookbackb" %in% names(params))
        stop("Parameter lookbackb not defined for bbands")
    if (!"sdParam" %in% names(params))
        stop("Parameter sdParam not defined for bbands")
    if (!"lookbackr" %in% names(params))
        stop("Parameter lookbackr not defined for RSI")
    if (!"threshold" %in% names(params))
        stop("Parameter lookback not defined for  RSI")
    if (!"series" %in% names(params))
        stop("Parameter series not defined for this strategy")
    if (params$threshold < 0 || params$threshold > 50)
        stop("Parameter lookback is not between 0 and 50")
}

initClStore  <- function(newRowList,series) {
	clStore <- matrix(0,nrow=maxRows,ncol=length(series))
	clStore <- updateClStore(clStore, newRowList, series, iter=1)
    return(clStore)
}

updateClStore <- function(clStore, newRowList, series, iter) {
    for (i in 1:length(series))
        clStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Close)
    return(clStore)
}

initStore <- function(newRowList,series) {
	return(list(iter=1,cl=initClStore(newRowList,series)))
}

updateStore <- function(store, newRowList, series) {
	store$iter <- store$iter + 1
	store$cl <- updateClStore(store$cl,newRowList,series,store$iter)
	return(store)
}
