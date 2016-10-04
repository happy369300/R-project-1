# FOR A GENERAL EXPLANATION OF REQUIREMENTS ON getNewPosList see rsi.R 

# This strategy only trades on certain markets, which is encoded in params$series.

# The strategy will be long (short) on contract whenever the close is below (above) 
# the lower (upper) Bollinger Band of the close.

maxRows <- 1100 # used to initialize a matrix to store closing prices

getOrders <- function(store, newRowList, currentPos, params) {

    allzero  <- rep(0,length(newRowList)) 

	if (is.null(store)) 
		store 	<- initStore(newRowList,params$series)
	else 
		store	<- updateStore(store, newRowList, params$series)
	
	  #marketOrders <- -currentPos
    pos <- allzero

    if (store$iter > params$lookback) {
       startIndex <-  store$iter - params$lookback
       #cat(startIndex,store$iter,'\n')
       
       #add a new param"series7" aims to input all close price data of series 7
       cl7<- newRowList[[params$series7]]$Close
       #convert the data in to vector format(clv7 means close price of series 7 in vector form)
       clv7<-as.vector(cl7)
       #calculate the mean of all cuurent known data of series 7 everyday (avoid look-ahead bias)
       #as long as the store$iter>params$lookback and ignore the NA data
       mean7ToDay<-mean(clv7[1:store$iter],na.rm = TRUE)
       
       
       for (i in 1:length(params$series)) {
           
           cl 		<- newRowList[[params$series[i]]]$Close
           #convert the close price data of the series into vector form
           clv<-as.vector(cl)
           #calculate the mean of all cuurent known data of the series everyday (avoid look-ahead bias) and ignore the NA data
           meanToDay<-mean(clv[1:store$iter],na.rm = TRUE)
           
           #calculate the ratio between the series and series 7 
           #using their everyday updated mean of all previous close price
           #to set the size of its market order to balance among 10 series
           ratioTo7 <-mean7ToDay/meanToDay
           #take the integer part of the result as the size of market order
           ratioTo7<-floor(ratioTo7)
           
           bbands 	<-
               last(BBands(store$cl[startIndex:store$iter,i],n=params$lookback,sd=params$sdParam))[c("dn","up")]
           if (cl < bbands["dn"]) {
               pos[params$series[i]] <- ratioTo7
           }
           else if (cl > bbands["up"]) {
               pos[params$series[i]] <- -ratioTo7
           }
       }
    }
    marketOrders <- pos

	return(list(store=store,marketOrders=marketOrders,
	                        limitOrders1=allzero,
	                        limitPrices1=allzero,
	                        limitOrders2=allzero,
	                        limitPrices2=allzero))
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
	store$iter 	<- store$iter + 1
	store$cl	<- updateClStore(store$cl,newRowList,series,store$iter) 
	return(store)
}
