# macd strategies on all series

maxRows <- 1100

getOrders <- function(store, newRowList, currentPos, params) {
  
  allzero  <- rep(0,length(newRowList)) 
  pos <- allzero
  
  if (is.null(store)) 
    store   <- initStore(newRowList,params$series)
  else 
    store  <- updateStore(store, newRowList, params$series) 
  #position 
  cl7<- newRowList[[7]]$Close 
  clv7<-as.vector(cl7)
  mean7ToDay<-mean(clv7[1:store$iter],na.rm = TRUE)
  
  if (store$iter > params$lookback) {
    startIndex <-  store$iter - params$lookback
   
    for (i in 1:length(params$series)) {      
      cl   	<- newRowList[[params$series[i]]]$Close
      clv<-as.vector(cl)  
      meanToDay<-mean(clv[1:store$iter],na.rm = TRUE)
      ratioTo7 <-mean7ToDay/meanToDay
      ratioTo7<-floor(ratioTo7)
      
      xdata <- matrix(store$cl[startIndex:store$iter,i])
      rsi <- last(RSI(xdata,n=params$lookback))
      
      if (rsi > (50+ params$threshold))
        pos[params$series[i]]<- -ratioTo7
      if (rsi < (50-params$threshold))
        pos[params$series[i]] <- ratioTo7
    }
  }
  marketOrders <-  pos
  
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
