
maxRows <- 1100
getOrders <- function(store, newRowList, currentPos, params) {
  
  if (is.null(store)) 
    store   <- initStore(newRowList,params$series)
  else 
    store  <- updateStore(store, newRowList, params$series)
  
  marketOrders<-rep(0,10)
 
  ranges <- sapply(1:length(newRowList),function(i) newRowList[[i]]$High - newRowList[[i]]$Low)
  
  positions <- round(params$maxPositionSize/(ranges+1))
  
  if(store$iter>params$lookback){
    startIndex <- store$iter - params$lookback
    for(i in 1:length(params$series)){
      cl<-newRowList[[params$series[i]]]$Close
      xdata<- as.matrix(store$cl[startIndex:store$iter])
      sma <- last(SMA(xdata,n=params$lookback)) 
      
      smaa <- sapply(1:length(sma), function(i) ifelse(i>=params$lookback,sma[[i]],newRowList[[i]]$Close))}}
 
  
  spread <- sapply(1:length(newRowList),function(i)
    params$spreadPercentage * (newRowList[[i]]$High -newRowList[[i]]$Low))
  
  limitOrders1  <- positions
  limitPrices1  <- sapply(1:length(newRowList),function(i) ifelse(i>=params$lookback, smaa[i]
                                                                  - spread[i]/2,newRowList[[i]]$Close- spread[i]/2))
  
  limitOrders2  <- -positions 
  limitPrices2  <- sapply(1:length(newRowList),function(i)   ifelse(i>=params$lookback,smaa[i]
                                                                   + spread[i]/2,newRowList[[i]]$Close+ spread[i]/2))
  
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=limitOrders1,
              limitPrices1=limitPrices1,
              limitOrders2=limitOrders2,
              limitPrices2=limitPrices2))
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
  store$iter   <- store$iter + 1
  store$cl	<- updateClStore(store$cl,newRowList,series,store$iter) 
  return(store)
}
