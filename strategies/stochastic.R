# stochastic strategies on all series

maxRows <- 1100 # used to initialize a matrix to store closing prices

getOrders <- function(store, newRowList, currentPos, params) {
  
  allzero  <- rep(0,length(newRowList)) 
  
  if (is.null(store)) 
    store 	<- initStore(newRowList,params$series)
  else 
    store	<- updateStore(store, newRowList, params$series)
  
  marketOrders <- -currentPos
  pos <- allzero
  
  if (store$iter > params$lookback) {
    startIndex <-  store$iter - params$lookback
    #cat(startIndex,store$iter,'\n')
    for (i in 1:length(params$series)) {
      cl 	<- newRowList[[params$series[i]]]$Close
      stoch  <-
        last(stoch(store$cl[startIndex:store$iter,i],14,3,3))[c("fastK","fastD")]
      
      if (stoch["fastK"] > stoch["fastD"]) {
        pos[params$series[i]] <- 1 #long
      }
      else if (stoch["fastK"] < stoch["fastD"]) {
        pos[params$series[i]] <- -1 #sort
      }
    }
  }
  marketOrders <- marketOrders + pos
  
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
