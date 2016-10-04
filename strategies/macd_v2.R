# macd strategies on all series

maxRows <- 1100 # used to initialize a matrix to store closing prices

getOrders <- function(store, newRowList, currentPos, params) {
  
  allzero  <- rep(0,length(newRowList)) 
  
  pos <- allzero
  
  if (is.null(store)) 
    store   <- initStore(newRowList,params$series)
  else 
    store  <- updateStore(store, newRowList, params$series)
  
  cl7<- newRowList[[7]]$Close #input all close price data of series 7
  
  clv7<-as.vector(cl7)#convert the data in to vector format(clv7 means close price of series 7)
  
  mean7ToDay<-mean(clv7[1:store$iter],na.rm = TRUE)#calculate the mean of all cuurent known data 
  #of series 7 everyday (no over looking)
  
  if (store$iter > params$lookback) {
    startIndex <-  store$iter - params$lookback
    
    for (i in 1:length(params$series)) {
      cl     <- newRowList[[params$series[i]]]$Close
      clv<-as.vector(cl)
      meanToDay<-mean(clv[1:store$iter],na.rm = TRUE)
      ratioTo7 <-mean7ToDay/meanToDay
      ratioTo7<-floor(ratioTo7)
      
      xdata <- matrix(store$cl[startIndex:store$iter,i])
      MACD  <- last(MACD(xdata,12,26,9,maType="EMA"))[c("macd","signal")]
      
      if (MACD["macd"] > MACD["signal"]&&MACD["macd"]>0) {
        pos[params$series[i]] <- -ratioTo7 #short
      } 
      else if (MACD["macd"] < MACD["signal"]&&MACD["macd"]<0) {
        pos[params$series[i]] <- ratioTo7 #long
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

