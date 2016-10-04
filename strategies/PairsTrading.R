# This strategy only trades on series 1，2，3，4，9，10.

# The strategy will be long (short) on contract whenever the spread is below (above) 
# the entry signal, which is the 1st quarter (3rd quarter) of the spread data
# the min and max data of spread are used as the stop loss

maxRows <- 1100 # used to initialize a matrix to store closing prices

getOrders <- function(store, newRowList, currentPos, params) {
  
  allzero  <- rep(0,length(newRowList)) 
  pos <- allzero
  
  if (is.null(store)) 
    store   <- initStore(newRowList,params$series)
  else 
    store	<- updateStore(store, newRowList, params$series)
  
  
  #Pairs Trading  
  
  #position 
  #add a new param"series2" aims to input all close price data of series 7
  cl7<- newRowList[[params$series7]]$Close
  #convert the data in to vector format(clv7 means close price of series 7 in vector form)
  clv7<-as.vector(cl7)
  #calculate the mean of all cuurent known data of series 7 everyday (no over looking)
  #as long as the store$iter>params$lookback and ignore the NA data
  mean7ToDay<-mean(clv7[1:store$iter],na.rm = TRUE)
  
  
  
  #pairs 1&2
  if (store$iter > params$lookback1) {
    pairs1<-cbind(store$cl[1:store$iter,1],store$cl[1:store$iter,2])
    
    x=pairs1[,1]
    y=pairs1[,2]
    
    data <- data.frame(a=y,b=x)
    
    fit <- lm(a~b+0,data=data)
    
    beta1<-coef(fit)[1]
    spread1 <- y - beta1*x
    test1=summary(spread1)
    
    
    cl1     <- newRowList[[params$series[1]]]$Close
    #convert the close price data of the series 1 into vector form
    clv1<-as.vector(cl1)
    #calculate the mean of all cuurent known data of the series 1 everyday (no over looking) and ignore the NA data
    meanToDay1<-mean(clv1[1:store$iter],na.rm = TRUE)
    
    #calculate the ratio between the series 1 and series 7 
    #using their everyday updated mean of all previous close price
    #to set the size of its market order to balance among 10 series
    ratio1To7 <-mean7ToDay/meanToDay1
    #take the integer part of the result as the size of market order
    ratio1To7<-floor(ratio1To7)
    
    cl2     <- newRowList[[params$series[2]]]$Close
    #convert the close price data of the series 2 into vector form
    clv2<-as.vector(cl2)
    #calculate the mean of all cuurent known data of the series 2 everyday (no over looking) and ignore the NA data
    meanToDay2<-mean(clv2[1:store$iter],na.rm = TRUE)
    
    #calculate the ratio between the series 2 and series 7 
    #using their everyday updated mean of all previous close price
    #to set the size of its market order to balance among 10 series
    ratio2To7 <-mean7ToDay/meanToDay2
    #take the integer part of the result as the size of market order
    ratio2To7<-floor(ratio2To7)
    
    if(beta1>0.027){
      if(spread1[store$iter]>test1[5]&&spread1[store$iter]<test1[6]){
        pos[params$series[1]] <- ratio1To7
        pos[params$series[2]] <- -ratio2To7
      }else if(spread1[store$iter]<test1[2]&&spread1[store$iter]>test1[1]){
        pos[params$series[1]] <- -ratio1To7
        pos[params$series[2]] <- ratio2To7
      }
    }
  }   #pairs1&2 ends
  
  
  #pairs 3&4
  if (store$iter > params$lookback2) {
    pairs2<-cbind(store$cl[1:store$iter,3],store$cl[1:store$iter,4])
    x2=pairs2[,1]
    y2=pairs2[,2]
    
    data2<- data.frame(c=y2,d=x2)
    fit2<- lm(c~d+0,data=data2)
    
    beta2<-coef(fit2)[1]
    spread2 <- y2 - beta2 * x2
    test2=summary(spread2)
    
    
    cl3     <- newRowList[[params$series[3]]]$Close
    clv3<-as.vector(cl3)
    meanToDay3<-mean(clv3[1:store$iter],na.rm = TRUE)
    ratio3To7 <-floor(mean7ToDay/meanToDay3)
    
    cl4     <- newRowList[[params$series[4]]]$Close
    clv4<-as.vector(cl4)
    meanToDay4<-mean(clv4[1:store$iter],na.rm = TRUE)
    ratio4To7 <-floor(mean7ToDay/meanToDay4)
    
    if(beta2>0.099){
      if(spread2[store$iter]>test2[5]&&spread2[store$iter]<test2[6]){
        pos[params$series[3]] <- ratio3To7
        pos[params$series[4]] <- -ratio4To7
      }else if(spread2[store$iter]<test2[2]&&spread2[store$iter]>test2[1]){
        pos[params$series[3]] <- -ratio3To7
        pos[params$series[4]] <- ratio4To7
      }
    }
  }#pairs 3&4 ends
  
  
  #pairs 9&10
  if (store$iter > params$lookback3) {
    pairs9<-cbind(store$cl[1:store$iter,9],store$cl[1:store$iter,10])
    
    x9=pairs9[,1]
    y10=pairs9[,2]
    
    data3 <- data.frame(a=y10,b=x9)
    
    fit3 <- lm(a~b+0,data=data3)
    
    beta3<-coef(fit3)[1]
    spread3 <- y10 - beta3*x9
    test3=summary(spread3)
    
    cl9     <- newRowList[[params$series[9]]]$Close
    clv9<-as.vector(cl9)
    meanToDay9<-mean(clv9[1:store$iter],na.rm = TRUE)
    ratio9To7 <-floor(mean7ToDay/meanToDay9)
    
    cl10     <- newRowList[[params$series[10]]]$Close
    clv10<-as.vector(cl10)
    meanToDay10<-mean(clv10[1:store$iter],na.rm = TRUE)
    ratio10To7 <-floor(mean7ToDay/meanToDay10)
    
    if(beta3>0.14){
      if(spread3[store$iter]>test3[5]&&spread3[store$iter]<test3[6]){
        pos[params$series[9]] <- ratio9To7
        pos[params$series[10]] <- -ratio10To7
      }else if(spread3[store$iter]<test3[2]&&spread3[store$iter]>test3[1]){
        pos[params$series[9]] <- -ratio9To7
        pos[params$series[10]] <- ratio10To7
      }
    }
  }#pairs9&10 ends

  marketOrders<-pos
  
  
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