#params   <-list(series=1:10,lookback1=26,lookback2=12,lookback3=40,      #params for pairs trading
                #spreadPercentage=0.8,maxPositionSize=70,                 #params for limit order
                #lookbackB=11,sdParam=1.2,seriesB=1:10,                   #params for bbands
                #lookbackM=37,seriesM=c(2,3,9),                           #params for macd
                #lookbackR=14,threshold=11,seriesR=1:10)                  #params for rsi



# This is a multi-indicator strategy created by team9, it consist of 
#pairs trading, bbands, macd, rsi and simple limit with position sizing strategies

maxRows <- 1100 # used to initialize a matrix to store closing prices

getOrders <- function(store, newRowList, currentPos, params) {  #major function of strategies
  
  allzero  <- rep(0,length(newRowList)) #initialize the variables that would be used later
  
  pos <- allzero #market order position of paris trading strategy
  
  posbuy<- allzero #market order position of bbands strategy
  possell<- allzero
  
  RSIsell<-allzero #market order position of RSI strategy
  RSIbuy<-allzero
  
  posMb<-allzero  #market order position of MACD strategy
  posMs<-allzero
  
  if (is.null(store))  #initialize data storage
    store   <- initStore(newRowList,params$series)
  else 
    store  <- updateStore(store, newRowList, params$series)
  
#Pairs Trading(works on pairs 1&2,3&4,9&10)  
    
  #position 
  cl7<- newRowList[[7]]$Close #input all close price data of series 7
  
  clv7<-as.vector(cl7)#convert the data in to vector format(clv7 means close price of series 7)
  
  mean7ToDay<-mean(clv7[1:store$iter],na.rm = TRUE)#calculate the mean of all cuurent known data 
                                                   #of series 7 everyday (no over looking)
  
 
  #pairs 1&2 begin
  if (store$iter > params$lookback1) {
    pairs1<-cbind(store$cl[1:store$iter,1],store$cl[1:store$iter,2])#combine the data of two series
    
    x=pairs1[,1] #close price of series 1
    y=pairs1[,2] #close price of series 2
    data <- data.frame(a=y,b=x) #convert the data structure to data frame
    fit <- lm(a~b+0,data=data)  #calculate the liner model value of the two series
    
    beta1<-coef(fit)[1]        #calculate coefficience value
    spread1 <- y - beta1*x     #calculate the spread
    test1=summary(spread1)     #store the summary value of all the past spread
    

    cl1     <- newRowList[[params$series[1]]]$Close
    clv1<-as.vector(cl1)#convert the close price data of the series 1 into vector form
    
    meanToDay1<-mean(clv1[1:store$iter],na.rm = TRUE)
    #calculate the mean of all cuurent known data of the series 1 everyday (no over looking) 
    #and ignore the NA data
    
    ratio1To7 <-mean7ToDay/meanToDay1
    #calculate the ratio between the series 1 and series 7 
    #using their everyday updated mean of all previous close price
    #to set the size of its market order to balance among 10 series
    
    ratio1To7<-floor(ratio1To7)#take the integer part of the result as the size of market order
    
    cl2     <- newRowList[[params$series[2]]]$Close
    clv2<-as.vector(cl2)#convert the close price data of the series 2 into vector form
    
    meanToDay2<-mean(clv2[1:store$iter],na.rm = TRUE)
    #calculate the mean of all cuurent known data of the series 2 everyday (no over looking)
    #and ignore the NA data
    
    #calculate the ratio between the series 2 and series 7 
    #using their everyday updated mean of all previous close price
    #to set the size of its market order to balance among 10 series
    ratio2To7 <-mean7ToDay/meanToDay2
    #take the integer part of the result as the size of market order
    ratio2To7<-floor(ratio2To7)
  
    #using the loweset beta between 1&2 pairs in data1 as critical value of future stop loss
    #the if condition (beta1>0.027) means if the difference between this pair did not go too far 
    #which could result to risks, the pairs trading between 1&2 can be conducted
    if(beta1>0.027){
      if(spread1[store$iter]>test1[5]&&spread1[store$iter]<test1[6]){ 
        pos[params$series[1]] <- ratio1To7
        pos[params$series[2]] <- -ratio2To7
      }else if(spread1[store$iter]<test1[2]&&spread1[store$iter]>test1[1]){
        pos[params$series[1]] <- -ratio1To7
        pos[params$series[2]] <- ratio2To7
       }
    }
  }#pairs1&2 ends
    
  
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
      
      #using the loweset beta between 3&4 pairs in data1 as critical value of future stop loss
      #the if condition (beta1>0.099) means if the difference between this pair did not go too far 
      #which could result to risks, the pairs trading between 3&4 can be conducted
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
#Pairs Trading (works on pairs 1&2,3&4,9&10) end   

  
  
#bbands(works on all ten series)
  if (store$iter > params$lookbackB) {
    startIndex <-  store$iter - params$lookbackB
    
    for (i in 1:length(params$series)) {
      cl   	<- newRowList[[params$series[i]]]$Close
      clv<-as.vector(cl)
      meanToDay<-mean(clv[1:store$iter],na.rm = TRUE)
      ratioTo7 <-mean7ToDay/meanToDay#
      ratioTo7<-floor(ratioTo7)#
     
      bbands 	<-
        last(BBands(store$cl[startIndex:store$iter,i],n=params$lookbackB,sd=params$sdParam))[c("dn","up")]
      if (cl < bbands["dn"]) {
        posbuy[params$series[i]] <- ratioTo7
      }
      else if (cl > bbands["up"]) {
        possell[params$series[i]] <- -ratioTo7
      }
     }
  }
#bbands (works on all ten series) end
 
  
  
#macd (works on series 2,3,9)
  if (store$iter > params$lookbackM) {
    startIndex <-  store$iter - params$lookbackM
    
    for (i in 1:length(params$seriesM)) {
      cl     <- newRowList[[params$seriesM[i]]]$Close
      clv<-as.vector(cl)
      meanToDay<-mean(clv[1:store$iter],na.rm = TRUE)
      ratioTo7 <-mean7ToDay/meanToDay
      ratioTo7<-floor(ratioTo7)
      
      xdata <- matrix(store$cl[startIndex:store$iter,i])
      MACD  <- last(MACD(xdata,12,26,9,maType="EMA"))[c("macd","signal")]
      
      if (MACD["macd"] > MACD["signal"]){
        posMs[params$seriesM[i]] <- -ratioTo7 #short
      } 
      else if (MACD["macd"] < MACD["signal"]) {
        posMb[params$seriesM[i]] <- ratioTo7 #long
      }
    }
  }
#macd (works on series 2,3,9) end
 
  
  
#rsi (works on all ten series)
  if (store$iter > params$lookbackR) {
    startIndex <-  store$iter - params$lookbackR
    
    for (i in 1:length(params$seriesR)) {
      
      cl     <- newRowList[[params$seriesR[i]]]$Close
      clv<-as.vector(cl)
      meanToDay<-mean(clv[1:store$iter],na.rm = TRUE)
      ratioTo7 <-mean7ToDay/meanToDay
      ratioTo7<-floor(ratioTo7)

      xdata <- matrix(store$cl[startIndex:store$iter,i])
      rsi <- last(RSI(xdata,n=params$lookbackR))
      
      if (rsi > (50+ params$threshold)){
        RSIsell[params$seriesR[i]]<-  -ratioTo7
      }
      if (rsi < (50-params$threshold)){
        RSIbuy[params$seriesR[i]] <- ratioTo7
      }
    }
  }
#RSI (works on all ten series) ends
 
  
#add all market orders from pairs trading, bbands, macd and rsi strategies together
marketOrders <-pos+posbuy+possell+posMb+posMs+RSIbuy+RSIsell
  #cat("marketOrders",marketOrders,"\n")



  
#Limit orders with position sizing (works on series 1,2,3,6,7,8,9,10)
  ranges <- sapply(1:length(newRowList),function(i) newRowList[[i]]$High - newRowList[[i]]$Low)
  
  positions <- round(params$maxPositionSize/(ranges+1))
  positions[4]<-0 
  positions[5]<-0 
 
  spread <- sapply(1:length(newRowList),function(i) params$spreadPercentage * (newRowList[[i]]$High -newRowList[[i]]$Low))
  
  limitOrders1  <- positions # BUY LIMIT ORDERS
  limitPrices1  <- sapply(1:length(newRowList),function(i) newRowList[[i]]$Close - spread[i]/2)
  
  limitOrders2  <- -positions # SELL LIMIT ORDERS
  limitPrices2  <- sapply(1:length(newRowList),function(i) newRowList[[i]]$Close + spread[i]/2)
#Limit orders with position sizing (works on series 1,2,3,6,7,8,9,10) end

  
  
  
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
  store$iter 	<- store$iter + 1
  store$cl	<- updateClStore(store$cl,newRowList,series,store$iter) 
  return(store)
}
