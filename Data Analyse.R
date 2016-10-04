source('data.R')
dataList <- getData(part=2)

for (k in 1:10){
  d <- dataList[[k]]
  
  
  dd=as.vector((d$Close))   #pick the Close column from the whole matrix and transform it to vector

  cc <- matrix(nrow=length(dd), ncol = 1)  
  for (i in 2:length(dd)){
    cc[i-1]=(dd[i]-dd[i-1])/dd[i-1]
  }                       #use cc to store the comparison results in a matrix
  
  cc[length(cc)]=0        #set the last row's value as 0, because the comparison result would be 1 less than the origin data
  
  c<-xts(cc, as.Date("1970-1-2")+1:length(cc))    #use c to store the data in xts form
  
  
  
  Q_2 <- 0
  Q_1 <- 0
  Q_0 <- 0
  Q_3 <- 0
  Q_p1 <- 0
  Q_p2 <- 0
  Q_p3 <- 0
  r<-0
  
  e <- apply(c,1,FUN=function(a)

  #test (high+low)/2 vs close
  #{if (a>=1.3*d$Close){a=3}
  # else if(a>=1.2*d$Close){a=2}
  # else if(a>d$Close){a=1}
  #else if (a==d$Close){a=0}
  #else if (a<=(d$Close-0.3*d$Close)){a=-3}
  # else if(a<=(d$Close-0.2*d$Close)){a=-2}
  # else {a=-3}})

  #test (close-open)/open = x
  #test (low-yesterday's low)/yesterday's low=x
  {if (a>=0.02){a=3} 
  else if (a>=0.015){a=2}
  else if (a>=0.0075){a=1}
  else if (a>=-0.0025){a=0}
  else if (a>=-0.0075){a=-1}
  else if (a>=-0.015){a=-2}
  else {a=-3}})
  
  x <- as.xts(e, order_by=as.Date(names(e)))

 
  for(i in e){
    if (i == 0) {Q_0 = Q_0 +1}
    else if (i == 1){Q_1 = Q_1 +1}
    else if (i == 2) {Q_2 = Q_2 +1}
    else if (i==3){Q_3 = Q_3 +1}
    else if (i == -1){Q_p1 = Q_p1 +1}
    else if (i == -2 ){Q_p2 = Q_p2 +1}
    else if (i == -3){ Q_p3 = Q_p3 +1}
  }
  
  Q <- c("List"=k ,"  -3 "=Q_p3,"  -2 "=Q_p2,"-1 "=Q_p1,"  0 "=Q_0,"  1 "=Q_1,"  2 "=Q_2,"  3 "=Q_3)
  print(Q)
  df<-diff(e)

  for(df in df){
    if(df==0){r=r+1}
    else{r=r}
  }
  R<-c("List"=k,r)
  
  print(R)
}
chart_Series(x,type="auto")
