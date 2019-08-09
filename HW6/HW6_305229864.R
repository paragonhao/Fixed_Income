rm(list <- ls())

bdttree <- as.data.frame(read.csv("HW6/Homework 6 bdttree.csv", header = F))

# term structure
dt <- as.data.frame(read.csv("HW6/Homework 6 pfilea.csv", header = F))

#volatility
voldat <- as.data.frame(read.csv("HW6/Homework 6 voldat.csv", header = F))

deltat <- 0.5
result <- matrix(0,nrow(dt),nrow(dt))
n <- ncol(result)

# calculate Rate error 
getBDTRates <- function(rateguess, timeCount){
  #actualTime <- (timeCount-1)/2
  volTime <- voldat[timeCount-1,1]
  
  #Create cash flow table and set final set value as 1
  #Cashflow table has one index more than time
  cftable <- matrix(0,nrow = timeCount+1, ncol = timeCount+1) 
  cftable[,timeCount+1] <- 1
  
  #using rguess, we can get all rates for last column
  currentRate <- sapply(1:timeCount,function(x){
    rateguess * exp(-2*(x-1)* volTime * sqrt(deltat))
    })
  
  currentRate <- c(currentRate,rep(0,(n-timeCount))) 
  result[,timeCount] <- currentRate
  
  # looping thru each col in the cashtable
  for(cfCount in (timeCount:1)){
    
    # sapply here is used to go thru the item in the current col of the cash table
    cf <- sapply(1:cfCount, function(x){
      (0.5 * cftable[x, cfCount+1] + 0.5 * cftable[x+1, cfCount+1])/(1+(result[x,cfCount]/2))
    })
    cftable[,cfCount] <- c(cf,rep(0,nrow(cftable)-cfCount))
  }
  
  return(cftable[1,1] -dt[timeCount,1])
}

# find out the first r 
result[1,1] <- ((1/dt[1,1])-1)*2


for(timeCount in (2:n)){
  # use uniroot to numerically solve for 
  optm <- uniroot(f=getBDTRates,interval=c(0,1),timeCount=timeCount,extendInt = "downX",maxiter = 10000)
  
  rates <- sapply(1:timeCount,function(x){
    optm$root*exp(-2*(x-1) * voldat[timeCount-1,1] * sqrt(deltat))
    })
  result[,timeCount] <- c(rates,rep(0,(n-timeCount))) 
}

plot(y=result[1,],x=c(1:30),main="Plot of calculated r star",type="l",ylab="rates",xlab="time")
points(y=result[1,],x=c(1:30),cex=2)
lines(y=bdttree[1,],x=c(1:30),col="red")
points(y=result[1,],x=c(1:30),pch=2)
legend("bottomright",c("calculated","provided"),pch=c(1,2))

GetProb <- function(n){
  term1 <- 0.5^n
  term2 <- sapply(c(0:n),function(x){factorial(n)/(factorial(n-x)*factorial(x))})
  return(term1*term2)
}

expected <- sapply(1:ncol(result),function(x){
  GetProb(x-1) %*% result[(1:x),x]
  })

forward = numeric()
pfilea  <- dt$V1
forward[1] = (1/pfilea[1]-1)*2

for (i in 2:30) {
  forward[i] = (pfilea[(i-1)]/pfilea[i]-1)*2
}

plot(y=expected,x=c(1:30),main="expected,forward rates with time horizon",xlab="time",ylab="Expected Rates",type="l")
lines(y=forward,x=c(1:30),col="red")
legend("bottomright",c("expected rates","forward rates"),col=c("black","red"),lty=1)
