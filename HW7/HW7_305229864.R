suppressMessages(require(data.table))

rm(list=ls())

# correlation matrix
corrin <- as.data.frame(read.csv("HW7/Homework_7_corrin.csv", header = F))

# cholesky decomposition of the correlation matrix
corchol <- as.matrix(read.csv("HW7/Homework_7_corchol.csv", header = F))

# D(T)
dt <- as.data.frame(read.csv("HW7/Homework_7_pfilea.csv", header = F))

# sigma 
sigma <- as.data.frame(read.csv("HW7/Homework_7_sigma.csv", header = F))

# Qn 1 

dt <- as.matrix(dt[1:20,], ncol=1)
sigma <- as.matrix(sigma[1:19,], ncol=1)

result <- matrix(0, nrow=20, ncol=20)
result[,1] <- dt
nSims <- 10000
deltat <- 0.5

for(i in (1:19)){
  rCur <- (1/result[i,i] -1)*2
  
  currStepNumber <- (19-i+1)
  
  # sigma for each step
  sigmaCurr <- sigma[1:currStepNumber]
  
  # dt for each step 
  dtCurr <- result[(i+1):20, i]
  
  # dim (currStepNumber X 10000) matrix of rnorms
  zmatCurr <- matrix(rnorm(n=nSims * currStepNumber) ,nrow=currStepNumber)
  
  # use cholesky decomposition of the correlation matrix to obtain
  # the matrix with dim(currStepNumber X 10000)
  corcholCur <- corchol[1:currStepNumber, 1:currStepNumber]
  # correlated brownian motion is equal to cholesky decomposition the standard norm random variables
  dZs <- corcholCur %*% zmatCurr
  
  tempResult <- rowMeans((dtCurr + rCur * dtCurr * deltat) + sigmaCurr * dZs * sqrt(deltat))
  
  tempResult <- c(rep(0, (i-1)),1, tempResult)
  result[,(i + 1)] <-  tempResult
}
result <- cbind(result , rep(1.0,20))
colnames(result) <- seq(0, 10,0.5)
rownames(result) <- seq(0.5,10,0.5)

# Qn 2 forward par rates for 1 -5 years semiannual coupon bonds 5 years forward

# let t = 5 be the t0 at 5 years forward
