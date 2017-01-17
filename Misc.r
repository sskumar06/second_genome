xj <- diag(1,10,10)


wj <- matrix(NA, ncol=10, nrow=4)

wj[,1] <- c(0,0,0,0)
wj[,2] <- c(0,0,0,1)
wj[,3] <- c(0,0,1,0)
wj[,4] <- c(0,0,1,1)
wj[,5] <- c(0,1,0,0)
wj[,6] <- c(0,1,0,1)
wj[,7] <- c(0,1,1,0)
wj[,8] <- c(0,1,1,1)
wj[,9] <- c(1,0,0,0)
wj[,10] <- c(1,0,0,1)

for(i in 1:10){
  for(j in 1:4){
      cat(sum(wj[j,i]*xj[,i]))
  }
  cat("\n")
}

library(quantreg)
