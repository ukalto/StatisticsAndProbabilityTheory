n <- 200
lambda <- 0.5
meanX1 <- 1/lambda
means <- NULL
allmeans <- NULL

for(i in 1:20){
  for (j in 1:200) {
    means = c(means,mean(rexp(j,rate = lambda)))
  }
  if(i == 1) plot(means)
  else points(means)
  allmeans <- c(allmeans, means)
  means <- NULL
}

abline(h = meanX1, col = "red")
abline(h = mean(allmeans), col = "green")

