lambda = .5
n = 200
means = NULL
mean1 <- 1/lambda

for (i in 1:20){
  means = c(means, mean(rexp(n,lambda)))
}

plot(means)
abline(h = mean1, col = "red")
abline(h = mean(means),  col="green")