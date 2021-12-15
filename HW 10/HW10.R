setwd("C:/Users/maxig/Desktop/Uni/Statistik/StatisticsAndProbabilityTheory/HW 10")
#'*Task 2*
load("temperatures.Rdata")
# 2a)
stripchart(temp, 
           main = "Processors Temperatures",
           ylab= "processors",
           xlab= "temperatures (in °C)",
           pch=1,
           col = rainbow(5, v=0.7)
)

# 2b)
for (i in 1:5) {
  md = mean(temp[[i]])
  sem = sd(temp[[i]])/sqrt(length(temp[[i]]))
  rect(md-sem, i-0.15, md+sem, i+0.15, density=10, col='black')
  lines(c(md,md), c(i-0.2, i+0.2), col='red', lwd=2)
}

# 2c)
# No, because the means are quite far apart from each other.

#'*Task 3*
# 3a)
k <- length(temp)
ni <- NULL
xi <- NULL
sum_temp <- 0
for (i in 1:k) {
  ni = c(ni, length(temp[[i]]))
  xi = c(xi, sum(temp[[i]])/ni[i])
  sum_temp = sum_temp + sum(temp[[i]])
}

n <- sum(ni)
x <- (1/n) * sum_temp

var_between <- 0
var_within <- 0
for (i in 1:k) {
  var_between = var_between + (ni[i] * (xi[i] - x)^2)
  
  for (xij in temp[[i]]) {
    var_within = var_within + ((xij - xi[i])^2)
  }
}

F = ((1/(k-1))*var_between)/((1/(n-k))*var_within)
p = pf(F, k-1, n-k)
p


# 3b)
x = NULL
gr = NULL
gr = factor(rep(1:k,ni))
for (i in 1:k) {
  for (n in temp[[i]]) {
    x = c(x, n)
  }
}
anova(aov(x~gr))

# 3c)
# The calculated F value is equal to the anova test.
# We do reject the hypothesis as p >= alpha 0.9995575 >= 0.95

# 3d)
# Independence: yes because in task 2 it is said that there a five different processors
# Normal-Distribution: no processor 4 for example is not normal distributed
# own(unknown) expectation: is true because we don't know the mean at it comes from five different processors
# the variance sigma^2 is constant: No, because they are not all from the same distribution

#'*Task 6*
create_plot <- function(d, col) {
  pvalues <- c()
  for (i in 1:10000){
    x <- rnorm(20,0,1)
    y <- rnorm(20,d,1)
    pvalues <- c(pvalues, t.test(x, y)$p.value)
  }
  hist(pvalues, 
       ylab="Density", 
       xlab="p-value", 
       main = sprintf("Distribution (d = %.2f)", d),
       col=col,
       prob=TRUE)
}

(par(mfrow=c(3,1)))
col = rainbow(3, alpha = 0.3)
create_plot(0, col[1])
create_plot(0.25, col[2])
create_plot(0.5, col[3])
