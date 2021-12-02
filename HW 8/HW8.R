#'*Task 2*
setwd("C:/Users/maxig/Desktop/Uni/Statistik/StatisticsAndProbabilityTheory/HW 8")
load("dist.Rdata")
mean <- mean(distanz)
h0 <- 550
n <- length(distanz)
sd <- sd(distanz)
sem <- sd / sqrt(n)
alpha <- 0.05

plot_distance <- function(data, mu) {
  mean <- mean(data)
  h0 <- 550
  n <- length(data)
  sd <- sd(data)
  sem <- sd / sqrt(n)
  alpha <- 0.05
  hist(data, main=sprintf("Distance %d",n), xlab = "distance in meter")
  abline(v=h0, col='blue', lwd = 1.5)
  abline(v=mean, col='green', lwd = 1.5)
  abline(v=c(mean+sem,mean-sem), col='red', lwd = 1.5)
  legend("topleft", 
         legend=c('error', 'mean', 'hypothesis'), 
         col=c('red', 'green', 'blue'), 
         cex=0.8, 
         lty=1,
         lwd=1.5)
}

# 2a)
# It is not bell-shaped.
plot_distance(distanz)
abline(v=c(mean+sem,mean-sem), col='red', lwd = 1.5)

# 2b)
t <- (mean - h0)/(sd / sqrt(n))
t

# 2c) 
p <- pt(-t, df=n - 1) * 2
p
printf <- function(...) cat(sprintf(...))
checkNull <- function(pV, alphaV) {
  if(pV > alphaV){
    printf("Do not reject the hypotheses because p = %.2f > %.2f = alpha",pV,alphaV)
  }else{
    printf("Reject the hypotheses because p = %.2f <= %.2f = alpha",pV,alphaV)
  }
}
checkNull(p,alpha)

# 2d)
# The values are around our expectation and the observation was correct.

# 2e)
t.test(distanz, mu=h0)

#'*Task 3*
# 3a)
dist_3x = NULL
for (i in 1:3){
  for (j in 1:n){
    dist_3x <- c(dist_3x, distanz[j])
  }
}
   
t.test(dist_3x, mu=h0)
# t is higher, p-value is lower and the confidence interval more compromised

# 3b)
(par(mfrow=c(2,1)))
plot_distance(distanz)
plot_distance(dist_3x)


# 3c)
# The second graph has two bars for the range of 200 meters
# The first graph only has one bar each 200 meters
# Because the sample size is bigger the error range is further away from
# hypothesis and closer to the mean

#'*Task 5*
load('waitingtimes.Rdata')
(par(mfrow=c(1,1)))
# 5a)
hist(wz, main="Waitingtimes", xlab="waittime (sec)", col="white")

# It is not bell-shaped

# 5b)
n <- length(wz)
mean <- mean(wz)
sd <- sd(wz)
sem <- sd / sqrt(n)
X <- (1/n) * sum(wz)
q = qnorm(0.99)
I <- c(X-(q*sem), X+(q*sem))

rect(I[1],0, I[2], 100, density = 10, col='green')
abline(v=mean, col='red', lwd=2)

#5c)
t.test(wz, mu=1.5, alternative = 'two.sided',conf.level = 0.99)
pnew = t.test(wz, mu=1.5, alternative = 'two.sided',conf.level = 0.99)$p.value
checkNull(pnew,.01)