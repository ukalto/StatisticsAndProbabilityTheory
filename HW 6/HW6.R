#'*Task 1*
#
workloads = c(25,13,7,9,44,3,2,33)
median(med)
#11
quantile(workloads, c(3/4, 1/3), type=2)
#75% 33.33333% 
#29  7
#'*Task 2*
#2a)
setwd("C:/Users/maxig/Desktop/Uni/Statistik/StatisticsAndProbabilityTheory/HW 6")
load("algorithms.Rdata")
boxplot(runningtimes, ylab = "Runningtimes", col=4, horizontal = TRUE)
#2b)
#. The third quartile of the times in A was about?
#  Answer: 32
#. The interquartile range of the times in B is about trice the interquartile range of A.
#  Answer: Interquartilsabstand ist B: 29-7 = 22, A: 32 -20 = 12 ist ca. das doppelte
#. Is n = 100?
#  Answer: Man kann keine genau Aussage aus dem Graphen darüber treffen
#. At least 50% in A were faster than the 25% slowest in B.
#  Answer: Ja da der Median von A größer als der Median von B ist.
#. At least 60% in A were faster than the 25% slowest in B.
#  Answer: Kann man nicht genau sagen, da man nicht weiß wo 60% sind.

#'*Task 3* 
k <- 100
x <- rnorm(sample(k:(2*k),1), runif(1,0,k), rexp(1,1/k))
#3a) 
#   Explain what is realized in x.
#   Answer: n = sample(k:(2*k),1) random value von 100-200
#           mean = runif(1,0,k) length of result = 1, min = 0 max = 100, 
#           => selects 1 random element from uniform distribution from 0 to 100
#           std = rexp(1,1/k) selects 1 random element from the exponential 
#           distribution with the rate k, f(x) = k {e}^{- k x}
#   => Von dem ganzen eine Normalverteilung

#3b)
sd = sd(x)
mean = mean(x)
hist(x, plot = TRUE, main = "Normalverteilung")
abline(v = c(mean-sd,mean,mean+sd), col = c("blue","red","blue"), lwd = 2, lty = c(2,1,2))
legend("topleft",legend = c("Mean", "Standard deviation"), col = c("red","blue"), lty = 1:2, lwd = 2)

#'*Task 6*
#6a) 
x <- seq(-5,5,0.1)
dnor <- dnorm(x)
plot(x, dnor, type = "l")

#6b)
fnt = qnorm(0.975)
nnt = qnorm(0.995)
nnnt = qnorm(0.9995)
fnb = qnorm(1-0.975)
nnb = qnorm(1-0.995)
nnnb = qnorm(1-0.9995)

#6c)
abline(v = c(fnt,nnt,nnnt,fnb,nnb,nnnb), col = c("red","blue","green"), lwd = 2, lty = c(1,2,3))
legend("topleft",legend = c("95","99","995"), col = c("red","blue","green"),lwd = 2, lty = c(1,2,3))
