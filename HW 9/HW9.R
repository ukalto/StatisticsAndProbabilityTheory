setwd("C:/Users/maxig/Desktop/Uni/Statistik/StatisticsAndProbabilityTheory/HW 9")
#'*Task 1*
# 1a)
first = rnorm(10) # N(0,1)
second = rnorm(20,mean=1) # N(1,1)
t = (mean(first)-mean(second))/sqrt(sd(first)^2/length(first) + sd(second)^2/length(second))
t 
# 1b)
t.test(first, second)
# Both values are always the same!
# 1c)
# data:  first and second
# t = -2.9355, df = 20.869, p-value = 0.007936
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  -2.0858412 -0.3555966
# sample estimates:
#  mean of x   mean of y 
# -0.08367504  1.13704385
# Rejected because p <= alpha 0.007 <= 0.025
#'*Task 2*
# 2a)
load("waitingtimes2.Rdata")
par(mfrow=c(2,1))
for(i in 1:length(wt)){
  hist(wt[[i]], xlab = "times" ,main = sprintf("Waitingtimes %d",i))
}
# Both are not Bell-Shaped
# 2b)
x = wt[[1]]
y = wt[[2]]

t = (mean(x)-mean(y))/sqrt(sd(x)^2/length(x) + sd(y)^2/length(y))
t

pnorm(t)*2
# Not rejected because p > alpha 0.7035 > 0.01
# 2c)
# t.test(x,y)
# data:  x and y
# t = -0.38163, df = 102.38, p-value = 0.7035
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  -0.3652981  0.2474064
# sample estimates:
#  mean of x mean of y 
# 0.957589  1.016535 
# p-value is not the exact same value

#'*Task 4*
load("tireData.Rdata")
par(mfrow=c(2,1))
left = tireData[1:20,1]
right = tireData[1:20,2]
# 4a)
hist(left)
hist(right)
# 4b)
t = t.test(left,right)
t
# 4c)
d = left-right
newt = t.test(d)
# 4d)
sdl = sd(left)
sdr = sd(right)
sd = sd(d)

#'*Task 6*
alpha = 0.05
H0 = 0

n1 = 10
n2 = 20
n3 = 20

sigma1= 3
sigma2 = 3
sigma3 = 1

t = 1:63
m = matrix(t,3)
colnames(m) = seq(-5, 5, 0.5)
rownames(m) = c("a", "b", "c")
index = 1
for (d in seq(-5,5,0.5)){
  reject1 = 0
  reject2 = 0
  reject3 = 0
  
  for(i in 1:1000){
    x1 = rnorm(n1, 0, sigma1)
    y1 = rnorm(n1, d, sigma1)
    x2 = rnorm(n2, 0, sigma2) 
    y2 = rnorm(n2, d, sigma2)
    x3 = rnorm(n3, 0, sigma3)
    y3 = rnorm(n3, d, sigma3)
    
    if (t.test(y1, x1)$p.value <= alpha){
      reject1 = reject1 +1
    }
    if (t.test(y2, x2)$p.value <= alpha){
      reject2 = reject2 + 1
    }
    if (t.test(y3, x3)$p.value <= alpha){
      reject3 = reject3 + 1
    }
  }
  m[1, index] = reject1
  m[2, index] = reject2
  m[3, index] = reject3
  index <- index+1
}

m = m/1000

print(m)
(par(mfrow=c(1,1)))
plot(seq(-5, 5, 0.5), m[1, 1:21], type="l", col="red", main="Testmächte bei a, b und c", xlab="d", ylab="Testmacht")
lines(seq(-5, 5, 0.5), m[2, 1:21], type="l", col="blue")
lines(seq(-5, 5, 0.5), m[3, 1:21], type="l", col="green")
legend("topright", legend = c("n=10, s=3", "n=20, s=3", "n=20, s=1"), col= c("red", "blue", "green"), lty=1)


