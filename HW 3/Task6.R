# Task 6a
x <- trees[,2]
# Round to the multiples of 10
round(x, digits = -1)
x <- round(x, digits = -1)
table(x)
# Round values to the tenth place
round(x, digits = 10)
x <- round(x, digits = 10)
table(x)


# Task 6b
fibonacci <- function(n) {
  if(n <= 1) {
    return(n)
  } else {
    return(fibonacci(n-1) + fibonacci(n-2))
  }
}
# Input
nterms = as.integer(readline(prompt="How many terms? "))
# number check
if(nterms <= 0) {
  print("Plese enter a positive integer")
} else {
  print("Fibonacci sequence:")
  for(i in 0:(nterms-1)) {
    print(fibonacci(i))
  }
}

# Sum function for Fibonacci numbers
mySum <- function(n) {
  s <- 0
  for(i in 2:n-1) {
    s <- s+fibonacci(i)
  }
  print(s)
}

mySum(15)
mySum(25)

n = as.integer(readline(prompt="Sum number: "))
# Sum of given n 
mySum(n)


# Task 6c
curve <- function(t) {
  if(t >= 0 && t < 2*pi) {
    n <- cos(2*t)
    k <- sin(3*t)
    t <- factorial(n)/(factorial(k)*factorial(n-k))
  }
}

plot(curve)
setwd("C:/Users/maxig/Desktop/Uni/Statistik/HW 3")
dev.print(pdf, "6c.pdf")




