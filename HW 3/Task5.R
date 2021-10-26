F <- function(x) {
  exp(x)/(1+exp(x))
  return(x);
}

f <- function(x) {
  exp(x)/(1+exp(x))^2
}

plot(f)

plot(F)

F(0.25)
F(0.5)
F(0.75)