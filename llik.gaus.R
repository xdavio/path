llik.gaus <- function(x,y.,beta) {
  n <- length(y.)
  xbetai <- x%*%beta
  sig2 <- (1/n)*sum((y.-xbetai)^2)
  sig <- sqrt(sig2)

  foo <- sum(dnorm(y.,xbetai,sig,T))
  foo
}

llik.gaus.der <- function(x,y,bta) {
  n <- length(y)
  mu <- x%*%bta
  sig2 <- (1/n)*sum((y-mu)^2)

  gen.der <- function(des.col) {
    summand <- des.col*(y-mu)
    foo <- sum(summand)/(sig2)
    foo
  }
 
  foo <- apply(x,2,gen.der)
  foo
}
