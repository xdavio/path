standardize <- function(x) {
  #debug mechanism
  if (!exists("x")) {
    x <- rnorm(100,mean=10,sd=4)
    x <- matrix(x,10,10)
  }

  #function actual
  foo <- function(u) {
    u <- (u-mean(u))/sd(u)
    u
  }
  out <- apply(x,2,foo)
  out
}
