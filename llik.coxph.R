llik.coxph <- function(x,y,gam) {

  #gam <- true.beta #debug only
  
  tmp <- llik.coxph.int(x,y,gam)
  observed <- tmp$observed
  y.obs <- tmp$y.obs
  y <- tmp$y
  no.obs <- tmp$no.obs

  
  #foo begins from each y.obs, moving ultimately to a construction of the groups of indices greater than each y.obs
  foo <- function(u) {
    K=(y>=u)
    x.=x[K,]
    foo <- sum(exp(x.%*%gam))
    foo
  }  
  parttwo <- -log(prod(sapply(y.obs,foo)))
  partone <- sum(x%*%gam)

  out <- partone + parttwo
  out
}

llik.coxph.der <- function(x,y,gam) {

  #gam <- true.beta #debug only
  
  tmp <- llik.coxph.int(x,y,gam)
  observed <- tmp$observed
  y.obs <- tmp$y.obs
  y <- tmp$y
  no.obs <- tmp$no.obs

  gen.der <- function(h) {
    #only generates for one dimension
    partone <- sum(x[observed,h])

    foo <- function(u) {
      K=(y>=u)
      x.=x[K,]
      a <- x.[,h]
      b <- exp(x.%*%gam)
      ab <- sum(a*b)
      d <- sum(b)
      ab/d
    }  
    parttwo <- sum(sapply(y.obs,foo))
    
    out <- partone-parttwo
    out
  }

  outvec <- sapply(1:length(gam),gen.der) #split for all elem of gradient
  outvec
}


llik.coxph.int <- function(x,y,gam) {
  y <- as.matrix(y)
  x <- as.matrix(x)

  #y should be an nx2 matrix
  # column 1 is the times
  # column 2 is 1 when the time was observed; 0 when the obs is the result of censor
  
  observed <- as.logical(y[,2])
  y.obs <- as.vector(y[observed,1])
  y <- y[,1]
  no.obs <- sum(observed)

  foo <- list(observed=observed,y.obs=y.obs,y=y,no.obs=no.obs)
  foo
}
