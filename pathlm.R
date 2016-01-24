pathlm.default <- function(x,y,annot,family="gaussian",lambda=1) {
  #inputs are x, y, family, (penalty?), (alpha), thresh, annot, lambda

  #output is fitted values, group coefs, indiv coefs (coef(pathobj) gives you the beta coefficients)

  #coerce inputs
  x <- as.matrix(x)
  y <- as.vector(y)
  annot <- as.matrix(annot)

  #determine if partition case or not and assign class
  foo <- sum(annot)
  if (foo == nrow(annot)) {
    foo <- pathalgpart(x,y,annot,family,lambda)
  } else {
    foo <- pathalg(x,y,annot,family,lambda)
  }

  
  #class(foo)="path" #this line produces an error that makes no sense
  ##Error in if (x$n == 1) x$name else paste(x$path, x$name, sep = .grid.pathSep) : 
  ##argument is of length zero

  #do some naming and beautification/standardization
  return(foo)
}
