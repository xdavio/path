build.xtilde <- function(gamma.guess,path.struct,x.obs=x) {
  #Depends strictly on the arguments passed
  #if (ncol(path.struct) != length(gamma.guess)) {
  #  return(NULL)
  #}

  #this function builds the xtilde referred to in the algorithm in sijian wang's paper. the idea is that the sum of the group effects that correspond to each covariate are multiplied into the design matrix columnwise. the following code performs that operation based on the K-length current guess of gamma, the annotation matrix (called path.struct here), and the observed x

  #okay so doing it this way is pretty unelegant
  foo <- t(apply(path.struct,MARGIN=1,FUN=function(x) return(x*gamma.guess)))
  foo <- apply(foo,MARGIN=1,FUN=function(x) return(sum(x)))
  foo <- t(apply(x.obs,MARGIN=1,FUN=function(x) return(x*foo)))
  

  #here's hopefully a more intelligent solution
  #gamma.guess <- as.vector(gamma.guess)
  #gamma.guess <- matrix(gamma.guess,length(gamma.guess),1,byrow=F)
  #mat <- path.struct%*%gamma.guess
  foo
}
