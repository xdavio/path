form.xi=function(x,theta,annot) {
  #the second step of sijian wang's algorithm requires that the covariates that correspond to each pathway be summed up (vector addition over the columns of the design matrix). 

  
  x.tilde <- t(apply(x,1,FUN=function(x) return(x*theta)))
  xi <- apply(annot,MARGIN=2,FUN=function(u) x.tilde%*%u)
  xi
}
