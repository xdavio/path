theta.group.not.zero=function(x,theta.tmp=theta.guess) {
  if (sum(abs(theta.tmp[as.logical(x)])) == 0) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}
