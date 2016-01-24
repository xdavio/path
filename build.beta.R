build.beta=function(gam,theta,annot) {
  #construct group vec
  grp <- as.vector(annot%*%gam)
  return(grp*theta)
}
