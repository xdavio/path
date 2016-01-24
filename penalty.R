penalty=function(gamma) {
  #need to get around the absolute value thing; it's bad because then I can't use the derivative of the objective function to access faster optimization techniques. 
  sum(abs(gamma))
}
