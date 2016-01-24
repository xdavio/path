objective=function(gamma.a,xi.=xi,y.=y,active.gam.=active.gam,fam.=family,K.=K) {
  gam=rep(0,length(active.gam.))
  gam[active.gam.]=gamma.a #gam is zero unless part of active set

  #print(str(fam.))
  if (fam. == "gaussian") {
    ll <- llik.gaus(xi.,y.,gam)
  } else if (fam. == "coxph") {
    ll <- llik.coxph(xi.,y.,gam)
  }
    
  foo <- -ll/length(y.) + penalty(gam)
  #note the division by n in the previous line
  foo
}
objective.der=function(gamma.a,xi.=xi,y.=y,active.gam.=active.gam,fam.=family,K.=K) {
  gam <- rep(0,length(active.gam.))
  gam[active.gam.] <- gamma.a #gam is zero unless part of active set

  if (fam.=="gaussian" | fam.=="default") { 
    ll.d <- llik.gaus.der(xi.,y.,gam)
  } else if (fam.=="coxph") {
    ll.d <- llik.coxph.der(xi.,y.,gam)
  }
    
  #note: dividing by n is causing problems; when removed functions better???
  #foo <- -ll.d/length(y.) + 1/3 #broken line
  foo <- -ll.d/length(y.) + 1 #"correct" line
  foo <- foo[active.gam.]
  foo
}
