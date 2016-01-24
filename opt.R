opt <- function(optim.method,family,K,xi,y,active.gam) {
  if ( optim.method != "default" ) {
    optim.method = "default"
  }
  if ( optim.method == "default") {
    if ( sum(active.gam) == 1 ) {
      foo <- optim(1,fn=objective,gr=objective.der,method="Brent",lower=0,upper=10,xi.=xi,y.=y,active.gam.=active.gam,fam.=family)$par
      #foo <- nlminb(rep(1,sum(active.gam)),objective=objective,gradient=objective.der,xi.=xi,y.=y,active.gam.=active.gam,fam.=family,lower=.1)
      #foo <- optim(rep(1,sum(active.gam)),fn=objective,method="Brent",lower=.1,upper=10,xi.=xi,y.=y,active.gam.=active.gam,fam.=family)$par
    } else if ( sum(active.gam) > 1 ) {
      #foo <- nlminb(rep(1,sum(active.gam)),objective=objective,gradient=objective.der,xi.=xi,y.=y,active.gam.=active.gam,fam.=family,lower=0)$par
      foo <- optim(rep(1,sum(active.gam)),method="L-BFGS-B",lower=0,fn=objective,gr=objective.der,K.=K,xi.=xi,y.=y,active.gam.=active.gam,fam.=family)$par
      #foo <- optim(rep(1,sum(active.gam)),method="Nelder-Mead",fn=objective,K.=K,xi.=xi,y.=y,active.gam.=active.gam,fam.=family)$par
    }
  }

  foo
}
