pathalgpart <- function(x,y,annot,family="gaussian",lambda=1,thresh=10^-4,maxit=10000,optim.method="L-BFGS-B") {
  #insert code for the path algorithm here.
  #input is the choice of likelihood (string), x, y, path

  #the debug mechanism follows:

  #EXAMPLE 1
  #this is the debug mechanism for the first example from wang's paper, in which there is no overlap
  #source("data/sim1.R");family="gaussian";lambda=.5;thresh=10^-4;maxit=10000;optim.method="L-BFGS-B";x=sim.data$x;y=sim.data$y;annot=sim.data$annot;true.beta=sim.data$beta
  #remember that you have to mess with lambda sometimes to get the alg to converge

  #EXAMPLE 2
  #data/debug for coxph/load.R
  #family="coxph";lambda=.2;thresh=10^-4;maxit=10000;optim.method="L-BFGS-B"
  
  
  #function actual
  library(glmnet)
  K <- ncol(annot)
  n <- nrow(x)
  p <- ncol(x)
  theta.guess=NULL
  count <- 1
  gamma.guess=rep(1,K)  #initial guess for gamma
  beta.house=array(0,c(maxit,p)) #store beta here
   x=standardize(x) #calling standardize function externally

  #in case of cox
  coxcheck <- function(u) {
    if ( u == "coxph") {
      return("cox")
    } else {
      return(u)
    }
  }
  if ( family == "coxph") {
    colnames(y) <- c("time","status")
  }
  
  
  while( count < maxit ) {
    #step 1
    #for debug remember to say lambda=1 and family="gaussian"
    fit <- glmnet(build.xtilde(gamma.guess,annot,x),y,family=coxcheck(family))
    theta.guess <- as.vector(coef(fit,s=lambda))
    if (family == "coxph") { begin=1 } else { begin=2 }
    theta.guess <- theta.guess[begin:length(theta.guess)] #what to do about intercept? I'm stripping it out here

    #step 2
     active.gam <- as.logical(apply(annot,2,theta.group.not.zero,theta.guess)) #tests indiv est; if all zero forany gamma group; takes that gamma out of the optimization procedure (o.w. optim alg may not converge (jitter zero gam doesn't affect objective fun))

    xi <- form.xi(x,theta.guess,annot)
    if (sum(active.gam) == 0) {
      print("All of the group parameter estimates are zero.")
      return(NULL)
    } else {
      gamma.guess.tmp <- opt(optim.method,family,K,xi,y,active.gam)
    }
      
    gamma.guess[active.gam] <- gamma.guess.tmp
    gamma.guess[-active.gam] <- 0    

    count <- count+1
    beta.house[count,] <- build.beta(gamma.guess,theta.guess,annot)

    #check convergence
    diff <- beta.house[count-1,]-beta.house[count,]
    diff <- sqrt(sum(diff^2))
    if (diff < thresh) {
      print(paste("The algorithm converged after ",count-1," iterations.",sep=""))
      break
    }
  }
  beta.est <- beta.house[count,]
  gamma.est <- gamma.guess
  theta.est <- theta.guess
  output <- list(beta=beta.est,gamma=gamma.est,theta=theta.est)
  output
}
