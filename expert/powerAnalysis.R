powerAnalysisb<-cmpfun(function(m=1000,mu=14.5,mu0=14,sd=0.75,kappa=1,alpha=0.05){
  alpha=alpha/m
   beta=0.20
   ncl=(1+1/kappa)*(sd*(qnorm(1-alpha/2)+qnorm(1-beta))/(mu-mu0))^2
   NCP=(mu-mu0)^2/sd^2*(ncl*ncl*kappa)/(ncl+kappa*ncl)
  Power=length(which(pchisq(rchisq(m,1,ncp=NCP),1,lower.tail=F)< alpha))
  return(list(ncp=NCP,n=ceiling(ncl*k+ncl),poer=Power))
})

powerAnalysisb()

                   