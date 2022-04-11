### probability of causing a cluster
p <- function(R,k,beta,Egamma){
  set.seed(123456)                                    
  N <- 10000   
  gamma <- rbeta(N, shape1=100*Egamma, shape2=100*(1-Egamma)) #rep(Egamma,N)
  alpha <- rep(0,N); get_p <- rep(0,N)
  for(i in 1:N){
    alpha[i] <- beta*gamma[i]
    get_p[i] <- 1-(k/(R+k))^k-alpha[i]*(1-(k/(R+k))^k)
  }
  return(get_p)
}

### effectiveness functions
de <- function(R,k,beta_odhc,beta_self,Egamma){
    N <- 10000  
    self_prob <- p(R,k,beta_self,Egamma)
    odhc_prob <- p(R,k,beta_odhc,Egamma) 
    de_sampling <- rep(0,N)
    for(i in 1:N){de_sampling[i] <- (self_prob[i]-odhc_prob[i])/self_prob[i]}
    return(de_sampling)
}

ar <- function(R,k,beta_odhc,beta_self,Egamma){
    N <- 10000  
    self_prob <- p(R,k,beta_self,Egamma)
    odhc_prob <- p(R,k,beta_odhc,Egamma)
    ar_sampling <- rep(0,N)
    for(i in 1:N){ar_sampling[i] <- odhc_prob[i]-self_prob[i]}
    return(ar_sampling)
}