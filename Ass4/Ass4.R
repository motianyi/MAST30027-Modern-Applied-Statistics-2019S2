data <- scan("Assign4Data.txt")
n = 100





# GibbsS <- function(mu0, tau0, m){
#   
#   mu.seq = rep(-1,m)
#   tau.seq = rep(-1,m)
#   
#   
#   mu.seq[1] = mu0
#   tau.seq[1] = tau0
#   
#   for (i in 2:m) {
#     mu.seq[i] = rnorm(1,mean(data),(1/(tau.seq[i-1]*n)))
#     tau.seq[i] = rgamma(1,n/2, scale = (2/sum((data-mu.seq[i])^2)))
# 
#   }
#   result  = list(mu = mu.seq, tau = tau.seq)
#   return(result)
# }
# set.seed(30027)
# m = 10000
# gibbsam1 = GibbsS(7,0.01,m)
# gibbsam2 = GibbsS(3,0.9,m)
# 
# # 
# par(mfrow=c(1,1))
# 
# #trace plot mu
# plot(1:m, gibbsam1$mu, type="l", col="red", ylim = c(0, max(gibbsam1$mu,gibbsam2$mu)), xlab = "iteration", ylab ="mu")
# points(1:m, gibbsam2$mu, type="l", col="blue")
# #trace plot tau
# plot(1:m, gibbsam1$tau, type="l", col="red", ylim = c(0, max(gibbsam1$tau,gibbsam2$tau)), xlab = "iteration", ylab ="tau")
# points(1:m, gibbsam2$tau, type="l", col="blue")
# 
# #combine 2 simulations
# gibbsam = list(mu =c(tail(gibbsam1$mu,m/2),tail(gibbsam2$mu,m/2)),tau =c(tail(gibbsam1$tau,m/2),tail(gibbsam2$tau,m/2)))
# 
# #marginal posterior distribution pf mu
# plot(density(gibbsam$mu), main="simulated pdf of mu ", xlab="mu", lwd=2)
# 
# #marginal posterior distribution pf tau
# plot(density(gibbsam$tau), main="simulated pdf of tau ", xlab="tau", lwd=2)
# 
# #posterior mean
# (mu_hat = mean(gibbsam$mu))
# (tau_hat = mean(gibbsam$tau))
# 
# sort_mu = sort(gibbsam$mu,decreasing = FALSE)
# descending_sort_mu = sort(gibbsam$mu,decreasing = TRUE)
# e_cdf = 1:m/m
# c(sort_mu[which(descending_sort_mu>=0.95)[1]],sort_mu[which(e_cdf>=0.95)[1]])
# c(sort_mu[m*0.05],sort_mu[m*0.95])
# 
# sort_tau = sort(gibbsam$tau,decreasing = FALSE)
# c(sort_tau[m*0.05],sort_tau[m*0.95])

########################################

#MH

prior <- function(param){
  tau = param[2]
  return(log(1/tau))
}


likelihood <- function(param){
  mu = param[1]
  tau = param[2]
  
  #log likelihood
  logL = dnorm(data, mean = mu, sd = sqrt(1/tau),log = TRUE)
  return(sum(logL))
}


posterior <- function(param){
  return(likelihood(param) + prior(param))
}

proposalfunction <- function(param){
  mu = param[1]
  tau = param[2]
  proposal_tau = rgamma(1,shape = 5*tau,rate = 5)
  proposal_mu = rnorm(1,mu,sqrt(proposal_tau))
  return(c(proposal_mu, proposal_tau))
}






mcmc <- function(startvalue, iterations){
  chain = array(dim = c(iterations+1, 2))
  chain[1,] = startvalue
  
  for(i in 1:iterations){
    proposal = proposalfunction(chain[i,])
    probab = exp(posterior(proposal) - posterior(chain[i,]))
    if(runif(1) < probab){
      chain[i+1,] = proposal
    }else{
      chain[i+1,] = chain[i,]
    }
  }
  return(chain)
}


set.seed(30027)
#start value
startvalue1 = c(1,1)
startvalue2 = c(5,0.1)

#MCMC
chain1 = mcmc(startvalue1,10000)
chain2 = mcmc(startvalue2,10000)

#burnin period
burnIn = 1


hist(chain1[-(1:burnIn),1],nclass=30, , main="Posterior of a", xlab="True value = red line",col = "blue" )
# hist(chain2[-(1:burnIn),1],nclass=30, , main="Posterior of a", xlab="True value = red line" ,col = "red")
abline(v = mean(chain[-(1:burnIn),1]), col="green")
# abline(v = trueA, col="red" )
hist(chain[-(1:burnIn),2],nclass=30, main="Posterior of b", xlab="True value = red line")
abline(v = mean(chain[-(1:burnIn),2]), col="green")
# abline(v = trueB, col="red" )

plot(chain1[-(1:burnIn),1], type = "l", xlab="True value = red line" , main = "Chain values of a",col = "red" )
points(chain2[-(1:burnIn),1],type="l", col="blue")
# abline(h = trueA, col="red" )
plot(chain1[-(1:burnIn),2], type = "l", xlab="True value = red line" , main = "Chain values of b", col = "red")
points(chain2[-(1:burnIn),2],type="l", col="blue")
# abline(h = trueB, col="red" )
