library(MASS)
data(quine)
plot(density(quine$Days, from = 0),main = "Days")

r = 1.5
k_hat = mean(quine[,'Days'])

#MLE
p_hat_mle = k_hat/(r+k_hat) 


simulation<- function(alpha,c,d) {
  x = rnorm(1)
  while (1==1) {
    if(x > -(1/c)){
      y = runif(1,0,dnorm(x))
      if(y < f(x,c,d)) return(x)
    }
  }
}

f <- function(x,c,d){
  return(exp(g(x,c,d)))
}

g <- function(x,c,d){
  return(d*log((1+c*x)^3) - d*(1+c*x)^3+d)
}

alpha = 1.2
lambda = 3
d = alpha -1/3
c = 1/sqrt(9*d)


mygamma = function(n,alpha,lambda){
  result = rep(0,n)
  for(i in 1:n) {
    result[i] = ((d * (1 + c*simulation(alpha,c,d))^3) / lambda)
  }
  return(result)
}


sim = mygamma(1000, 1.2, 3)
plot(qgamma(1:1000/1001,1.2,3),sort(sim))
abline(0,1,col="red")




result = rep(0,1000)
for(i in 1:1000) {
  result[i] = ((d * (1 + c*simulation(alpha,c,d))^3) / lambda)
}
result