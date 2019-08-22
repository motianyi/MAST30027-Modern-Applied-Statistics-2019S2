load("assign1.Robj")
model = glm(pima_subset$test~pima_subset$bmi,family = binomial(logit))
summary(model)

##install.packages('package_name', dependencies=TRUE, repos='http://cran.rstudio.com/')


#Q1a
library(faraway)
data(orings)
logL<- function(beta, orings){
  eta = cbind(rep(1,length(orings$temp)),orings$temp)%*%beta
  return(sum(orings$damage*log(pnorm(eta))+(6-orings$damage)*log(1-pnorm(eta))))
}
#maximize
(betahat = optim(c(10,-.1), logL,orings = orings, control = list(fnscale=-1))$par)


##Q1b
beta0 = betahat[1]
beta1 = betahat[2]
(eta = beta0+orings$temp*beta1)

#cdf of standard normal
D0 = pnorm(eta)

#first derivative(pdf of standard normal)
D1 = dnorm(eta)

#second derivative
D2=1/sqrt(2*pi)*exp(-(eta^2)/2)*(-1*eta)

x = orings$temp
y = orings$damage
m = 6
(Lb1b1=sum( (y*x^2*(D0*D2-D1*D1))/(D0^2)+(y-m)*(x^2*((1-D0)*D2+D1*D1))/((1-D0)*(1-D0))))
(Lb1b0=sum( y*x*(D0*D2-D1*D1)/(D0^2)+(y-m)*x*((1-D0)*D2+D1*D1)/((1-D0)*(1-D0))))
(Lb0b0=sum( y*(D0*D2-D1*D1)/(D0^2)+(y-m)*((1-D0)*D2+D1*D1)/((1-D0)*(1-D0))))

(J = -matrix(c(Lb0b0,Lb1b0,Lb1b0,Lb1b1),2,2))
(variance = solve(J))

(Beta0CI = c(betahat[1] - qnorm(0.975)*sqrt(variance[1,1]),betahat[1] + qnorm(0.975)*sqrt(variance[1,1])))
(Beta1CI = c(betahat[2] - qnorm(0.975)*sqrt(variance[2,2]),betahat[2] + qnorm(0.975)*sqrt(variance[2,2])))

##Q1c Likelihood Ratio Test

#phatN for the null model
n=rep(6,length(y))
(phatN = sum(y)/sum(n))

#Max log likelihood for Reduced model
(MaxlogL.R = sum(y*log(phatN))+sum(6-y)*log(1-phatN))

#Max log likelihood for Full model
(MaxlogL.F = logL(betahat, orings))

#Likelihood Ratio Statistics
(LR = -2*(MaxlogL.R-MaxlogL.F))

#LR follow chi-square distribution with 1 degree of freedom
(pValue = pchisq(df=1,LR,lower.tail = FALSE))

#Since p-value is less than 0.05,reject null hypothesis (H0)
#Thus beta-1 is not equal to 0


#Q1d
t=c(1,31)

#estimate 
eta = t(t)%*%betahat
(pnorm(eta))
#the estimate of damage when temperature equals 31 Fahrenheit is 0.9896084

#95%CI for estimate
etaCI = c(t%*%betahat-qnorm(0.975)*sqrt(t(t)%*%variance%*%t),
          t%*%betahat+qnorm(0.975)*sqrt(t(t)%*%variance%*%t))
(pnorm(etaCI))
#The 95% Confidence interval for estimate probability of damage when temperature 
#equals to 31 Fahrenheit is (0.7169598, 0.9999744)


#Q1e
str(orings)

plot(damage/6 ~ temp, orings, xlim=c(25,85), ylim=c(0,1),
     xlab="Temperature", ylab="Prob of damage")
x <- seq(25,85,1)
ilogit <- function(x) exp(x)/(1+exp(x))
logitmod <- glm(cbind(damage,6-damage) ~ temp, family=binomial, orings)
(betahatLogit=logitmod$coefficients)
lines(x, ilogit(betahatLogit[1] + betahatLogit[2]*x), col="red",)
lines(x, pnorm(betahat[1] + betahat[2]*x), col="green")
legend(60,1,legend = c("logit","inverse standard normal"), col = c("red","green"),lty=1)





