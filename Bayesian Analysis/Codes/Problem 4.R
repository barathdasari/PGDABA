
#prior
values<- seq(991,1010)
probability<- rep(.05,20)

#Data
x<-c(1003,995,999,1001,994,998,996,1002,1004,998,994,994,995,995,1001,997,1006,997,998,994)
n<-length(x)
sigma<-sqrt(32)
sigma

#Posterior distribution for mu
posterior<-c()
for(i in 1:20){
  mu<-values[i]
  likelihood<- prod(dnorm(x,mu,sigma))
  posterior<-c(posterior,likelihood*0.05)
}

posterior_prob<-posterior/sum(posterior)
result <- data.frame(Value=values, Proba=round(posterior_prob,3))
result

#Posterior mean 
posterior_mean<- sum(values*posterior_prob)
posterior_mean

#Normal Prior for mu ~N(mu0, tau0^2)
mu0<-mean(values)
tau0<-sd(values)
xbar<-mean(x)
mu1<- (xbar * n/sigma^2 + mu0 * 1/tau0^2)/( n/sigma^2+ 1/tau0^2)
mu1
tau1<- sqrt(1/( n/sigma^2+ 1/tau0^2))
c(mean=mu1, sd=tau1)

#Credible Interval
c(low=qnorm(.025,mu1,tau1), high=qnorm((1-.025),mu1,tau1))

#P(mu<1000 under posterior distribution)
N<-5000
rmu<-rnorm(N, mu1, tau1)
rprob<- pnorm(1000, rmu, sigma)
mean(rprob)

#posterior distribution for σ2
nu1<- n/2+40
nu1
lambda1<-10+sum(((x-998)^2/2))
lambda1

posterior_sig<-c()
for(i in 1:20){
  mu<-values[i]
  likelihood<- prod(dgamma(x,shape = nu1,rate = lambda1))
  posterior_sig<-c(posterior_sig,likelihood*0.05)
}

posterior_prob1<-posterior_sig/sum(posterior_sig)
result1 <- data.frame(Value=values, Proba=round(posterior_prob1,3))
result1
