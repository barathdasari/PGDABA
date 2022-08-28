
##prior
values<-c(.05, .1, .4, .5, .6)
prob<-c(1/5,1/5,1/5,1/5,1/5)

##Data
x<-c(.05, .08, .1, .12, .05)
n<-length(x)
sigma<-.02
## Posterior for mu
## possible values of mu c(.05, .1, .4, .5, .6)

posterior<-c()
for(i in 1:5){
mu<-values[i]
likelihood<- prod(dnorm(x,mu,sigma))
posterior<-c(posterior,likelihood*1/5)
}

posterior_prob<-posterior/sum(posterior)

rbind(values, round(posterior_prob,3))

##Posterior mean

sum(values*posterior_prob)

#####Normal Prior for mu ~N(mu0, tau0^2)

mu0<-mean(values)
tau0<-sd(values)

xbar<-mean(x)
mu1<- (xbar * n/sigma^2  + mu0 * 1/tau0^2)/( n/sigma^2+ 1/tau0^2)
mu1

tau1<- sqrt(1/( n/sigma^2+ 1/tau0^2))

c(mean=mu1, sd=tau1)

# credible interval
c(low=qnorm(.025, mu1, tau1), high=qnorm(.975, mu1, tau1)  )


##P(Ynew>.15)

N<-1000
rmu<-rnorm(N, mu1, tau1)

rprob<- 1- pnorm(.15, rmu, sigma)
mean(rprob)



