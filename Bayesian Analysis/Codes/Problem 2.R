#Data
x <- c(7,5,8,4,6)
n<- length(x)

#Prior
nu0<- 1
lambda0<- 0.2

#Prior Distribution
xx<- seq(0,100,length=100)
pdf_prior<- dgamma(xx,shape=nu0,rate=lambda0)
plot(xx,pdf_prior)

#Posterior for mu
nu1<- nu0+n
nu1
lambda1<- lambda0 + sum(x)
lambda1

post_mean<- nu1/lambda1
post_mean

# 95% CL
high<-qgamma(.975, shape=nu1, rate=lambda1)
low<-qgamma((1-.975),shape=nu1, rate=lambda1)
c(low,high)

#Predictive distribution
N<-1000
rtheta<- rgamma(N, shape=nu1, rate=lambda1)
prob<- 1- pexp(q = 7, rate = rtheta)
mean(prob)
  