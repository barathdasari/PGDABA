#I-beam example

# Data-  x1,...,xn~N(mu, sigma^2), 
#mu=5 known, theta=1/sigma^2 unknown

x<-c(5.19, 4.72, 4.81, 4.87,4.88)

n<-length(x)
mu<-5

#Prior on theta

nu0<-80
lambda0<-20

xx<-seq(0, 5, length=100)
pdf_prior<- dgamma(xx, shape=nu0, rate=lambda0)
plot(xx, pdf_prior, xlab="theta", ylab="pdf")

##Posterior for theta

nu1<- nu0 + n/2
lambda1<- lambda0 +  sum((x-mu)^2)/2
pdf_posterior<-dgamma(xx,shape=nu1, rate=lambda1)
lines(xx, pdf_posterior, col="red", lwd=2)

##Posterior Inference
#posterior mean for theta
nu1/lambda1

# 99% credible interval for theta

low<-qgamma(.005, shape=nu1, rate=lambda1)
high<-qgamma((1-.005), shape=nu1, rate=lambda1)
c(low,high)

# Predictive distribution

#P(sag for a new item >4.8)=P(X_new>4.8)

N<-1000
rtheta<- rgamma(N, shape=nu1, rate=lambda1)

rprob<-1-pnorm(4.8, mean=5, sd=1/sqrt(rtheta))
mean(rprob)



#  how can I visualize the pdf of Xnew?
#  Expected value of X_new?

### Simulation of X_new

Xnew_vec<-c()
N<-10000


for(i in 1:N){  
### First simulate theta
rtheta<- rgamma(1, nu1, rate=lambda1)

### Given rtheta generate Xnew from N(5,1/rtheta)
rXnew<- rnorm(1, 5, 1/sqrt(rtheta))
Xnew_vec<-c(Xnew_vec,rXnew)
}

hist(Xnew_vec, freq=FALSE)
plot(density(Xnew_vec))
mean(Xnew_vec)
mean(Xnew_vec>4.8)

















