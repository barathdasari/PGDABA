#install.packages('BaM', dependencies=TRUE)
library(BaM)

library(MASS)  #used for multivariate normal simulation
data(pbc.vote)
Data<-pbc.vote

Y<-Data$badballots
tech<- (Data$technology == "Votomatic")*1
X<-with(Data,cbind(1,tech, new, size, Republican, white))

X[1:5,]

reg<- lm(Y~X[,-1])
summary(reg)

beta_hat<- solve(t(X)%*%X)%*%t(X)%*%Y
n<-length(Y)
k<-dim(X)[2]
sigmasq_hat<- 1/(n-k) *  (t(Y) %*% ( diag(rep(1,n)) - X%*%solve(t(X)%*%X)%*%t(X) ) %*% Y)
beta_var <- as.numeric(sigmasq_hat) * solve(t(X)%*%X)
beta_se<- sqrt( diag(beta_var))

# estimates rounded to 3 decimal places
print(cbind(round(beta_hat,3), round(beta_se,3)))


###Bayesian analysis

nu<- 0   
lambda<- 0

# For Gibbs sampling take some starting value for theta
set.seed(sqrt(23))
rtheta <- 1

rsig2<- 1/rtheta
#prior mean for beta
beta0<- cbind(rep(0,k))
#prior variance for beta
g<- .1 # Fixed and known  Try g=1 to indicate one experience of similar data size from where we believe coeff are =0,  1/2 to indicate 2 experiences 
rSigma <- g*rsig2 * solve(t(X) %*% X)

A1<- 1/rsig2 * t(X)%*%X  + solve(rSigma)
beta1<- solve(A1)%*%(1/rsig2 * t(X)%*%Y + solve(rSigma)%*%beta0)

# simulate a value of beta given theta(or equivalently beta given sigma^2)
rbeta<- cbind(c(mvrnorm(1, beta1, solve(A1))))

print(rbeta)


#simulate a value of theta given previous value of beta
nu1<- n/2 + k/2 + nu
lambda1<-  t(Y- X%*%rbeta)%*%(Y-X%*%rbeta)/2 + t(rbeta-beta0)%*%t(X)%*%X%*%(rbeta-beta0)/(2*g) + lambda  
rtheta <- rgamma(1,shape=nu1,rate=lambda1)
print(rtheta)


data_sim<- rbind(c(rbeta, rtheta))
print(data_sim)

N_MCMC<- 10000

for(count in 1:N_MCMC){
  
  rsig2<- 1/rtheta
  #prior mean for beta
  beta0<- cbind(rep(0,k))
  #prior variance for beta
  rSigma <- g*rsig2 * solve(t(X) %*% X)
  
  A1<- 1/rsig2 * t(X)%*%X  + solve(rSigma)
  beta1<- solve(A1)%*%(1/rsig2 * t(X)%*%Y + solve(rSigma)%*%beta0)
  
  # simulate a value of beta given theta(or equivalently beta given sigma^2)
  rbeta<- cbind(c(mvrnorm(1, beta1, solve(A1))))
  
  
  
  #simulate a value of theta given previous value of beta
  nu1<- n/2 + k/2 + nu
  lambda1<-  t(Y- X%*%rbeta)%*%(Y-X%*%rbeta)/2 + t(rbeta-beta0)%*%t(X)%*%X%*%(rbeta-beta0)/(2*g) + lambda  
  rtheta <- rgamma(1,shape=nu1,rate=lambda1)
  
  data_sim<- rbind(data_sim, c(rbeta, rtheta))
}


l<-N_MCMC/2
u<- N_MCMC
means<- apply(data_sim[l:u,-7], MARGIN=2, FUN=mean)
sd<- apply(data_sim[l:u,-7], MARGIN=2, FUN=sd)
Q025<- apply(data_sim[l:u,-7], MARGIN=2, FUN=quantile, prob=.025)
Q975<- apply(data_sim[l:u,-7], MARGIN=2, FUN=quantile, prob=.975)
out<-data.frame(cbind(round(means,3), round(sd,3),round(Q025,3), round(Q975,3) ))
colnames(out)<- c("Est", "SE", "Q025", "Q975")

nmm<-colnames(X)
nmm[1]<-"Intercept"
row.names(out)<-nmm    
print(out)


