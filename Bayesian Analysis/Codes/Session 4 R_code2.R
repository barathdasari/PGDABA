
##beta-binomial problem

y<-17
n<-116

##b) CLT based 95% CI  -classical
phat<-y/n
phat

low<- phat - 1.96* sqrt(phat*(1-phat)/n)
upper<-phat + 1.96* sqrt(phat*(1-phat)/n)
c(low, upper)

##c) Posterior based on Jeffery's prior Beta(.5,.5)

a<-.5
b<-.5

#posterior
a1<- a+y
b1<- b+n-y

# Estimate=E[p|y]
a1/(a1+b1)

# 95% "credible" interval
p<-seq(0,1,length=100)
pdf<- dbeta(p,a,b)
pdf1<- dbeta(p,a1,b1)
plot(p, pdf, type='l', ylim=c(0, max(pdf1)))
lines(p,pdf1, col="blue")
## we want an interval of values for p which has 95% probability
##under posterior pdf

## quantile or percentile based credible interval
L<- qbeta(.025, a1, b1)
U<- qbeta(.975, a1, b1)
c(L,U)


##d) Posterior based on informative prior Beta(1,4)

a<-1
b<-4

#posterior
a1<- a+y
b1<- b+n-y

# Estimate=E[p|y]
a1/(a1+b1)

# 95% "credible" interval
p<-seq(0,1,length=100)
pdf<- dbeta(p,a,b)
pdf1<- dbeta(p,a1,b1)
plot(p, pdf, type='l', ylim=c(0, max(pdf1)))
lines(p,pdf1, col="blue")
## we want an interval of values for p which has 95% probability
##under posterior pdf

## quantile or percentile based credible interval
L<- qbeta(.025, a1, b1)
U<- qbeta(.975, a1, b1)
c(L,U)



