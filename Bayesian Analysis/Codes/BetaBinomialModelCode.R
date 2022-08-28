## Beta-Binomial Model

#Data
X<-c(0,0,0,0,0)
n<-length(X)

#Prior for p  ~ Beta(a,b)

##a and b computed using matching of moments
m<-.044
v<-.033^2

aplusb<- m*(1-m)/v-1
a<- aplusb * m
b<- aplusb - a
c(a,b)

p_x<- seq(0,1,length=100)
p_pdf<-dbeta(p_x, a,b)

### Posterior distribtuion for p: beta(a1, b1)

a1<- a+ sum(X)
b1<- b+n-sum(X)
a1
b1
p_pdf_post<-dbeta(p_x,a1,b1)
  
yhigh<-max(p_pdf_post)
plot(p_x, p_pdf, type='l', ylim=c(0,yhigh), lwd=2)
lines(p_x, p_pdf_post, col="red",lwd=2)

# posterior inference

#posterior mean and sd
c(mean=a1/(a1+b1), sd=sqrt(a1*b1/((a1+b1)^2 * (a1+b1+1))))

# 95% interval for p

L<-qbeta(.025, a1,b1)
U<-qbeta(.975, a1, b1)

c(L,U)



















