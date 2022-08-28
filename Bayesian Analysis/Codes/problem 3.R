#Data

x<- c(28, 15, 11, 21, 11, 17, 21, 15, 14, 18)
n<- length(x)

#Prior is flat uniform distribution

#Posterior for mu

nu1<- sum(x)
nu1 
lambda1<- n 
lambda1

# 95% CL
high<-qgamma(.975, shape=nu1, rate=lambda1)
low<-qgamma((1-.975),shape=nu1, rate=lambda1)
c(low,high) 