#Prior

prior_poss<-c(0.01,0.05,0.10)

prior_prob<-c(2/5,2/5,1/5)

rbind(prior_poss, prior_prob)

### Prior Summaries

prior_mean<-sum(prior_poss*prior_prob)
prior_mean

#variance= E(p^2) -(E)(p))^2
prior_variance<-sum(prior_poss^2*prior_prob)-prior_mean^2
prior_SD<-sqrt(prior_variance)
prior_SD


#Posterior

post_poss<-c(0.01,0.05,0.10)

#post probability without normalizing constant
for_post_prob <- (1-post_poss)^5*prior_prob
for_post_prob

post_prob<- for_post_prob / sum(for_post_prob )    
post_prob

rbind(post_poss, post_prob)

### Posterior Summaries

post_mean<-sum(prior_poss*post_prob)
post_mean

post_variance<- sum(post_poss^2*post_prob)-post_mean^2
post_SD<-sqrt(post_variance)
post_SD

