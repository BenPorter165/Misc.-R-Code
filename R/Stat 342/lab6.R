############################################################
##Activity 1
############################################################

##MOM estimator for alpha
alphahatMOM<- function(sample){
  (mean(sample))^2/(((length(sample)-1)/length(sample))*var(sample))}

##MOM estimator for beta
betahatMOM<- function(sample){
  (((length(sample)-1)/length(sample))*var(sample))/mean(sample)}

##Calculate alphahatMOM and betahatMOM for a sample
sample1<- rgamma(25, shape = 3, scale = 25)
alphahatMOM(sample1)
betahatMOM(sample1)

##Calculate alphahatMOM and betahatMOM for a new sample
sample2<- rgamma(25, shape = 3, scale = 25)
alphahatMOM(sample2)
betahatMOM(sample2)

##Repeat above three more times.

##For samples of size n = 25
manyvalues<- rgamma(25*10000, shape = 3, scale = 25)
manysamples<- matrix(manyvalues, byrow = T, ncol = 25)
alphahatMOM25<- apply(manysamples, 1, alphahatMOM)
betahatMOM25<- apply(manysamples, 1, betahatMOM)

hist(alphahatMOM25)
mean(alphahatMOM25)
var(alphahatMOM25)

hist(betahatMOM25)
mean(betahatMOM25)
var(betahatMOM25)

##For samples of size n = 100
manyvalues<- rgamma(100*10000, shape = 3, scale = 25)
manysamples<- matrix(manyvalues, byrow = T, ncol = 100)
alphahatMOM100<- apply(manysamples, 1, alphahatMOM)
betahatMOM100<- apply(manysamples, 1, betahatMOM)

hist(alphahatMOM100)
mean(alphahatMOM100)
var(alphahatMOM100)

hist(betahatMOM100)
mean(betahatMOM100)
var(betahatMOM100)

##For samples of size n = 1000
manyvalues<- rgamma(1000*10000, shape = 3, scale = 25)
manysamples<- matrix(manyvalues, byrow = T, ncol = 1000)
alphahatMOM1000<- apply(manysamples, 1, alphahatMOM)
betahatMOM1000<- apply(manysamples, 1, betahatMOM)

hist(alphahatMOM1000)
mean(alphahatMOM1000)
var(alphahatMOM1000)

hist(betahatMOM1000)
mean(betahatMOM1000)
var(betahatMOM1000)

############################################################
##Activity 2
############################################################

##MLE estimator for alpha
alphahatMLE<- function(sample){
  sampleinfo<- log(mean(sample)) - sum(log(sample))/length(sample)
  alphahat<- rep(0,20)
  alphahat[1]<- (3 - sampleinfo + sqrt((sampleinfo - 3)^2 + 24*sampleinfo))/(12*sampleinfo)
  
  for(i in 2:20) {
    alphahat[i]<- alphahat[i-1] - (sum(log(sample))/length(sample) - log(mean(sample)) + log(alphahat[i-1]) - digamma(alphahat[i-1]))/(1/alphahat[i-1] - trigamma(alphahat[i-1]))
  }
  alphahat[20]
}

##MLE estimator for beta
betahatMLE<- function(samplemean, alphahat){
  (1/alphahat)*samplemean}

##Calculate alphahatMLE and betahatMLE for a sample
sample1<- rgamma(25, shape = 3, scale = 25)
alphahat1<- alphahatMLE(sample1)
alphahat1
betahatMLE(mean(sample1), alphahat1)

##Calculate alphahatMLE and betahatMLE for another sample
sample2<- rgamma(25, shape = 3, scale = 25)
alphahat2<- alphahatMLE(sample2)
alphahat2
betahatMLE(mean(sample2), alphahat2)

##Repeat above three more times.

##For samples of size n = 25
manyvalues<- rgamma(25*10000, shape = 3, scale = 25)
manysamples<- matrix(manyvalues, byrow = T, ncol = 25)
alphahatMLE25<- apply(manysamples, 1, alphahatMLE)
gammameans<- apply(manysamples, 1, mean)
betahatMLE25<- betahatMLE(gammameans, alphahatMLE25)

hist(alphahatMLE25)
mean(alphahatMLE25)
var(alphahatMLE25)

hist(betahatMLE25)
mean(betahatMLE25)
var(betahatMLE25)



##For samples of size n = 100
manyvalues<- rgamma(100*10000, shape = 3, scale = 25)
manysamples<- matrix(manyvalues, byrow = T, ncol = 100)
alphahatMLE100<- apply(manysamples, 1, alphahatMLE)
gammameans<- apply(manysamples, 1, mean)
betahatMLE100<- betahatMLE(gammameans, alphahatMLE100)


hist(alphahatMLE100)
mean(alphahatMLE100)
var(alphahatMLE100)

hist(betahatMLE100)
mean(betahatMLE100)
var(betahatMLE100)

##For samples of size n = 1000
manyvalues<- rgamma(1000*10000, shape = 3, scale = 25)
manysamples<- matrix(manyvalues, byrow = T, ncol = 1000)
alphahatMLE1000<- apply(manysamples, 1, alphahatMLE)
gammameans<- apply(manysamples, 1, mean)
betahatMLE1000<- betahatMLE(gammameans, alphahatMLE1000)

hist(alphahatMLE1000)
mean(alphahatMLE1000)
var(alphahatMLE1000)



############################################################
##Activity 3
############################################################

##Estimated Expected Values for alpha MOM
mean(alphahatMOM25)
mean(alphahatMOM100)
mean(alphahatMOM1000)

##Estimated Expected Values for alpha MLE
mean(alphahatMLE25)
mean(alphahatMLE100)
mean(alphahatMLE1000)

##Estimated Expected Values for beta MOM
mean(betahatMOM25)
mean(betahatMOM100)
mean(betahatMOM1000)

##Estimated Expected Values for beta MLE
mean(betahatMLE25)
mean(betahatMLE100)
mean(betahatMLE1000)

##Estimated MSE for alpha MOM
mean((alphahatMOM25 - 3)^2)
mean((alphahatMOM100 - 3)^2)
mean((alphahatMOM1000 - 3)^2)

##Estimated MSE for alpha MLE
mean((alphahatMLE25 - 3)^2)
mean((alphahatMLE100 - 3)^2)
mean((alphahatMLE1000 - 3)^2)

##Estimated MAD for alpha MOM
mean(abs(alphahatMOM25 - 3))
mean(abs(alphahatMOM100 - 3))
mean(abs(alphahatMOM1000 - 3))

##Estimated MAD for alpha MLE
mean(abs(alphahatMLE25 - 3))
mean(abs(alphahatMLE100 - 3))
mean(abs(alphahatMLE1000 - 3))

##Estimated MSE for beta MOM
mean((betahatMOM25 - 25)^2)
mean((betahatMOM100 - 25)^2)
mean((betahatMOM1000 - 25)^2)

##Estimated MSE for beta MLE
mean((betahatMLE25 - 25)^2)
mean((betahatMLE100 - 25)^2)
mean((betahatMLE1000 - 25)^2)

##Estimated MAD for beta MOM
.
