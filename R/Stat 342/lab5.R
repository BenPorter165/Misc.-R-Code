##Activity 1

##Function to calculate a t statistic when mu = 65.5
tstat<- function(x){
  (mean(x)-65.5)/(sd(x)/sqrt(length(x)))
}


##generate samples of size 5 from normal distribution
manyvalues<- rnorm(10000*5, 65.5, 2.5)
manysamples<- matrix(manyvalues, byrow = T, ncol = 5)

##Calculate t distribution for each sample
tfromnormal<- apply(manysamples, 1, tstat)

hist(tfromnormal)
mean(tfromnormal)
var(tfromnormal)
sd(tfromnormal)

##Repeat above for parts (b) and (c) with n = 25 and n = 100
##generate samples of size 25 from normal distribution
manyvalues<- rnorm(10000*25, 65.5, 2.5)
manysamples<- matrix(manyvalues, byrow = T, ncol = 25)

##Calculate t distribution for each sample
tfromnormal<- apply(manysamples, 1, tstat)

hist(tfromnormal)
mean(tfromnormal)
var(tfromnormal)
sd(tfromnormal)

##generate samples of size 25 from normal distribution
manyvalues<- rnorm(10000*100, 65.5, 2.5)
manysamples<- matrix(manyvalues, byrow = T, ncol = 100)

##Calculate t distribution for each sample
tfromnormal<- apply(manysamples, 1, tstat)

hist(tfromnormal)
mean(tfromnormal)
var(tfromnormal)
sd(tfromnormal)
##############################################################################
##Activity 2

##generate samples of size 3 from uniform distribution
manyvalues<- runif(10000*3, 0, 1)
manysamples<- matrix(manyvalues, byrow = T, ncol = 3)

##Calculate sample mean for each sample
unifmean<- apply(manysamples, 1, mean)

##Determine how many of these 10,000 sample means are greater than 0.75
sum(ifelse(unifmean >=0.75, 1, 0))

1 - pnorm(1.5, 0, 1)
##Repeat above for parts (d) and (e) with n = 20 and update probability (sample mean greater than 0.6)

##generate samples of size 20 from uniform distribution
manyvalues<- runif(10000*20, 0, 1)
manysamples<- matrix(manyvalues, byrow = T, ncol = 20)

##Calculate sample mean for each sample
unifmean<- apply(manysamples, 1, mean)

##Determine how many of these 10,000 sample means are greater than 0.6
sum(ifelse(unifmean >=0.6, 1, 0))
hist(unifmean)

1 - pnorm(1.549193338, 0, 1)
###############################################################################
##Activity 3

