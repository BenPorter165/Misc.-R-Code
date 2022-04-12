
#1a.
qgamma(0.95, shape=180, scale=5/18)

#b.
1-pgamma(56.28382, shape=180, scale=1/3)

#c.
1-pgamma(56.28382, shape=180, scale=1/3)

1-pgamma(56.28382, shape=180, scale=7/18)

1-pgamma(56.28382, shape=180, scale=4/9)



#######################################################
##Problem 3
#######################################################

##Read in the data
vardata<- read.csv("varianceci.csv", header = T)
vardata$Values<- as.numeric(vardata$Values)
vardata<- as.data.frame(vardata)

##Summary statistics
summary(vardata$Values)
sd(vardata$Values)

##Histogram
hist(vardata$Values)

a = sum((vardata$Values)^2)

b = qchisq(0.975, 20)

c = qchisq(0.025, 20)

a/b

a/c
########################################################
##Function to calculate the variance statistics in boot
########################################################

sigma2<- function(data, indices){
  d <- data[indices, ]
  return(var(d))
}

##Obtain bootstrapped confidence interval. You will need
##to install the R Package boot before proceeding.
library(boot)
bootvars<- boot(vardata, sigma2, R = 100000)
boot.ci(bootvars, conf = 0.95, type = "bca")

########################################################
##Problem 4
########################################################

poisdata<- read.csv("poissondata.csv", header = T)
poisdata<- as.numeric(poisdata$Values)

poisdata
sum(poisdata)

qgamma(0.025, shape=308, scale=25.5)

qgamma(0.975, shape=308, scale=25.5)

25*303

