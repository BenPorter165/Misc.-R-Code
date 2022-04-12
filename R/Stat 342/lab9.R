####################################################################
##Activity 1
####################################################################
1 - pgamma(13.2897, shape = 25, scale = 0.4)

1 - pgamma(13.2897, shape = 25, scale = 0.6)
1 - pgamma(13.2897, shape = 25, scale = 0.8)
1 - pgamma(13.2897, shape = 25, scale = 1)


1 - pgamma(13.2897, shape = 100, scale = 0.10)


1 - pgamma(13.2897, shape = 100, scale = 0.15)
1 - pgamma(13.2897, shape = 100, scale = 0.2)
1 - pgamma(13.2897, shape = 100, scale = 0.25)




####################################################################
##Activity 2
####################################################################

##Function to calculate the Exact Confidence Interval for Beta
exactci<- function(x, alpha){
  lbound<- mean(x)/qgamma(1 - alpha/2, shape = length(x), scale = 1/length(x))
  ubound<- mean(x)/qgamma(alpha/2, shape = length(x), scale = 1/length(x))
  cat("Exact Confidence Interval for Beta = ", lbound, ubound, "\n")
}

##Generate a sample of size n = 20 from Exponential with Beta = 5
expsample<- rexp(20, 1/5)
##Calculate the Exact 95% Confidence Interval for Beta using the sample data
exactci(expsample, 0.05)

##Repeat code above 4 more times

##Function to calculate an Approximate Confidence Interval for Beta
approxci<- function(x, alpha){
  lbound<- sqrt(length(x))*mean(x)/(sqrt(length(x)) + qnorm(1 - alpha/2, 0, 1))
  ubound<- sqrt(length(x))*mean(x)/(sqrt(length(x)) + qnorm(alpha/2, 0, 1))
  cat("Approx. Confidence Interval for Beta = ", lbound, ubound, "\n")
}

##Generate a sample of size n = 20 from Exponential with Beta = 5
expsample<- rexp(20, 1/5)
##Calculate the Approximate 95% Confidence Interval for Beta using the sample data
approxci(expsample, 0.05)

##Repeat code above 4 more times

##Function to estimate the coverage rates of both methods for calculating 
##a Confidence Interval for Beta
compareci<- function(alpha, n, beta){
  exactcov<- rep(99, 100000)
  approxcov<- rep(99, 100000)
  for (i in 1:100000){
    expsample<- rexp(n, 1/beta)
    exactlbound<- mean(expsample)/qgamma(1 - alpha/2, shape = n, scale = 1/n)
    exactubound<- mean(expsample)/qgamma(alpha/2, shape = n, scale = 1/n)
    exactcov[i]<- ifelse((beta > exactubound) | (beta < exactlbound), 0, 1)
    approxlbound<- sqrt(n)*mean(expsample)/(sqrt(n) + qnorm(1 - alpha/2, 0, 1))
    approxubound<- sqrt(n)*mean(expsample)/(sqrt(n) + qnorm(alpha/2, 0, 1))
    approxcov[i]<- ifelse((beta > approxubound) | (beta < approxlbound), 0, 1) 
  }
  exactrate<- sum(exactcov)/100000
  approxrate<- sum(approxcov)/100000
  cat("Coverage Rate for Exact CI = ", exactrate, "\n")
  cat("Coverate Rate for Approx CI = ", approxrate, "\n")
}


##Compare the coverage rates for the two methods when
# alpha = 0.05, n = 20, beta = 5
compareci(0.05, 20, 5)
# alpha = 0.01, n = 20, beta = 5
compareci(0.01, 20, 5)
# alpha = 0.1, n = 20, beta = 5
compareci(0.10, 20, 5)

##Compare the coverage rates for the two methods when 
# alpha = 0.05, n = 100, beta = 5
compareci(0.05, 100, 5)
# alpha = 0.01, n = 100, beta = 5
compareci(0.01, 100, 5)
# alpha = 0.1, n = 100, beta = 5
compareci(0.10, 100, 5)


compareci(0.05, 20, 600)
compareci(0.05, 100, 600)
