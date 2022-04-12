##Below is an example with data fitting problem 1 from
##the take home final exam.

##Enter the data for the vector Y
##There are 20 observations so the vector has 
##20 rows and 1 column
Y<- c(21, 21, 22, 18, 22, 25, 21, 20, 19, 21, 
      21, 19, 19, 22, 22, 21, 19, 19, 20, 19)

##Enter the data for the matrix X
##There are 20 observations and only one x variable,
##so the matrix has 20 rows and 1 column
X<- c(25, 24, 26, 22, 25, 28, 23, 23, 22, 26, 
      25, 23, 22, 25, 24, 25, 23, 22, 24, 23) 

##You can find the estimate of beta using the formula
##(X^TX)^(-1)X^TY
##We will calculate the first part of the formula
##(X^TX)^(-1) separately since that is needed for the 
##variance of betahat
invmatrix<- solve(t(X)%*%X)
betahat<- invmatrix%*%t(X)%*%Y
solve(sum(X^2))
##We now can obtain an estimate of the vector Y from the 
##model Yhat = X*betahat
Yhat<- X%*%betahat

##and use that estimate to find an estimate of sigma^2
sigmahat2<- (sum((Y - Yhat)^2))/19
ybar = (sum(Y))/20

UB = (betahat + 2.09*sqrt((sigmahat2*(invmatrix))))
LB = (betahat - 2.09*sqrt((sigmahat2*(invmatrix))))

cbind(LB, UB)

##############################################################
##Below is an example of data fitting problem 2 from the take
##home final exam. 

##Here are the data for the Y vector. 
##There are 35 observations so the vector has 
##35 rows and 1 column
Y<- c(25, 27, 24, 25, 25, 26, 27, 27, 26, 25, 
      31, 32, 31, 31, 31, 32, 33, 30, 32, 30,
      33, 32, 31, 31, 32, 29, 31, 30, 28, 30, 
      30, 30, 31, 29, 31)

##Here are the columns in the X matrix
##There are three x variables so we will enter the data
##for each x variable individually.
x1<- c(rep(1, 10), rep(0, 25))
x2<- c(rep(0, 10), rep(1, 15), rep(0, 10))
x3<- c(rep(0, 25), rep(1, 10))

##and then put the three columns together to form the X matrix
X<- cbind(x1, x2, x3)

##You can find the estimate of beta using the formula
##(X^TX)^(-1)X^TY
invmatrix<- solve(t(X)%*%X)
##Now we can calculate betahat
betahat<- invmatrix%*%t(X)%*%Y

##Using the estimator of beta from the model, we can 
##obtain an estimate of the vector Y from the model 
##Yhat = X*betahat
Yhat<- X%*%betahat

sigmahat2<- (sum((Y - Yhat)^2))/34

##and use this estimate to determine the values of the 
##sums of squares
SST<- sum((Y - mean(Y))^2)
SSE<- sum((Y - Yhat)^2)
SSM<- sum((Yhat - mean(Y))^2)
SST
SSE
SSM
SSE/32
SSM/2

top = SSM/2
bottom = SSE/32
top/bottom


