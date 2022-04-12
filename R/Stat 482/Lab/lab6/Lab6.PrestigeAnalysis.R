library(ggplot2)
library(psych)
library(car)

##Read in the data set
  pdata<-read.csv(file.choose(), header=T)

##Least Squares Regression Analysis
  model1<-lm(prestige ~ education + income + type, data = pdata)
  summary(model1)
  
##CI for slopes
  confint(model1, level = 0.95)

##Model with Education and Income  
  model2<- lm(prestige ~ education + income, data = pdata)
  summary(model2)

##Test for significance of type of occupation
  anova(model2, model1)

##Define variable values for a new observation
  newjob = data.frame(education = 12.5, income = 9000, type = "bc")

##Confidence Interval for mean
  predict.lm(model1, newjob, interval = "confidence")
  
##Prediction Interval for observed value
  predict.lm(model1, newjob, interval = "prediction")
  
