##Load necessary libraries
  library(ggplot2)
  library(psych)
  library(car)

##Read in data file
  pdata<-read.csv(file.choose(), header=T)

##Center quantitative independent variables
  pdata$cincome <- pdata$income - mean(pdata$income)
  pdata$ceducation <- pdata$education - mean(pdata$education)
  
##Set up a new variable in data set with longer labels for the
##occupation type variable
  pdata$typeLabels<- factor(pdata$type, 
                  levels=c("bc", "prof", "wc"), 
                  labels=c("Blue Collar", 
                           "Professional", 
                           "White Collar"))

#################################################################
##Model from last week's lab
##Least Squares Regression Analysis:
##ceducation, cincome, type, interaction between cincome*type
#################################################################
  
  model5<-lm(prestige ~ ceducation + cincome + type + 
               cincome*type, data = pdata)
  summary(model5)
  
#################################################################
##Least Squares Regression Analysis:
##ceducation, cincome, type, interaction between ceducation*cincome
#################################################################
  
  model6<-lm(prestige ~ ceducation + cincome + type + 
               cincome*ceducation, data = pdata)
  summary(model6)
  
##Finding intercept values for equations for Question 3
  ##Intercepts
  intBlue2<- model6$coefficients[1]
  intProf2<- model6$coefficients[1]+model6$coefficients[4]
  intWhite2<- model6$coefficients[1]+model6$coefficients[5]
  intBlue2
  intProf2
  intWhite2
  
#################################################################
##Least Squares Regression Analysis:
##ceducation, cincome, type, 
##interaction between ceducation*cincome
##interaction between cincome*type
#################################################################
  
  model7<-lm(prestige ~ ceducation + cincome + type + 
               cincome*ceducation + cincome*type, data = pdata)
  summary(model7)
  
  anova(model6, model7)