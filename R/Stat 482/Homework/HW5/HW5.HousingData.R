##Load necessary packages
  library(ggplot2)
  library(psych)
  library(car)

##Read in the data file
  housedata<- read.csv(file.choose(), header=T)

##Set up category = N for the Garage variable to be the 
##baseline category
  housedata$Garage<- factor(housedata$Garage, 
                            levels = c("N", "S", "L"))
head(housedata)
##Least Squares Regression Analysis
  house.model<-lm(SalePrice ~ Age + BasementArea + LivingArea + 
                    TotalRoom + Garage, data = housedata)
  summary(house.model)

##Test for significance of Garage
  ##Enter the codes to conduct this inference
  house.model1<-lm(SalePrice ~ Age + BasementArea + LivingArea + 
                    TotalRoom, data = housedata)
  summary(house.model1)
  
##Inference for categorical variable with more than two categories
  house.model2<- lm(SalePrice ~ Age + BasementArea + LivingArea + 
                TotalRoom + Garage, data = housedata)
  summary(house.model2)
  
  anova(house.model1, house.model2)
##Confidence interval for Age
  confint(house.model, "Age", level = 0.95)
  
##Confidence and Prediction intervals for new house
  newHouse<- data.frame(Age=21, BasementArea=1000, 
                        LivingArea=1000, TotalRoom=12, Garage="N")
  predict(house.model, newHouse, interval = "confidence")
  predict(house.model, newHouse, interval = "prediction")
  
  
