##open libraries
library(ggplot2)

##open data file
  cars<- read.csv(file.choose(), header=T)

  colnames(cars)

##Basic Scatterplot with regression line
  ggplot(cars, aes(x=Horsepower, y=Highway.mpg))+ 
    geom_point(shape=16, size=3)+ 
    labs(x = "Horsepower",
         y = "Highway MPG", 
         title = "Vehicle Highway MPG vs Horsepower")+
    theme_bw()+
    theme(plot.title = element_text(hjust=0.5, size = rel(2)))+    
    theme(axis.title.y = element_text(size = rel(1.4)))+
    theme(axis.title.x = element_text(size = rel(1.4)))+
    theme(axis.text.x = element_text(size = rel(1.6)))+
    theme(axis.text.y = element_text(size = rel(1.6)))+
    geom_smooth(method="lm", se=FALSE)
    
##Correlation
  cor(cars$Horsepower, cars$Highway.mpg)

##Find SLR estimates 
  predicted.MPG<- lm(Highway.mpg ~ Horsepower, data = cars)
  
##Get summary of regression line
  summary(predicted.MPG)

##Predicted value for 6th observation
  predicted.MPG$fitted.values[6]

##Residual value for the 6th observation
  predicted.MPG$residuals[6]
  
##Prediction or estimated mean for a specific value of HorsePower
  new.HorsePower <- data.frame(Horsepower = 178)
  predict.lm(predicted.MPG, new.HorsePower)
  
##ANOVA - for SSM and SSE
  anova(predicted.MPG)
  