##Load libraries
  library(ggplot2)

##Read in data set
  air<-read.csv(file.choose(), header=T)

##Basic Scatterplot with regression line
  ggplot(air, aes(x=DeptDelay, y=ArrvDelay))+ 
    geom_point(shape=16, size=3)+ 
    labs(x = "DeptDelay", 
         y = "ArrvDelay",
         title = "DeptDelay vs. ArrvDelay")+ 
    theme_bw()+
    theme(plot.title = element_text(hjust=0.5, size = rel(2)))+
    theme(axis.title.y = element_text(size = rel(1.4)))+
    theme(axis.title.x = element_text(size = rel(1.4)))+
    theme(axis.text.x = element_text(size = rel(1.6)))+
    theme(axis.text.y = element_text(size = rel(1.6)))+
    geom_smooth(method="lm", se=FALSE)

##Correlation 
  cor(XX, XX)

##Least Squares Regression Analysis
  delay.model<-lm(ArrvDelay ~ DeptDelay, data = air)
  summary(delay.model)

##Predicted value for 33rd observation
  delay.model$fitted.values[33]
  
##Residual value for the 33rd observation
  delay.model$residuals[33]
  
##Prediction or estimated mean for a specific value of Departure Delay
  new.DeptDelay <- data.frame(DeptDelay = 50)
  predict.lm(delay.model, new.DeptDelay)
  
##ANOVA - for SSM and SSE
  anova(delay.model)
