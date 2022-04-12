##Load necessary packages
  library(ggplot2)
  library(psych)
  library(car)

##Read in the data file
  housedata<- read.csv(file.choose(), header=T)

  ggplot(housedata, aes(x = Age, y = SalePrice))+ 
    geom_point(shape=16, size=3)+ 
    labs(x = "Age",
         y = "SalePrice", 
         title = "SalePrice vs Age")+
    theme_bw()+
    theme(plot.title = element_text(hjust=0.5, size = rel(2)))+    
    theme(axis.title.y = element_text(size = rel(1.4)))+    
    theme(axis.title.x = element_text(size = rel(1.4)))+    
    theme(axis.text.x = element_text(size = rel(1.6)))+    
    theme(axis.text.y = element_text(size = rel(1.6)))+    
    geom_smooth(method = "lm", se = FALSE)
##Set up category = N for the Garage variable to be the 
##baseline category
  housedata$Garage<- factor(housedata$Garage, 
                            levels = c("N", "S", "L"))
  
  housedata$Age2<- (housedata$Age)^2
  
##Quadratic Model with Age
  agequad.model<-lm(SalePrice ~ Age + Age2, data = housedata)
  
##Interaction Model with Age and Garage
  agegarage.model<- lm(SalePrice ~ Age + Garage + Age*Garage, data = housedata)
  summary(agegarage.model)
##Intercept and Slope for Garage = N
  agegarage.model$coefficients[1]
  agegarage.model$coefficients[2]

##Intercept and Slope for Garage = S
  agegarage.model$coefficients[1] + agegarage.model$coefficients[3]
  agegarage.model$coefficients[2] + agegarage.model$coefficients[5]
  
##Intercept and Slope for Garage = L
  agegarage.model$coefficients[XX] + agegarage.model$coefficients[XX]
  agegarage.model$coefficients[XX] + agegarage.model$coefficients[XX]
  
##model with just age
  age.model<- lm(SalePrice ~ Age + Garage, data = housedata)
  
##Test for significance of interaction between Age and Garage
  anova(age.model, agegarage.model)

##Center LivingArea variable in data 
  mean(housedata$LivingArea)
  housedata$cLivingArea<- housedata$LivingArea - mean(housedata$LivingArea)
  
##Interaction Model with Age and centered LivingArea
  agearea.model<- lm(SalePrice ~ Age + cLivingArea + Age*cLivingArea, data = housedata)
  summary(agearea.model)
  
  agearea.model$coefficients

  

  
  
