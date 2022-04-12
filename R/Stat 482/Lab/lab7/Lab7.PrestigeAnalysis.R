##Load necessary libraries
  library(ggplot2)
  library(psych)
  library(car)

##Read in data file
  pdata<-read.csv(file.choose(), header=T)

##Create a quadratic term for average income
  pdata$income2 <- pdata$income^2

##Fit model with quadratic term 
  model3<- lm(prestige ~ education + income + income2 + type, 
              data = pdata)
  summary(model3)

##Center quantitative independent variables
  pdata$cincome <- pdata$income - mean(pdata$income)
  pdata$ceducation <- pdata$education - mean(pdata$education)
  pdata$cincome2<- pdata$cincome^2
  
##Fit centered model with quadratic term
  model4<-lm(prestige ~ ceducation + cincome + cincome2 + type, 
             data = pdata)
  summary(model4)


  pdata$typeLabels<- factor(pdata$type, levels=c("bc", "prof", "wc"), 
                            labels=c("Blue Collar", "Professional", "White Collar"))
  
  ggplot(pdata, aes(y=prestige, x=income, color=typeLabels))+
    geom_point()+
    labs(x = "Income", 
         y = "Prestige", 
         title = "Prestige vs. Income (by Type)")+
    theme_bw()+
    scale_color_brewer(name="Occupation Type", palette="Set1")+
    theme(plot.title = element_text(hjust=0.5, size = rel(2)))+
    theme(axis.title.y = element_text(size = rel(1.4)))+
    theme(axis.title.x = element_text(size = rel(1.4)))+
    theme(axis.text.x = element_text(size = rel(1.6)))+
    theme(axis.text.y = element_text(size = rel(1.6)))
  

  model5<-lm(prestige ~ ceducation + cincome + type + 
               cincome*type, data = pdata)
  summary(model5)
  
  ##You can use the coefficients in model5 to calculate prediction equations
  
  ##Example - Intercepts
  ##Blue Collar
  intBlue1<- model5$coefficients[1]
  ##Professional
  intProf1<- model5$coefficients[1]+model5$coefficients[4]
  ##White Collar
  intWhite1<- model5$coefficients[1]+model5$coefficients[5] 
  
  ##For Income
  slopeIncBlue1<- model5$coefficients[3]
  slopeIncProf1<- model5$coefficients[3] + model5$coefficients[6] 
  slopeIncWhite1<- model5$coefficients[3] + model5$coefficients[7]
  
  ggplot(pdata, aes(y=prestige, x=income, color=typeLabels))+
    geom_point()+
    labs(x = "Income", 
         y = "Prestige", 
         title = "Prestige vs. Income (by Type)")+
    theme_bw()+
    scale_color_brewer(name="Occupation Type", palette="Set1")+
    theme(plot.title = element_text(hjust=0.5, size = rel(2)))+
    theme(axis.title.y = element_text(size = rel(1.4)))+
    theme(axis.title.x = element_text(size = rel(1.4)))+
    theme(axis.text.x = element_text(size = rel(1.6)))+
    theme(axis.text.y = element_text(size = rel(1.6)))
  

