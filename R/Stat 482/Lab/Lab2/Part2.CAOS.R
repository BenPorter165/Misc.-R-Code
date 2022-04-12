##open libraries
library(ggplot2)

##open data file
pretest<- read.csv(file.choose(), header=T)

##Check for column names
colnames(pretest)

##Boxplot
ggplot(pretest, aes(x=factor(Section), y=CAOS))+ 
  geom_boxplot()+ 
  labs(x = "Section",
       y = "CAOS Score", 
       title = "CAOS Score by Section")+
  ylim(0, 40)+
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5, size = rel(2)))+    
  theme(axis.title.y = element_text(size = rel(1.4)))+
  theme(axis.title.x = element_text(size = rel(1.4)))+
  theme(axis.text.x = element_text(size = rel(1.6)))+
  theme(axis.text.y = element_text(size = rel(1.6)))
    
##Find parameter estimates - sets baseline category to Section 1
  predicted.CAOS<- lm(CAOS ~ factor(Section), data = pretest)

##Find parameter estimates - sets baseline category to Section 4
  pretest$Section<- factor(pretest$Section, levels = c(4, 1, 2, 3))
  predicted.CAOS.2<- lm(CAOS ~ factor(Section), data = pretest)

##Get summary of model estimates
  summary(predicted.CAOS)

##Estimated mean values for all observations
  predicted.CAOS$fitted.values

##Residual values for all observations
  predicted.CAOS$residuals
  
##ANOVA
  anova(predicted.CAOS)
  