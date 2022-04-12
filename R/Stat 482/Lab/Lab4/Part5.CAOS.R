##open libraries
  library(ggplot2)
  library(multcomp)

##open data file
  pretest<- read.csv(file.choose(), header=T)
  
##Define the variable Section to be a categorical variable
  pretest$Section<- factor(pretest$Section, levels = c(1, 2, 3, 4))

##Side-by-Side Boxplot
  ggplot(pretest, aes(x=Section, y=CAOS))+ 
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
  predicted.CAOS<- lm(CAOS ~ Section, data = pretest)

##Get summary of model estimates with baseline category to Section 1
  summary(predicted.CAOS)
  
##Estimated mean values for all observations
  predicted.CAOS$fitted.values

##Residual values for all observations
  predicted.CAOS$residuals
  
##ANOVA Table and F-test statistic
  anova(predicted.CAOS)
  
##Ad-hoc tests using Tukey HSD method
  compare.CAOS <- glht(predicted.CAOS, 
                       linfct = mcp(Section = "Tukey"))
  
##Obtain the pairwise confidence intervals
  confint(compare.CAOS)
##Obtain the pairwise confidence interval graph
  plot(compare.CAOS)
##Obtain the connected letter display
  cld(compare.CAOS)
