##Opening libraries
  library(ggplot2)
  library(car)

##open data file
  cars<-read.csv(file.choose(), header=T)

##Scatterplot with regression line
  ggplot(cars, aes(x=Horsepower, y=Highway.mpg))+ 
    geom_point(shape=16, size=3) + 
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
    
##Fit simple linear regression model 
  predicted.MPG<- lm(Highway.mpg ~ Horsepower, data = cars)
  
##Get summary of regression line
  summary(predicted.MPG)

##Checking Assumptions
  out.MPG = fortify(predicted.MPG)
  head(out.MPG)
  
##Residual Plot
  ggplot(out.MPG, aes(x=.fitted, y=.stdresid))+ 
    geom_point(shape=16, size=3)+ 
    labs(x = "Predicted Highway MPG",
         y = "Standardized Residuals",
         title = "Standardized Residual Plot")+ 
    geom_hline(yintercept=0)+
    theme_bw()+
    theme(plot.title = element_text(hjust=0.5, size = rel(2)))+
    theme(axis.title.y = element_text(size = rel(1.4)))+
    theme(axis.title.x = element_text(size = rel(1.4)))+
    theme(axis.text.x = element_text(size = rel(1.6)))+
    theme(axis.text.y = element_text(size = rel(1.6)))

##Levene Test
  ##Step 1: Create two groups
  out.MPG$yHatCategory <- ifelse(out.MPG$.fitted < median(out.MPG$.fitted), c("group1"), c("group2")) 
  out.MPG$yHatCategory<- factor(out.MPG$yHatCategory, levels = c("group1", "group2"))
  ##Step 2: Run test
  leveneTest(.resid ~ yHatCategory, data=out.MPG)
  
##Histogram and QQplot
  par(mfrow=c(1,2)) ##get 2 graphs in the same window
  
  ggplot(out.MPG, aes(x = .resid))+
    geom_histogram()+
    labs(x = "Residuals",
         title = "Histogram of Residuals")+ 
    theme_bw()+
    theme(plot.title = element_text(hjust=0.5, size = rel(2)))+
    theme(axis.title.y = element_text(size = rel(1.4)))+
    theme(axis.title.x = element_text(size = rel(1.4)))+
    theme(axis.text.x = element_text(size = rel(1.6)))+
    theme(axis.text.y = element_text(size = rel(1.6)))

  ggplot(out.MPG, aes(y = .resid))+
    stat_qq()+
    stat_qq_line()+
    labs(x = "Theoretical Normal Quantiles",
         y = "Residuals",
         title = "Normal Q-Q Plot")+
    theme_bw()+
    theme(plot.title = element_text(hjust=0.5, size = rel(2)))+
    theme(axis.title.y = element_text(size = rel(1.4)))+
    theme(axis.title.x = element_text(size = rel(1.4)))+
    theme(axis.text.x = element_text(size = rel(1.6)))+
    theme(axis.text.y = element_text(size = rel(1.6)))

##Shapiro Wilk Test
  shapiro.test(out.MPG$.resid)

