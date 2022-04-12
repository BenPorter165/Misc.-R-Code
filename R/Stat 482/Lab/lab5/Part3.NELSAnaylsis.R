##Loaded necessary packages
  library(ggplot2)
  library(psych)
  library(car)
  library(GGally)

##Read in data file
  math<-read.csv(file.choose(), header=T)

##Regression with math self-efficacy = x1, 
##math identity = x2, science identity = x3
  model1 <- lm(x1txmth ~ x1mtheff + x1mthid + x1sciid, 
               data = math)
  summary(model1)

##Checking Assumptions

  ##Create a data frame including results from the lm() 
  ##function
    outmodel1 = fortify(model1)
        
  ##Create residual plot with predicted values on x-axis 
  ##and residuals on y-axis
    ggplot(outmodel1, aes(x=.fitted, y=.resid))+ 
      geom_point(shape=1, size=3)+ 
      labs(x = "Predicted Math Score",
           y = "Residuals",
           title = "Residual Plot")+
      geom_hline(yintercept=0)+
      theme_bw()+
      theme(plot.title = element_text(hjust=0.5, size = rel(1.6)))+
      theme(axis.title.y = element_text(size = rel(1.4)))+
      theme(axis.title.x = element_text(size = rel(1.4)))+
      theme(axis.text.x = element_text(size = rel(1.6)))+
      theme(axis.text.y = element_text(size = rel(1.6)))
 
  ##Create residual plot with predicted values on x-axis 
  ##and STANDARDIZED residuals on y-axis
    ggplot(outmodel1, aes(x=.fitted, y=.stdresid))+ 
      geom_point(shape=1, size=3)+ 
      labs(x = "Predicted Math Score",
           y = "Standardized Residuals",
           title = "Standardized Residual Plot")+
      geom_hline(yintercept=0)+
      theme_bw()+
      theme(plot.title = element_text(hjust=0.5, size = rel(1.6)))+
      theme(axis.title.y = element_text(size = rel(1.4)))+
      theme(axis.title.x = element_text(size = rel(1.4)))+
      theme(axis.text.x = element_text(size = rel(1.6)))+
      theme(axis.text.y = element_text(size = rel(1.6)))

  ##Normal QQ Plot
    qqnorm(outmodel1$.resid, main = "Normal Q-Q Plot",
           xlab = "Theoretical Normal Quantiles", 
           ylab = "Sample Residuals",
           plot.it = TRUE, datax = FALSE)  
    qqline(outmodel1$.resid)  ##Adds line to plot

  ##Histogram of residuals
    hist(outmodel1$.resid, 
         main="Histogram of Residuals", xlab="Residuals")

##Levene test for Homoskedasticity
  ##Step 1: Create two groups
    outmodel1$yHatCategory <- ifelse(outmodel1$.fitted < median(outmodel1$.fitted), 
                                     c("group1"), c("group2"))
    outmodel1$yHatCategory<- as.factor(outmodel1$yHatCategory)
  ##Step 2: Run test
    leveneTest(.resid ~ yHatCategory, data=outmodel1)

  ##Shapiro-Wilk test for Normality
    ##Does not work because the sample size is larger than 5000.
