##Loaded necessary packages
  library(ggplot2)
  library(psych)
  
##Load data file
  mam<- read.csv(file.choose(), header=T)

##Basic Scatterplot
  ggplot(mam, aes(x = BrainWeight, y = GestationTime))+ 
    geom_point(shape=16, size=3)+ 
    labs(x = "Brain Weight in Grams",
         y = "Gestation Time in Days", 
         title = "Gestation Time vs Brain Weight")+
    theme_bw()+
    theme(plot.title = element_text(hjust=0.5, size = rel(2)))+    
    theme(axis.title.y = element_text(size = rel(1.4)))+    
    theme(axis.title.x = element_text(size = rel(1.4)))+    
    theme(axis.text.x = element_text(size = rel(1.6)))+    
    theme(axis.text.y = element_text(size = rel(1.6)))+    
    geom_smooth(method = "lm", se = FALSE)

##Correlation
  cor(mam$BrainWeight, mam$GestationTime)

##Simple Linear Regression Model
  modelMam <- lm(GestationTime ~ BrainWeight, data = mam)
  summary(modelMam)

##Checking Assumptions
  outMam = fortify(modelMam)

##Create residual plot with predicted values on x-axis 
##and STUDENTIZED residuals on y-axis. Include +/- 2
##and +/-3 cutoffs
  ggplot(outMam, aes(x=.fitted, y=.stdresid))+ 
    geom_point(shape=16, size=3) + 
    labs(x = "Predicted Gestation",
         y = "Studentized Residuals",
         title = "Studentized Residual Plot")+
    geom_hline(yintercept=0)+
    geom_hline(yintercept=2, colour="blue")+
    geom_hline(yintercept=-2, colour="blue")+
    geom_hline(yintercept=3, colour="red")+
    geom_hline(yintercept=-3, colour="red")+
    theme_bw()+
    theme(plot.title = element_text(hjust=0.5, size = rel(2)))+
    theme(axis.title.y = element_text(size = rel(1.4)))+
    theme(axis.title.x = element_text(size = rel(1.4)))+
    theme(axis.text.x = element_text(size = rel(1.6)))+
    theme(axis.text.y = element_text(size = rel(1.6)))
    
##Histogram of studentized residuals
  hist(outMam$.stdresid, 
       main = "Histogram of Studentized Residuals", 
       xlab = "Studentized Residuals")

##Leverage values
##Calculate cutoffs
  lev.cutoff.low <- 2*(1+1)/53
  lev.cutoff.high <- 3*(1+1)/53

##Create plot with leverage values on x-axis and 
##studentized residuals on y-axis, size by Cook's D
  ggplot(outMam, aes(x = .hat, y = .stdresid, size = .cooksd))+ 
    geom_point()+ 
    labs(x = "Leverage Values",
         y = "Studentized Residuals",
         title = "Leverage and Influence Plot")+
    geom_hline(yintercept=0)+
    geom_vline(xintercept=lev.cutoff.low, colour="blue")+
    geom_vline(xintercept=lev.cutoff.high, colour="red")+
    geom_hline(yintercept=2, colour="blue")+
    geom_hline(yintercept=-2, colour="blue")+
    geom_hline(yintercept=3, colour="red")+
    geom_hline(yintercept=-3, colour="red")+
    guides(size = guide_legend(title = "Cook's D"))+
    theme_bw()+
    theme(plot.title = element_text(hjust=0.5, size = rel(2)))+
    theme(axis.title.y = element_text(size = rel(1.4)))+
    theme(axis.title.x = element_text(size = rel(1.4)))+
    theme(axis.text.x = element_text(size = rel(1.6)))+
    theme(axis.text.y = element_text(size = rel(1.6)))
    
##Analysis without Man
  
##Create subset of data with Man removed
##The outlier was in row 26
  mamNoMan = mam[-26,]
  
##Correlation
  cor(mamNoMan$BrainWeight, mamNoMan$GestationTime)
  
##Model
  modelMamNoMan <- lm(GestationTime ~ BrainWeight, 
                      data = mamNoMan)
  summary(modelMamNoMan)
  
##Scatterplot with both regression lines (with and without man)
  d=data.frame(s=c(modelMam$coefficients[2], modelMamNoMan$coefficients[2]), 
               ic=c(modelMam$coefficients[1], modelMamNoMan$coefficients[1]), 
               RegressionLine=c('Man', 'No Man'))
  
  ggplot(mam, aes(x=BrainWeight, y=GestationTime))+ 
    geom_point(shape=16, size=3) + 
    labs(x = "Brain Weight in Grams",
         y = "Gestation Time in Days", 
         title = "Gestation Time vs Brain Weight")+
    theme_bw()+
    theme(plot.title = element_text(hjust=0.5, size = rel(2)))+    
    theme(axis.title.y = element_text(size = rel(1.4)))+    
    theme(axis.title.x = element_text(size = rel(1.4)))+    
    theme(axis.text.x = element_text(size = rel(1.6)))+    
    theme(axis.text.y = element_text(size = rel(1.6)))+    
    geom_abline(aes(intercept = ic, slope = s, 
                    colour = RegressionLine), 
                    data=d, show.legend = TRUE)+
    guides(color = guide_legend(title = "Regression Line"))
    
##Multiple Regression Model
  modelMamMult <- lm(GestationTime ~ BrainWeight + BodyWeight + 
                      TotalSleep + LifeSpan, data=mam)
  summary(modelMamMult)
    
##Create a data frame including results from the lm() function
  outMamMult = fortify(modelMamMult)
  
  lev.cutoff.low.mult<- 2*(4+1)/47
  lev.cutoff.high.mult<- 3*(4+1)/47
    
##Create plot with leverage values on x-axis and 
##STANDARDIZED residuals on y-axis, size by Cook's D
  ggplot(outMamMult, aes(x=.hat, y=.stdresid, size=.cooksd))+ 
    geom_point()+ 
    labs(x = "Leverage Values",
         y = "Studentized Residuals",
         title = "Leverage and Influence Plot",
         subtitle = "Multiple Regression Model")+
    geom_hline(yintercept=0)+xlim(0, 1)+
    geom_vline(xintercept=lev.cutoff.low.mult, colour="blue")+
    geom_vline(xintercept=lev.cutoff.high.mult, colour="red")+
    geom_hline(yintercept=2, colour="blue")+
    geom_hline(yintercept=-2, colour="blue")+
    geom_hline(yintercept=3, colour="red")+
    geom_hline(yintercept=-3, colour="red")+
    guides(size=guide_legend(title="Cook's D"))+
    theme_bw()+
    theme(plot.title = element_text(hjust=0.5, size = rel(2)))+
    theme(plot.subtitle = element_text(hjust=0.5, size = rel(1.4)))+
    theme(axis.title.y = element_text(size = rel(1.4)))+
    theme(axis.title.x = element_text(size = rel(1.4)))+
    theme(axis.text.x = element_text(size = rel(1.6)))+
    theme(axis.text.y = element_text(size = rel(1.6)))
    