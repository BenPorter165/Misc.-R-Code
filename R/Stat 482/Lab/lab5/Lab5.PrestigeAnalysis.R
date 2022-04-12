##Load necessary packages
  library(ggplot2)
  library(psych)
  library(car)

##Read in the data file
  pdata<-read.csv(file.choose(), header=T)
head(pdata)
##Least Squares Regression Analysis
  model1<-lm(prestige ~ education + income + type, 
             data = pdata)
  summary(model1)
  
  ##Correlation matrix
  cormatrix <- cor(pdata[,c(2,3,5)])
  cormatrix
##scatterplot
  pairs(~ education + income + prestige, 
        data = pdata, 
        main = "Simple Scatterplot Matrix", 
        labels = c("Education", 
                   "Income", 
                   "Prestige"))
  
##Plots of residuals
  ##Create a data frame including results from the 
  ##lm() function
  outmodel1 = fortify(model1)
  
  ##Create standardized residual plot with predicted values 
  ##on x-axis
  ggplot(outmodel1, aes(x=.fitted, y=.stdresid))+ 
    geom_point(shape=16, size=3)+ 
    labs(x = "Predicted Prestige",
         y="Studentized Residuals",
         title="Studentized Residual Plot")+
    geom_hline(yintercept=0)+
    theme_bw()+
    theme(plot.title = element_text(hjust=0.5, size = rel(2)))+
    theme(axis.title.y = element_text(size = rel(1.4)))+
    theme(axis.title.x = element_text(size = rel(1.4)))+
    theme(axis.text.x = element_text(size = rel(1.6)))+
    theme(axis.text.y = element_text(size = rel(1.6)))+
    geom_hline(yintercept=2, colour="blue")+
    geom_hline(yintercept=-2, colour="blue")+
    geom_hline(yintercept=3, colour="red")+
    geom_hline(yintercept=-3, colour="red")

##Levene test for Homoskedasticity
  ##Step 1: Create two groups
  outmodel1$yHatCategory <- ifelse(outmodel1$.fitted < median(outmodel1$.fitted), c("group1"), c("group2")) 
  outmodel1$yHatCategory <- factor(outmodel1$yHatCategory, levels = c("group1", "group2"))
  ##Step 2: Run test
  leveneTest(.resid ~ yHatCategory, data=outmodel1)

##Histogram of residuals
  hist(outmodel1$.resid, 
       main="Histogram of Residuals", xlab="Residuals")
  
##Normal QQ Plot
  qqnorm(outmodel1$.resid, main = "Normal Q-Q Plot",
         xlab = "Theoretical Normal Quantiles", 
         ylab = "Sample Residuals",
         plot.it = TRUE, datax = FALSE, pch = 16)
  qqline(outmodel1$.resid)

##Shapiro-Wilk test for Normality
  shapiro.test(outmodel1$.resid)
  
##Create plot with leverage values on x-axis and 
  ##STANDARDIZED residuals on y-axis, size by cooks d
  ##Leverage cut-offs:
  lev.cutoff.low<-2*(4+1)/102
  lev.cutoff.high<-3*(4+1)/102
  
ggplot(outmodel1, aes(x=.hat, y=.stdresid, size=.cooksd))+ 
  geom_point()+ 
  labs(x = "Leverage Values",
       y = "Standardized Residuals",
       title = "Influence Plot")+
  geom_hline(yintercept=0)+
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5, size = rel(2)))+
  theme(axis.title.y = element_text(size = rel(1.4)))+
  theme(axis.title.x = element_text(size = rel(1.4)))+
  theme(axis.text.x = element_text(size = rel(1.6)))+
  theme(axis.text.y = element_text(size = rel(1.6)))+
  geom_hline(yintercept=2, colour="blue")+
  geom_hline(yintercept=-2, colour="blue")+
  geom_hline(yintercept=3, colour="red")+
  geom_hline(yintercept=-3, colour="red")+
  guides(size=guide_legend(title="Cook's D"))+
  geom_vline(xintercept=lev.cutoff.low, colour="blue")+
  geom_vline(xintercept=lev.cutoff.high, colour="red")
  
