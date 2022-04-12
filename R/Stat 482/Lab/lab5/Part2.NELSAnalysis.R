##Loaded necessary packages
  library(ggplot2)
  library(psych)
  library(car)
  library(GGally)

##Read in data file
  math<-read.csv(file.choose(), header=T)

##Set up sex and parent employment variables to be categorical
  math$x1sex<- as.factor(math$x1sex)
  math$x1par1emp<- as.factor(math$x1par1emp)
  
##Descriptive Statistics
  describe(math$x1txmth) #math scores
  describe(math$x1mtheff) #math self-efficacy
  describe(math$x1mthid) #math identity
  describe(math$x1sciid) #science identity
  summary(math$x1par1emp) #parent 1 employment
  summary(math$x1sex) #sex  
  
##Descriptive statistics of math scores for males
  summary(math$x1txmth[math$x1sex==0])
  
##Descriptive statistics of math scores for females
  summary(math$x1txmth[math$x1sex==1])

##Scatterplot matrix
  pairs(~ x1txmth + x1mtheff + x1mthid + x1sciid, 
        data = math, 
        main = "Simple Scatterplot Matrix", 
        labels = c("Math Score", 
                   "Math Efficacy", 
                   "Math Identity", 
                   "Science Identity"))
  
##Correlation matrix
  cormatrix <- cor(math[,c(3:6)])
  cormatrix

##Scatterplot and Correlation Matrix
  ggpairs(math[,c(3:6)], 
          diag = list(continuous = wrap("barDiag")))+
    labs(title = "Scatterplot and Correlation Matrix")+
    theme_bw()+
    theme(plot.title = element_text(hjust=0.5, size = rel(2)))
    
##Regression with self-efficacy = x1, 
##math identity = x2, science identity = x3
  model1 <- lm(x1txmth ~ x1mtheff + x1mthid + x1sciid, 
               data = math)
  summary(model1)
  
##Regression with math identity = x1 and sex = x2
  model2 <- lm(x1txmth ~ x1mthid + x1sex, data = math)
  summary(model2)
  
##Regression with math identity = x1 and 
##first parent employment = x2 
  model3<- lm(x1txmth ~ x1mthid + x1par1emp, data = math)
  summary(model3)

  