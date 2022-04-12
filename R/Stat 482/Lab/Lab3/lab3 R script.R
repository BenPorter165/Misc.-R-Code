library(ggplot2)
library(car)

statcan<-read.csv(file.choose(), header=T)
colnames(statcan)

##Basic Scatterplot with Estimated Simple Linear Regression Line
ggplot(statcan, aes(x=Height, y=Armspan))+ 
  geom_point(shape=16, size=3) + 
  labs(x = "Height", 
       y = "Armspan", 
       title = "Height vs. Armspan")+
  theme_bw()+
  geom_smooth(method="lm", se=FALSE)+
  theme(plot.title = element_text(hjust=0.5, size = rel(1.6)))+
  theme(axis.title.y = element_text(size = rel(1.4)))+
  theme(axis.title.x = element_text(size = rel(1.4)))+
  theme(axis.text.x = element_text(size = rel(1.6)))+
  theme(axis.text.y = element_text(size = rel(1.6)))

##Simple Linear Regression Model Estimate
modelarm <- lm(Armspan ~ Height, data = statcan)
summary(modelarm)

##Checking Assumptions
outarm = fortify(modelarm)

##Standardized Residual Plot  
ggplot(outarm, aes(x=.fitted, y=.stdresid)) + 
  geom_point(shape=16, size=3) + 
  labs(x = "Predicted Height", 
       y = "Standardized Residuals", 
       title = "Standardized Residual Plot")+
  geom_hline(yintercept=0)+
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5, size = rel(1.6)))+
  theme(axis.title.y = element_text(size = rel(1.4)))+
  theme(axis.title.x = element_text(size = rel(1.4)))+
  theme(axis.text.x = element_text(size = rel(1.6)))+
  theme(axis.text.y = element_text(size = rel(1.6)))

#Levene Test
outarm$yHatCategory <- ifelse(outarm$.fitted < median(outarm$.fitted), 
                              c("group1"), c("group2")) 
outarm$yHatCategory <- factor(outarm$yHatCategory, 
                              levels = c("group1", "group2"))

leveneTest(.resid ~ yHatCategory, data=outarm)

##Histogram and QQ Plot for Residuals  
par(mfrow=c(1,2))

hist(outarm$.resid, main="Histogram of Residuals", xlab="Residuals")

qqnorm(outarm$.resid, main = "Normal Q-Q Plot",
       xlab = "Theoretical Normal Quantiles", 
       ylab = "Sample Residuals",
       plot.it = TRUE, datax = FALSE, pch = 16)
qqline(outarm$.resid) 

par(mfrow=c(1,1))

##Shapiro-Wilk test for Normality
shapiro.test(outarm$.resid)

##Confidence Interval for slope
confint(modelarm, level=0.95)

##Prediction and Confidence Intervals

#Set new Height value
new.Height = data.frame(Height = 173)

#Prediction Interval
predict(modelarm, new.Height, interval="prediction")

##Confidence Interval
predict(modelarm, new.Height, interval="confidence")

##Add Estimation Interval Bands and Prediction Interval Bands to scatterplot

##Compute Estimation Intervals for each observation
predsCI <-predict(modelarm, statcan, interval="confidence")

##Compute Prediction Intervals for each observation
predsPI <-predict(modelarm, statcan, interval="prediction")

##Add the interval limits to the dataset
statcan$lwrCI <- predsCI[,'lwr']
statcan$uprCI <- predsCI[,'upr']
statcan$lwrPI <- predsPI[,'lwr']
statcan$uprPI <- predsPI[,'upr']

##Create Scatterplot
ggplot(statcan, aes(x=Height, y=Armspan))+ 
  geom_point(shape=16, size=3)+ 
  geom_ribbon(aes(ymin=lwrPI, ymax=uprPI), 
              fill="red", colour = "red", alpha=0.1)+ 
  geom_ribbon(aes(ymin=lwrCI, ymax=uprCI), 
              fill="blue", colour = "blue", alpha=0.1)+ 
  labs(x = "Height", 
       y = "Armspan", 
       title = "Height vs. Armspan")+
  geom_smooth(method="lm", se=FALSE)+
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5, size = rel(1.6)))+
  theme(axis.title.y = element_text(size = rel(1.4)))+
  theme(axis.title.x = element_text(size = rel(1.4)))+
  theme(axis.text.x = element_text(size = rel(1.6)))+
  theme(axis.text.y = element_text(size = rel(1.6)))




