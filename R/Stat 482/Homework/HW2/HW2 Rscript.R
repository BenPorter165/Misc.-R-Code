library(ggplot2)
library(car)

diamonds<-read.csv(file.choose(), header=T)
colnames(diamonds)

##Basic Scatterplot with Estimated Simple Linear Regression Line
ggplot(diamonds, aes(x=Weight, y=Price))+ 
  geom_point(shape=16, size=3) + 
  labs(x = "Weight", 
       y = "Price", 
       title = "Weight vs. Price")+
  theme_bw()+
  geom_smooth(method="lm", se=FALSE)+
  theme(plot.title = element_text(hjust=0.5, size = rel(1.6)))+
  theme(axis.title.y = element_text(size = rel(1.4)))+
  theme(axis.title.x = element_text(size = rel(1.4)))+
  theme(axis.text.x = element_text(size = rel(1.6)))+
  theme(axis.text.y = element_text(size = rel(1.6)))

##Simple Linear Regression Model Estimate
modeldiamonds <- lm(Price ~ Weight, data = diamonds)
summary(modeldiamonds)

##Checking Assumptions
outdiamonds = fortify(modeldiamonds)

##Standardized Residual Plot  
ggplot(outdiamonds, aes(x=.fitted, y=.stdresid)) + 
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
outdiamonds$yHatCategory <- ifelse(outdiamonds$.fitted < median(outdiamonds$.fitted), 
                              c("group1"), c("group2")) 
outdiamonds$yHatCategory <- factor(outdiamonds$yHatCategory, 
                              levels = c("group1", "group2"))

leveneTest(.resid ~ yHatCategory, data=outdiamonds)

##Histogram and QQ Plot for Residuals  
par(mfrow=c(1,2))

hist(outdiamonds$.resid, main="Histogram of Residuals", xlab="Residuals")

qqnorm(outdiamonds$.resid, main = "Normal Q-Q Plot",
       xlab = "Theoretical Normal Quantiles", 
       ylab = "Sample Residuals",
       plot.it = TRUE, datax = FALSE, pch = 16)
qqline(outdiamonds$.resid) 

par(mfrow=c(1,1))

##Shapiro-Wilk test for Normality
shapiro.test(outdiamonds$.resid)

##Confidence Interval for slope
confint(modeldiamonds, level=0.95)

##Prediction and Confidence Intervals

#Set new Weight value
new.Weight = data.frame(Weight = 0.3)

##Confidence Interval
predict(modeldiamonds, new.Weight, interval="confidence")

#Prediction Interval
predict(modeldiamonds, new.Weight, interval="prediction")





































