library(ggplot2)
library(car)
air<- read.csv(file.choose(), header=T)
colnames(air)

##Scatterplot
ggplot(air, aes(x=DeptDelay, y=ArrvDelay))+ 
  geom_point(shape=16, size=3)+ 
  labs(x = "DeptDelay",
       y = "ArrvDelay", 
       title = "DeptDelay vs ArrvDelay")+
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5, size = rel(2)))+    
  theme(axis.title.y = element_text(size = rel(1.4)))+
  theme(axis.title.x = element_text(size = rel(1.4)))+
  theme(axis.text.x = element_text(size = rel(1.6)))+
  theme(axis.text.y = element_text(size = rel(1.6)))+
  geom_smooth(method="lm", se=FALSE)

##lm
delay.model<- lm(ArrvDelay ~ DeptDelay, data = air)
summary(delay.model)

##Residual plot
out.ArrvDelay = fortify(delay.model)
head(out.ArrvDelay)

ggplot(out.ArrvDelay, aes(x=.fitted, y=.stdresid))+ 
  geom_point(shape=16, size=3)+ 
  labs(x = "Predicted DeptDelay",
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
out.ArrvDelay$yHatCategory <- ifelse(out.ArrvDelay$.fitted < median(out.ArrvDelay$.fitted), c("group1"), c("group2")) 
out.ArrvDelay$yHatCategory<- factor(out.ArrvDelay$yHatCategory, levels = c("group1", "group2"))
##Step 2: Run test
leveneTest(.resid ~ yHatCategory, data=out.ArrvDelay)

##Histogram and QQplot
par(mfrow=c(1,2)) ##get 2 graphs in the same window

hist(out.ArrvDelay$.resid, main="Histogram of Residuals", xlab="Residuals")
qqnorm(out.ArrvDelay$.resid, main = "Normal Q-Q Plot",
       xlab = "Theoretical Normal Quantiles", ylab = "Residuals",
       plot.it = TRUE, datax = FALSE)
qqline(out.ArrvDelay$.resid)

##Shapiro Wilk Test
shapiro.test(out.ArrvDelay$.resid)

##part 2 memory
memory<- read.csv(file.choose(), header=T)
colnames(memory)

##lm
memory.model<- lm(Words ~ Age, data = memory)
summary(memory.model)

##Estimated mean values for all observations
memory.model$fitted.values

##Boxplot
ggplot(memory, aes(x=factor(Age), y=Words))+ 
  geom_boxplot()+ 
  labs(x = "Age",
       y = "Words", 
       title = "Words by Age")+
  ylim(0, 40)+
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5, size = rel(2)))+    
  theme(axis.title.y = element_text(size = rel(1.4)))+
  theme(axis.title.x = element_text(size = rel(1.4)))+
  theme(axis.text.x = element_text(size = rel(1.6)))+
  theme(axis.text.y = element_text(size = rel(1.6)))

##Residual value for the specific observation
memory.model$residuals[15]
memory.model$residuals[65]


##Check for constant variance assumption (side by side boxplot of residuals)
ggplot(out.Words, aes(x=Age, y=.resid))+ 
  geom_boxplot()+ 
  labs(x = "Age",
       y = "Words", 
       title = "Words by Age")+
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5, size = rel(2)))+    
  theme(axis.title.y = element_text(size = rel(1.4)))+
  theme(axis.title.x = element_text(size = rel(1.4)))+
  theme(axis.text.x = element_text(size = rel(1.6)))+
  theme(axis.text.y = element_text(size = rel(1.6)))

##Standard deviation by group 
tapply(memory$Words, memory$Age, sd)

##Histogram and QQplot
par(mfrow=c(1,2)) ##get 2 graphs in the same window

hist(out.Words$.resid, main="Histogram of Residuals", xlab="Residuals")
qqnorm(out.Words$.resid, main = "Normal Q-Q Plot",
       xlab = "Theoretical Normal Quantiles", ylab = "Residuals",
       plot.it = TRUE, datax = FALSE)
qqline(out.Words$.resid)

##Shapiro Wilk Test
shapiro.test(out.Words$.resid)









