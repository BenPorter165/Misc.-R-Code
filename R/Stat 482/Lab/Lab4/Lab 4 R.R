##open libraries
library(ggplot2)
library(multcomp)

##open data file
introstat<- read.csv(file.choose(), header=T)
colnames(introstat)
head(introstat)
##Define the variable Section to be a categorical variable
introstat$Course<- factor(introstat$Course, levels = c("Stat 101", "Stat 104", "Stat 226"))
summary(introstat)
##Side-by-Side Boxplot
ggplot(introstat, aes(x=Course, y=Credit.Hours))+ 
  geom_boxplot()+ 
  labs(x = "Course",
       y = "Credit.Hours", 
       title = "Credit.Hours by Course")+
  ylim(0, 170)+
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5, size = rel(2)))+    
  theme(axis.title.y = element_text(size = rel(1.4)))+
  theme(axis.title.x = element_text(size = rel(1.4)))+
  theme(axis.text.x = element_text(size = rel(1.6)))+
  theme(axis.text.y = element_text(size = rel(1.6)))

##Find parameter estimates - sets baseline category to Section 1
predicted.Credit.Hours<- lm(Credit.Hours ~ Course, data = introstat)

##Get summary of model estimates with baseline category to Section 1
summary(predicted.Credit.Hours)

##Estimated mean values for all observations
predicted.Credit.Hours$fitted.values

##Residual values for all observations
predicted.Credit.Hours$residuals

##ANOVA Table and F-test statistic
anova(predicted.Credit.Hours)

##Checking Assumptions
out.Credit.Hours = fortify(predicted.Credit.Hours)
head(out.Credit.Hours)

##Add the Course variable to data frame
out.Credit.Hours$Course<- introstat$Course

## constant variance assumption

##Residuals plot
ggplot(out.Credit.Hours, aes(x=Course, y=.resid))+ 
  geom_boxplot()+ 
  labs(x = "Course",
       y = "Credit.Hours", 
       title = "Credit.Hours by Course Residuals Plot")+
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5, size = rel(2)))+    
  theme(axis.title.y = element_text(size = rel(1.4)))+
  theme(axis.title.x = element_text(size = rel(1.4)))+
  theme(axis.text.x = element_text(size = rel(1.6)))+
  theme(axis.text.y = element_text(size = rel(1.6)))


##Histogram and QQplot
par(mfrow=c(1,2)) ##get 2 graphs in the same window

hist(out.Credit.Hours$.resid, main="Histogram of Residuals", xlab="Residuals")
qqnorm(out.Credit.Hours$.resid, main = "Normal Q-Q Plot",
       xlab = "Theoretical Normal Quantiles", ylab = "Residuals",
       plot.it = TRUE, datax = FALSE)
qqline(out.Credit.Hours$.resid)

##Shapiro Wilk Test
shapiro.test(out.Credit.Hours$.resid)

##Ad-hoc tests using Tukey HSD method
compare.Credit.Hours <- glht(predicted.Credit.Hours, 
            linfct = mcp(Course="Tukey"))

##Obtain the pairwise confidence intervals
confint(compare.Credit.Hours)
##Obtain the pairwise confidence interval graph
plot(compare.Credit.Hours)
##Obtain the connected letter display
cld(compare.Credit.Hours)








