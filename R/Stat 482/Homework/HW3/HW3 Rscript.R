##open libraries
library(ggplot2)
library(multcomp)

##open data file
introstat<- read.csv(file.choose(), header=T)
colnames(introstat)
head(introstat)

introstat$Course<- factor(introstat$Course, levels = c("Stat 101", "Stat 104", "Stat 226"))

##Find parameter estimates - sets baseline category to Section 1
predicted.Hours<- lm(Hours ~ Course, data = introstat)
summary(predicted.Hours)

##check assumptions
out.Hours = fortify(predicted.Hours)
head(out.Hours)

##Residuals plot
ggplot(out.Hours, aes(x=Course, y=.resid))+ 
  geom_boxplot()+ 
  labs(x = "Course",
       y = "Residual", 
       title = "Hours by Course Residuals Plot")+
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5, size = rel(2)))+    
  theme(axis.title.y = element_text(size = rel(1.4)))+
  theme(axis.title.x = element_text(size = rel(1.4)))+
  theme(axis.text.x = element_text(size = rel(1.6)))+
  theme(axis.text.y = element_text(size = rel(1.6)))

##Shapiro Wilk Test
shapiro.test(out.Hours$.resid)

##Histogram and QQplot
par(mfrow=c(1,2)) ##get 2 graphs in the same window

hist(out.Hours$.resid, main="Histogram of Residuals", xlab="Residuals")
qqnorm(out.Hours$.resid, main = "Normal Q-Q Plot",
       xlab = "Theoretical Normal Quantiles", ylab = "Residuals",
       plot.it = TRUE, datax = FALSE)
qqline(out.Hours$.resid)

##ANOVA Table and F-test statistic
anova(predicted.Hours)


##Ad-hoc tests using Tukey HSD method
compare.Hours <- glht(predicted.Hours, 
                      linfct = mcp(Course="Tukey"))
compare.Hours
##Obtain the pairwise confidence intervals
confint(compare.Hours)
##Obtain the pairwise confidence interval graph
plot(compare.Hours)
##Obtain the connected letter display
cld(compare.Hours)


















