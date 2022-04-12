library(ggplot2)
diamonds<-read.csv(file.choose(), header=T)
colnames(diamonds)

ggplot(diamonds, aes(x=Weight, y=Price))+ 
  geom_point(shape=16, size=3)+ 
  labs(x = "Weight", 
       y = "Price",
       title = "Weight vs. Price")+ 
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5, size = rel(2)))+
  theme(axis.title.y = element_text(size = rel(1.4)))+
  theme(axis.title.x = element_text(size = rel(1.4)))+
  theme(axis.text.x = element_text(size = rel(1.6)))+
  theme(axis.text.y = element_text(size = rel(1.6)))+
  geom_smooth(method="lm", se=FALSE)

cor(diamonds$Weight, diamonds$Price)

diamondslm<-lm(Price ~ Weight, data = diamonds)
summary(diamondslm)

diamondslm$fitted.values[14]
diamondslm$residuals[14]

new.Weight <- data.frame(Weight = 0.3)
predict.lm(diamondslm, new.Weight)

anova(diamondslm)