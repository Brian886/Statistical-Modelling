#set working directory
getwd()
setwd("C:/Statsmodel/Class 02")

#import data
library(readxl)
cars <- read_excel("usedcars.xlsx")

#Check the type of variables
str(cars)
typeof(cars$fuel)
unique(cars$fuel)
unique(cars$age)

#summary data based on fuel type
by(cars[,c(2,3,4,5)], cars$fuel, summary)

#plot boxplot
library(ggplot2)
library(scales)

png("boxplot.png", width = 900, height = 600)
plot <- ggplot(data=cars, mapping = aes(y=price, x= fuel))+
  geom_boxplot()+
  theme_minimal()+
  labs(
    title="Boxplot of the prices of cars based on the type of fuel",
    y="Car prices",
    tag="First Grapgh"
  )+
  scale_y_continuous(labels = comma)+
  theme(text = element_text(family="serif"))+
  stat_summary(fun.y = mean, geom="point", shape=23, size=2)
dev.off()

#important descriptive statistic
#coefficient of variation
sd(cars$price)/mean(cars$price)
lapply(cars[,c(2,3,4,5)], mean)
laaply(cars[,c(2,3,4,5)], sd)
#check quantile
quantile(cars$price)
sapply(cars[,c(2,3,4,5)], quantile)


#check the moments package
install.packages("moments")
moments::skewness(cars$price)
moments::kurtosis(cars$price) - 3
#it is not exactly the alpha 4 indicator
#it does not subtract the 3 value
#it should be compared to 3 instead

#Histogram
ggplot(cars, aes(x=fuel))+
  geom_histogram(aes(y=..density..),color="black", fill="wheat2", bins = 10)+
  theme_minimal()+
  geom_vline(aes(xintercept=mean(HP)), color='dodgerblue4',
             linetype="dashed", size=1)+
  labs(title="Histogram of Size Variable")+
  theme(text=element_text(family="serif"))+
  stat_function(fun=dnorm, args = list(mean=mean(cars$HP), sd=sd(cars$HP)))

