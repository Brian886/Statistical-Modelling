#set working directory
setwd("C:/Egyetem/Statmodel/videós")

#import
library(readxl)
Flat <- read_excel("BP_Flat.xlsx")

#structure
str(Flat)
#qualitative variables should be factor

#descriptive statistics
psych::describe(Flat)
#price is right-skewed
#data problem in floor variable?

#eliminate -1 from floor variable
Flat <- Flat[Flat$Floor>=0,]

#calculation of covariance
round(cov(Flat[,1:4]),3)
#in the diagonal we have the variances
round(var(Flat$Price),3)


#correlation matrix
#correlation is a normalized covariance
cormatrix <- cor(Flat[,1:4])
round(cormatrix,3)

#visualisation for correlation
install.packages("corrplot")

corrplot::corrplot(cormatrix, method = "number")
corrplot::corrplot(cormatrix, method = "color")

#We can filter out the "spill-over" effects
#It is the partial correlation, the correlation without the 
#effect of the other variables
install.packages("ppcor")
library(ppcor)
round(pcor(Flat[,1:4])$estimate,2)

#ggplot to check the relation between price and size
library(ggplot2)
ggplot(Flat, aes(x=Size,y=Price))+
  geom_point()+theme_minimal()
ggplot(Flat, aes(x=Size,y=Price))+
  geom_point()+stat_smooth(method = "lm")

#run regression model
model1 <- lm(Price ~ Size, data=Flat)
model1
summary(model1)

#test the parameter of Size variable
#H0: B(size)=0 (not significant)
#H1: B(size)!=0 (significant)
library(lmtest)
coeftest(model1)
t_test <- coeftest(model1)[2,1]/coeftest(model1)[2,2]
df_ttest <- nrow(Flat)-1-1
#We reject H0, it is significant

#calculate R^2
#SSR (residuals)
residuals <- model1$residuals
SSR <- sum(residuals^2)
SSR2 <- sum((Flat$Price-model1$fitted.values)^2)

#SST
SST <- sum((Flat$Price-mean(Flat$Price))^2)
var(Flat$Price)
SST/(1405-1)

#SSE (explained)
SSE <- SST-SSR
SSE2 <- sum((model1$fitted.values-mean(Flat$Price))^2)

#R^2
SSE/SST
1-SSR/SST
#It shows the explanatory power of the model
summary(model1)$r.squared

#In two-variable case, the R-squared is the square of the correlation
cor(Flat$Price, Flat$Size)^2

#We can calculate it by using the estimated value of y
cor(Flat$Price, model1$fitted.values)^2

#global F-test
#test the relevancy of the whole model
Ftest <- SSE/1/(SSR/(1405-1-1))
Ftest

#p-value
pf(Ftest,1,(1405-1-1),lower.tail = FALSE)
#it is close to 0, we reject H0, the model is relevant

# 95 % confidence interval for B parameter of Size
confint(model1)


##############Path analysis###############
Path <- read_excel("Path.xlsx")
#Model for all the variables
modelv3 <- lm(Price ~ Area+Age, data=Path)
summary(modelv3)
#regression between the explanatory variables
modelx2x1 <- lm(Age ~ Area, data=Path)
summary(modelx2x1)
#Direct effect of the area variable
modelx1 <- lm(Price ~ Area, data=Path)
summary(modelx1)
