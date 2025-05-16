setwd("C:/Stats/Class 11")
library(readxl)
covid <- read.csv("COVID_Deaths_Hungary.csv")
str(covid)
#---------------------------------------------------
simple_model <- lm(DeathsCOVID ~ Nurses, data=covid)
summary(simple_model)
#The number of nurses explains about 18% of the variation in COVID mortality across the districts (R2=17.6), 
#which can be considered a moderate explanatory power since 10<R2<50.
#This explanatory power seems not to collapse to 0 even in districts not included in our sample, 
#as the F-test p-value is 1.12×10−5, meaning that the null hypothesis H0:R2=0
#can be rejected at all usual significance levels.
#According to the residual standard deviation, 
#the COVID mortality rate estimated by our model differs from the actual data by an average of ±1.22percentage points.

#check out the correlation matrix of all numeric variables in COVID dataset
library(corrplot)

corr_matrix <- cor(covid[, 2:5])
corr_matrix
corrplot(corr_matrix, method="number")

###############---------------------------------------------------------------------------
#2)OLS Estimation of Multivariate Linear Regression
#----------------------------------------------------------------------------------------
multvariatemodel <- lm(DeathsCOVID ~ Nurses+Unemp+WomenOlder65 ,data=covid)
summary(multvariatemodel)
