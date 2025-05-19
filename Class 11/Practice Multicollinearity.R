##########################
#multicollinearity
###########################
library(readxl)
setwd("C:\Stats\Class 11")
fat <- read_excel("FAT_DATA.xlsx")

fatmodel <- lm(Fat ~ ., data= fat)
summary(fatmodel)

#we did not expect that the weight is not significant #i think lower the value the better
#possible reason: multicollinearity
#check correlation between explanatory vairables
library(corrplot)
corrmattrix <- cor(fat[2:15])
corrmattrix
corrplot(corrmattrix, method="number")
corrplot(corrmattrix, method="color")
#weight is highly correlated with with BMI? Neck, Chest, Hip,....etc

#calculate the VIF indicators
library(car)
round(vif(fatmodel),3)
#higher, higher risk of multicollinearity
#estimation variance of the parameter of weight is 34 times (*34)
#value is high due to multicollinearity
#greater than 10, so it is a harmful multicollinearity
#in the case of weight variable 

#calculate it manually
vif_model <- lm(Weight~. -Fat, data=fat)
summary(vif_model)
vif_weight <- 1/(1 - summary(vif_model)$r.squared)
vif_weight
#also 33.787

#tolerance indicator
tol_weight <- 1 - summary(vif_model)$r.squared
tol_weight
#0.03, 3%
# R^2 is 97%
#97% pf the variance of the weight variable is explained by the other explanatory vaiable which is super high

#the diagonal of the inverse correlation matrix of the explanatory variables contains the VIF indicators
round(diag(solve(cor(fat[2:15]))),3)
#weight, BMI, Chest, Hip has serious multicollinearity issues
