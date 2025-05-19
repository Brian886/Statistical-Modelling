#######################################-------------------------------
#Multicolllinearity
#######################################-------------------------------
fat <- read_excel("FAT_DATA.xlsx")

fatmodel <- lm(Fat~., data=fat)
summary(fatmodel)

#we did not expect that the weight is not significant #i think lower the value the better
#possible reason: multicollinearity
#check the correlation between the explanatory variables
install.packages("corrplot")
library(corrplot)
corrplot(cor(fat[2:15]), method = 'color')

#calculate the VIF indicators
library(car)
round(vif(fatmodel),3)
#for the weight variable the VIF indicator is 34
#estimation variance of the parameter of weight is 34 times
#higher because of the multicollinearity
#it is greater than 10, so there is harmful multucollinearity
#in case of the weight variable

#calculate it manually
vif_model <- lm(Weight~.-Fat, data=fat)
vif_weight <- 1/(1-summary(vif_model)$r.squared)
vif_weight

#tolerance indicator
to1_weight <- 1-summary(vif_model)$r.squared
to1_weight
#the unique information content of the weight variable is just 3%
#97% of the variance of the weight variable is explained by
#the other explanatory variables 

#the diagonal of the inverse correlation matrix of the explanatory variables contains the VIF indicators
round(diag(solve(cor(fat[2:15]))), 3)
round(vif(fatmodel), 3)


