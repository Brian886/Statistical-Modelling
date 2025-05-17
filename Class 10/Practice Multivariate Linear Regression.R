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


#------------------------------------------------------------
#3)Marginal Effect of Predictors
#-----------------------------------------------------------
summary(multvariatemodel)
#beta1. +0.04
#Covid mortality rate is expected to rise by 0.04% point when num of nurses
#per 10,000 inhabitants increase by 1, ceteris paribus
#beta2. +0.0028
#beta3. +0.2656
#beta0. -0.15

#3.1) Partial elasticity
Betas <- coef(multvariatemodel) #save the coefficients to a separate vector for further calculations
Betas #first element is beta0

#elasticity
#unemployment on COVID mortality is in the Pécs district, 
#where the number of nurses per 10,000 people is 6.5, 
#unemployment is 222/10,000, and the proportion of women over 65 is 13.3%:
elasticity <- (Betas[3]*222)/(Betas[1]+Betas[2]*6.5+Betas[3]*222+Betas[4]*13.3)
elasticity
#result is already in %
#0.15%. 
#if the 222 unemployment rate is increased by 1% and ceteris paribus, the
#corvid mortality is expected to increase by 0.15%.

#Let’s compute the unemployment elasticity in the Siklós district, 
#where Nurses=6.7, Unemp=692, WomenOlder65=12.7:
elasticity2 <- (Betas[3]*682)/(Betas[1]+Betas[2]*6.7+Betas[3]*692+Betas[4]*12.7)
elasticity2

#3.2)Partial correlation
mean(covid$Nurses)
mean(covid$Unemp)
#Comparing βj
#coefficients across such different averages would be unfair, 
#since +1 unit is relatively more significant among nurses than
#among unemployed people.

#let us check with correlation
cor(covid$DeathsCOVID, covid$Nurses)
cor(covid$DeathsCOVID, covid$Unemp) 

#we prefer partial correlations use ppcor library
install.packages("ppcor") 
library(ppcor)

pcor(covid[,2:5]) # columns 2-5 contain variables in our regression
#unemployment has a +0.383 partial correlation with COVID mortality 
#after controlling for the effects of the number of nurses and 
#the proportion of women over 65.
#In contrast, the number of nurses has only a very weak partial 
#correlation with COVID mortality, with an absolute value below 0.05 (0.047) 
#after controlling for unemployment and the proportion of women over 65.

#3.3) Path analysis
#PredictDeathsCOVID=β1×Nurses+β2×Unemp+β0.
## first, get the multivariate regression with Nurses and Unemp
twopredictor_model <- lm(DeathsCOVID ~ Nurses+Unemp, data=covid)
#get the coefficients
twopredictor_model$coefficients
#save the 2nd and 3rd elements in the vectors: note that the 1st element is Beta_0!
Beta_nurses_covid <- twopredictor_model$coefficients[2]
Beta_unemp_covid <- twopredictor_model$coefficients[3]

#The magnitude of the effect on the green arrow is simply given by the coefficient β1
#of the bivariate regression PredictUnemp=β1×Nurses+β0.
# first, get the simple (bivariate regression)
unemp_nurses_model <- lm(Unemp~Nurses, data=covid)
# get the vector of coefficients
unemp_nurses_model$coefficients
# save the 2nd element in the vectors: note that the 1st element is Beta_0!
Beta_unemp_nurses <- unemp_nurses_model$coefficients[2]

#direct effect of nurses on COVID deaths (red arrow)
direct_nurse_covid <- Beta_nurses_covid
direct_nurse_covid

## indirect effect of nurses on COVID deaths (green*blue arrow)
indirect_nurse_covid <- Beta_unemp_nurses*Beta_unemp_covid
indirect_nurse_covid

# total effect = direct + indirect effects
total_nurses_covid <- direct_nurse_covid+indirect_nurse_covid
total_nurses_covid

#value is exactly the same as the slope of the original simple bivariate regression PredictCOVIDDeaths=β1×Nurses+β0
simple_model$coefficients


#3.4) Partial t-test
summary(multvariatemodel)
# 0.096 means that the β of Nurses is 0.04 in the sample, 
#but if we ran the regression on new districts outside the observed sample, this value could fluctuate around 0.04 with an expected variability of ±0.096
