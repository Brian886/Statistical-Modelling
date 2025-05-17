#import wage1 file
library(readxl)
wages <- read_excel("wage1.xls")

#structure
str(wages)
#we know that the nonwhite, female, married are qualitative variables
#we treat these variables as factors
wages[,c(5,6, 7)] <- lapply(wages[, c(5,6, 7)], as.factor)

#first model
#wage and experience
model1 <- lm(wage ~ exper, data=wages)
summary(model1)
#wage = B0 +B1*exper + e
#wage estimate = 5.37 +0.03*exper (estimated y, so we wont add error)
#B0: exper = 0 if there is no experience of the employee, the expected hourly earning is 5.37 dollars
#B1: increase of 0.03 dollars in hourly earning on average when increase 1 year of experience

#R^2 coefficient of determination
summary(model1)$r.squared #0.01275

#test the parameter of the experience separately
#separate -> partial t test #just wanna know the direct effect
#H0: Beta_exp = 0: experience has no significant effect on wages in the population
#H1: B(exper) != 0 (significant)

install.packages("lmtest")
library(lmtest)
#check coefficient?
coeftest(model1) #I think summary is enough

#t - test for the parameters
t <- (summary(model1)$coefficients[2,1]-0) / summary(model1)$coefficients[2,2]
t
t_test <- coeftest(model1)[2,1]/coeftest(model1)[2,2]

#the relevancy of the model (test of the whole model)
#global F test
#it is in the output 
summary(model1)
#6.766: f-test
#p-value: 0.00955

#H0: the model is not relevant
#H1: the model is relevant
#we reject the H0 as the model is relevant outside the observed data
#there is at least one significant variable

#in case of binary regression, there is a connection between t and F test (wald test?)
#F = t^2
coeftest(model1)[2,3]^2
#6.76577 = 6.766

##we can estimate confidence interval for the Beta parameters
confint(model1, level = 0.95)
##the interval of experience does not contain 0 value
#it is significant different from 0 (statistically)
#so the experience variable is significant

#Multiple regression model
#educ + exper + tenure + nonwhite + female + married, data = wages
model2 <- lm(wage ~ educ + exper + tenure + nonwhite + female + married, data=wages)
summary(model2)
#According to the t test which are the significant variables?
Betatable <- summary(model2)$coefficients
Betatable
Betatable[order(Betatable[,4]), ]
#education is the most significant with the lowest p-value

##we reject H0 (pvalue < 0.05), so the educ is significant variable?????????

#for the experience, the p value > alpha, we fail to reject H0, it is not 
#significant. SOOOOOOO, Why the experience is not significant?

#could the tenure variable contain information of experience? 
#we can check with simple correlation
cor(wages$tenure, wages$exper)
#It is just 0.5, it is not that high
#so it is not the reason why the exper is not significant

#non white is not significant
#female is significant
#hourly wages of female is 1.7 lesser is other explanatory variables remains the same

#married is not significant

#compare model 1 and 2
#adjusted R^2
summary(model1)$adj.r.square
summary(model2)$adj.r.square
#in the second model the adjusted R squared is higher, the second model is better
#calculate adjusted R^2 manually 
adjr2 <- 1-(1-summary(model2)$r.squared)*((nrow(wages)-1)/(nrow(wages)-7))
adjr2

#Akaike
AIC(model1)
AIC(model2)
#model 2 wins

#Schwarz
BIC(model1)
BIC(model2)

#if you dont want to calculate it manually, you can install the package
install.packages("broom")
broom::glance(model1)
broom::glance(model2)

#Test the joint significance of new variables by wald F test
library(lmtest)
waldtest(model1, model2, test = "F")
#58.403
anova(model1, model2)
#anova and waltest provide the F-test value both are the same

#HO: B(educ) = B(exper)=B(tenure)=B(nonwhite)=B(female)=B(married)=0
#H1: at least one of these Beta is not 0
#right sided test 
#F: 58.403
#p-value is almost 0
#we reject the H0, as at least one of these variable has a significant relationship in the population

#calculate it manually
R_ur <- summary(model2)$r.squared
R_r <- summary(model1)$r.squared
q <- 6-1 #k-1
p <- 7
n <- nrow(wages)
f <-  ((R_ur-R_r)/q)/((1-R_ur)/(n-p))
summary(model2)
summary(model1)
f
#58.40277, anova and waltest=58.403
