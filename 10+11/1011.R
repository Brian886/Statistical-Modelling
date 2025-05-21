setwd("C:/Stats/ExtraPractice/10+11")
library(readxl)
data <- read.csv("unemployed.csv")
str(data)
############################################################
model1 <- lm(UCOMP ~ . , data=data)
summary(model1)
#1)Report the estimated coefficient of the MARRIED variable, and interpret its coefficient
#If the status of the housefold is married, the amount of unemployment compensation received is expected to reduce
#by 769 USD while other variables remains the same, ceteris paribus. 
#If we take 2 household who are identical to every predictor in the model, the one who is married is 
#expected to receive 770 USD less for unemployment compensation than the one who is unmarried. 

#2) Identify the variable with the strongest and weakest multicollinearity. For these 
#variables, specify what percentage of their variance cannot be explained by the other 
#explanatory variables.
library(car)
v<-vif(model1)
v
#Married has the strongest multicollinearity as it has the biggest VIF,
#biggest VIF = squared standard error is inflated by multicollinearity the most
#while EDUC has the weakest multicollinearity.with the smallest VIF
#lowest VIF = squared standard error is inflated by multicollinearity the least

1-1/v[c("MARRIED", "EDUC")]
#59.39% of the variability of Married is explained by other variables
#6.67% of the variability of EDUC is  explained by the other variables

#3) 
#List the non-significant variables at the α = 1% significance level. What kind of 
#statistical measure was applied to identify these variables and why?
summary(model1)
#the non-significant are SPOUSEY, and WHITE at 1% significance level. 
#we used the partial t test. H0: The effect of the variable has no significant effect on UCOMP at the population.
#the p value for both are 50% and 1.5%. which is greater than alpha. So we fail to rejust H0. 

#4)  Using at least two different methods, examine whether the non-significant variables
#identified in task 3 can be jointly excluded from the model.
#I think need to compare the models so wald-test, adjusted r, lm test....
model2 <- lm(UCOMP ~ . -SPOUSEY-WHITE, data=data)
summary(model2)
#wald test
#H0: R^2 unristricted = R^2 ristricted
#H1: R^2 unristricted != R^2 ristricted
n <- nrow(data)
q <- 2
p <- 8+1

Ru <- summary(model1)$r.squared
Rr <- summary(model2)$r.squared

f_test <- ((Ru-Rr)/q)/((1-Ru)/(n-p))

p <- 1-pf(f_test,q, n-p)
p
#At 1% significance level, we would fail to reject H0. 
#This means that the the explanatory power of adding SPOUSEY and WHITE would not be significant in the population. 

#spent too much time, have easier methods
#ICS
AIC(model1, model2) #prefer unrestricted
BIC(model1, model2) #prefer restricted
#The IC that is more lenient with additional predictor (Akaike) prefers the original model, while the Bayesian IC
# that is more strict on new variables prefers the restricted model. It's a draw on this front.

#wald but auto way use anova
anova(model1, model2)
# p-value = 4.7% mixed case, a bit closer to H1 (prefering the restricted model)

#Wald-test is also in the inconlusive range (1%-10%) with its p-value, but slightly closer to 1%, 
#so slightly preferring the restricted model,
# So, the decision is based on the modeller's preference on the number of predictors in the model.

#5) . Test the hypothesis that the sum of the coefficients of the MALE and MARRIED 
#variables is equal to zero at the α = 5% level. Clearly state the null (H₀) and alternative 
#hypotheses (H₁), provide the value of the test statistic, the p-value, and the decision.
#Explain the economic interpretation of the hypothesis test result.

#H0: B(male)+B(married) = 0
#H1: B(male)+B(married) != 0

#1) sum of 2 coefficient
sum_of_coeff <- sum(model2$coefficients[c("MALE", "MARRIED")])
sum_of_coeff
model2$coefficients[c("MALE", "MARRIED")]
model2

#2) Get the variance and covariance of these coefficients
sum_of_coeffs = sum(model1$coefficients[c("MALE", "MARRIED")])

covariance_of_coeffs <- vcov(model1)
squared_SE_MALE <- covariance_of_coeffs["MALE", "MALE"]
squared_SE_MARRIED <- covariance_of_coeffs["MARRIED", "MARRIED"]
cov_MALE_MARRIED <- covariance_of_coeffs["MALE", "MARRIED"]


#stanard error of the sum
SE_for_the_sum <- sqrt(squared_SE_MALE + squared_SE_MARRIED + 2*cov_MALE_MARRIED)

#test statistic
test_stat_for_the_sum <- (sum_of_coeffs-0)/SE_for_the_sum

#degree of freedom
df_for_t_distribution <- model1$df.residual

#p_value
p_value <- 2*pt(-abs(test_stat_for_the_sum), df = df_for_t_distribution)
p_value
# p-value = 88% --> >5% --> H0 can't be rejected, the sum of the two coefficients can be considered as 0 in the population
# this means that the marginal effects of these two variables on unemployment compensation cancel each other out:
# --> the amount of expected compensation for males disappears if there is also a wife in the picture
# --> probably the wife usually has a job, so the amount of compensation is reduced since the male doesn't need to
#     finance the whole household

#6) Extend the model by including the square of the UHOURS variable, and the interaction 
#between UHOURS and MALE. In this new model, determine the marginal effect of the 
#number of hours the household head spends unemployed on unemployment 
#compensation for a female household head with an average number of unemployed 
#hours. Interpret the result.

library(ggplot2)
ggplot(data = data, aes(x=UHOURS, y= UCOMP))+
  geom_point()+
  geom_smooth(method = "lm")

ggplot(data = data, aes(x=UHOURS^2, y= UCOMP))+
  geom_point()+
  geom_smooth(method = "lm")

hist(data$UHOURS)
hist(data$UHOURS^2)

model3 <- lm(data=data, UCOMP~.+I(UHOURS^2)+UHOURS*MALE)
summary(model3)
#marginal effect in multiregression model
#so need to do partial derivative
#d_pred_ucomp/d_uhours = B(uhours)+2*B(uhours^2)+*UHOURS+B(uhours:male)*MALE
#= 3.299e+00 + 2*-4.062e-04*UHOURS + 1.292e-01*MALE

# marg effect for a female household head with an average number of unemployed hours
avr_uhours <-mean(data$UHOURS)

3.299e+00 + 2*-4.062e-04*avr_uhours + 1.292e-01*0
#if hour female household spent an additional hour unemployed compared to the average,
#while all other parameters remain the same, the household can expect an additional 2.6USD compensation. 

#7) Examine with the appropriate statistical test and with graphical tools whether the 
#extension of the model in task 6 is justified!
#ramsey reset since it involve transformation and interaction

#H0: The specification of the model is correct
#H1: The specificaiton of the model is incorrect

library(lmtest)
resettest(model1)#p_value = 0.0827
resettest(model3)#p_value = 0.002261
#we reject H0 at all common significance level.
#the specification of the model that has added non-linear variables is incorrect.

ggplot(data, aes(x=UHOURS, y=UCOMP)) + geom_point() + geom_smooth(method=lm) + geom_smooth(color="red")
#the fitted non-linear trendline does not seem to deviate much from the linear,
#suggesting that it is not needed to add UHOURS^2 to the model.

ggplot(data, aes(x=UHOURS, y=UCOMP, color=as.factor(MALE))) + geom_point() + geom_smooth(method=lm)
#There is a slight difference in the slope for male and female, 
#the confidence intervals do not only intersect in a region where we have a very few female observations, 
#suggesting that adding the interaction term is quesitonable

##########################################################
#statistics_scores.xlsx
#########################################################
setwd("C:/Stats/ExtraPractice/10+11")
library(readxl)
data <- read_excel("statistics_scores.xlsx")
model1 <- lm(Score~ PrevGrade+ D_Always+D_Mostly+D_Sometimes+PrevGrade*D_Always+PrevGrade*D_Mostly+PrevGrade*D_Sometimes, data=data)
summary(model1)

#1)  For two students who mostly attend seminars, how much higher is the expected midterm 
#score for the student whose Statistics I grade is higher by one?
model1$coefficients["PrevGrade"]+model1$coefficients["PrevGrade:D_Mostly"]
#by + 0.37

#2) Interpret the interaction coefficient between PrevGrade and D_Sometimes
model1$coefficients["PrevGrade:D_Sometimes"]
#The additional benefit in third Stat 2 midterm score of 1 extra point in stat 1 score for someone who 
#attends class sometimes compared to never is expected to get 0.45 lower marks

#3)  Test whether the interaction terms are jointly significant at the α = 1% level. State the 
#null (H₀) and alternative hypotheses (H₁). Provide the value of the test statistic, the pvalue, and your conclusion
#reset
#ANOVA test
#H0: The R^2 of the model without interaction is not significantly lower compared to model with interaction
#H1: The R^2 of the model without interaction is significantly lower compared to model with interaction

model2 <- lm(Score~ PrevGrade+ D_Always+D_Mostly+D_Sometimes, data=data)
anova(model1, model2)
#test-stat: 0.06133. 6.1%
#Fail to reject H0. The interaction terms are not jointly significant in the population. 
#model without interaction is better

#4)In the reduced model (from task 3), test whether the D_Sometimes variable is significant 
#at the α = 5% level. What does the result imply? Is it worthwhile to attend seminars only 
#sometimes in terms of the midterm exam score?

summary(model2)
#Ho: The D_Sometimes is not significant in the population
#H1: The D_Sometimes is significant in the population
#p-value: 59.41%. Fail to reject H0. D_Sometimes is not significant in the population on midterm score.
#We can drop it in our model. 
#Therefore, it is not worth to attend seminars
# sometimes instead of never (the reference category). It is better to always stay at home,
# You can expect no additional points in the midterm by attending this rarely to seminars.

#5) Use the simpler version of the White test to examine the presence of heteroskedasticity 
#in the original (full) model at the α = 5% level. If heteroskedasticity is detected, address 
#it using the Generalized Least Squares (GLS) method. If heteroskedasticity is not 
#detected, retain the original model.
data$sq_errors <- model2$residuals^2
library(ggplot2)
ggplot(data=data, aes(x=Score, y=sq_errors))+geom_point()
#H0: residuals are homoskedastic
#H1: residuals are heteroskedastic
library(skedastic)
white(model1, interactions=F)
#p-value is 94.3%
#the residuals are homoskedastic. 

#6)What alternative method can be used to address heteroskedasticity? How do the results 
#differ compared to the GLS method?
 
# An alternative is White's robust standard errors. This method does not re-estimate the whole model, it just corrects
# the coefficient's standard error formula so that the test statistics calculated from them follow a t-distribution
# under H0, resulting in valid p-values for the partial t-tests of coefficients.

# GLS is an alternative estimation to OLS that uses the squared prediction errors of the original OLS estimation as
# weights in a second round of OLS estimations. Resulting in new coefficient estimates and producing a new model
# where the error term is now homoskedastic due to the weighting with the original squared errors.