#import the data
library(readxl)
wages <- read_excel("wage1.xls")
wages[, 5:7] <- lapply(wages[, 5:7], as.factor)
#-------------------------------------------------------------------------------
#first model
model1 <- lm(wage~educ+exper+tenure+nonwhite+female+married, data=wage)
summary(model1)
est_y <- model1$fitted.values
errorsq_ml <- model1$residuals^2
plot(est_y, errorsq_ml)
#the error is growing

#gonna use Koenker test for the first model#robust version of BP test
library(lmtest)
bptest(model1, studentize = TRUE)
#H0: homoskedasticity, error term is constant
#H1: heteroskedasticity, error term is not constant
#Reject H0. The error term is heteroskedasticity
#estimation is not BLUE
#cant use t test and f test in the regression
#we dont know which explanatory variable is significant in the regression model

#not do white test
install.packages("skedastic")

#H0: error term is homoskedasticity
#H1: error term is heteroskedasticity
skedastic::white(model1, interactions = T)
#p value is almost 0, we reject H0. The error term of model1 is heteroskedasticity

#we did a lot of transformation and use nonlinear variables
#could it solve the problem of heteroskedasticity?
#it is possible to solve the problem by changing the functional form
#we check it in model 3(log + interaction+quadratic term)
model3 <- lm(log(wage)~educ+exper+tenure+nonwhite+female+married+exper*female+I(exper^2), data=wage)
summary(model3)

#check error term on model3
#i am gonna use koenker test
#H0: error term is homoskedasticity
#H1: error term is heteroskedasticiy
library(lmtest)
bptest(model3, studentize = T)
#p-value is 4.4%.
#if alpha is 5%, we still reject 
#H0. But if alpha is 1, we fail to reject H0. 
#close to homoskedasticity

#to make it sure that we can use the model, we apply HCCM - robust stantard error
install.packages("car")
library(car)
coeftest(model3, vcov. = hccm(model3))
#compare the robust standard error model to the original one
install.packages("stargazer")
stargazer::stargazer(list(coeftest(model3), coeftest(model3, vcov. = hccm(model3))), type="text", digits=3)
