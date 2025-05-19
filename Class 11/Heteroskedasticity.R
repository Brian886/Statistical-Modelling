setwd("C:/Statsmodel/Class 11")
#import the data
library(readxl)
wage <- read_excel("wage1.xls")
wage[, 5:7] <- lapply(wage[, 5:7], as.factor)
#####------------------------------------------------------------------

#first model
model1 <- lm(wage~educ+exper+tenure+nonwhite+female+married, data=wage)
summary(model1)
est_y <- model1$fitted.values
errorsq_ml <- model1$residuals^2
plot(est_y, errorsq_ml)
#variance is increasing, so it seems heteroskedastic

#Koenker test for the first model
library(lmtest)
bptest(model1, studentize = TRUE)
#H0: homoskedasticity
#H1: heterskedasticity
#p_value is almost 0, so we reject H0, the error term is heteroskedastic
#so the estimation is not BLUE
#we can't use t and F tests in the regression, so we don't which variable
#is significant

install.packages("skedastic")
skedastic::white(model1, interactions = T)
#p-value is almost 0, we reject H0, the error term is heteroskedastic

#we did a lot of transformation and use nonlinear variables
#could it solve the problem of heteroskedasticity?
#it is possible to solve the problem by changing the functional form
#we check it in model 3(log + interaction+quadratic term)
model3 <- lm(log(wage)~educ+exper+tenure+nonwhite+female+married+female*exper+I(exper^2), data=wage)
summary(model3)

bptest(model3, studentize = T)
#P-value is 4.4%
#we are close to homoskedasticity

#to make it sure that we can use the model, we apply HCCM - robust stantard error
install.packages("car")
library(car)
coeftest(model3, vcov. = hccm(model3))
#compare the robust standard error model to the original one
install.packages("stargazer")
stargazer::stargazer(list(coeftest(model3), coeftest(model3, vcov. = hccm(model3))), type="text", digits=3)


