setwd("C:/Statsmodel/Class 11")
#import the data
library(readxl)
wage <- read_excel("wage1.xls")
wage[, 5:7] <- lapply(wage[, 5:7], as.factor)
#----------------------------------------------------------------------
#first model
model1 <- lm(wage~educ+exper+tenure+nonwhite+female+married, data=wage)
summary(model1)
#reference category is the 0
#the average of female worker is -1.7 hourly wage compared to male worker, ceteris paribus 
#on average married worker earns 0.5 more compared to unmarried workers, ceteris paribus

#we try to modify the functional form
#we would like to use the interaction between
#experience and female variables
#we would like to use the quadratic term of experience
#everything from model 1 + interation(female*exper) + quadratic term(exper^2)
model2 <- lm(wage~educ+exper+tenure+nonwhite+female+married+female*exper+I(exper^2), data=wage)
summary(model2)
#all the variables in connection with experience became significant

#How can we determine the effect of plus one year experience?
#marginal effect - partial derivative acc to experience variable
#calculate the effect of plus 1 year experience
#for a a male worker with 10 years of experience
betas <- model2$coefficients
betas
betas[3]+2*betas[8]*10+betas[9]*0
betas[3]+2*betas[8]*10+betas[9]*1 #for female worker with 10 years of experience 

#check the histogram of dependent variable
hist(wage$wage)
#the wage is right-skewed
#maybe it is lognormal, so if we take the log of it, it could become symmetrical
hist(log(wage$wage))

#build a model for the logarithm of the wage
#logarithm means natural log (ln)
model3 <- lm(log(wage)~educ+exper+tenure+nonwhite+female+married+female*exper+I(exper^2), data=wage)
summary(model3)
#interpret the effect of education
exp(model3$coefficients)
#one plus year in education results 8.5% increase in the hourly earning on average ceteris paribus
#marginal effect of experience: e^(partial derivative)

