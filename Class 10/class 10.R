setwd("C:/Statsmodel/Class 10")
#alpha is 5% here, 95% significance level

#import wagel file
library(readxl)
wages <- read_excel("wage1.xls") #1970s usa

#structure
str(wages)
#nonwhite, female, married, are qualitative variable
#we treat, these variables as factors
wages[,5:7] <- lapply(wages[,5:7], as.factor)
wages[,5:7] 

#First model 
#wage and experience
model1 <- lm(wage~exper, data = wages)
summary(model1)
#wage = B0+B1*exper+e
#wage_hat = 5.3733+0.03*exper(estimated y, so we won't add error)
#B0: experience=0 if there is no experience of the employee, the expected
#hourly earning is 5.37 dollars 
#B1: 0.03 plus one year in experience results 0.03 dollar increase in the 
#hourly earnings on average

#R-squared
summary(model1)$r.squared
#explanatory power of the model is 1.3%, quite low

#test the parameter of experience separately
#separate --> partial t test
#H0: B(exper) = 0 # (not significant)
#wage = B0+B1*exper+e, so 0*exper
#H1: B(exper) !=0 (significant)
install.packages("lmtest")
library(lmtest)
coeftest(model1)
#t test for the parameters
t_test <- coeftest(model1)[2,1]/coeftest(model1)[2,2]
t_test


#the relevancy of the model (test of the whole model)
#Global F test
#it is in the output
summary(model1)
#F-test: 6.766
#p-value: 0.009555
#H0: the model is not relevant
#H1: the model is relevant 
#we reject the H0, the model is relevent, there is at least
#one significant variable

#in case of binary regression, there is a connection between t and F test
#F = t^2
coeftest(model1)[2,3]^2

#we can estimate confidence interval for the Beta parameters
confint(model1)
#the interval of experience does not contain the 0 value
#so the experience variable is significant 

#multiple regression model
model2 <- lm(wage~educ+exper+tenure+nonwhite+female+married, data=wages)
summary(model2) #use t test for significance decision, p-value(for educ high t test, almost 0 pvalue)
#Acc to the t test which are the significant variables?
#alpha = 0.05
#for the experience, the p>alpha, we fail to reject H0, it is not significant 
#could the tenure variable contain the information of experience?
cor(wages$exper, wages$tenure)
#it is just 0.5, it is not that high
#this is not the reason why the experience is not significant
#nonwhite is not significance - it is not a good variable
#female dummy is significant 
#hourly average earning of a female is less by 1.74 dollars compared to
#a male worker on average if other variable is ceteris paribus
#married is not significant 

#compare model1 and model2
#adjusted r-squared
summary(model1)$adj.r.squared
summary(model2)$adj.r.squared
#in the second model, the adjusted R-squared is higher, the second model is better
#calculate the adjusted R-squared in the second model
adjR_m2 <- 1-(1-summary(model2)$r.squared)*((nrow(wages)-1)/(nrow(wages)-7)) #p = num of B variable(explainatory variable) + the intercept which is 1

#Akaike
AIC(model1)
AIC(model2)
#AIC based on SSR(residuals), so we would like to minimize it
#model2 is better

#Schwarz
BIC(model1)
BIC(model2)
#model2 is better

install.packages("broom")
broom::glance(model1)
broom::glance(model2)

#Test the joint significance of new variables by Wald F test
waldtest(model1, model2, test="F")
anova(model1, model2)
#H0: B(educ)=B(tenure)=B(nonwhite)=B(female)=B(married)=0
#H1: at least one of these Beta is not 0
#F: 58.4
#p-value is almost 0 
#we reject H0, at least one of these variables has significant effect on wages

#calculate it manually
R_ur <- summary(model2)$r.squared
R_r <- summary(model1)$r.squared
q <- 5 #no need to add one here
n_pur <- nrow(wages)-7 #6 explanatory+intercept
F_test <-(R_ur-R_r)/q/((1-R_ur)/n_pur)
F_test

#LM Chi2-test
waldtest(model1, model2, test = "Chi")

#Extra point homework:
#Calculate the Chi^2 test and the p-value of the test manually

#What is the reason, why the experience is not significant?
library(ggplot2)
ggplot(wages, aes(y=wage, x=exper))+geom_point()+
  geom_smooth(method="lm")+theme_minimal() #linear
#the regression line does not fit on the data
ggplot(wages, aes(y=wage, x=exper))+geom_point()+
  geom_smooth(method="loess")+theme_minimal()
#it is more like quadratic than linear
#it is the reason why the experience is not significant in the model

model3 <- lm(wage~educ+exper+tenure+nonwhite+female+married+exper*female+I(exper^2), data=wages) 
#use I() in lm function to be able to use square ^2
summary(model3)
#by changing the specification of the model. changing the functional form
#the experience variable became significant
#check the marginal effect of experience
#partial derivative
#0.236-2*0.004*experience-0.057*female
#the marginal effect of experience depends ont the sex and the level of experience

#you can calculate for given cases by inbuilt function
install.packages("margins")
library(margins)
#we calculate it fo the first 6 observations of the dataset
head(dydx(wages, model3, "exper"))
