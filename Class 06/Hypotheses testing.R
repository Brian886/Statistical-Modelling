setwd("C:/Statsmodel/Class 06")

#import the file
library(readxl)
income <- read_excel("Households_Income.xlsx")

####Hypotheses testing####
#Test the following statement at 5% significance level
#the annual average income of the a household in Hungary is 5m HUF 
#mu=5000
#H0: mu=5000
#H1: mu!=5000
#two-sided test
n <- nrow(income)
sm <- mean(income$Income)
sd_inc <- sd(income$Income)
#what should the test function be?
#parametric test, for the mean
#it could be z-test, t-test and as. z-test
#we have a large sample - asymptotoc case
mu0 <- 5000
z_test <- (sm-mu0)/(sd_inc/sqrt(n))
z_test
#try to find the critical values
#we have 2 critical values a lower and upper one
#because it is 2-sided
alpha <- 0.05
c_l <- qnorm(alpha/2)
c_u <- qnorm(1-alpha/2)
c_u
z_test
#the value of the test is lower than the lower critical value, in this way
#it is in the critical range, so we reject H0, it means that the true value
#of the avarage household income is not 5 million HUF

#in R,  we find t test for one_sample mean test
t.test(income$Income, mu=mu0, alternative = "two.sided")
#the test function is exactly the same as our calculation, and it is right!
#it gives 95% confidence interval of the mean
#and it is clear from that interval that the expected value
#could not be 5000
#p-value is almost 0
#we reject H0, the average income is not 5 million 
pnorm(z_test) #half of the p_value, because it is a 2 sided test
p_value <- 2*pnorm(z_test)
p_value

#Statement : the average household income is at least 4.5 million 
#mu >= 4.5 (da yu deng yu)
#H0: mu<4.5
#H0T: mu=4.5
#one-sided test, left-tailed
#here I use 95% significance
mu0 <- 4500
z_test2 <- (sm-mu0)/(sd_inc/sqrt(n))
z_test2

#lower critical value
c_lower <- qnorm(alpha)
c_lower 

#test value is greater than the critical value, it is in the acceptance and 
#we fail to reject the technical H0, we know nothing about H0 and the greater part
#so it means that, according to this test the average annual household
#income is 4.5 million HUF

#we feel that we have a problem
#This statement is not appropriate to test 
Hmisc::smean.cl.normal(income$Income)
#95% confidence interval does not contain the 4.5 million value
#so the mean could not be 4.5 million
#But we accept it in the hypothesis testing, so it seems that our
#decision is Type II error 
#because the statement was wrong. (because 4.5m, 4500 is not in the interval(lower bound is 4587))

#just for fun we calculate the p-value
p_value2 <- pnorm(z_test2)
p_value2
#if p value is almost 1, it is a sign there might be a problem 
t.test(income$Income, mu=4500, alternative = "less")
#it says the same 

#right state,emt to test it: the average household income is max 4.5 million HUF
#mu<=4500
#H0: mu <=4500
#H0T: mu=4500
#H1: mu>4500
#one-sided, right-tailed
z_test3 <- (sm-4500)/((sd_inc/sqrt(n))))

#critical value - upper
c_upper <- qnorm(1-alpha)
c_upper 

#the test value is greater than the critical value, so we reject H0 and 
#the technical H0,  in this way the average annual income of a household
#is greater than 4.5 million HUF

#p-value
1-pnorm(z_test3)
pnorm(z_test3, lower.tail = FALSE)
#large sample here, so doesnt matter if we use t distribution or standard normal

t.test(income$Income, mu = 4500, alternative ="greater")
