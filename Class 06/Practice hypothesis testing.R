setwd("C:/Stats/Class 05")

#import the household income file
library(readxl)
HH <- read_excel('Households_Income.xlsx')

str(HH)

####Hypotheses testing####
#Test the following statement at 5% significance level
#the annual average income of the a household in Hungary is 5m HUF 
#mu=5000
#H0: mu=5000
#H1: mu!=5000
#two-sided test

mean <- mean(HH$Income)
n <- nrow(HH)
sd <- sd(HH$Income)
#Ho null hypothesis: The average household income in Hungary is 5 million HUF
#H1 alternative hypothesis: The average household income in Hugary is significantly smaller or greater than 5 million HUF
#H1 mu < 5000 or > 5000

#what should the test function be?
#we can test the mean as it the means of samples would follow a normal distribution
#it could be z-test, t-test and as. z-test
#but since we have a very large sample, it will be a asymtotic case
H0_mu <- 5000 #so this is test statistic
z_test <- (mean - H0_mu)/(sd/sqrt(n)) #and this is test function? hmmmm

#step 3 find critical value and pvalue
#critical value first
alpha <- 0.05
c_u <- qnorm(1-alpha/2,  mean = 0, sd= 1)
c_l <- qnorm(alpha/2)
#got two cause is 2 sided
#lets see the result
c(z_test, c_l)
#The vlaue of the test statistic, here is z score is way lower than the critical value. -11 compare to -1.96. 
#Hence I can say that the mean of the house holder income of 5million is rejected. 

##in R,  we find t test for one_sample mean test
t.test(HH$Income, mu=H0_mu, alternative="two.sided")
?t.test()
#we can see that the t test is the same as our calculation, so we are right
#so it is clear based on the confidence interval of 95%
#that the mean will not be 5million
#and the p value is almost zero, so rejecting it wont cause error

#manually find p value
pnorm(z_test) #alpha/2
pvalue <- 2*pnorm(z_test) #need to multiply by 2 cause this is two sided
pvalue 
#conclusion is that we reject the mean for being 5 million HUF


#Task 2
#Statement : the average household income is at least 4.5 million 
#mu >= 4.5 (da yu deng yu)
#H0: house hold income mean  = is 4.5 million 
#H1: house hold income mean is less than <4.5
#now it is a one sided test

#step 2 test statistics
#I am gonna test the mean
#need to know the distribution
alpha <- 0.05
mean_H0 <- 4500
z_test <- (HHmean -mean_H0)/(sd/sqrt(n)) 

c <- qnorm(alpha)

#let me try to find p value
p_value <- pnorm(z_test)
p_value
#p_value is very high, almost close to 0, there might be a huge mistake somewhere

#this is a type 2 error. It means that we accept a false H0.
#4.5 million is below the lower bound of the 95% confidence interval
#even though we accept the H0 mean is not 4.5
z <- qnorm(1-alpha/2)
sd <- sd(HH$Income)

se <- sd/sqrt(n) * sqrt(1-n/N)

delta <- z*se

upper <- HHmean + delta
lower <- HHmean - delta
c(lower, upper)
#the critical value is at -1.64, and the z test score is 4.72. The z test is within the distribution 
#Hence we fail to reject H0. We fail to reject that the mean is 4.5 million
#hence the mean annual household income is 4.5 million
#this statement is not appropriate to test
Hmisc::smean.cl.normal(HH$Income)
