#set working directory
getwd()
setwd("C:/Statsmodel/Class 02")

#import the Tesla File
library(readxl)
Tesla <- read_excel("TSLA.xlsx")

str(Tesla)
?str

#check it 
hist(Tesla$TESLA)
#we can change the frequeny to relative frequencies
hist(Tesla$TESLA, freq = FALSE)

#find the parameters of normal distribution
#mean(expected value) and the standard deviation
mu <- mean(Tesla$TESLA)
sigma <- sd(Tesla$TESLA)

#calculate probabilities
#use the density function
#16 dollars daily gain
dnorm(16, mean=mu, sd=sigma)

#intergral of the density function
#daily loss is greater than 50 dollars
#P(Yi<x)
#P(Yi<-50)
pnorm(-50, mean=mu, sd=sigma)

#daily gain is greater than 20 dollars
1-pnorm(20, mean=mu, sd=sigma)
 
#daily gain is between 2 and 52 dollars
pnorm(52, mean=mu, sd=sigma) - pnorm(2, mean = mu, sd=sigma)

#compare the practice and theory
pnorm(52, mean=mu, sd=sigma) - pnorm(2, mean = mu, sd=sigma)
#probability is almost 47%

#check it from the sample that we have
#try to calculate the relative frequency of the change being
#between 2 and 52
Tesla_filter <- Tesla[Tesla$TESLA>2&Tesla$TESLA<52,]
nrow(Tesla_filter)/nrow(Tesla)

#theoretical value is greater than the relative frequency
#check the reason

library(ggplot2)
ggplot(Tesla, aes(x=TESLA))+
  geom_histogram(aes(y=after_stat(density)))+theme_minimal()
#it seens the kurtosis is source of the difference of the probability
ggplot(Tesla, aes(x=TESLA))+
  geom_histogram(aes(y=after_stat(density)))+theme_minimal()+
  stat_function(fun=dnorm,
                args = list(mean=mu, sd=sigma),
                col="red")

#standard normal distribution
#standardize the TESLA variable
Tesla$z <- (Tesla$TESLA - mean(Tesla$TESLA))/sd(Tesla$TESLA)
mean(Tesla$z)
hist(Tesla$z)
#sigma rules (1,2,3)
#standard deviation is 1
#probability between -1 and +1
pnorm(1)-pnorm(-1) #one sigma rule 68.3%
pnorm(2)-pnorm(-2) #95.5%
pnorm(3)-pnorm(-3) #99.7%

#Task a(probably will come out on midterm)
pnorm(3, mean=3.6, sd=0.9)
1 - pnorm(4.2, mean=3.6, sd=0.9)

#task b
pnorm(4.5, mean=3.6, sd=0.9)-pnorm(2.7, mean=3.6, sd=0.9)


#task c
1 - pnorm(5, mean=3.6, sd=0.9)

#task d first parameter is quantile 0.95
qnorm(0.95, mean=3.6, sd=0.9)

#--------------------------------------------------------------------------

#exponential distribution
Surv <- read_excel("CancerSurvival.xlsx")
hist(Surv$SurvMonth)
library(psych)
describe(Surv$SurvMonth)

#what is the probability that after chemotherapy the given person
#will survive exactly one year
dexp(12, rate=1/mean(Surv$SurvMonth))

#what is the probability that after chemotherapy the given person
#will survive exactly more than 2 year
1 - pexp(24, rate = 1/mean(Surv$SurvMonth))

#what is the probability that after chemotherapy the given person
#will survive exactly more than 2 year but less than 3 years
pexp(36, rate = 1/mean(Surv$SurvMonth)) - pexp(24, rate = 1/mean(Surv$SurvMonth))

#what is the time at which the given person will not survive with 99%
qexp(0.99, rate = 1/mean(Surv$SurvMonth))

#the reason for that high number could be presence of outliers
ggplot(Surv, aes(x=SurvMonth))+
  geom_histogram(aes(y=after_stat(density)))+
  stat_function(fun=dexp,
                args = list(rate=1/mean(Surv$SurvMonth)), 
                col="red")
