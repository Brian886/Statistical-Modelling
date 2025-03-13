setwd("C:/Statsmodel/Class 04")

library(readxl)
cars <- read_excel("usedcars.xlsx")

colnames(cars) <- c("Number", "Fuel", "HP", "Mileage", "Trunk", "Price", "Age")

#check the descriptive statistics for the Price variable
psych::describe(cars$Price) #is the s, the sample sd, sigma is population sd

#estimate the 95% confidence interval of the price 
meanprice <- mean(cars$Price)
#we have no info about the sd of the population
#so we estimate the corrected sample sd 
sdprice <- sd(cars$Price)

#calculate the uncertainty multiplier
#sample size is large, we have asymptotic case(large sample) here
#so we use standard normal distribution
#laci use k
# 1 - alpha=0.95
aplha <- 0.05
z <- qnorm(1-aplha/2)
se <- sdprice/sqrt(nrow(cars))
se

#margin of error
delta <- z*se
lower <- meanprice-delta
upper <- meanprice+delta
c(lower, upper)
#in case of resampling, in 95 cases out of 100, this interval covers
#the mean price of the used cars in Hungary (in the population)

#check functions to calculate confidence interval
install.packages("Hmisc")
library(Hmisc)

smean.cl.normal(cars$Price)
c(lower,upper)
#hmmmm, these are not the same. because of diff in distribution

#in English literature they usually use the student t distribution
#calculate the confidence interval by Student t distribution
n <- nrow(cars)
t <- qt(0.975, df =  n-1)

delta_t <- t*se
lower_t <- meanprice - delta_t
upper_t <- meanprice + delta_t
c(lower, upper)
c(lower_t, upper_t)
smean.cl.normal(cars$Price)#it uses t distribution
#if n -> to infinite, then the Student t distribution converges to the standard normal distribution

#check this fact
qnorm(0.975) #z=1.96
qt(0.975, df=100-1) #t=1.98
#n=5000
qnorm(0.975) #1.96
qt(0.975, df=1000-1)#1.962
#for 1000 elements they are almost the same
#n=30
qnorm(0.975) #1.96
qt(0.975, df = 30-1) #2.05
#small samples the difference is bigger

#calculate the 98% confidence level of the mileage variable
mean_mileage <- mean(cars$Mileage)
sd_mileage <- sd(cars$Mileage)
z_mileage <- qnorm(0.99)
delta_mileage <- z_mileage*sd_mileage/sqrt(n)
lower_mileage <- mean_mileage-delta_mileage
upper_mileage <- mean_mileage+delta_mileage
c(lower_mileage, upper_mileage)

#How large sample do we need if we would like to get the same 
#margin of error but on 90% confidence level?
z_new <- qnorm(0.95) #=1-0.1/2
n_new <- (z_new*sd_mileage/delta_mileage)^2
n_new

#calculate the sample with 98% confidence level, but we reduce
#the margin of error by 50% 
n_halferror <- (z_mileage*sd_mileage/(delta_mileage/2))^2
n_halferror
n_halferror/n
#4 times higher

#####confidence interval for proportion
#estimate the ratio of cars in population which are older than 15 years
#create a variable for that 
cars$oldcar <- ifelse(cars$Age>15, 1, 0)
table(cars$oldcar)[2]

prop <- table(cars$oldcar)[2]/n
prop 
#confidence level 99% 
z_prop <- qnorm(0.995)
se_prop <- sqrt(prop*(1-prop)/n)
delta_prop <- z_prop*se_prop
lower_prop <- prop -delta_prop
upper_prop <- prop+delta_prop
c(lower_prop, upper_prop)
#in case of resampling in 99 cases out of 100 on average, this interval 
#covers the ratio of the old cars (older than 15 years) in the Hungarian
#used car market
table(cars$Fuel)
#estimate the proportion of diesel fuel type cars
#alpha = 0.02
p_diesel <- table(cars$Fuel)["Dízel"]/n
p_diesel

z_diesel <- qnorm(0.99)
se_diesel <- sqrt(p_diesel*(1-p_diesel)/n)
delta_diesel <- z_diesel*se_diesel
c(p_diesel-delta_diesel, p_diesel+delta_diesel)

#try to estimate confidence interval for the mean price of different fuel
#confidence level 95%(1-alpha)
#significance level 5% (alpha)
#smean.cl.normal function - we accept that, it uses t distribution
by(cars$Price, cars$Fuel, smean.cl.normal)
#The mean prices of benzin and diesel type are the same statistically
#the mean price of hybrid cars is significantly higher than the other two



#Practice task 7.15
#Data
n <- 100
mean_weight <- 20
sd_weight <- 1.78
alpha <- 0.05

#95% confidence interval for the mean weight of children
#there is no information about the distribution in the population
#Do we have a large sample or small sample?
#it is exactly at the edge of small and large samples
#use t distibution here, cause higher probability extreme value can occur, so wider confidence interval
#so lower mistake
#to make the less harmless decision, we chose student t distribution
t_mult <- qt(1-alpha/2, df=n-1)
se <- sd_weight/sqrt(n)
delta <- t_mult*se
c(mean_weight-delta, mean_weight+delta)

#proportion of children who are heavier than 21 kg
k <- 21
p <- k/n

#to estimate the proportion, we use standard normal distribution
z <- qnorm(1-alpha/2)
delta_p <- z*sqrt(p*(1-p)/n)
c(p-delta_p, p+delta_p)

#The standard deviation of children’s body weight.
# to estimate the proportion, we use standard normal distribution

z<- qnorm( 1-alpha/2)
delta_p <- z*sqrt(p*(1-p)/n)
c(p-delta_p, p+ delta_p)

#The standard divination of children body weight
chi_1<-(qchisq(1-alpha/2, df = n-1))


chi_2<-(qchisq(alpha/2, df = n-1))


Chi_s_lower<- ((n-1)*sd_weight^2)/(chi_1)
Chi_s_upper<- ((n-1)*sd_weight^2)/(chi_2)

sqrt(Chi_s_lower)                                   
sqrt(Chi_s_upper)
