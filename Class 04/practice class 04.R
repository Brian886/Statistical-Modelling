getwd()
setwd("C:/Stats/Class 04")

#run the used cars data also rename the columns at the same time
library(readxl)
cars <- read_excel("usedcars.xlsx")
colnames(cars) <- c("Number", "Fuel", "HP", "Mileage", "Trunk", "Price", "Age")

#check the descriptive statistics for the Price variable
install.packages("psych")
psych::describe(cars$Price)

#check the cars data
str(cars)

#estimate the 95% confidence interval of the price 
alpha <- 0.05
cars_m <- mean(cars$Price)
S <- sd(cars$Price)
z <- qnorm(1-alpha/2, mean = cars_m, sd = S)
delta <- z*(S/sqrt(nrow(cars))) #margin of error

lower_bound <- cars_m - delta
upper_bound <- cars_m + delta

#The confidence interval is 
c(lower_bound, upper_bound)
#in case of resampling, in 95 cases out of 100 is within this interval covers
#the mean price of the used cars in Hungary (in the population) estimated

#check functions to calculate confidence interval with library
install.packages("Hmisc")
library(Hmisc)

?smean.cl.normal()

smean.cl.normal(cars$Price)
#hmmmm, these are not the same. because of diff in distribution(above is normal, below is t)

#in English literature they usually use the student t distribution
#calculate the confidence interval by Student t distribution
?qt
n <- nrow(cars)
t <- qt(0.975, df = n-1)

#Margin of error
S_mean <- S/sqrt(n)
delta_t <- t*S_mean

#confidence interval of 95% using t distribution
lower_t <- cars_m - delta_t
upper_t <- cars_m + delta_t
c(lower_t, upper_t)
smean.cl.normal(cars$Price) #compare with the function #it uses t distribution
#if n -> to infinite, then the Student t distribution converges to the standard normal distribution


#now check the fact that if n -> to infinity, student t will converge to standard normal distribution
#we can do this by adjusting the degree of freedom, n-1 here
z <- qnorm(0.975)
z
#now we see student t t score with n = 100
t <- qt(0.975, df=100-1)
t
#got difference one sia. okay now increase n to 200
t <- qt(0.975, df=200-1)
t
#still got diff now n=1000
t <- qt(0.975, df=1000-1)
t
#a bit of difference but close now increase to 5000
t <- qt(0.975, df=5000-1)
t
#wow now is supper close. imagine you increase it more

#calculate the 98% confidence level of the mileage variable I will use standard normal distribution here
alpha <- 0.02
S_mileage <- sd(cars$Mileage)
se <- S_mileage/sqrt(nrow(cars))
mean_mileage <- mean(cars$Mileage)
z_mileage <- qnorm(1-alpha/2) #no need put mean and sd value 
#margin error
delta_m <- z_mileage * se
delta_m

#calculate the 98% interval
lower_mileage <- mean_mileage - delta_m
upper_mileage <- mean_mileage + delta_m
c(lower_mileage, upper_mileage)

#How large sample do we need if we would like to get the same 
#margin of error but on 90% confidence level? delta = 5234.959 I'll look at the formula
alpha <- 0.1
z_mileage <- qnorm(1-alpha/2)

n_90 <- ((z_mileage*S_mileage)/delta_m)^2
n_90

#calculate the sample with 98% confidence level, but we reduce
#the margin of error by 50% 
#try not to assign alpha
z_mileage <- qnorm(0.99)
delta_m <- delta_m/2
n_98 <- ((z_mileage*S_mileage)/delta_m)^2
n_98
n_98/nrow(cars)
#to achieve 50% reduction of margin error, an increase of 4 times higher of population is needed compared to the ori sample size

#####confidence interval for proportion i use 99%
#estimate the ratio of cars in population which are older than 15 years
#create a variable for that
#i will create a new column for that too
cars$oldcar <- ifelse(cars$Age > 15, 1, 0)

p <- sum((cars$oldcar)/n)
alpha <- 0.01
z <- qnorm(1-alpha/2)
variance <- p*(1-p)/n
se <- sqrt(variance)

#margin of error
delta <- z * se

#now the confidence! vamos
lower_p <- p - delta
upper_p <- p + delta
c(lower_p, upper_p)
#in case of resampling in 99 cases out of 100 on average, this interval 
#covers the ratio of the old cars (older than 15 years) in the Hungarian
#used car market

#estimate the proportion of diesel fuel type cars
#alpha = 0.02
table(cars$Fuel)
alpha <- 0.02
diesel_cars <- table(cars$Fuel)[2]

diesel_p <- diesel_cars/n
diesel_p

z_diesel <- qnorm(1-alpha/2)
var_diesel <- diesel_p*(1-diesel_p)/n
se_diesel <- sqrt(var_diesel)
se_diesel

#convidence interval of 98%
lower_diesel <- diesel_p - z_diesel*se_diesel
upper_diesel <- diesel_p + z_diesel*se_diesel
c(lower_diesel, upper_diesel)

##try to estimate confidence interval for the mean price of different fuel
#confidence level 95%(1-alpha)
#significance level 5% (alpha)
#smean.cl.normal function - we accept that, it uses t distribution
#use by() function. powerful tool for applying a function to subsets of data
library(Hmisc)
?by

by(cars$Price, cars$Fuel, smean.cl.normal)
#The mean prices of benzin and diesel type are the same statistically
#the mean price of hybrid cars is significantly higher than the other two