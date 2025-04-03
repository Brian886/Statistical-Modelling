setwd("C:/Statsmodel/Class 07")

#2 sample test
##check another example
library(readxl)
insurance <- read_excel("insurancefee.xlsx")
insurance$sex <- as.factor(insurance$sex)
insurance$smoker <- as.factor(insurance$smoker)

#Statement: there is no difference between male and female
#in case of the average insurance charge
#male - female
#Statment: mean(male)-mean(female) = 0
#H0: mean(male)-mean(female) = 0
#H1: != (two.sided)
table(insurance$sex)
#these are large sample - is an asymptotic z-test

mean_m <- mean(insurance$charges[insurance$sex == "male"])
mean_f <- mean(insurance$charges[insurance$sex == "female"])
by(insurance$charges, insurance$sex, mean)

var_m <- var(insurance$charges[insurance$sex == "male"])
var_f <- var(insurance$charges[insurance$sex == "female"])

n_m <- table(insurance$sex)[2]
n_f <- table(insurance$sex)[1]

z_test <- (mean_m-mean_f)/sqrt(var_m/n_m + var_f/n_f)
z_test

#calculate the p-value
p_value <- 2*(1-pnorm(z_test))
p_value
#if alpha is 5% we reject H0, the average insurance fee
#is different for male and female
#no delta here cause delta = 0