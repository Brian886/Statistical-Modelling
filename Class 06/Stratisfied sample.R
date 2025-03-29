setwd("C:/Statsmodel/Class 06")

#import the file
library(readxl)
income <- read_excel("Households_Income.xlsx")
#sample method here uses simple random sample without replacement because we won't choose same household twice

#Population size
N <- 4111240 #num of households in Hungary
n <- nrow(income)

sample_mean <- mean(income$Income)
s <- sd(income$Income)

#confidence level
#95%
alpha <- 0.05 #aka significent level
z <- qnorm(1-alpha) #uses standard normal with mean at 0 and sd at 1 
se_inc <-s/sqrt(n)*sqrt(1-n/N)
se_inc
#in case of resampling, the mean of the sample income deviates +- 31.5214 from the population mean income

#confidence interval
c(sample_mean-z*se_inc, sample_mean+z*se_inc)
#this survey is representative for the settlements in Hungary
#it could be a stratified sample by the settlements
stat_table <- aggregate(Income~Settlement, data=income, FUN=mean)
stat_table$sd <- aggregate(Income~Settlement, data=income, FUN=sd)[,2]
stat_table$sample_size <- table(income$Settlement)
stat_table

#How can we calculate the population siye for the stratums
stat_table$population <- round(stat_table$sample_size/n*N, 0)
stat_table

#calculate the within variance and sd
within_var <- sum(stat_table$sd^2*(stat_table$sample_size-1))/n-1
within_var
within_sd <- sqrt(within_var)
within_sd
#the income of household defers by 2.8 million from the mean income of the it's settlement type of the given household
 

#standard error of the PS(proportionally stratisfied) sample
se_ps <- within_sd/sqrt(n)*sqrt(1-n/N)
se_ps
se_inc
#PS sample provides lower standard error compared to the SR without replacement
#confidence interval
weighted_mean <- sum(stat_table$sample_size*stat_table$Income)/n
weighted_mean
#it is the same as the sample_mean was
c(sample_mean-z*se_ps, sample_mean+z*se_ps) #PS sample
c(sample_mean-z*se_inc, sample_mean+z*se_inc)#simple random sample without replacement
