setwd("C:/Stats/Class 06")

#import the household file
library(readxl)
HH <- read_excel("Households_Income.xlsx")
#sample method here uses simple random sample without replacement because we won't choose same household twice

#Population size
N <- 4111240 #num of households in Hungary
n <- nrow(HH) #numer of households in the sample

str(HH)
HHmean <- mean(HH$Income)
HHs <- sd(HH$Income)

#task 1 find the income mean confidence interval (mean of population mean income)
#confidence level 95%
#SR here
alpha <- 0.05
z <- qnorm(1-alpha/2)
se <- (HHs/sqrt(n)) * sqrt(1-n/N) #in case of resampling the mean of the sample income would likely deviates around 31k HUF from the population mean income
delta <- z*se
delta
c(HHmean - delta, HHmean + delta)

#task 2
#this survey is representative for the settlements in Hungary
#it could be a stratified sample by the settlements
#find the mean of household income based on each settlement(statisfied)

#manual way
HH$Bp <- ifelse(HH$Settlement == "Budapest", 1, 0)
Bpdata <- HH[HH$Bp == 1,]
Bpmean <- mean(Bpdata$Income)
Bp_n <- nrow(Bpdata)
Bpratio <- Bp_n/n #around 19% of the household in the sample are from Budapest
Bp_s <- sd(Bpdata$Income)
Bp_N <- Bpratio * N

#now find the standard error of the Budapest household income
Bp_se <- sqrt(Bpratio^2 * Bp_s^2/Bp_n * (1-Bp_n/Bp_N))
Bp_delta <- z*Bp_se
#confidence interval
c(Bpmean-Bp_delta, Bpmean+Bp_delta)

#Task 3
#How can we calculate the population size for the stratums
#here I will estimate the Population in Budapest cause I am lazy
#95% 
Bp_pop_se <- sqrt(Bpratio*(1-Bpratio)/Bp_n) * sqrt(1-Bp_n/Bp_N)
Bp_pop_delta <- z*Bp_pop_se
#confidence interval of Budapest population ratio
c(Bpratio - Bp_pop_delta, Bpratio + Bp_pop_delta)
#confidence interval of Budapest poulation
c(Bpratio - Bp_pop_delta, Bpratio + Bp_pop_delta) * N

#using aggregate function to find out the within settlement type variance for each variance
#for proportionate sample, using within settlement variance is good enough to find the standard error of each settlement into the original SR se formula
stat_table <- aggregate(Income ~ Settlement, data=HH, FUN = mean) #aggregate the income based on each settlement
stat_table
?aggregate
#do for sd within settlement
stat_table$sd <- aggregate(Income~Settlement, data=HH, FUN=sd)[,2]
stat_table
#use the PR advantage of sample ratio = pop ratio to find out the proportion population 
stat_table$sample_size <- table(HH$Settlement)
stat_table
table(HH$Settlement)
stat_table$pop_size <- aggregate(Income~Settlement, data=HH, FUN=stat_table$sample_size/n * N)
stat_table
#! the sd here is the sd of each settlement Bb, city, Town, Village
#so find sw now the overall within-group sd
within_s <- sqrt(sum((stat_table$sample_size - 1)*stat_table$sd^2 / n-1))
within_s #this means that when I take a household income, it would likely defer from it's settlement mean income by 2848k HUF?
se_pr <- within_s/sqrt(n)*sqrt(1-n/N)                 
se_pr
 

#PS sample provides lower standard error compared to the SR without replacement
#confidence interval
PS_mean <- sum(stat_table$sample_size * stat_table$Income)/n
PS_mean
##it is the same as the sample_mean was
c(HHmean - se_pr, HHmean +se_pr)
c(HHmean - se, HHmean + se)
#why is this happening?

#this is due to efficiency of PS sampling
#lets check it out

#variance ratio 
#s^2 between / s^2 
#or 1 - s^2 within/s^2
#or 1 - relative efficiency
var_ratio <- round((1 - within_s^2/HHs^2)*100, 2)
var_ratio
#here it means that
#The settlement type only accounts for 1.89% of the variability of the household income

#variance ratio = standard error ratio of PS/SR
se_ratio <- round((1-se_pr^2/se^2)*100, 2)
se_ratio
#here, the proportional starisfication reduces the standard error of the stratisfied sample by
#1.98% only. quite small here

#relative efficiency
#se_ps^2/se_sr^2
#s^2 within/s^2
(se_pr^2/se^2)
(within_s^2/HHs^2)
