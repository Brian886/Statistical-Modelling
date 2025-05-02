setwd("C:/Stats/Class 08")

library(readxl)

data <- read_excel("StackOverflowHungary2020.xlsx")

str(data)

#non-parametric tests
#Goodness of fit test 
#1-pchisq(14.29, df=6)

#representativeness according to employment type
sample_freq <- table(data$Employment)
sample_freq
#in hungary population(2020), full-time 85%, part-time 4%, and self-employed 11%

pop_prop <- c(0.85, 0.04, 0.11)

#H0: Sample data is representative
#H1: Sample data not representative

#Use Chi-square test
#df = num of bins - num of estimated parameters - 1 = 3-0-1 = 2
chisq.test(sample_freq, p = pop_prop) #p can also be probability
#test statistics is 5.29

#right tailed test
#we need to find upper critical value
#significance level 5%
cu <- qchisq(p = 0.95, df = 2) #(quantile, degree of freedom)
#upper critical value is 5.99 
#we see that the test value is lower than the critical value 5.29<5.99
#it is an accepted area. We fail to reject H0. The sample data is representative of the population
?qchisq()
1-pchisq(5.289, df = 2)
#p-value is 0.07
#so we failed to reject H0 .   

#check if each expected frequency has 5 elements
nrow(data)*pop_prop >= 5

#expected frequency based on theoretical proportion
representative_result <- chisq.test(sample_freq, p=pop_prop)
representative_result$expected

#inbuilt test
shapiro.test(data$MonthlyHuf) # used to decide whether or not a sample fits a noral distribution or not
#H0: normal distribution 
#p value is almost 0. Reject H0. The distribution is not a normal distribution 

#Other tests: Shapiro-Wilk, Kolmogorov-Smirnov, Jarque-Bera,
#Doornik-Hansen

#test of uniformity
listed <- c(12, 18, 26, 24, 20)
prob <- c(0.2, 0.2, 0.2, 0.2, 0.2)
chisq.test(listed, p = prob)
#p-value is 19.91. Fail to reject H0. The distribution is a uniform distribution


#test of normality
hist(data$Age1stCode)
#The histogram is roughly normally distributed, but it has a slight right tail.
#Question: Is this slight right-skew due to sampling error? →
#Hypothesis test! :)

#H0: The distribution is normal
#H1: The distribution is not normal
sample_mean <- mean(data$Age1stCode)
s <- sd(data$Age1stCode)
#split normal distribution into 5 bins (quintiles)
norm_quintiles <- qnorm(c(0, 0.2, 0.4, 0.6, 0.8,1), mean = sample_mean, sd = s)
norm_quintiles
#Next, we create a frequency table of the fj values corresponding to the normal distribution quintiles.
observed_freq <- table(cut(data$Age1stCode, breaks = norm_quintiles))
observed_freq
chi2_result <- chisq.test(observed_freq)
chi2_result$statistic
p_value_chi2 <- 1-pchisq(chi2_result$statistic, df = 5-2-1)
p_value_chi2*100 #in percentage format

chi2_result$expected


#check based on 10 bins(deciles)
norm_decile <- qnorm(c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7,0.8,0.9, 1), mean = sample_mean, sd = s)
norm_decile
observed_decile <- table(cut(data$Age1stCode, breaks = norm_decile))
observed_decile 

chi2_result_decile <- chisq.test(observed_decile)
chi2_result_decile$statistic
p_value_chi2_decile <- 1-pchisq(chi2_result_decile$statistic, df = 10-2-1)
p_value_chi2_decile*100
#we can safely reject the H0 hypothesis. The distribution is not normal

#test of homogenity
#developers using Windows and those using other operating systems have the same level of job satisfaction

#H0: The 2 groups have identical distributions in the population (windows and non-windows)
#H1: The 2 groups have different distributions 

#fist group the data into 2 categories, windows and non-windows
unique(data$OpSys)

data$OpSys_Groupped <- ifelse(data$OpSys == "Windows", "Windows", "Non-Windows")
table(data$OpSys_Groupped)

crosstab <- table(data[, c("JobSat", "OpSys_Groupped")])
crosstab
prop.table(crosstab, 2)

#function can calculate df auto
chisq.test(crosstab)

#fail to reject H0
#So in the population (i.e., outside of our sample), 
#the distrobution of job satisfaction levels can be considered IDENTICAL between Windows and non-Windows users.

homogenity_result <- chisq.test(crosstab)
homogenity_result$expected
prop.table(homogenity_result$expected, 2)

homogenity_result$observed - homogenity_result$expected

homogenity_result$expected >= 5

data$JobSat_v2 <- ifelse(data$JobSat %in% c("Very dissatisfied", "Slightly dissatisfied"), "Dissatisfied", 
                         data$JobSat)

crosstab_v2 <- table(data[, c("JobSat_v2", "OpSys_Groupped")])
crosstab_v2

chisq.test(crosstab_v2)

n <-nrow(data)
