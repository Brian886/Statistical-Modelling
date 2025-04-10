setwd("C:/Statsmodel/Class 08")
library(readxl)
data <- read_excel("StackOverflowHungary2020.xlsx")


#non-parametric tests
#Goodness
#1-pchisq(14.29, df=6)

#representativeness according to employment type
#In hungary in 2020 full-time 85%, part-time 4%
#and self-employed 11%
sample_freq <- table(data$Employment)
sample_freq 

#population proportions
pop_prop <- c(0.85, 0.04, 0.11)

#H0: representative
#H1: not
#Chi-square test
#degrees of freedom. k-b-1 = 3-0-1
chisq.test(sample_freq, p=pop_prop)
#test statistics: 5.29
#right-tailed test
#upper critical value
#significance level: 5%
c_upper <- qchisq(0.95, df=2)
#5.99
#The test value is less than the critical value
#it is in acceptance area, we fail to reject H0, representative

#p-value 7.1%, so the p>alpha, we fail to reject H0, representative

#these are large sample tests
#generally the test value depends on the sample size

#what would be the decision if we have the same employment type
#proportions but the sample size is 420
#in this case we doubled the sample size
#so the test value is doubled as well
#test-value: 5.2885
new_test_value <- 2*5.2885
#calculate the p-value for that case
1-pchisq(new_test_value, df=2)
#less than 1%, we reject H0 in that case, so it is not representative

#test of normality
#test the normality of salary(is it normal distribution)
hist(data$MonthlyHuf)
#right-skewed, so it would be funny if we would accept H0
#H0: fllows normal distribution
#H1: not 

#parameters of normal distribution
sample_mean <- mean(data$MonthlyHuf)
s <- sd(data$MonthlyHuf)
#working with quintiles (5, 0-20-40-60-80-100)
quintiles <- qnorm(c(0,0.2, 0.4, 0.6, 0.8, 1), mean=sample_mean, sd=s)

quintiles 
obs_freq <- table(cut(data$MonthlyHuf, breaks = quintiles))
obs_freq 

#test
chisquaretest <- chisq.test(obs_freq)
chisquaretest$paremeter #k-b-1 = 5-0-14, it is wrong
#we used the sample sd and mean, these are estimated parameters
#df=5-2-1=2
#we calculate the p-value manually
p_value_norm <- 1-pchisq(chisquaretest$statistic, df=5-2-1)
p_value_norm
#p-value is almost 0, we reject H0, the salary does not 
#follow normal distribution

#inbuilt test
shapiro.test(data$MonthlyHuf)
#H0: normal distribution 
#p-value is almost 0, the salary does not follow normal distribution

#Other tests: Shapiro-Wilk, Kolmogorov-Smirnov, Jarque-Bera,
#Doornik-Hansen

#test of homogenity
unique(data$OpSys)

#make it easier: windows or not 
data$windows <- ifelse(data$OpSys=="Windows", "Windows", "NotWindows")
table(data$windows)

crosstab <- table(data$JobSat, data$windows)
crosstab

#proportions
prop.table(crosstab)
#H0: distribution of job satisfaction categories are
#identical for the windows variable groups
chisq.test(crosstab)
#p-value is 20.4%, we fail reject H0, the two satisfaction
#distribution are identical 

#flats_bp file
#relation between variables
flats <- read_excel("flats_bp.xlsx")
flats$Type <- as.factor(flats$Type)
flats$Condition <- as.factor(flats$Condition)

#relation between qualitative variables
#relation between wall type and condition

#step 1 create a cross table
crosstable <- table(flats$Type, flats$Condition)
crosstable

#step 2
#test of independence 
#H0: there is no relation (independent from each other)
#H1: there is significant relation
#association
ind_test <- chisq.test(crosstable)
ind_test
#what is the df here?
#df=(r-1)*(c-1)=(2-1)*(4-1)=3
#p_value is 4.82%
#p<alpha, we reject H0, there is a significant relation between
#the wall type and the condition 

#another function
install.packages("gmodels")
gmodels::CrossTable(flats$Type, flats$Condition, 
                    chisq = T, sresid = T,
                    fisher = T,
                    format="SPSS")

#step 3 calculate the crame
#strength of the relation
install.packages("questionr")
questionr::cramer.v(crosstable)
#Cramer: 0.2 - weak relation 



#another question
#relation between the condition and price
#qualitative and quantitative variables
#method: Analysis of Variance (ANOVA)

#let's check with a graph
library(ggplot2)
ggplot(flats, aes(x=Condition, y=Price))+geom_boxplot()
#depends on within and between variance

#ANOVA
anova_table <- aov(Price~Condition, data=flats)
#condition is between
#residual is within

#create table with n, sample means and sd
stat_table <- aggregate(Price~Condition, data=flats, FUN=mean)
stat_table
stat_table$freq <- table(flats$Condition)
stat_table
stat_table$sd <- aggregate(Price~Condition, data=flats, FUN=sd)[,2] #aggregate creates 2 columns, we want the second column only
stat_table

within_ss <- sum((stat_table$freq-1)*stat_table$sd^2) 
sum(anova_table$residuals^2) #within sum of squares

between_ss <- sum(stat_table$freq*(stat_table$Price-mean(flats$Price))^2) #minus mean of original price
between_ss

total_ss <- within_ss+between_ss
total_ss2 <- (nrow(flats)-1)*var(flats$Price)#numerator of the corrected variance
#corrected var=SST/(n-1)

#variance ratio - H^2, eat^2
between_ss/total_ss
#45.45% explanatory power
#H - strength of relation
sqrt(between_ss/total_ss)
#moderate relation

#it is significant
summary(anova_table)
#p-value is almost 0, we reject H0, there is significant relation 
#between the condition and price 