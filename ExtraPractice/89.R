#store data
setwd("C:/Pasāda/Corvinus Data Science In Business/Semester 4 (year 2)/Statistical Modelling/Seminar/ExtraPractice")
library(readxl)
data <- read_excel("PracticeData89.xlsx", sheet="Income") 
str(data)

#1)Examine the nature of the relationship between income and education graphically.
library(ggplot2)
ggplot(data=data, aes(x=Education, y= Income, fill=Education))+
  geom_boxplot()

#2)Calculate and interpret the within-group standard deviation
#with respect to income and education.
#let us try variance ratio: how strong is the relationship
#use anova
#aov(Income~Education, data=data)
#within-group standard deviaton 
#not anova
#use helper table 
helper_table <- aggregate(Income ~ Education, data=data, FUN=sd) #result of income is within sd, not actual income
helper_table$frequency <- table(data$Education) #table count the occurance
helper_table

n <- nrow(data)

within_sd <- sqrt(sum(((helper_table$frequency-1)*helper_table$Income^2 )/ (n-1)))
within_sd
#a random person income(numerical) is expected to deviate from the mean
#of it's own education level(category) by 4221.903USD              

#3. Evaluate the strength of the relationship in both the sample and the population
# i can think of correlation, ANOVA, variance of ratio. I think is the variance of ratio
# SSB/SST 

aov(Income ~ Education, data=data)
SST <- 977654896 + 5311691402
SSB <- 977654896
vor <- SSB/SST *100
vor
#Education level can explain 15.5% of the income variance. 
#vor is between 10% and 50% so there is a moderate significant relationship between income and 
#educaiton level in the sample and popultion. 
#find the p-value
oneway.test(Income ~ Education, data=data, var.equal = FALSE)
#p-value is close to 0. We reject H0
#H0: relation is not significant in pop
#H1: relation is significant in pop

#4) Build a regression model using income as the dependent variable, with the number of
#trainings and work experience as explanatory variables. Assess the explanatory power
#of the model.

model1 <- lm(Income ~ Training + Experience , data=data)
summary(model1)
#44.5% of the variance of income can be explained by the number of trainings and work experience

#5)5. Interpret the intercept and the coefficient of experience.
#when training is 0 and experience is 0, the expected income would be $1479
#If the number of experience increase by 1 year, while training remains the same (ceteris paribus)
#then the expected income is expected to increase by $390.43

## Out of two employees with the same number of trainings completed in the past 5 years, the one with 1 extra year of
# work experience is expected to earn 390 USD more a month.

#6)Calculate and interpret the elasticity of the number of trainings at both the mean and
#median levels.
elasticity <- summary(model1)$coefficients[2,1]*mean(data$Training)/(summary(model1)$coefficients[1,1] + summary(model1)$coefficients[2,1]*mean(data$Training) + summary(model1)$coefficients[3,1]*mean(data$Experience)) 
elasticity
#The predicted income is expected to increase by 0.10% when a 1% increase of training at mean level and expeience is at the mean level too. 
summary(model1)$coefficients[2,1]
mean(data$Training)
# If for an employee the number of completed trainings increases by 1% compared to the mean value of 2.759,
# their income is expected to increase by 0.10%

e2 <- summary(model1)$coefficients[2,1]*median(data$Training)/(summary(model1)$coefficients[1,1] + summary(model1)$coefficients[2,1]*median(data$Training) + summary(model1)$coefficients[3,1]*median(data$Experience))
e2
median(data$Training)
#If for an employee completed training increased by 1% compared to the median value of 3,
#then the predicted income is expected to increase by 0.12%

#7) Which variables can be considered to have no effect in the population with 90%
#confidence? Justify your choice!
summary(model1)
#the training variable is condiered to have no effect in the popultion with 90% confidence due to
#to its high (10.19%) p-value. It means that we can reject HO: the effect of training is not significant 
#outside the sample. So fail to reject it. 

###########################################______-----------------------------------
#Apartment
#------------------------------------------------------------------------------------
apartment<- read_excel("PracticeData89.xlsx", sheet="Apartment") 
str(apartment)
apartment$AirCond <- as.factor(apartment$AirCond) #so no is 1, yes is 2

#Using quintiles, test whether apartment prices can be considered exponentially
#distributed at conventional significance levels based on the sample data. Clearly state
#the null and alternative hypotheses, perform the necessary calculations, check the
#conditions of the test, and provide a written conclusion. Pay attention to precise wording
#and justification.

#test of exponentially
#check with simple graph first
hist(apartment$Price)
#H0: The prices follows a expponential distribution
#H1: the prices do not follow a exponential distribution

lambda <- 1/mean(apartment$Price)

#create into bins
quintiles <- qexp(c(0, 0.2, 0.4, 0.6, 0.8, 1), rate=lambda)
quintiles

#put the data into bins
observed_freq <- table(cut(apartments$Price, breaks=quintiles))
observed_freq

#do chisq test
chisq_result <- chisq.test(observed_freq)
chisq_result

#p-value (right test)
p <- 1-pchisq(chisq_result$statistic, df=5-1-1)
p*100

#is the minimum of each bin having 5 observation met?
#chisq_result$expected
0.2*500#test assumption met

#conclusion: Reject Ho as p_value is almost 0. This means that the distribution of price is not
#a exponential distribution in the population. The variability of it in the sample
#is due to sampling error. 

#2)Test whether the distribution of the number of bathrooms is the same for apartments
#with and without air conditioning at conventional significance levels. Clearly state the
#null and alternative hypotheses, perform the necessary calculations, check the
#assumptions of the test (and if necessary, logically combine categories), and provide a
#written conclusion. Be careful with precise wording and justification.

#test of homogenity
#H0: bathroom distribution is the same for apartments with and without air conditioning
#H1: bathroom distribution is different for apartments with and without air conditioning

unique(apartment$AirCond)

crosstab <- table(apartment[, c("Bathrooms", "AirCond")])
crosstab
chisq.test(crosstab)
chisq.test(crosstab)$expected

# The expected frequencies in the contingency table under H0 are lower than 5 for apartments with 6 bathrooms.
# Merge them with apartments with 5 bathrooms
apartment$bathmerge <- ifelse(apartment$Bathrooms >= 5, "5+", apartment$Bathrooms)

crosstab <- table(apartment[, c("bathmerge", "AirCond")])
crosstab
chisq.test(crosstab)$expected
#now the expected is >5 which is good
chisq.test(crosstab)

# p-value = 0.3812 > 0.1 --> H0 can't be rejected on any common significance levels -->
# --> The distribution for the number of bathrooms is not significantly different for apartments with and
#     without air conditioning on any common significance levels

#3)Identify where and by how much the sample frequency deviates most from the expected
#frequencies under the assumption of identical number of bathrooms distributions.
crosstab - chisq.test(crosstab)$expected
#we have about 3.9 more apartment with 3 bathsroom with airconditioning than we expect to have 
#in case of completely identical bathroom distribution
#However, according to the result of Task 2 this difference is not significant. Because identical

#4)Construct a regression model in which apartment price is explained by area and number
#of bathrooms. Interpret the coefficient of area and its 99% confidence interval.
model2 <- lm(Price~Area+Bathrooms, data=apartment)
summary(model2)
ggplot(data=apartment, aes(x=Area, y=Price))+
  geom_point()+
  geom_smooth(method="lm")
# The apartment price is expected to increase by 36.35 thousand AED if area increases by 1 m^2 while
# the number of bathrooms remains the same.
confint(model2, level=0.99)
#This effect is expected to be between 33.15 and 39.54 thousand AED
#for unobserved apartments with 99% confidence


#5)Evaluate the explanatory power of the model for unobserved apartments. State the null
#and alternative hypotheses, perform the necessary calculations, and provide a written
#conclusion. Use precise terminology and justification

#H0: R^2 = 0
#H0: R^2 != 0

summary(model2)

#70.57% of the price variablility can be explained by the area and number of bathrooms. 
#p-value is close to 0 -> reject H0 at all significant level
#The model has significant positive explanatory power on all common significance level
#in the population of unobserved data

#6)Determine the direct, indirect, and total effect of the number of bathrooms. Interpret
#these values and their relationships in context.

#direct effect of bathrooms to price
coefficient_model2 <- summary(model2)$coefficients
coefficient_model2
bath_price <- coefficient_model2[3,1]
bath_price

#direct effect of bathrooms to area
model_area_baths <- lm(Area ~  Bathrooms, data=apartment)
summary(model_area_baths)
bath_area <- summary(model_area_baths)$coefficients[2,1]
bath_area

#effect of area to price
area_price <- coefficient_model2[2,1]

#indict bath to price
indirect_bath_price <- bath_area*area_price
indirect_bath_price 

#total effect
total <- bath_price + indirect_bath_price
total 

#direct:-665.1625. While area remains the same, and bathroom increase by 1, the 
#expected price of the apartment is expected to decrease by 665 AED thousand
#indirect: 1911.799. However, a extra bathroom is expected to increase the area,
#which would lead to a expected price increase of 1911 thousand AED.
#total: 1246.636. These 2 effect will result a expected increase of 1236 thousand AED when
#one extra bathroom is added. 
