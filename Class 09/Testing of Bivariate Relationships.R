setwd("C:/Stats/Class 09")

library(readxl)

data <- read_excel("CSOK.xlsx")
str(data)

#set settlement and type to factor 0 or 1, and children as logical
data$settlement <- as.factor(data$settlement)
data$type <- as.factor(data$type)
data$CSOK3children <- as.logical(data$CSOK3children)

str(data)

#now investigate the relationship between the housing price (a numerical variable) and 
#whether a property is eligible for CSOK benefits after having 3 children (a categorical variable)
library(ggplot2)
ggplot(data = data, aes(y = price, x=CSOK3children, fill = CSOK3children)) +
  geom_boxplot()

############################################################################
#variance ratio 
#what extent does the CSOK eligibility explain the fluctuations in housing prices

aov(price ~ CSOK3children, data = data)

###########################################################################
#F -test
#########################################################################

#H0: The relationship in the population is not significant, that is, variance ratio = 0 in the population
#H1: The relationship in the population is significant, that is, variance ratio > 0 in the population

#check if the nominal variable has at least 100 samples(CSOK3Children)
table(data$CSOK3children)
#min 100 for each group is true

#calculating the p-value of the F-test
#use the oneway.test function 
oneway.test(price ~ CSOK3children, data=data, var.equal = FALSE)
#decision: we reject H0. The relationship is significant. CSOK eligibility significantly explains 
#variation in property prices in the population. ű
#There is a moderate and significant 12.3% price-raising effect for properties eligible for CSOK


#######################################################################################
#Association (categorical and categorical relationship)
####################################################################################
#examine the relationship between the type of settlement where the apartment is located, 
#Settlement (categorical variable),
#and whether the house is eligible for the CSOK benefit for 3 children (categorical variable).

ggplot(data = data, aes(x=settlement, fill=CSOK3children))+
  geom_bar(position = "fill")

#contingency table
table(data[, c("settlement", "CSOK3children")])
#crosstab but with proportion(or percentage)
round(prop.table(table(data[, c("settlement", "CSOK3children")]))*100, 1)

#marginal distribution. proportion of csok-eligible properties within Budapest
round(prop.table(table(data[, c("settlement", "CSOK3children")]), 1)*100, 1)
#within Budapest, 64% of properties are CSOK eligible and 36% are not

###########################################################
#3.1) Chi-squared test of independence
#H0: There is no significant relationship in the population
#H1: There is a significant relationship in the population
crosstab<-table(data[, c("settlement", "CSOK3children")])
test_of_independence <- chisq.test(crosstab)
test_of_independence
#check if each expected frequencies is minimum 5
test_of_independence$expected
#Decision, reject H0. There is a significant relationship in the population
n <- nrow(data)
cramer <- sqrt(test_of_independence$statistic /(n*(2-1)))
cramer
#based on Cramer's V coefficient, there is a weak relationship between settlement and CSOK3children.
#he association we detected with the stacked column chart
#(namely, that CSOK-eligible properties are typically found in small towns) 
#is not very strong among the observed properties.