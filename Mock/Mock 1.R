setwd("C:/Stats/mock")

#3a)
?rexp
#parameters
lambda <- 0.02
size <- 10000

#Generate the random population
population <- rexp(n=size, rate=lambda)

#theoretical expected value and standard deviation
#calculation is the same
theory_mean <- 1/lambda
theory_sd <- 1/lambda

#observed parameters
observed_mean <- mean(population)
observed_sd <- sd(population)
#The obeserved mean and sd are not exactly the same, but are very similar due to the central limit theorum. 

#b)
samples <- sample(population, size = 100, replace = TRUE)

for (i in 1:(100-1)){
  samples <- rbind(samples, sample(population, size = 100, replace = TRUE))
}

samples <- as.data.frame(samples)
rownames(samples) <- paste0("Samples", 1:100)
colnames(samples) <- paste0("Element", 1:100)

samples$mean <- apply(samples[,1:100], 1, mean)
head(samples[, 97:101])

library(ggplot2)
ggplot(samples, aes(x=mean)) + 
  geom_histogram(aes(y=after_stat(density)))+
  theme_minimal()

hist(samples$mean)

#c)
alpha <- 0.05

z <- qnorm(1 - alpha/2)
se <- observed_sd/sqrt(100)
delta <- z*se

samples$MeanLower <- samples$mean - delta
samples$MeanUpper <- samples$mean + delta

#d 
mean((observed_mean >= samples$MeanLower) & (observed_mean <= samples$MeanUpper))
#The hit rate here is 0.93 which is 93% confidence interval. Unfortunately it did not reach the expected confidence interval 
#with 95% confidence. 

#4 long case study
library(readxl)
HH <- read_excel("PracticeData.xlsx")

n <- nrow(HH)
str(HH)

#a) Estimate the 90% confidence interval of the proportion of the houses with excellent condition! 
excellent <- HH[HH$Condition == "excellent",]
en <- nrow(excellent)

ep <- en/n

alpha <- 0.1
z <- qnorm(1-alpha/2)
se <- sqrt(ep*(1-ep)/n)
delta <- z*se

lower <- ep - delta
upper <- ep + delta

c(lower, upper)

#b) Estimate the 98% confidence interval of the average price for each town! Depict a bar 
#chart of the mean prices with the confidence intervals!
table(HH)

library(rcompanion)
confident_price <- groupwiseMean(Price~Town, data = HH, conf=0.98, digits= 2)
?groupwiseMean

library(ggolot2)
ggplot(confident_price, aes(x=Town, y= Mean, fill = Town))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin=Trad.lower, ymax=Trad.upper))+
  theme_minimal()

#c)Test the following statement: the average price is more than 25 million HUF!
#(alpha=0.05) Calculate the p-value manually
#H1: mean > 25 000
#H0: mean <= 25000
#H0T: mean = 25000
H0_mean <- 25000
HH_S <- sd(HH$Price)
HHmean <- mean(HH$Price)

#z-test 
z_test <- (HHmean - H0_mean)/(HH_S/sqrt(n))

#critical value and p_value
cu <- qnorm(1-0.05)

p_value <- 1 - pnorm(z_test)
#decision:
#Fail to reject null hypothesis

#d) According to a survey, 48% of properties for sale are larger than 100 square metres.
#Test that fact in this sample! (alpha=0.05) Calculate the p-value manually!
#Statement: 48% of properties for sale are larger than 100 square metres.
#P = proportion of houses are larger than 100 square metres
#H0: P = 0.48
#H1: P != 0.48
alpha <- 0.05
P0 <- 0.48
Pn <- nrow(HH[HH$Size > 100, ])
P <- Pn/n

#z_test
z_test <- (P-P0)/sqrt(P0*(1-P0)/n)

#Critical value and p_value
cl <- qnorm(alpha/2)
cu <- qnorm(1-alpha/2)

p_value <- pnorm(z_test)
#decision
#Fail to reject null hypothesis

#e) We assume that, the properties are cheaper on average in Abony compared to Aszód. 
#Test that assumption! (alfa=0.05) Calculate the p-value manually!
#2 sample hypothesis test

# Y= Abony
# X = Aszód 
 

#H1: mean(Y)-mean(X) < 0
#H0: mean(Y)-mean(X) >= 0
#H0T: mean(Y)-mean(X) = 0

#before testing z_test, we need to find the variances of the groups
Y <- HH[HH$Town == "Abony", ]
X <- HH[HH$Town == "Aszód", ]

Ymean <- mean(Y$Price)
Xmean <- mean(X$Price)
Ysd <- sd(Y$Price)
Xsd <- sd(X$Price)
Yn <- nrow(Y)
Xn <- nrow(X)

#z_test
z_test <- (Ymean - Xmean)/sqrt((Ysd^2/Yn)+(Xsd^2/Xn))

#critical value (left test) and p_value
cl <- qnorm(alpha)

p_value <- pnorm(z_test)

#reject null hypothesis
