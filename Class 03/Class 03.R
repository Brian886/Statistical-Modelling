#Simulations and sampling
#try to generate random value from uniform distribution
number <- runif(n=1) #random num between 0 und 1

#let's create a sample from this distribution
sample1 <- runif(n=50)
sample1
hist(sample1)

#try to transform [0,1] to [40,160]
sample1 <- (sample1*(160-40))+40
sample1
hist(sample1)

#dataframe
sample1_df <- as.data.frame(sample1)
library(ggplot2)
ggplot(sample1_df, aes(x=sample1))+
  geom_histogram(aes(y=after_stat(density)), bins=6)+
  stat_function(fun=dunif,
                args = list(min=40, max=160),
                col='red')+
  theme_minimal()

low_bound <- 40
upp_bound <- 160

#sample1 <- runif(n=50)
#transfrom them to U(40,160)
#with quantile function
sample <- qunif(sample1, min = low_bound, max=upp_bound)


#use this for normal distribution
mu <- 5
sigma <- 1

#we would like to get the same results
set.seed(1992)
sample1 <- runif(n=50)
sample_normal <- qnorm(sample1, mu, sigma)
norm_df <- as.data.frame(sample_normal)

#plot
ggplot(norm_df, aes(x=sample_normal))+
  geom_histogram(aes(y=after_stat(density)), bins=6)+
  stat_function(fun=dnorm,
                args = list(mean=mu, sd=sigma),
                col='red')+
  theme_minimal()

#create uniform, normal and exponential distribution
#and 1-1 sample from them
#Normal distributed sample N(50,10)
mu <- 50
sigma <- 10
set.seed(1992)
sample_normal <- rnorm(n=50, mu, sigma)

#exponential distributed sample with lambda = 0.002
lambda <- 0.002
set.seed(1992)
sample_exponential <- rexp(n=50, rate= lambda)
?rexp

#sample from uniform distribution (7,51)
low_bound <- 7
upp_bound <- 51
sample_uniform <- runif(n=50, min=low_bound, max=upp_bound)

#merge them in a dataframe
randomdata <- data.frame(normal=sample_normal, 
                         exponential=sample_exponential,
                         uniform=sample_uniform)

str(randomdata)

#Normally distributed sample
#the mean is 50 and the standard deviation is 10
#exponential
#the mean is 500 and the standard deviation is also 500
#1/lambda 
#Uniform
#the mean is (a+b)/2 = (7+51)/2 = 29
#sd is (b-a)/sqrt(12)=(51-7)/sqrt(12) = 12.7

#create a table to check these values in the samples
norm_stats <- c(
  mu, 
  sigma,
  qnorm(0.5, mu, sigma), #Median
  pnorm(40, mu, sigma)#P(Yi<40)
) #theory value

expstats <- c(
  (1/lambda),
  (1/lambda),
  qexp(0.5, rate = lambda),
  pexp(40, rate=lambda)
)

unistats <- c(
  ((low_bound+upp_bound)/2),
  ((upp_bound-low_bound)/sqrt(12)),
  qunif(0.5, min=low_bound, max = upp_bound),
  punif(100, min=low_bound, max = upp_bound)
)

#create a table for theoretical measures
TheoreticalMeasures <- data.frame(Normal=norm_stats,
                                  Exponential=expstats,
                                  Uniform=unistats)

rownames(TheoreticalMeasures) <- c("Expected value",
                                   "Standard deviation",
                                   "Median",
                                   "P(Yi<40)")

round(TheoreticalMeasures, 3)

#calculate the measures for samples
norm_sample_stats <- c(mean(randomdata$normal),
                       sd(randomdata$normal),
                       median(randomdata$normal),
                       sum(randomdata$normal<40)/nrow(randomdata))

exp_sample_stats <- c(mean(randomdata$exponential),
                      sd(randomdata$exponential),
                      median(randomdata$exponential),
                      sum(randomdata$exponential<40)/nrow(randomdata))

uni_sample_stats <- c(mean(randomdata$uniform),
                      sd(randomdata$uniform),
                      median(randomdata$uniform),
                      sum(randomdata$uniform<40)/nrow(randomdata))

#merge them
SampleMeasures <- data.frame(Normal=norm_sample_stats,
                             Exponential = exp_sample_stats,
                             Uniform=uni_sample_stats)

rownames(SampleMeasures) <- c("Mean", "Standard deviation", "Median", "P(Yi<40)")


round(TheoreticalMeasures, 3)
round(SampleMeasures, 3)

#create a function to generate samples
GenerateSample <- function(sample_size, random_seed){
  #Normal distributed sample N(50,10)
  mu <- 50
  sigma <- 10
  set.seed(random_seed)
  sample_normal <- rnorm(n=sample_size, mu, sigma)
  
  #exponential distributed sample with lambda = 0.002
  lambda <- 0.002
  set.seed(random_seed)
  sample_exponential <- rexp(n=sample_size, rate= lambda)
  
  #sample from uniform distribution (7,51)
  low_bound <- 7
  upp_bound <- 51
  set.seed(random_seed)
  sample_uniform <- runif(n=sample_size, min=low_bound, max=upp_bound)
  
  #merge them in a dataframe
  randomdata <- data.frame(normal=sample_normal, 
                           exponential=sample_exponential,
                           uniform=sample_uniform)
  
  #calculate the measures for samples
  norm_sample_stats <- c(mean(randomdata$normal),
                         sd(randomdata$normal),
                         median(randomdata$normal),
                         sum(randomdata$normal<40)/nrow(randomdata))
  
  exp_sample_stats <- c(mean(randomdata$exponential),
                        sd(randomdata$exponential),
                        median(randomdata$exponential),
                        sum(randomdata$exponential<40)/nrow(randomdata))
  
  uni_sample_stats <- c(mean(randomdata$uniform),
                        sd(randomdata$uniform),
                        median(randomdata$uniform),
                        sum(randomdata$uniform<40)/nrow(randomdata))
  
  #merge them
  SampleMeasures <- data.frame(Normal=norm_sample_stats,
                               Exponential = exp_sample_stats,
                               Uniform=uni_sample_stats)
  
  rownames(SampleMeasures) <- c("Mean", "Standard deviation", "Median", "P(Yi<40)")
  
  return(SampleMeasures)
  
}

#check the function
TheoreticalMeasures 
GenerateSample(50, 1992)
GenerateSample(10000, 1992)

#Central limit theorem
setwd("C:/Statsmodel/Class 03")
library(readxl)
swimming <- read_excel("LIDLBalaton2022.xlsx") #is a population
print(swimming)

#population
Popmean <- mean(swimming$TIME)
Popstd <- sd(swimming$TIME) #sample standard deviation
RealPopstd <- sqrt(sum((swimming$TIME - Popmean)^2)/nrow(swimming)) #population sd
Popprop <- sum(swimming$TIME>180/nrow(swimming))

#100 elements sample from this population IID liked sample of n=100
set.seed(1992)
selected_into_sample <- sample(rownames(swimming), size=100, replace = TRUE) #to get IID like
swimming_sample <- swimming[selected_into_sample, ]
#it is an IID sample from the population
c(mean(swimming_sample$TIME), sd(swimming_sample$TIME))

set.seed(2022)
selected_into_sample2 <- sample(rownames(swimming), size=100, replace = TRUE) 
swimming_sample2 <- swimming[selected_into_sample2, ]
c(mean(swimming_sample2$TIME), sd(swimming_sample2$TIME))

#method of resampling
set.seed(1992)
samples <- sample(swimming$TIME, size = 100, replace = TRUE)

for(index in 1:(10000-1)){
  set.seed(1992+index)
  samples <- rbind(samples, sample(swimming$TIME, size = 100, replace = TRUE))
}

samples <- as.data.frame(samples)  

rownames(samples) <- paste0("sample", 1:10000)
colnames(samples) <- paste0("Element", 1:100)
head(samples)
