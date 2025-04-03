#task 7-39

#a)
mu <- 450
samplemean <- 446
pop_sd <- 10
n <- 25
alpha <- 0.05

#H0: mu = 450
#H1: mu! = 450 (two-sided)
#it is a z-test
#reason: technically it is an IID sample, we assume that the allowed 
#standard deviation is the sd of the population (known pop sd), sample
#size is small, population is normally distributed 

#calculate the test value
z_test <- (samplemean - mu)/(pop_sd/sqrt(n))

#two critical values
lower_cv <- qnorm(alpha/2)
upper_cv <- qnorm(1-alpha/2)

#test value is lower than the lower critical value, in this way
#we reject H0, the expected weight of the cans is not 450
#in hungary a factory produce 700000 cans per day
#700000*4 = 280000
#2800000/1000g = factory safe 2800 tons of ingredients. jian shang

#task b)
pnorm(z_test)
p_value <- 2*pnorm(z_test)
#p_value is 4.55% #before calculation we can predict it based on 2 sigma rule
#p_value is less than alpha (significance level), we reject the H0
#should reject based on critical value too

#task c)
#Testt the expected loading weight, assuming
#that we have no information about the population standard deviation!

#in that case we should use the (corrected) sample deviation
#use t test because we dont know the population sd (based on 3 conditions)
sample_sd <- 11
t_test <- (samplemean-mu)/(sample_sd/sqrt(n)) #only difference in the sd
t_test
#-1.82
lower_cv_t <- qt(alpha/2, df=n-1) #degree of freedom are the possibility, when estimate something we lose 1 df
#cause of fater tails, the value is lower and greater
upper_cv_t <- qt(1-alpha/2, df=n-1)
#we fail to reject H0, the expected weight of the cans is 450g 


#task d) test for the standard deviation 
#H0: sigma <=10
#H0T: sigma = 10
#H1: sigma > 10 (one-sided, raight-tailed)
chi_square_test <- ((n-1)*sample_sd^2)/(10^2)
chi_square_test
#29.04
#we have just an upper critical value
chi_upper <- qchisq(1-alpha, df=n-1)
chi_upper
?qchisq
#upper cr.value is greater than the test value, in this way
#we we fail to reject the techincal H0, the condition is true for 
#standard deviation (standard deviation is 10g)

#calculate the p-value
p_value <- 1 - pchisq(chi_square_test, df=24)
p_value
#p>alpha, fail to reject techincal H0
