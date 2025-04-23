setwd("C:/Stats/Assignment")

install.packages("readxl")
library(readxl)

data <- read_excel("stats_sport.xlsx")

#2. Draw at least 500 IID and 500 SR samples of size 100 from the selected quantitative variable.
#Draw iid samples
set.seed(2003.5)
Samples_iid <- sample(data$Overall, size = 100, replace = TRUE)

for(i in 1:(500-1)){
  set.seed(2003.5 + i)
  Samples_iid <- rbind(Samples_iid, sample(data$Overall, size = 100, replace=TRUE))
}

Samples_iid <- as.data.frame(Samples_iid)

rownames(Samples_iid) <- paste0("Sample", 1:500)
colnames(Samples_iid) <- paste0("Element", 1:100)
head(Samples_iid,5)

#Draw SR sample (without replacement)
set.seed(2003.5)
Samples_sr <- sample(data$Overall, size = 100, replace = FALSE)

for (i in 1:(500-1)){
  set.seed(2003.5 + i)
  Samples_sr <- rbind(Samples_sr , sample(data$Overall, size = 100, replace=FALSE))
}

Samples_sr <- as.data.frame(Samples_sr)

rownames(Samples_sr) <- paste0("Sample", 1:500)
colnames(Samples_sr) <- paste0("Element", 1:100)

head(Samples_sr, 5)

#3. Identify which qualitative variable would be the most beneficial for stratification and justify your choice. 
#Based on this qualitative variable, also take 500 proportionally stratified (PS)
#random samples of size 100 from the quantitative variable!

#The qualitative variable chosen is the continent.
#Because we would like to know which continent produces the most talent

#500 proportionally stratified random sample of size 100 of "overall"

##get strata proportion
continent_prop <- prop.table(table(data$Continent))
continent_prop

set.seed(2003.5)

#create matrix to store samples
Samples_pr <- matrix(nrow = 500, ncol = 100)

for (i in 1:500){
  set.seed(2003.5 + i)
  sample_pr <- numeric(100)

  starting <- 1
  for (continent in names(continent_prop)){
    #to calculate how many to sample from the coninent stratum(if 0.6 then 60 out of 100)
    n <- round(100 * continent_prop[continent])
    
    #Get population from each continent
    continent_pop <- data$Overall[data$Continent == continent]
    
    #sample from each continent
    sample_pr[starting:(starting + n - 1)] <- sample(continent_pop, size = n, replace = FALSE)
    starting <- starting + n
  }
  
  #if due to rounding we didn't get exactly 100, adjust
  if (starting <= 100){
    remaining <- 100 - starting + 1
    sample_pr[starting:100] <- sample(data$Overall, size = remaining, replace = FALSE)
  }
  
  Samples_pr[i, ] <- sample_pr
}

Samples_pr <- as.data.frame(Samples_pr)

rownames(Samples_pr) <- paste0("Sample", 1:500)
colnames(Samples_pr) <- paste0("Element", 1:100)


#######################################################################################################
#4) Compute the mean for the quantitative variable in each sample.

Samples_iid$iid_mean<- apply(Samples_iid[, 1:100], 1, mean)
Samples_sr$sr_mean <- apply(Samples_sr[, 1:100], 1, mean)
Samples_pr$pr_mean <- apply(Samples_pr[, 1:100], 1, mean)

#check
head(Samples_iid[, 97:101])
head(Samples_sr[, 97:101])
head(Samples_pr[, 97:101])

###################################################################################################
#5) Determine the mean and standard deviation of the chosen quantitative variable for the entire dataset.
#Compute the theoretical mean squared error (MSE) of the mean estimation for all three sampling methods
#based on the formulas learned during the course. 
#In this task, you may consider the standard deviation of the full dataset as the population standard deviation.

mu <- mean(data$Overall)
sigma <- sd(data$Overall)
n <- 100
N <- nrow(data)

iid_mse_a <- sigma^2/n
sr_mse_a <- (sigma^2/n)*(1-n/nrow(data))

helper_table <- aggregate(Overall ~ Continent, data = data, FUN= mean)
helper_table$sd <- aggregate(Overall ~ Continent, data = data, FUN = sd)[,2]
helper_table$sample_size <- table(data$Continent)
helper_table
within_sd <- sqrt(sum((helper_table$sample_size-1)*helper_table$sd^2)/(N-1))
within_sd

ps_mse_a <- (within_sd^2/n)*(1-n/N)

c(iid_mse_a, sr_mse_a, ps_mse_a)

##########################################################################################################
#6) Calculate the empirical mean squared error for each of the three sampling methods, 
#defined as the variance of the sample means around the population mean.

iid_mse_b <- mean((Samples_iid$iid_mean - mu)^2)
sr_mse_b <- mean((Samples_sr$sr_mean - mu)^2)
ps_mse_b <- mean((Samples_pr$pr_mean - mu)^2)

c(iid_mse_b, sr_mse_b, ps_mse_b)

########################################################################################################
#7) Calculate the empirical mean squared errors using bias and empirical standard error.
#MSE = Bs^2 + Se^2

classic_var<-function(x){
  return (mean((x-mean(x))^2))
}

bs_iid_mean <- mean(Samples_iid$iid_mean) - mu
bs_sr_mean <- mean(Samples_sr$sr_mean) - mu
bs_ps_mean <- mean(Samples_pr$pr_mean) - mu

se_iid_mean <- sqrt(classic_var(Samples_iid$iid_mean))
se_sr_mean <- sqrt(classic_var(Samples_sr$sr_mean))
se_ps_mean <- sqrt(classic_var(Samples_pr$pr_mean))

iid_mse_c <- bs_iid_mean^2 + se_iid_mean^2
sr_mse_c <- bs_sr_mean^2 + se_sr_mean^2
ps_mse_c <- bs_ps_mean^2 + se_ps_mean^2

c(iid_mse_c, sr_mse_c, ps_mse_c)





















