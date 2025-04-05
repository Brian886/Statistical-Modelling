setwd("C:/Stats/mock")

?rexp
#parameters
lamda <- 0.02
size <- 10000

#Generate the random population
population <- rexp(n=size, rate=lamda)
population
