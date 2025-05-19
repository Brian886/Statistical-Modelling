#####log-log model
setwd("C:/Stats/Class 11")
prodfun <- read.csv("Szakag03EN.csv")
# all numerical numbers nice

#linear model
linfun <- lm(data = prodfun, Net_turnover~ .)
summary(linfun)
#let see with graph
library(ggplot2)
ggplot(data=prodfun, aes(x=Amortisation, y=Net_turnover))+
  geom_point()+
  stat_smooth(method=lm)
#doesn't look nice

ggplot(data=prodfun, aes(x=log(Amortisation), y=log(Net_turnover)))+
  geom_point()+
  stat_smooth(method="loess")

ggplot(data=prodfun, aes(x=log(Amortisation), y=log(Net_turnover)))+
  geom_point()+
  stat_smooth(method=lm)
#with log looks super good 

#log log model
loglog <- lm(log(Net_turnover)~log(Fixed_asset)+log(Current_assets)+log(Liabilities)+
               log(Personal_expenses)+log(Amortisation), data = prodfun)
#error because we have null values
prodfun <- lapply(prodfun, function(x) ifelse(x==0, NA, x))
prodfun <- as.data.frame(prodfun)
prodfun <- prodfun[complete.cases(prodfun), ] 
#from 479 to 476 observations
loglog <- lm(log(Net_turnover)~log(Fixed_asset)+log(Current_assets)+log(Liabilities)+
               log(Personal_expenses)+log(Amortisation), data = prodfun)
summary(loglog)

#log-log interpretation
#constant elasticity model
#B parameter shows the elasticity too 
#log-log percentage-percentage
#One percentage dollar increase in fixed asset would result in -0.29 % change in net_turnover


#which model is better? the linear or the loglog?
#we can't use the AIC, BIC and adjusted R-squared to compared the linear 
#and log-log model
#because the dependent variables are different
#in the loglog model the logarithm of the net turnover is the dependent 
#comparison acc to the residual sum of squares for the levels

#test for the specification of the model
#Ramsey RESET test #a form of global F-test
resettest(loglog)
#H0: the specification of the model is correct
#H1: the specification of the model is not correct
#p-value is almost 0, so the specification is not correct,
#we should change the functional form 
#the log log model function is not correct