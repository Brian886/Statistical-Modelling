#####log-log model
prodfun <- read.csv("Szakag03EN.csv")
#linear model
linfun <- lm(Net_turnover~., data=prodfun)
summary(linfun)

#log-log model
loglog <- lm(log(Net_turnover)~log(Fixed_asset)+log(Current_assets)+log(Liabilities)+
               log(Personal_expenses)+log(Amortisation), data=prodfun)
#error, because we have 0 values
prodfun <- lapply(prodfun, function(x) ifelse(x==0, NA, x))
prodfun <- as.data.frame(prodfun)
prodfun <- prodfun[complete.cases(prodfun), ]
#we dropped the 0 values, we lost 3 observations
loglog <- lm(log(Net_turnover)~log(Fixed_asset)+log(Current_assets)+log(Liabilities)+
               log(Personal_expenses)+log(Amortisation), data=prodfun)
summary(loglog)
#log-log interpretation
#constant elasticity model
#B parameter shows the elasticity too 
#log-log percentage-percentage

#which model is better? the linear or the loglog?
#we can't use the AIC, BIC and adjusted R-squared to compared the linear 
#and log-log model
#because the dependent variables are different
#in the loglog model the logarithm of the net turnover is the dependent 
#comparison acc to the residual sum of squares for the levels 

#test for the specification of the model

#joint significance test, so is global F-test

#test for the specification of the model
#Ramsey RESET test
resettest(loglog)
#H0: the specification of the model is correct
#H1: the specification of the model is not correct
#p-value is almost 0, so the specification is not correct,
#we should change the functional form 
#the log log model function is not correct
