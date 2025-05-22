setwd("C:/Statsmodel/Exam")
library(readxl)
data <- read_excel("auto.xlsx")
#alpha = 5
#---------------------------------------------
#1) 
str(data)

data$Condition <- as.factor(data$Condition)
data$Fuel <- as.factor(data$Fuel)
data$Condition <- relevel(data$Condition, ref="Normal")
data$Fuel <- relevel(data$Fuel, ref="Diesel")

#2)
#H0: Relationship not significant
#H1: Relatiobship is significant

#helper_table <- aggregate(Price~ Condition, data=data, FUN=sd)
#helper_table
#helper_table$freq <- table(data$Condition)
#helper_table$freq
oneway.test(Price~ Condition, data=data, var.equal = FALSE)
#p value: 2.2e-16
#we reject H0. 

#3)
aov(Price~ Condition, data=data)
vor <- 9112795236/(9112795236+49620234059)
vor
#15% 

#4) 
model1 <- lm(Price ~ ., data=data)
summary(model1)

#5)
data$sq_errors <- model1$residuals^2
library(ggplot2)
ggplot(data = data, aes(x=Price, y=sq_errors))+geom_point()

#h0: homoskedasticity
#H1: heteroskedasticity

library(lmtest)
bptest(model1, studentize = TRUE)
#p_value: < 2.2e-16
#residuals are heteroskadastic

#6) 
#H0: no significant relationship
#H1: there is 
model2 <- lm(Price ~ Condition+Fuel+Mileage+Age+HP+I(Age^2)+Fuel:Mileage , data=data)
summary(model2) #adjusted R^2: 0.8914 
summary(model1) #adjusted R^2:0.8842


AIC(model1, model2)
BIC(model1, model2)

lmtest::resettest(model1)

#7) 
library(car)
vif(model2)

#8)
summary(model2)
str(data)

-8.7276+3.7665+1

#9)
model3 <- lm(log(Price) ~ Condition+Fuel+Mileage+Age+HP+I(Age^2)+Fuel:Mileage , data=data)
summary(model3)

AIC(model2, model3)
BIC(model2, model3)
