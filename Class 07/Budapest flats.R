#flats budapest file
setwd("C:/Statsmodel/Class 07")

#import
library(readxl)
flat <- read_excel("flats_bp.xlsx")
#200 observation
#iid sample from the real estate market in Budapest
flat$Type <- as.factor(flat$Type)
flat$Condition <- as.factor(flat$Condition)

#proportion test for the wall type
#statement: P(panel) <= 0.1 (10%)
#H0: P<=0.1
#H0T: P=0.1
#H1: P>0.1

alpha <- 0.05
n <- nrow(flat)
#we use table lah
table(flat$Type)
k <- table(flat$Type)[2]
p <- k/n
P <- 0.1

z_test <- (p-P)/sqrt(P*(1-P)/n)
z_test

#upper critical value
upper_cv_p <- qnorm(1-alpha)

#test value is greater than the critical value, so we
#reject H0T, H0, the proportion of the panel type flats
#is greater than 10%
p_value <- 1-pnorm(z_test)
#use the prop.test function to check this statement
?prop.test
flat$Type <- relevel(flat$Type, ref = "Panel")
prop.test(table(flat$Type), p = P, alternative="greater", conf.level = 0.95, correct = FALSE)  


#now do 2 sample test
#research question: price of brick flats is higher on average
#than the price of panel flats
#Statement: mean(brick) - mean(panel) > 0
#H1: mean(brick) - mean(panel) > 0
#H0: mean(brick)-mean(panel) <= 0
#H0T: mean(brick)-mean(panel)=0
table(flat$Type)
#we have one small sample and one large sample
# no asymtotic case, cause both of the sample must be large in order to use 
#no simple z test cause we dont know to population standard deviation


#it can only be a t test
#condition of the t test: variances should be equal ####
#so we need a test before we test the expected 

#test the varainces
#F test
#H0: var(brick) - var(panel) = 0
#calculate it manually at home
var.test(x=flat$Price[flat$Type == "Brick"],
         y = flat$Price[flat$Type =="Panel"])
#p_value is almost 0, in this way we reject H0,
#the variances of brick and panel flats are not the same

#the simple t test is not suitable in that case
#but there is a Welch t test, we can apply it in those cases
#when the variances are not equal 

#H0T: mean(brick) - mean(panel)=0
#we should follow brick-panel (if the other way round would be a left sided case)
t.test(x=flat$Price[flat$Type == "Brick"],
       y = flat$Price[flat$Type =="Panel"],
       var.equal = FALSE) #use Welch here
#p-value is almost 0, so we reject H0T and H0,
#briack flats are more expensive than the panels. ###very important #human part of the data analysis
