#SSC 442 Class Ex 1
#Team Awesome (19) - Sayem Lincoln, Joshua Schwimmer, John Townshend.


library(dplyr)

#Initial regression model
data <- read.csv("bank.csv")

data$balance.f <- factor(data$balance)
is.factor(data$balance.f)

summary(lm(balance~ age+day+duration+campaign+previous, data = data))

# F-test
res.ftest1 <- var.test(age ~ y, data =data)
res.ftest2 <- var.test(balance~y  , data =data)
res.ftest3 <- var.test(day ~ y, data =data)
res.ftest4 <- var.test(duration ~ y, data =data)
res.ftest5 <- var.test(campaign ~ y, data =data)
res.ftest6 <- var.test(previous ~ y, data =data)

res.ftest1
#The p-value of F-test is p = 2.682e-16 which is lesser than the significance level 0.05. 
#In conclusion, there is a significant difference between the two variances.

res.ftest2
#The p-value of F-test is p = 5.734e-11 which is greater than the significance level 0.05. 
#In conclusion, there is no significant difference between the two variances.

res.ftest3
#The p-value of F-test is p = 0.9707 which is greater than the significance level 0.05. 
#In conclusion, there is no significant difference between the two variances.

res.ftest4
#The p-value of F-test is p < 2.2e-16 which is lesser than the significance level 0.05. 
#In conclusion, there is a significant difference between the two variances.

res.ftest5
#The p-value of F-test is p < 2.2e-16 which is lesser than the significance level 0.05. 
#In conclusion, there is a significant difference between the two variances.

res.ftest6
#The p-value of F-test is p = 4.938e-14 which is lesser than the significance level 0.05. 
#In conclusion, there is a significant difference between the two variances.

#Resulting model 
data1 <- subset(data, select = c(6,10))

data1$balance.f <- factor(data1$balance)
is.factor(data1$balance.f)

summary(lm(balance~day, data = data1))

#The resulting model ouputs the following-
#Residual standard error: 3010 on 4519 degrees of freedom
#Multiple R-squared:  7.529e-05,	Adjusted R-squared:  -0.000146 
#F-statistic: 0.3403 on 1 and 4519 DF,  p-value: 0.5597

#comparing it to the previous model's output -
#Residual standard error: 2999 on 4515 degrees of freedom
#Multiple R-squared:  0.008092,	Adjusted R-squared:  0.006993 
#F-statistic: 7.367 on 5 and 4515 DF,  p-value: 6.893e-07

#We can see that the F-statistic ,Adjusted R-squared, and p-value went down and 
#Residual standard error value, Multiple R-squared went up.

#As the In general, a F-statistic is a ratio of two quantities that are 
#expected to be roughly equal under the null hypothesis, which produces an 
#F-statistic of approximately 1. So, the new model's F statistic is closer to 1
#thus the new model has a better F statistic and the new model is an improvement on 
#the previous model.  
