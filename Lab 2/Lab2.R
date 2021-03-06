#SSC 442 Lab 2
#Team Awesome (19) - Sayem Lincoln, Joshua Schwimmer, John Townshend.


library(ggplot2)
library(cdata)

#Lab 2
#Excercise 1
#Part 1 
#ameslist <- read.table("https://msudataanalytics.github.io/SSC442/assets/ames.csv",header = TRUE,sep = ",")
ameslist<-read.csv("ames.csv", header = TRUE, sep = ",")


dfAmes <- data.frame(ameslist)
dfAmesT <- t(dfAmes)
cnt <- 0
lsAmes <- vector()
scatter <- vector()

#algorithm for seperating the int columns from string
#NOTE: we use the the transpose because the for loop goes by row.

for(row in dfAmesT){
  
  #print(integer(row))
  cnt <- cnt+1
  if(is.na(row) == FALSE) {
    n <- as.numeric(row)
    if(is.numeric(n)==TRUE && is.na(n) == FALSE){
      lsAmes <- c(lsAmes, dfAmes[cnt])
      #print(n)
    }
  }
  #once we loop through the first value in each column then we stop the loop
  
  if(cnt == NROW(dfAmesT)){
    break
  }
}

#converting the list to a df
Ames <- data.frame(lsAmes)
keep <- c( "LotArea", "OverallQual", "OverallCond", "GrLivArea", "BedroomAbvGr", 
           "TotalBsmtSF", "TotRmsAbvGrd", "Fireplaces", "GarageCars", "PoolArea",
           "GarageArea", "SalePrice")

dfAmesCleaner <- Ames[keep]
dfAmesCleanerT <- t(dfAmesCleaner)

#The Ames dataframe is saved. And the text file for the Ames dataframe is also uploadecd to the repo. 


#Part 2 
#sactter plot matrix
panel.cor <- function(x, y){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt)
}
upper.panel<-function(x, y){
  points(x,y, pch = 19)
}

pairs(dfAmesCleaner[,1:12], upper.panel = upper.panel, lower.panel = panel.cor)

#Part 2
#I believe there is correlation between the variables and SalePrice, as a house condition should depend on its price.
#I believe all of them should have some form of correlations with SalePrice, each of them influencing the price and vice versa.

#Part 3
#The ones that I believe to have a correlation to SalePrice are -
# "LotArea" "OverallQual"  "OverallCond"  "GrLivArea"    "BedroomAbvGr" "TotalBsmtSF"  "TotRmsAbvGrd" "GarageArea"   
#As these graphs show a scatter plot compared to the others when plotted in a scatter plot matrix
#as from  a scatter plot a coorelation causation trejectory can be formed, 
#so I believe these Variables are correlated to SalePrice. So from Part 2 only these three variables 
#does not show any correlation - "Fireplaces"   "GarageCars"   "PoolArea" whereas the rest do.

#Part 4
plot(dfAmesCleaner$SalePrice, dfAmesCleaner$GrLivArea, main="Sale Prive vs GrLivArea Graph",
     xlab="Sale Price", ylab="GrLivArea", pch=19)+abline(lm(dfAmesCleaner$GrLivArea~ dfAmesCleaner$SalePrice, data=dfAmesCleaner), col="red")# regression line (y~x)

#The largest outlier that is above the regression line - 
#it is the one with GrLiveArea of 5642 and SalePrice of 160000
#Produce the other information about this house?
#The other informations about the house - 
#"LotArea"  - 63887    "OverallQual" - 10  "OverallCond" - 5  "GrLivArea" - 5642  
#"BedroomAbvGr" - 3 "TotalBsmtSF"- 6110  "TotRmsAbvGrd"-12  "Fireplaces" - 3 
#"GarageCars" - 3   "PoolArea" - 480    "GarageArea" - 1418  "SalePrice" - 160000 



#Exercise 2 
#Part 1
#Simple linear regression - running SalePrice against Garage Area 
lm.fit1 = lm(dfAmesCleaner$SalePrice~dfAmesCleaner$GarageArea)

#Part 2
lm.fit2 = lm(dfAmesCleaner$SalePrice ~ MSSubClass+LotFrontage+LotArea
             +OverallQual+OverallCond+YearBuilt+YearRemodAdd+MasVnrArea+
               BsmtFinSF1+ BsmtFinSF2+BsmtUnfSF+TotalBsmtSF+X1stFlrSF+
               X2ndFlrSF+LowQualFinSF+GrLivArea+BsmtFullBath+BsmtHalfBath+
               FullBath+HalfBath+BedroomAbvGr+KitchenAbvGr+TotRmsAbvGrd+
               Fireplaces+GarageYrBlt+GarageCars+GarageArea+WoodDeckSF+
               OpenPorchSF+EnclosedPorch+X3SsnPorch+ScreenPorch+PoolArea+
               MiscVal+MoSold+YrSold, data = Ames)
summary(lm.fit2)

#Is there a relationship between the predictors and the response?
#Ans - Yes there is a relation ship between them, they are corraleted to 
#each other. If there was no correaltion then residuals would not be possible. 

#Which predictors appear to have a statistically significant
#relationshipto the response?
#Ans - The followings processed a statistical value -
#Id, MSSubClass, LotFrontage, LotArea, OverallQual, OverallCond, YearBuilt, YearRemodAdd
# "MasVnrArea, BsmtFinSF1, BsmtFinSF2, BsmtUnfSF, X1stFlrSF, X2ndFlrSF, LowQualFinSF
#"BsmtFullBath, BsmtHalfBath, FullBath, HalfBath"      "BedroomAbvGr"  "KitchenAbvGr"  
#"TotRmsAbvGrd" "Fireplaces"    "GarageYrBlt"   "GarageCars"    "GarageArea"    "WoodDeckSF"    
#"OpenPorchSF", "EnclosedPorch" "X3SsnPorch"   "ScreenPorch"   "PoolArea"      "MiscVal"       
#"MoSold", "YrSold", "SalePrice"    
#so the following should have be statistically significant. 

#What does the coefficient for the year variable suggest?
#Ans - It suggests that Year is negatively correlated to SalePrice, meaning 
#as the number of years passed some property values went down, which is a 
#common notion for property prices, and that is relfected by the coeffecient 
# that is processed out from the residuals and the model.


#Exercise 3
plot(lm.fit2)

#Comment on any problems you see with the fit. 
#Some of the variables have NA values so those variables do not form
#a residual output. So, forming a regression on all Variavbles does 
#not better the model.

#Do the residual plots suggest any unusually large outliers? 
#Ans- Yes there are some large outliers in all the graphs. 

#Does the leverage plot identify any observations with unusually high leverage?
#Ans - Yes it does, there is a levargae value of 0.7. which is 
#a very high outlier. 

#Exercise 4
#linear regression models with some well-chosen interaction effects
#linear models for ***
lm.fit3 = lm(dfAmesCleaner$SalePrice ~ MSSubClass+LotArea
             +OverallQual+OverallCond+YearBuilt+MasVnrArea+X1stFlrSF+
               X2ndFlrSF+HalfBath+KitchenAbvGr+TotRmsAbvGrd+GarageCars, data = Ames)
summary(lm.fit3)
plot(lm.fit3)

#linear models for **
lm.fit4 = lm(dfAmesCleaner$SalePrice ~ 
               BsmtFinSF1+ BsmtFullBath+KitchenAbvGr+ScreenPorch, data = Ames)
summary(lm.fit4)
plot(lm.fit4)

#linear models for *
lm.fit5 = lm(dfAmesCleaner$SalePrice ~ Fireplaces+WoodDeckSF+PoolArea , data = Ames)
summary(lm.fit5)
plot(lm.fit5)

#linear models for .
lm.fit6 = lm(dfAmesCleaner$SalePrice ~ LotFrontage, data = Ames)
summary(lm.fit6)
plot(lm.fit6)

#Do any interactions appear to be statistically significant?
#Ans - Yes, the linear regression model 3, seems to have  a more 
#of a statistical significance then that of the rest, as the data 
#shows more correlation, then the rest of the models - as the other 
#model's data reprersntation is all over the chart. 

#Part 5
#trying ln(x) 
log_Ames <- data.frame(log(Ames[,1:38]+1))
#linear model for ln(x)
lm.fit7 = lm(SalePrice ~ MSSubClass+LotFrontage+LotArea
             +OverallQual+OverallCond+YearBuilt+YearRemodAdd+MasVnrArea+
               BsmtFinSF1+ BsmtFinSF2+BsmtUnfSF+TotalBsmtSF+X1stFlrSF+
               X2ndFlrSF+LowQualFinSF+GrLivArea+BsmtFullBath+BsmtHalfBath+
               FullBath+HalfBath+BedroomAbvGr+KitchenAbvGr+TotRmsAbvGrd+
               Fireplaces+GarageYrBlt+GarageCars+GarageArea+WoodDeckSF+
               OpenPorchSF+EnclosedPorch+X3SsnPorch+ScreenPorch+PoolArea+
               MiscVal+MoSold+YrSold, data = log_Ames)
summary(lm.fit7)
#plot
plot(lm.fit7)


#trying x^(2)
xsquared_Ames <- data.frame((Ames[,1:38]+1)^2)
#linear model for x^(2)
lm.fit8 = lm(SalePrice ~ MSSubClass+LotFrontage+LotArea
             +OverallQual+OverallCond+YearBuilt+YearRemodAdd+MasVnrArea+
               BsmtFinSF1+ BsmtFinSF2+BsmtUnfSF+TotalBsmtSF+X1stFlrSF+
               X2ndFlrSF+LowQualFinSF+GrLivArea+BsmtFullBath+BsmtHalfBath+
               FullBath+HalfBath+BedroomAbvGr+KitchenAbvGr+TotRmsAbvGrd+
               Fireplaces+GarageYrBlt+GarageCars+GarageArea+WoodDeckSF+
               OpenPorchSF+EnclosedPorch+X3SsnPorch+ScreenPorch+PoolArea+
               MiscVal+MoSold+YrSold, data = xsquared_Ames)
summary(lm.fit8)
#plot
plot(lm.fit8)


#trying square root
root_Ames <- data.frame(sqrt((Ames[,1:38]+1)))
#linear model for square root
lm.fit9 = lm(SalePrice ~ MSSubClass+LotFrontage+LotArea
             +OverallQual+OverallCond+YearBuilt+YearRemodAdd+MasVnrArea+
               BsmtFinSF1+ BsmtFinSF2+BsmtUnfSF+TotalBsmtSF+X1stFlrSF+
               X2ndFlrSF+LowQualFinSF+GrLivArea+BsmtFullBath+BsmtHalfBath+
               FullBath+HalfBath+BedroomAbvGr+KitchenAbvGr+TotRmsAbvGrd+
               Fireplaces+GarageYrBlt+GarageCars+GarageArea+WoodDeckSF+
               OpenPorchSF+EnclosedPorch+X3SsnPorch+ScreenPorch+PoolArea+
               MiscVal+MoSold+YrSold, data = root_Ames)
summary(lm.fit9)
#plot
plot(lm.fit9)

#Do any of these make sense to include in a model of SalePrice? 
#Ans - Yes, it does. The log function can be used to plot prices for 
#market inflation - as the log function is used for calculating elastic demand
#thus making adjustments within the log function depending on the inflation rate
#can help us formulate a regression model for SalePrice of a house during and after inflation.
#So, much like the above example other  regression models for economic situations that impact the 
#price of a house can also be plotted out - by using the log, square and square root functions. 

#Comment on your findings.
#The graphs are a representation of the data being processed through
#all three individual functions - they all show correlation and none
#of them present a graph with large deviation, some of the plots are dispersed  
#compared to the other ones but a regression line can still be plotted out.



