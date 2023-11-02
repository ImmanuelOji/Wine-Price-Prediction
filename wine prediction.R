wine = read.csv('wine.csv')
str(wine)
summary(wine)
## using AGST to predict price
## regression model
model1 = lm(Price ~ AGST, data=wine)
summary(model1)
# residuals or error terms
model1$residuals
## sum of errors or SSE
SSE = sum(model1$residuals^2)
SSE
## adding another variable to our regression model
model2 = lm(Price ~ AGST + HarvestRain, data=wine)
summary(model2)
## R squared and adjustable R Squared increased
## showing that adding harvest rain improved our model
## sum of errors SSE
SSE = sum(model2$residuals^2)
SSE
## build a model with all independent variables
model3 = lm(Price ~ AGST + HarvestRain+ WinterRain + Age + FrancePop, data = wine)
summary(model3)
## SSE
SSE = sum(model3$residuals^2)
SSE


## assignment 
model4 = lm(Price ~ HarvestRain + WinterRain, data = wine)
summary(model4)


## removing France pop
model5 = lm(Price ~ AGST + HarvestRain + WinterRain + Age, data = wine )
summary(model5)
SSE = sum(model5$residuals^2)
SSE


## correlation between winter rain and price
cor(wine$WinterRain, wine$Price)
## correlation between age and France population
cor(wine$Age, wine$FrancePop)
## correlation between all the variables
cor(wine)



## loading the test data
wineTest = read.csv('wine_test.csv')
str(wineTest)
## predicting for the new data using model5
predictTest = predict(model5, newdata = wineTest)
predictTest
## R squared value for our test set
SSE = sum((wineTest$Price - predictTest)^2)
SST = sum((wineTest$Price - mean(wine$Price))^2)
1-SSE/SST
