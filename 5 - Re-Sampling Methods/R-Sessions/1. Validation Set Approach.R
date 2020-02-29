library(ISLR)

attach(Auto)

set.seed(1)
train = sample(nrow(Auto),196)

lm.fit = lm(mpg~horsepower, data = Auto, subset = train) #using subset function to train

mean((mpg-predict(lm.fit,Auto[-train]))^2) #MSE = 24.168

lm.fit2 <- lm(mpg~poly(horsepower,2), data = Auto, subset = train) #Quadratic
mean((mpg - predict(lm.fit2,Auto[-train]))^2) #MSE - 19.059

lm.fit3 <- lm(mpg~poly(horsepower,3), data = Auto, subset = train) #Cubic
mean((mpg-predict(lm.fit3, Auto[-train]))^2) #MSE 19,0954


