library(ISLR)
library(MASS)
library(boot) #for boot()

attach(Portfolio)
fix(Portfolio)

alpha.fn = function(data,index) {  #Function passes data set and 1:100
  X = data$X[index]         #X is an array
  Y = data$Y[index]         #Y is an array
  return ((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}

alpha.fn(Portfolio,1:100) #check to see if this is working

set.seed(3)
alpha.fn(Portfolio,sample(100,100,replace = T))

boot(Portfolio,alpha.fn,R=1000)



#Estimating the Accuracy of a Linear Regression Model


boot.fn <- function(data,index){
  return(coef(lm(mpg~horsepower,data = data, subset = index)))
}

boot.fn(Auto,1:392)

boot.fn(Auto, sample(392,392,replace = T))
boot(Auto,boot.fn, R=1000)
plot(boot(Auto,boot.fn, R=1000))

summary (lm(mpg~horsepower ,data=Auto))$coef

