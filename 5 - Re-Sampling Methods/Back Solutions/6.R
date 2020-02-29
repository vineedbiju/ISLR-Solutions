library(ISLR)
library(MASS)
library(boot)

attach(Default)
glm.fit <- glm(default~income+balance,data = Default, family = binominal)
summary(glm.fit)

boot.fn = function(Data,index){
  glm.fit <- glm(default~income+balance, data = Data, subset = index, family = binomial)
  coef(glm.fit)
}

boot(Default,boot.fn, 1000) 

boot.fn(Default, sample(10000,500, replace = T))



