library(ISLR) 
library(boot) #boot for cv.___() functions
attach(Auto)
set.seed(1)

train = sample(nrow(Auto),196)

glm.fit <- glm(mpg~horsepower, data= Auto) #since family not mentioned, linear regression done
coef(glm.fit) #if checked with lm(, yeilds the same coefficients)

#glm( used becasue the cv.glm() can be used which gives cross validation error

cv.err <- cv.glm(Auto,glm.fit)

cv.err$delta #Cross Validation Error
# Here the both numbers are identical upto two decimal points

#Increasingly complex Data

cv.error = rep(0,5)
for (i in 1:10) {
  glm.fit <- glm(mpg~poly(horsepower,i),data = Auto)
  cv.error[i] <-cv.glm(Auto,glm.fit)$delta[1]
}

print(cv.error)

plot(cv.error)



