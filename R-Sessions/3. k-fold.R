library(ISLR) 
library(boot) #boot for cv.___() functions
attach(Auto)
set.seed(1)

train = sample(nrow(Auto),196)

cv.error.10 <- rep(0,10)

for (i in 1:10) {
  glm.fit <- glm(mpg~poly(horsepower,i), data= Auto)
  cv.error.10[i] <- cv.glm(Auto,glm.fit, K=10)$delta[1]
}
plot(cv.error.10)
