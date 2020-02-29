library(ISLR)
library(MASS)
library(boot)

set.seed(3)

x <- rnorm(100)
y <- x-2*x^2+rnorm(100)

plot(x,y)

qns <- data.frame(x,y)

cv.err = rep(0,4)
for (i in 1:4) {
  glm.fit <- glm(y~poly(x,i), data = qns)
  cv.err[i] <- cv.glm(qns, glm.fit)$delta[1]
  print(cv.err[i])
}

plot(cv.err, type = "b")
