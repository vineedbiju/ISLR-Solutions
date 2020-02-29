library(MASS)
library(boot)
u = mean(Boston$medv)
se = sd(Boston$medv)/sqrt(nrow(Boston))

attach(Boston)
boot.fn <- function(Data, index) {
  mean(Data[index])
}

boot.fn(medv, sample(506,400, replace = T))

boot(medv, boot.fn, 1000)
