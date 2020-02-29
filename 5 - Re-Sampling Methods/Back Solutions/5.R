library(ISLR)
library(MASS)

#a
attach(Default)
fix(Default)
summary(Default$default)
glm.fit <- glm(default~income+balance, data = Default, family = binomial)

#b
train <- sample(nrow(Default),7000)
train.data <- Default[train,]
test.data <- Default[-train,]

glm.fit.ii <- glm(default~income+balance, data = train.data, family = binomial)
glm.fit.ii.prob <- predict(glm.fit.ii,test.data)
glm.fit.ii.pred <- as.factor(ifelse(glm.fit.ii.prob>0.5,"Yes","No"))

table(glm.fit.ii.pred,test.data[,"default"])
mean(glm.fit.ii.pred!=test.data[,"default"])

#d
glm.fit3 <- (glm(default~., data = train.data, family = binomial()))
glm.pred3 <- as.factor(ifelse(predict(glm.fit3, test.data)>0.5,"Yes","No"))
mean(glm.pred3!=test.data$default)
