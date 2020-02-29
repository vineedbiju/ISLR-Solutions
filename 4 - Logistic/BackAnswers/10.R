library(ISLR)
library(MASS)
fix(Weekly)

#a
str(Weekly)
summary(Weekly)
pairs(Weekly, col = Weekly$Direction)

#b
attach(Weekly)
glm.fit <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = Weekly,
               family = binomial)

summary(glm.fit)

#No predictors,except Lag2, appear to be significant at 5% level

#c
contrasts(Direction)

for (i in 1:10){
  glm.pred <- ifelse(predict(glm.fit, type = "response")>=(i/10),"Up","Down")
  print(c('for', i/10, mean(glm.pred!=Direction)))
  
}
remove(i)
table(glm.pred, Direction)

#d

train = Year<2008
glm.fit2 <- glm(Direction~Lag2, data = Weekly,family = binomial,
               subset = train)
glm.pred2 <- ifelse(predict(glm.fit2,Weekly[!train,])>0.5, "Up","Down")

mean(glm.pred2!=Direction[!train])


#e
library(MASS)
lda.fit <-lda(Direction~Lag2, data = Weekly, subset = train)
lda.fit
lda.pred <- predict(lda.fit, Weekly[!train,], type = 'response')

table(lda.pred$class,Direction[!train])
mean(lda.pred$class!=Direction[!train])

#f
?knn
qda.fit <-qda(Direction~Lag2, data = Weekly, subset = train)
qda.fit
qda.pred <- predict(qda.fit, Weekly[!train,], type = 'response')

table(qda.pred$class,Direction[!train])
mean(qda.pred$class!=Direction[!train])

#g

library(class)

train.knn <- cbind(Weekly[train,3])
test.knn <- cbind(Weekly[!train,3])

train.Direction <- Weekly[train,9]
test.Direction <- Weekly[!train,9]

knn.pred <- knn(train.knn,test.knn,train.Direction,k=1)

mean(knn.pred != test.Direction)
