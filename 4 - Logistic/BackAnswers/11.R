#a
library(ISLR)
library(MASS)
attach(Auto)
mpg01 <- as.factor(ifelse(mpg>median(mpg),1,0))
data <- data.frame(Auto,mpg01)

#b
pairs(data[,-10], col = mpg01)

for (i in 1:ncol(data)){ #boxplot of all variables vs mpg01
  boxplot(data[,i]~mpg01, ylab = names(data)[i])
  readline()
}
round(cor(data[,-c(9,10)]),1)
#High Correlation between variables too
#selecting displacement, horsepower, acceleration, weight

#Next improvement - 1. y labels 2. Main Label 3. Check if y axix is numeric

#c
train <- sample(nrow(data),round(0.7*nrow(data)),0)
train.data <- data[train,]
test.data <- data[-train,]

#d
lda.fit <- lda(mpg01~acceleration+displacement+weight+horsepower, data = train.data,
               family = binomial)
mean(test.data[,"mpg01"]!=predict(lda.fit,test.data,type = 'response')$class)
table(test.data[,"mpg01"],predict(lda.fit,test.data,type = 'response')$class)

#e
qda.fit <- qda(mpg01~acceleration+displacement+weight+horsepower, data = train.data,
               family = binomial)
mean(test.data[,"mpg01"]!=predict(qda.fit,test.data,type = 'response')$class)
table(test.data[,"mpg01"],predict(qda.fit,test.data,type = 'response')$class)

#f
glm.fit <-glm(mpg01~acceleration+displacement+weight+horsepower, data = train.data,
              family = binomial)
glm.pred <- as.factor(ifelse(predict(glm.fit, test.data, type = "response")>0.5,1,0))
mean(test.data[,"mpg01"]!=glm.pred)
table(test.data[,"mpg01"],glm.pred)

#g
library(class)
train.Y <- train.data[,"mpg01"]
train.data <- train.data[,-c(9,10)]
test.Y <- test.data[,"mpg01"]
test.data <- test.data[,-c(9,10)]

knn.err = rep(0,30)
for (i in 1:30) {
  knn.pred <- knn(train.data,test.data,train.Y, k =i)
  knn.err[i] = mean(knn.pred!=test.Y)
}

plot(knn.err)

