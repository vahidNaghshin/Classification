library(MASS)
library(ISLR)
library(fpp2)

#Problem 4-10
names(Weekly)
attach(Weekly)
autoplot(ts(Weekly[, "Volume"], start = 1990, frequency = 52))+ylab("Weekly volume in billions dollars")
autoplot(ts(Weekly[, "Today"], start = 1990, frequency = 52))+ylab("Weekly percentage return")
cor(Weekly[2:8])
glm.fit=glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume , data=Weekly ,family=binomial)
summary(glm.fit)

glm.probs=predict(glm.fit,type="response")
glm.pred[glm.probs >.5]="Up"
table(glm.pred, Direction)

train=(Year <2009)
Weekly.2008= Weekly [train ,]
Weekly.held_out= Weekly [!train ,]
Direction.held_out=Direction[!train]
#Log reg
glm.fit <- glm(Direction ~ Lag2 ,data=Weekly, family=binomial,subset=train)
glm.probs=predict(glm.fit,Weekly.held_out,type="response")
glm.pred=rep("Down",length(Weekly.held_out))
glm.pred[glm.probs >.5]="Up"
x =table(glm.pred,Direction.held_out)
(x[1,1]+x[2,2])/(x[1,1]+x[2,2]+x[1,2]+x[2,1])
#LDA
glm.fit <- lda(Direction ~ Lag2 ,data=Weekly, family=binomial,subset=train)
glm.probs=predict(glm.fit,Weekly.held_out,type="response")
lda.class=glm.probs$class
x =table(lda.class ,Direction.held_out)
(x[1,1]+x[2,2])/(x[1,1]+x[2,2]+x[1,2]+x[2,1])
#QDA
glm.fit <- qda(Direction ~ Lag2 ,data=Weekly, family=binomial,subset=train)
glm.probs=predict(glm.fit,Weekly.held_out,type="response")
glm.probs
qda.class=glm.probs$class
x =table(qda.class ,Direction.held_out)
(x[1,1]+x[2,2])/(x[1,1]+x[2,2]+x[1,2]+x[2,1])
#KNN
library(class)
set.seed (1)
predictor.train = Lag2[train]
predictor.test = Lag2[!train]
Direction.train=Direction[train]
knn.pred=knn(data.frame(predictor.train), data.frame(predictor.test), Direction.train, k=1)
x = table(knn.pred ,Direction.held_out)
#total correct prediction ratio
(x[1,1]+x[2,2])/(x[1,1]+x[2,2]+x[1,2]+x[2,1])

#Problem 4-11
attach(Auto)
idx <- mpg>median(mpg)
mpg01 <- mpg
mpg01[idx]<-1
mpg01[!idx]<-0
new.data <- data.frame(mpg01, Auto)
pairs(new.data, 
      main="Simple Scatterplot Matrix")
cor(displacement, horsepower)
train.data <- new.data[1:75,]
test.data <- new.data[77:101,]
#LDA
glm.fit <- lda(mpg01 ~ horsepower+acceleration ,data=train.data, family=binomial)
glm.probs=predict(glm.fit,test.data,type="response")
lda.class=glm.probs$class
x =table(lda.class ,test.data[, 'mpg01'])
(x[1,1]+x[2,2])/(x[1,1]+x[2,2]+x[1,2]+x[2,1])
#QDA
glm.fit <- qda(mpg01 ~ horsepower+acceleration ,data=train.data, family=binomial)
glm.probs=predict(glm.fit,test.data,type="response")
qda.class=glm.probs$class
x =table(qda.class ,test.data[, 'mpg01'])
(x[1,1]+x[2,2])/(x[1,1]+x[2,2]+x[1,2]+x[2,1])
#Log reg
glm.fit <- glm(mpg01 ~ horsepower+acceleration ,data=train.data, family=binomial)
glm.probs=predict(glm.fit,test.data,type="response")
glm.pred=rep(0,length(test.data))
glm.pred[glm.probs >.5]=1
x =table(glm.pred,test.data[, 'mpg01'])
(x[1,1]+x[2,2])/(x[1,1]+x[2,2]+x[1,2]+x[2,1])
#KNN
library(class)
set.seed (1)
predictor.train = cbind(train.data[, 'horsepower'], train.data[, "acceleration"])
predictor.test = cbind(test.data[, 'horsepower'], test.data[, "acceleration"])
mpg01.train=train.data[, 'mpg01']
knn.pred=knn(data.frame(predictor.train), data.frame(predictor.test), mpg01.train, k=1)
x = table(knn.pred ,test.data[, 'mpg01'])
#total correct prediction ratio
(x[1,1]+x[2,2])/(x[1,1]+x[2,2]+x[1,2]+x[2,1])
#Problem 4-12
attach(Boston)
names(Boston)
idx <- crim>median(crim)
crim01 <- crim
crim01[idx]<-1
crim01[!idx]<-0
new.data <- data.frame(crim01, Boston)
pairs(new.data, 
      main="Simple Scatterplot Matrix")
train.data <- new.data[1:400,]
test.data <- new.data[401:506,]
#LDA
glm.fit <- lda(crim01 ~ zn+indus+nox+rm ,data=train.data, family=binomial)
glm.probs=predict(glm.fit,test.data,type="response")
lda.class=glm.probs$class
x =table(lda.class ,test.data[, 'crim01'])
(x[1,1]+x[2,2])/(x[1,1]+x[2,2]+x[1,2]+x[2,1])
#QDA
glm.fit <- qda(crim01 ~ zn+indus+nox+rm ,data=train.data, family=binomial)
glm.probs=predict(glm.fit,test.data,type="response")
qda.class=glm.probs$class
x =table(qda.class ,test.data[, 'crim01'])
x
(x[1,1]+x[2,2])/(x[1,1]+x[2,2]+x[1,2]+x[2,1])
#Log reg
glm.fit <- glm(crim01 ~ zn+indus+nox+rm ,data=train.data, family=binomial)
glm.probs=predict(glm.fit,test.data,type="response")
glm.pred=rep(0,length(test.data))
glm.pred[glm.probs >.5]=1
x =table(glm.pred,test.data[, 'crim01'])
(x[1,2])/(x[1,1]+x[1,2])






