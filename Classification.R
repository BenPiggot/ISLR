library(ISLR)
data(Smarket)
names(Smarket)
pairs(Smarket, col=Smarket$Direction)
glm.fit <- glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket,
               family=binomial)
summary(glm.fit)
glm.probs <- predict(glm.fit,type="response")
glm.probs[1:5]
glm.pred <- ifelse(glm.probs>.5, "Up", "Down")
head(glm.pred, 100)
table(glm.pred, Smarket$Direction)
train <- Smarket$Year < 2005
glm.fit <- glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket,
               family=binomial, subset=train)
summary(glm.fit)
glm.probs <- predict(glm.fit, newdata=Smarket[!train,],
                     type="response")
glm.pred <- ifelse(glm.probs>.5, "Up", "Down")
Direction.2005 <- Smarket$Direction[!train]
table(glm.pred,Direction.2005)

glm.fit <- glm(Direction ~ Lag1+Lag2, data=Smarket,
               family=binomial, subset=train)
summary(glm.fit)
glm.probs <- predict(glm.fit, newdata=Smarket[!train,],
                     type="response")
glm.pred <- ifelse(glm.probs>.5, "Up", "Down")
Direction.2005 <- Smarket$Direction[!train]
table(glm.pred,Direction.2005)

library(MASS)
lda.fit <- lda(Direction~Lag1+Lag2, data=Smarket, subset=Year<2005)
lda.fit
plot(lda.fit)
Smarket.2005 <- subset(Smarket,Year==2005)
lda.pred <- predict(lda.fit, Smarket.2005)
lda.pred[1:5,]
class(lda.pred)
data.frame(lda.pred)[1:40,]
table(lda.pred$class, Smarket.2005$Direction)
mean(lda.pred$class == Smarket.2005$Direction)
data(Caravan)
dim(Caravan)
standardized <- scale(Caravan[,-86])
var(standardized[,2])
test <- 1:1000
train <- standardized[-test,]
test <- standardized[test,]

train.Y <- Caravan$Purchase[-c(1:1000)]
test.Y <- Caravan$Purchase[c(1:1000)]
set.seed(1)
install.packages("class")
library(class)
knn.pred <- knn(train, test, train.Y, k=3)
table(knn.pred, test.Y)
mean(knn.pred)
mean(test.Y != knn.pred)
data(Weekly)
dim(Weekly)
head(Weekly)
boxplot(Weekly$Lag1, Weekly$Direction)
boxplot(Weekly$Volume, Weekly$Direction)

ggplot(Weekly, aes(x=Lag1, y=Lag2)) + geom_point(aes(colour=Weekly$Direction))
glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, 
               data=Weekly, family=binomial)
summary(glm.fit)
train <- Weekly[Weekly$Year<2009,]
test <- Weekly[Weekly$Year>=2009,]
plot(Weekly$Volume, Weekly$Lag3)
lda.fit <- lda(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data=Weekly)
lda.fit
str(test)
str(train)
glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, 
               data=train, family=binomial)
glm.probs <- predict(glm.fit, test, type="response")
glm.pred <- ifelse(glm.probs>.35, "Up", "Down")
table(glm.pred,test$Direction)
mean(glm.pred == test$Direction)
table(pred.glm)
lda.fit <- lda(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, 
               data=train)
pred.lda <- predict(lda.fit, test)
table(pred.lda$class, test$Direction)
qda.fit <- qda(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, 
               data=train)
pred.qda <- predict(qda.fit, test)
table(pred.qda$class, test$Direction)
mean(pred.qda$class != test$Direction)

test.Y <- Weekly$Direction[Weekly$Year>=2009]
train.Y <- Weekly$Direction[Weekly$Year<2009]
str(test.Y)
str(test)
length(train.Y)
length(train)
knn.pred <- knn(train, test, train.Y, k=1)
table(knn.pred, test.Y)
mean(knn.pred)

data(Auto)
median(Auto$mpg)
glimpse(A uto)
str(Auto)
Auto$mpg01 <- ifelse(Auto$mpg>22.75, 1, 0)
Auto$mpg01 <- as.factor(Auto$mpg01)
ggplot(Auto, aes(x=mpg, y=horsepower)) + geom_point(aes(colour=Auto$mpg01))
ggplot(Auto, aes(x=weight, y=mpg)) + geom_point(aes(colour=Auto$mpg01))
ggplot(Auto, aes(x=weight, y=acceleration)) + geom_point(aes(colour=Auto$mpg01))
train.A <- Auto[-as.numeric(rownames(test.A)),]
test.A <- Auto[seq(2,392,by=4),]
lda.fit <- lda(mpg01 ~ horsepower + weight + displacement + year,
               data=train.A)
lda.pred <- predict(lda.fit,test.A)
table(lda.pred$class, test.A$mpg01)
mean(lda.pred$class == test.A$mpg01)
table(test.A$mpg01)
qda.fit <- qda(mpg01 ~ horsepower + weight + displacement + year,
               data=train.A)
qda.pred <- predict(qda.fit,test.A)
table(qda.pred$class, test.A$mpg01)
mean(qda.pred$class == test.A$mpg01)
glm.A <- glm(mpg01 ~ horsepower + weight, 
             data=train.A, family=binomial)
summary(glm.A)
glm.probs <- predict(glm.A, test.A, type="response")
glmA.pred <- ifelse(glm.probs>.5,1,0)
table(glmA.pred,test.A$mpg01)

dim(Auto)
standardized <- scale(Auto[,-c(9,10)])
var(standardized[,2])
test.A <- standardized[seq(2,392,by=4),]
train.A <- standardized[-as.numeric(rownames(test.A)),]
train.AY <- Auto$mpg01[-as.numeric(rownames(test.A))]
test.AY <- Auto$mpg01[seq(2,392,by=4)]

knn.pred <- knn(train.A, test.A, train.AY, k=1)
table(knn.pred, test.AY)
mean(knn.pred == test.AY)

Power <- function(x) {
    print(2^3)
}
Power(x)
Power2 <- function(x,a) {
    print(x^a)
}
Power2(4,8)
Power2(10,3)

Power3 <- function(x,a) {
    result <- x^a
    return(result)
}

Power3(3,5)
x <- 1:100
Power3(x,2)
plot(x,Power3(x,2),xlab="Integer", ylab="Integer Squared (Log Scale)",
     type="n",log="y")
lines(x,Power3(x,2),col="red")
log(10000)

data(Boston)
str(Boston)
?Boston
Boston$chas <- as.factor(Boston$chas)
sample.B <- sample(1:dim(Boston)[1],size=dim(Boston)[1]/2,replace=F)
train.B <- Boston[-sample.B,]
test.B <- Boston[sample.B,]
glm.B <- glm(chas ~., data=Boston, family=binomial)
summary(glm.B)
glm.B <- glm(chas ~ indus + tax + medv - 1, data=Boston, family=binomial)
summary(glm.BT)
table(Boston$chas)

glm.BT <- glm(chas ~ indus + tax + medv - 1, data=train.B, family=binomial)
bos.prob <- predict(glm.BT, test.B)
glmB.pred <- ifelse(bos.prob>.5,1,0)
table(glmB.pred)

train.BY <- Boston$chas[-sample.B]
test.BY <- Boston$chas[sample.B]
knn.pred <- knn(train.B, test.B, train.BY, k=3)
table(knn.pred, test.BY)

lda.bos <- lda(chas ~ indus + tax + medv, data=train.B)
lda.bos
lda.Bpred <- predict(lda.bos,test.B)
table(lda.Bpred$class,test.B$chas)

qda.bos <- qda(chas ~ indus + tax + medv, data=train.B)
qda.bos
qda.Bpred <- predict(qda.bos,test.B)
table(qda.Bpred$class,test.B$chas)

median(Boston$crim)
Boston$hiCrime <- ifelse(Boston$crim > 0.25651, 1, 0)
table(Boston$hiCrime)
Boston$hiCrime <- as.factor(Boston$hiCrime)
glm.C <- glm(hiCrime ~., data=train.B, family=binomial)
summary(glm.C)
glm.C <- glm(hiCrime ~ indus + age + lstat + nox, data=train.B, family=binomial)
summary(glm.C)
crime.prob <- predict(glm.C, test.B)
crime.pred <- ifelse(crime.prob > 0.4, 1, 0)
table(crime.pred, test.B$hiCrime)
mean(crime.pred == test.B$hiCrime)
glimpse(Boston)
?Boston

lda.crime <- lda(hiCrime ~ age + lstat + indus, data=train.B)
lda.crime
lda.Cpred <- predict(lda.crime,test.B)
table(lda.Cpred$class,test.B$hiCrime)
mean(lda.Cpred$class == test.B$hiCrime)

qda.crime <- qda(hiCrime ~ lstat + age  , data=train.B)
qda.crime
qda.Cpred <- predict(qda.crime,test.B)
table(qda.Cpred$class,test.B$hiCrime)

train.CY <- Boston$hiCrime[-sample.B]
test.CY <- Boston$hiCrime[sample.B]
knn.pred <- knn(train.B, test.B, train.CY, k=3)
table(knn.pred, test.CY)
mean(knn.pred==test.CY)
ggplot(Boston, aes(x=lstat, y=age)) + geom_point(aes(color = Boston$hiCrime))
ggplot(train.B, aes(x=lstat, y = age)) + geom_point(aes(color = train.B$hiCrime))
ggplot(test.B, aes(x=lstat, y = age)) + geom_point(aes(color = test.B$hiCrime))
