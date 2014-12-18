install.packages("boot")
library(boot)
data(Auto)
?cv.glm
plot(mpg ~ horsepower, data=Auto)
glm.fit <- glm(mpg ~ horsepower, data=Auto)
cv.glm(Auto,glm.fit)$delta
cv.glm(Auto,glm.fit)
fit <- lm(mpg ~ horsepower, data=Auto)
lm.influence(fit)$h
loocv <- function(fit) {
    h <- lm.influence(fit)$h
    mean((residuals(fit)/(1 - h))^2)
}

loocv(glm.fit)

cv.error <- rep(NA,5)
degree <- 1:5
for(d in degree) {
    glm.fit <- glm(mpg ~ poly(horsepower, d), data=Auto)
    cv.error[d] <- loocv(glm.fit)
}

cv.error
plot(degree, cv.error, type="b")

cv.error10 <- rep(NA,5)
for(d in degree) {
    glm.fit <- glm(mpg ~ poly(horsepower, d), data=Auto)
    cv.error10[d] <- cv.glm(Auto, glm.fit, K=10)$delta[1]
}

cv.error10
plot(degree, cv.error10, type="b")
lines(degree, cv.error, type="b",col="red")

alpha <- function(x,y) {
    vy <- var(y)
    vx <- var(x)
    cxy <- cov(x,y)
    (vy - cxy)/(vx + vy - 2*cxy)
}

data(Portfolio)
alpha(Portfolio$X, Portfolio$Y)

alpha.fn <- function(data,index) {
    with(data[index,],alpha(X,Y))
}

alpha.fn(Portfolio,1:100)

set.seed(1)
alpha.fn(Portfolio, sample(1:100,100,replace=TRUE))
boot.out <- boot(Portfolio,alpha.fn,R=1000)
boot.out
plot(boot.out)

x <- rnorm(100)
boot(100, rnorm,R=100)

data(Default)
str(Default)
glm.def <- glm(default ~ income + balance, data=Default, 
               family=binomial)

sample <- sample(1:dim(Default)[1], dim(Default)[1]/2, 
                    replace=FALSE)
train.def <- Default[sample,]
cv.def <- Default[-sample,]
head(train.def)
glm.def <- glm(default ~ income + balance, data=train.def, 
               family=binomial)
train.prob <- predict(glm.def, train.def)
train.pred <- ifelse(train.prob > .5, 1, 0)
table(train.pred, train.def$default)

cv.prob <- predict(glm.def, cv.def)
cv.pred <- ifelse(cv.prob > 0.5, 1, 0)
table(cv.pred, cv.def$default)

str(Default)
glm.def2 <- glm(default ~ income + balance + student, data=train.def, 
               family=binomial)
summary(glm.def2)
summary(glm.def)
train.prob2 <- predict(glm.def2, train.def)
train.pred2 <- ifelse(train.prob2 > .5, 1, 0)
table(train.pred2, train.def$default)

cv.prob2 <- predict(glm.def2, cv.def)
cv.pred2 <- ifelse(cv.prob2 > 0.5, 1, 0)
table(cv.pred2, cv.def$default)

set.seed(5)
summary(glm(default ~ income + balance, data=Default, 
            family=binomial))$coef

glm.def3 <- glm(default ~ income + balance, data=Default, 
    family=binomial)

boot.fn <- function(data=Default,index) {
    with(data[index,],glm(default ~ income + balance, 
                 family=binomial))$coef
}
boot.fn(Default, 1:10000)
boot.fn(Default, sample(1:10000, 10000, replace=T))
xx <- boot(Default, boot.fn, R=1000)
?boot

data(Weekly)
summary(glm(Direction ~ Lag1 + Lag2, data=Weekly, family=binomial))
glm.stock <- glm(Direction ~ Lag1 + Lag2, data=Weekly, family=binomial)
glm.stock <- glm(Direction ~ Lag1 + Lag2, data=Weekly[-1,], family=binomial)
head(Weekly[-1,])
head(Weekly)
probUP <- predict(glm.stock, Weekly[1,])
pred.up <- ifelse(probUP > 0.5, 1, 0)
c(pred.up, Weekly[1,9])
2==Weekly[1,9]
summary(glm.stock)

str(Weekly$Direction)

Weekly$Direction <- factor(Weekly$Direction,levels = c(0,1))

x <- rep(NA,nrow(Weekly))
for(i in 1:nrow(Weekly)) {
    glm.fit <- glm(Direction ~ Lag1 + Lag2, data=Weekly[-i,], family=binomial)
    probUP <- predict(glm.fit, Weekly[i,])
    pred.up <- ifelse(probUP > 0, "Up", "Down")
        if(pred.up==Weekly[i,9]){
            x[i] <- 0
        } else {
            x[i] <- 1
    }
}

x
mean(x)
length(grep("Up",Weekly$Direction))/length(Weekly$Direction)


set.seed(39)
y <- rnorm(100)
x <- rnorm(100)
y = x-2*x^2 + rnorm(100)
plot(x,y)
fake <- data.frame(x,y)

fit1 <- glm(y ~ x, data=fake)
fit2 <- glm(y ~ x + I(x^2), data=fake)
fit3 <- glm(y ~ x + I(x^2) + I(x^3), data=fake)
fit4 <- glm(y ~ x + I(x^2) + I(x^3) + I(x^4), data=fake)
fit5 <- glm(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5), data=fake)
fit6 <- glm(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6), data=fake)
summary(fit1)
summary(fit2)
summary(fit3)
summary(fit4)

loocv <- function(fit) {
    h <- lm.influence(fit)$h
    mean((residuals(fit)/(1 - h))^2)
}

loocv(fit1)
loocv(fit2)
loocv(fit3)
loocv(fit4)
loocv(fit5)

z <- c(loocv(fit1),loocv(fit2),loocv(fit3),loocv(fit4),loocv(fit5),loocv(fit6))
plot(1:6, z, type="b", xlab="Number of Coefficients", ylab="MSE")
cv.glm(fake, fit6, K=5)$delta


library(MASS)
data(Boston)
mu.hat <- mean(Boston$medv)
se.medv <- (sd(Boston$medv))/(sqrt(length(Boston$medv)))
sd(Boston$medv)
sqrt(length(Boston$medv))
SE <- function(x,i) {(sd(x[i]))/(sqrt(length(x[i])))}
BootSE <- boot(Boston$medv,SE,50)
str(BootSE)
BootSE$Bootstrap
BootSE
c((mu.hat - (2*0.41)), (mu.hat + (2*0.41)))
t.test(Boston$medv)
median(Boston$medv)

MED <- function(x,i) {(median(x[i]))}
BootMED<- boot(Boston$medv,MED,3)
Boston$medv
sample(Boston$medv, 3)
TenPC <- quantile(Boston$medv, .1)
NinetyPC <- quantile(Boston$medv, .9)

TEN <- function(x,i) {quantile((x[i]), 0.1)}
BootTEN <- boot(Boston$medv,TEN ,1000)
