library(MASS)
data(Boston)
glimpse(Boston)
basic <- lm(medv ~ black + lstat, data=Boston)
summary(basic)
medv.z <- (Boston$medv - mean(Boston$medv))/(sd(Boston$medv))
black.z <- (Boston$black - mean(Boston$black))/(sd(Boston$black))
lstat.z <- (Boston$lstat - mean(Boston$lstat))/(sd(Boston$lstat))
summary(lm(medv.z ~ black.z + lstat.z - 1))

data(Hitters)
install.packages("leaps")
install.packages("glmnet")
install.packages("pls")
basic$call[[2]]

##############################################################

set.seed(21)
x <- rnorm(100)
e <- rnorm(100)
y = 2 + 0.7*x + 1.02*x^2 + 1.3*x^3 + e
fake <- data.frame(x,y)

regfit.full <- regsubsets(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) +
                          I(x^7) + I(x^8) + I(x^9) + I(x^10), data=fake, nvmax=10)

reg.summary <- summary(regfit.full)
plot(reg.summary$cp,xlab="Number of Variables", ylab="Cp", type="b")
plot(reg.summary$adjr2,xlab="Number of Variables", ylab="Adj. R Squared", type="b")
plot(reg.summary$bic,xlab="Number of Variables", ylab="BIC", type="b")
which.min(reg.summary$rsq)
str(reg.summary)

regfit.fwd <- regsubsets(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) +
                             I(x^7) + I(x^8) + I(x^9) + I(x^10), data=fake, nvmax=10, 
                            method="forward")
regfwd.summary <- summary(regfit.fwd)
plot(regfit.fwd, scale="Cp")
plot(regfwd.summary$cp,xlab="Number of Variables", ylab="Cp", type="b")

regfit.back <- regsubsets(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) +
                             I(x^7) + I(x^8) + I(x^9) + I(x^10), data=fake, nvmax=10, 
                            method="back")
regback.summary <- summary(regfit.back)
plot(regfit.back, scale="Cp")
plot(regback.summary$cp,xlab="Number of Variables", ylab="Cp", type="b")

X <- model.matrix(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) +
                      I(x^7) + I(x^8) + I(x^9) + I(x^10) - 1, data=fake)

fit.lasso <- glmnet(X,y,alpha=1)
plot(fit.lasso, xvar="lambda", label=TRUE)
fit.lasso$lambda
coef(fit.lasso)

cv.lasso <- cv.glmnet(X,y,alpha=1)
plot(cv.lasso)
coef(cv.lasso)

fit.ridge <- glmnet(X,y,alpha=0)
plot(fit.ridge, xvar="lambda", label=TRUE)

##############################################################

data(College)
str(College)
dim(College)

train <- sample(c(1:777), 500, replace=FALSE)
train

fit <- lm(Apps ~., data=College[train,])
summary(fit)
pred.fit <- predict(fit, College[-train,])
sqrt(mean((pred.fit - College$Apps[-train])^2))

x <- model.matrix(Apps ~., data=College)
y <- College$Apps

ridge.fit <- glmnet(x[train,],y[train],alpha=0)
plot(ridge.fit, xvar="lambda", label=TRUE)
pred.ridge <- predict(ridge.fit, x[-train,])
str(pred.ridge)
str(y)
ridge.cv <- cv.glmnet(x[train,],y[train],alpha=0)
str(pred.ridge)
rmse <- sqrt(apply((y[-train]-pred.ridge)^2, 2, mean))
plot(log(ridge.fit$lambda),rmse, type="b", xlab="log lambda")

lasso.fit <- glmnet(x[train,],y[train],alpha=1)
plot(lasso.fit, xvar="lambda", label=TRUE)
pred.lasso <- predict(lasso.fit, x[-train,])
rmse <- sqrt(apply((y[-train] - pred.lasso)^2, 2, mean))
plot(log(lasso.fit$lambda),rmse, type="b", xlab="log lambda")

pcr.fit <- pcr(Apps ~., data=College, subset=train, scale=TRUE, 
               validation="CV")
summary(pcr.fit)
validationplot(pcr.fit)
pcr.pred <- predict(pcr.fit, College[-train,])
summary(pcr.pred)
sqrt(mean((College$Apps[-train] - pcr.pred)^2))

pls.fit <- plsr(Apps ~., data=College, subset=train, 
                scale=TRUE, validation="CV")
summary(pls.fit)
validationplot(pls.fit)

pls.pred <- predict(pls.fit, College[-train,])
summary(pls.pred)
sqrt(mean((College$Apps[-train] - pls.pred)^2))


##############################################################

set.seed(46)
sim <- matrix(rnorm(20000),1000,20)
sim.coef <- rnorm(c(1:20), mean=1, sd=1)
ind <- which(sim.coef %in% sample(sim.coef, 10))
sim.coef[ind] <- 0
y <- sim %*% sim.coef + rnorm(1000, mean=5, sd=2)
dim(sim)
dim(sim.coef)
head(y)
sim.data <- data.frame(cbind(y, sim))
head(sim.data)
names(sim.coef) <- colnames(sim.data)[-1]
sim.coef
train <- sample(1:1000, 900, replace=FALSE)
head(train)

reg.sim <- regsubsets(X1~. , data=sim.data[train,],nvmax=20)
reg.summary <- summary(reg.sim)
plot(reg.summary$cp, xlab="Number of Variables", ylab="Cp", type="b")
plot(reg.summary$bic, xlab="Number of Variables", ylab="BIC", type="b")

dim(sim.data)
pred.sim <- predict.regsubsets(reg.sim, sim.data[-train,], id=20)
sqrt(mean((pred.sim - sim.data$X1[-train])^2))

val.errors <- rep(NA,20)
x.test <- model.matrix(X1 ~., data=sim.data[-train,])
for(i in 1:20) {
    coefi <- coef(reg.sim, id=i)
    pred <- x.test[,names(coefi)]%*%coefi
    val.errors[i] <- mean((sim.data$X1[-train] - pred)^2)
}
plot(val.errors, xlab="Number of Variables", type="b")
points(beta.error, col="blue", pch=19)

beta.error <- rep(NA,20)
for(i in 1:20) {
    coefi <- coef(reg.sim, id=i)
    beta.error[i] <- sum((na.omit(sim.coef[names(coefi)] - coefi))^2)
}
plot(beta.error, xlab="Number of Variables", type="b")

coefi <- coef(reg.sim, id=20)
coef(reg.sim,10)
sim.coef
names(coefi)
names(sim.coef)
sum((na.omit(sim.coef[names(coefi)] - coefi))^2)

plot(sqrt(val.errors), ylab="Root MSE", xlab="Number of Variables", 
     type="b", pch=19, ylim=c(0,4))
points(25*beta.error, col="blue", pch=19)

##############################################################

data(Boston)
glimpse(Boston)
reg.bos <- regsubsets(crim ~., data=Boston, nvmax=10)
reg.summary <- summary(reg.bos)
plot(reg.summary$cp, xlab="Number of Variables", ylab="Cp", type="b")
plot(reg.summary$bic, xlab="Number of Variables", ylab="BIC", type="b")

dim(Boston)

train <- sample(506, 350, replace=FALSE)
reg.train <- regsubsets(crim ~., data=Boston[train,], nvmax=13)
reg.summary <- summary(reg.train)
plot(reg.summary$cp, xlab="Number of Variables", ylab="Cp", type="b")
plot(reg.summary$bic, xlab="Number of Variables", ylab="BIC", type="b")

pred.sim <- predict.regsubsets(reg.train, Boston[-train,], id=20)

val.errors <- rep(NA,13)
x.test <- model.matrix(crim ~., data=Boston[-train,])
for(i in 1:13) {
    coefi <- coef(reg.train, id=i)
    pred <- x.test[,names(coefi)]%*%coefi
    val.errors[i] <- sqrt(mean((Boston$crim[-train] - pred)^2))
}

plot(val.errors, type="b", ylab="RMSE", xlab="Number of Variables")

x <- model.matrix(crim ~., data=Boston)
y <- Boston$crim

fit.lasso <- glmnet(x,y,alpha=1)
plot(fit.lasso, xvar="lambda", label=TRUE)
fit.lasso$lambda
coef(fit.lasso)

cv.lasso <- cv.glmnet(x,y,alpha=1)
plot(cv.lasso)
coef(cv.lasso)

lasso.fit <- glmnet(x[train,],y[train],alpha=1)
plot(lasso.fit, xvar="lambda", label=TRUE)
pred.lasso <- predict(lasso.fit, x[-train,])
rmse <- sqrt(apply((y[-train] - pred.lasso)^2, 2, mean))
plot(log(lasso.fit$lambda),rmse, type="b", xlab="log lambda")

fit.ridge <- glmnet(x[train,],y[train],alpha=0)
plot(fit.ridge, xvar="lambda", label=TRUE)
pred.ridge <- predict(fit.ridge, x[-train,])
rmse <- sqrt(apply((y[-train] - pred.ridge)^2, 2, mean))
plot(log(ridge.fit$lambda),rmse, type="b", xlab="log lambda")

summary(lm(crim~., data=Boston[train,]))

pcr.fit <- pcr(crim ~., data=Boston, subset=train, scale=TRUE, 
               validation="CV")
summary(pcr.fit)
validationplot(pcr.fit)
pcr.pred <- predict(pcr.fit, Boston[-train,])
summary(pcr.pred)
sqrt(mean((Boston$crim[-train] - pcr.pred)^2))

pls.fit <- plsr(crim ~., data=Boston, subset=train, 
                scale=TRUE, validation="CV")
summary(pls.fit)
validationplot(pls.fit)
pls.pred <- predict(pls.fit, Boston[-train,])
summary(pls.pred)
sqrt(mean((Boston$crim[-train] - pls.pred)^2))

install.packages("gam")
