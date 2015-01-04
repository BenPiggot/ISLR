par(mfrow=c(1,1))
library(ISLR)
library(MASS)
library(boot)
data(Wage)
cv.error <- c(NA,10)
for(i in 1:10) {
    fit <- glm(wage ~ poly(age, i),data=Wage)
    cv.error[i] <- cv.glm(Wage, fit, K=10)$delta[1]
}
plot(cv.error, type="b")

bestfit <- lm(wage ~ poly(age,3), data=Wage)
plot(Wage$age, Wage$wage, col="dark gray")
lines(age.grid, predict(bestfit, list(age=age.grid)), col="dark green", lwd=2)

##############################################################
    
loocv <- function(fit) {
    h <- lm.influence(fit)$h
    mean((residuals(fit)/(1 - h))^2)
}

cv.error2 <- rep(NA,9)
for(i in 2:10){
    fit2 <- glm(wage ~ cut(age, i), data=Wage)
    cv.error2[i] <- loocv(fit2)
}
cv.error2 <- cv.error2[2:10]
plot(cv.error2, type="b")

best.fit2 <- lm(wage ~ cut(age,7), data=Wage)
pred <- predict(best.fit2, list(age=age.grid), se=TRUE)
se.bands <- cbind(pred$fit + 2*pred$se, pred$fit - 2*pred$se)

plot(Wage$age,Wage$wage, col="dark grey")
lines(age.grid, predict(best.fit2, list(age=age.grid)), col="dark green", lwd=2)
matlines(age.grid,se.bands,col="dark green",lty=2)

##############################################################

data(Auto)
str(Auto)
polyfit <- lm(acceleration ~ poly(mpg,3), data=Auto)
mpglims <- range(Auto$mpg)
mpg.grid <- seq(from=mpglims[1], to=mpglims[2])
pred.mpg <- predict(polyfit, newdata=list(mpg=mpg.grid), se=TRUE)
plot(Auto$mpg, Auto$acceleration, col="dark grey")
lines(mpg.grid, pred.mpg$fit, col="dark green", lwd=2)

spl.fit <- lm(acceleration ~ bs(mpg, knots=c(20,35)), data=Auto)
pred.spl <- predict(spl.fit, newdata=list(mpg=mpg.grid), se=TRUE)
lines(mpg.grid, pred.spl$fit, col="red", lwd=2)

spl.fit2 <- lm(acceleration ~ ns(mpg, df=4), data=Auto)
pred.spl2 <- predict(spl.fit2, newdata=list(mpg=mpg.grid), se=TRUE)
lines(mpg.grid, pred.spl2$fit, col="black", lwd=2)

smooth.fit <- smooth.spline(Auto$mpg, Wage$acceleration, cv=TRUE)
lines(smooth.fit, col="red", lwd=2)
smooth.fit$df

?gam
par(mfrow=c(1,3))
gam.fit <- gam(mpg ~ s(acceleration,df=5) + s(horsepower, df=5) + s(weight, df=5), data=Auto)
plot(gam.fit)

#################################################################

data(Boston)

par(mfrow=c(1,1))
poly.bos <- lm(nox ~ poly(dis, 3), data=Boston)
summary(poly.bos)
plot(Boston$dis, Boston$nox, col="dark grey")
dislims <- range(Boston$dis)
dis.grid <- seq(from=dislims[1], to=dislims[2], .2)
bos.pred <- predict(poly.bos, newdata=list(dis=dis.grid), se=TRUE)
lines(dis.grid, bos.pred$fit, col="dark green", lwd=2)
?Boston

polybosplot <- function(x) {
    plot(Boston$dis, Boston$nox, col="dark grey",
         xlab="Distance from Boston employment centers", 
         ylab="Nitrogen Oxide Concentration ") 
    dislims <- range(Boston$dis)
    dis.grid <- seq(from=dislims[1], to=dislims[2], .2)
    for(i in 1:x) {
        polyfit <- lm(nox ~ poly(dis, i), data=Boston)
        pred <- predict(polyfit, newdata=list(dis=dis.grid), se=TRUE)
        lines(dis.grid, pred$fit, col=i,lwd=2)
    }
}

par(mfrow=c(1,2))
polybosplot(10)

cv.error <- c(NA,10)
for(i in 1:10) {
    fit <- glm(nox ~ poly(dis, i),data=Boston)
    cv.error[i] <- cv.glm(Boston, fit, K=10)$delta[1]
}
plot(cv.error, type="b")

spl.fit <- lm(nox ~ bs(dis, 4), data=Boston)
summary(spl.fit)
pred.spl <- predict(spl.fit, newdata=list(dis=dis.grid), se=TRUE)
plot(Boston$dis, Boston$nox, col="dark grey")
lines(dis.grid, pred.spl$fit, col="red", lwd=2)

spl.fit2 <- lm(nox ~ ns(dis, df=4), data=Boston)
pred.spl2 <- predict(spl.fit2, newdata=list(dis=dis.grid), se=TRUE)
plot(Boston$dis, Boston$nox, col="dark grey")
lines(dis.grid, pred.spl2$fit, col="red", lwd=2)
summary(spl.fit2)

splinebosplot <- function(x) {
    plot(Boston$dis, Boston$nox, col="dark grey",
         xlab="Distance from Boston employment centers", 
         ylab="Nitrogen Oxide Concentration ") 
    dislims <- range(Boston$dis)
    dis.grid <- seq(from=dislims[1], to=dislims[2], .2)
    for(i in 3:x) {
        splfit <- lm(nox ~ ns(dis, i), data=Boston)
        spl.pred <- predict(splfit, newdata=list(dis=dis.grid), se=TRUE)
        lines(dis.grid, spl.pred$fit, col=i,lwd=2)
    }
}
par(mfrow=c(1,1))
splinebosplot(10)


cv.error2 <- c(NA,10)
for(i in 1:10) {
    splfit <- glm(nox ~ ns(dis, df=i),data=Boston)
    cv.error2[i] <- cv.glm(Boston, splfit, K=10)$delta[1]
}
plot(cv.error2, type="b")

############################################################

data(College)
dim(College)
train <- sample(777, 500, replace=FALSE)
str(College)
regfit.fwd <- regsubsets(Outstate~., data=College[train,], nvmax=16, method="forward")
regfwd.summary <- summary(regfit.fwd)
plot(regfwd.summary$bic,xlab="Number of Variables", ylab="", type="b")

c(Private, Room.Board,
Terminal, perc.alumni, Expend, Grad.Rate)

error <- c(NA,10)
for(i in 1:10) {
gam.fit <- gam(Outstate ~ Private + s(Room.Board,i) + s(Terminal,i) + 
                   s(perc.alumni,i) + s(Expend,i) + s(Grad.Rate,i), 
                   data=College[train,])
pred.gam <- predict(gam.fit, College[-train,])
summary(pred.gam)
error[i] <- sqrt(mean((pred.gam - College$Outstate[-train])^2))
}
plot(error, type="b")

gam.best <- gam(Outstate ~ Private + s(Room.Board,5) + s(Terminal,5) + 
                   s(perc.alumni,5) + s(Expend,5) + s(Grad.Rate,5), 
                data=College[train,])
pred.bestgam <- predict(gam.best, College[-train,])
summary(pred.bestgam)
sqrt(mean((pred.bestgam - College$Outstate[-train])^2))
plot(pred.bestgam,College$Outstate[-train])

str(gam.fit)
summary(gam.fit)
sqrt(gam.fit$deviance/gam.fit$df.residual)

mean(College$Outstate)

basic.fit <- lm(Outstate ~ Private + Room.Board + Terminal +
                perc.alumni + Expend + Grad.Rate, data=College[train,])
summary(basic.fit)
pred.basic <- predict(basic.fit, College[-train,])
sqrt(mean((pred.basic - College$Outstate[-train])^2))
plot(pred.basic, College$Outstate[-train])

###########################################################

set.seed(496)
X1 <- rnorm(100, mean=4, sd=1.5)
X2 <- rnorm(100, mean=7, sd=2.8)
E <- rnorm(100, mean=2, sd=1.2)
Y = 2.3 + -0.9*X1 + 2.1*X2 + E

beta0 <- rep(NA,1000)
beta1 <- rep(NA, 1000)
beta2 <- rep(NA, 1000)
beta1[1] <- 3

for(i in 1:1000) {
    a = Y - beta1[i]*X1
    beta2[i] = lm(a ~ X2)$coef[2]
    a = Y - beta2[i]*X2
    beta0[i] <- lm(a ~ X1)$coef[1]
    beta1[i + 1] = lm(a ~ X1)$coef[2]  
}

beta1[1:20]
beta2[1:20]
beta0[1:20]

plot(beta1, type="b", xlim=c(0,10), ylim=c(-6,6))
lines(beta2, type="b",col="blue")
lines(beta0, type="b",col="red")

fit <- lm(Y ~ X1 + X2)
abline(h=fit$coef[1], col="red")
abline(h=fit$coef[2])
abline(h=fit$coef[3], col="blue")

#############################################################

set.seed(79)
p <- 100
n <- 1000
sim <- matrix(NA,n,p)
for (i in 1:100) {
    sim[,i] <- rnorm(1000, mean=sample(c(0:10),1), sd=sample(c(1:3),1))
}

coef <- sample(seq(-5,5,.1), 100, replace=T)
ind <- which(coef %in% sample(coef, 80))
coef[ind] <- 0
Y <- sim %*% coef + rnorm(1000, mean=100, sd=20)

betas <- matrix(NA,100, 100)
betas[1,] <- rnorm(100, mean=100, sd=100)
temp <- rep(0,100)


for(j in 1:10) {
    for(i in 1:99) {  
        a = Y - betas[i,j]*sim[,j]
        temp <- lm(a ~ sim[,-j])$coef[2:100]
        a = t(Y) - temp %*% t(sim[,-j])
        betas[i + 1, j] = lm(t(a) ~ sim[,j])$coef[2]
    }
}

betas[1:10,1:10]
plot(betas[,1], xlim=c(0,10), type="b")

a = Y - betas[1,1]*sim[,1]
betas[1,-1] <- lm(a ~ sim[,-1])$coef[2:100]
a = t(Y) - betas[1,-1] %*% t(sim[,-1])
betas[1 + 1, 1] = lm(t(a) ~ sim[,1])$coef[2]
summary(lm(t(a) ~ sim[,1]))

a = Y - betas[1,2]*sim[,2]
betas[1,-2] <- lm(a ~ sim[,-2])$coef[2:100]
a = t(Y) - betas[1,-2] %*% t(sim[,-2])
betas[2 + 1, 1] = lm(t(a) ~ sim[,2])$coef[2]