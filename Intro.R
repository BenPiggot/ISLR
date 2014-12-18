install.packages("ISLR")
library(ISLR)

install.packages("lubridate")
library(lubridate)
localWage <- Wage %>%
    select(logwage, year, race) %>%
    filter(race == "2. Black") %>%
    arrange(year) %>%
    mutate(wages = exp(logwage)) 
    
glimpse(localWage)

localWage %>%
    ggvis(~year, ~logwage) %>%
    ayer_histograms(width = 0.5) %>%
    add_axis("x", title = "Year")

?prop

data(College)
glimpse(College)
fix(College)
rownames(College) 
College[,1]
head(College)
pairs(College[,1:5])
par(mfrow=c(1,2))
par(mfrow=c(1,1))
plot(College$Private, College$Outstate)
Elite <- rep("No", nrow(College))
Elite[College$Top10perc > 50] = "Yes"
Elite
Elite <- as.factor(Elite)
College <- cbind(College, Elite)
glimpse(College)
plot(College$Elite, College$Outstate)
par(mfrow=c(1,1))
hist(College$Books)
hist(College$Grad.Rate)

data(Auto)
glimpse(Auto)
Auto <- na.omit(Auto)
range(Auto$mpg)
range(Auto$acceleration)
sd(Auto$mpg)
Auto <- Auto[-c(10:85),]
sd(Auto$mpg)
plot(Auto$horsepower, Auto$mpg)
barplot(Auto$mpg, Auto$cylinders)
plot(Auto$cylinders, Auto$mpg)
Auto$cylinders <- as.factor(Auto$cylinders)
str(Auto$cylinders)

library(MASS)
data(Boston)
pairs(Boston[,1:6])
glimpse(Boston)
plot(Boston$crim, Boston$black)
plot(Boston$black, Boston$crim)
plot(Boston$indus, Boston$crim)
?Boston
plot(Boston$medv, Boston$crim)
hist(Boston$crim)
rownames(Boston)
glimpse(Boston)
head(Boston)
x <- nrow(Boston[which(Boston$chas == 1),])
mean(Boston$chas/x)
min(Boston$medv)
Boston[5,]
median(Boston$ptratio)
mean(Boston$ptratio)
hist(Boston$ptratio)
nrow(Boston[which(Boston$rm > 7),])
nrow(Boston[which(Boston$rm > 8),])
Boston[which(Boston$rm > 8),]
sample(1:dim(Boston)[1], 30)
y=x
x <- seq(-pi, pi, len=50)
x
y = x
f = outer(x,y, function(x,y) cos(y)/(1+x^2))
contour(x,y,f)
contour(x,y,f, nlevels=45, add=T)
fa <- (f - t(f))/2
contour(x,y,fa, nlevels=45)
image(x,y,fa)
persp(x,y,fa)
persp(x,y,f)
persp(x,y,f,theta=120, phi=0)
?outer

plot(medv ~ lstat, data=Boston)
fit1 <- lm(medv ~ lstat, data=Boston)
abline(fit1, col="red")
names(fit1)
confint(fit1)
predict(fit1, data.frame(lstat=c(5,10,15)
        ),interval="confidence")

fit2 <- lm(medv ~ lstat + age, data=Boston)
summary(fit2)
.5^12 *100

fit3 <- lm(medv ~ ., data=Boston)
summary(fit3)
par(mfrow=c(2,2))
plot(fit3)
fit4 <- update(fit3, ~.-age-indus)
summary(fit4)
fit5 <- lm(medv ~ lstat*age, Boston)
fit6 <- lm(medv ~ lstat + I(lstat^2), Boston); summary(fit6)
par(mfrow=c(1,1))
plot(Boston$lstat, Boston$medv)
points(Boston$lstat, fitted(fit6), col="red", pch=20)
fit7 <- lm(medv ~ poly(lstat, 4), data=Boston)
points(Boston$lstat, fitted(fit7), col="blue", pch=20)
plot(1:20, 1:20, pch=1:20)
data(Carseats)
fit1 <- lm(Sales ~. + Income:Advertising + Age:Price, Carseats)
summary(fit1)
contrasts(Carseats$ShelveLoc)
