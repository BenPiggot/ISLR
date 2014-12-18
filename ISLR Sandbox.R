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
    layer_histograms(width = 0.5) %>%
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

