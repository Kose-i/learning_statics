# Logistic Regression

#Section 6.1
dose <- c(1:5)
effect <- c(2,3,9,15,18)
effect.prop <- effect / 20
plot(dose, effect.prop)

dose <- c(1:5)
effect <- c(2,3,9,15,18)
glm(cbind(effect, 20 - effect)~dose, family=binomial)

# connection between Temperature and Fail
library(vcd)
SpaceShuttle
summary(SpaceShuttle)
x<-SpaceShuttle$Temperature[-4]
y<-SpaceShuttle$Fail[-4]
plot(x,y,xlab='Temperature',ylab='Failure(2:ON, 1:OFF)')
# using glm function

library(vcd)
glm(Fail~Temperature,data=SpaceShuttle,family=binomial)

library(MASS)
head(housing)

result <- glm(Sat ~ Cont, data = housing, weight = Freq, family = binomial)
result

#Section 6.2
#Section 6.3
library(vcd)
result <- glm(Fail~Temperature+Pressure,data=SpaceShuttle,family=binomial)
result

summary(result)


#Section 6.4
step(glm(Fail ~ Temperature + Pressure, data = SpaceShuttle, family = binomial))

step(glm(Fail ~ Temperature + Pressure, data = SpaceShuttle, 
family = binomial),k=log(23))

#Section 6.5
summary(iris)

library(nnet)
multinom(formula=Species~Sepal.Length,data=iris)


summary(housing)

library(MASS)
polr(formula = Sat ~ Cont, data = housing, weights = Freq)

Ctable <- xtabs(Freq~Cont+Sat,data=housing)
mosaicplot(Ctable,col=TRUE)

#Section 6.6
head(infert)

library(survival)
clogit(case ~ spontaneous + induced + strata(stratum), data = infert)
