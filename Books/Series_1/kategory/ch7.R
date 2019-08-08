#Section 7.1
t <- c(0:5)            # Count accident
x <- c(8,6,8,3,4,1)   # Count Day of each accident
m  <- sum(x)           # Calculate counts of data
s <- t %*% x
r <- s/m              # Average number of occurrenes
r                      # Estimate
ci <- c(r-2*sqrt(r/m),r+2*sqrt(r/m))  # confidence interval
ci

x <- c(8,6,8,3,4,1)                     # Days of each accident
names(x) <- c(0:5)                      # name x
y <- dpois(c(0:5),lambda=r)*sum(x)      # Calculate expected frequency
lines(barplot(x,ylim=c(0,10)),y,type="l")

x <- matrix(c(8,6,8,3,4,1,c(0:5)),nr=6)
x

library(vcd)
result <- goodfit(x,type="poisson",method="MinChisq")
result

summary(result)
result$par

plot(result)

result <- goodfit(x,type="poisson",method="ML")

x <- c(0,0,0,0,0,0,0,0,1,1,1,1,1,1,2,2,2,2,2,2,2,2,3,3,3,4,4,4,4,5)
library(vcd)
result <- goodfit(x,type="poisson",method="MinChisq")
result

#Section 7.2
boxplot(count~spray,data=InsectSprays)

glm(count~spray,data=InsectSprays,family="poisson")

#Section 7.3
accident<-c(45703,8906,7938,12091,7327,9820,11526,6525)
pref<-as.factor(c('A','B','C','D','E','F','G','H'))
result1<-glm(accident~pref,family="poisson")
result1

pop<-c(5056,859,1453,1828,1203,1143,1730,1373) 
result2<-glm(accident~pref,offset=log(pop),family="poisson")
result2

#Section 7.4
set.seed(1)
n<-1000
lambda<-rgamma(n,shape=5,scale=3)
y <-rpois(n,lambda)
mean(y)
var(y)

set.seed(2)
n=20
v=3
dose <-c(0:4)*5
x <-rep(dose,n)
a<-exp(3+0.05*dose)/v
lambda <-rgamma(5*n,shape=a,scale=v)
y <-rpois(5*n,lambda)
dataNB <-data.frame(x,y)

boxplot(y~x,data=dataNB)

by(y,x,mean)
by(y,x,var)

glm.nb(y~x,data=dataNB)

glm(y~x,family=poisson,data=dataNB)




