#Section 3.1
#Section 3.2 

binom.test(8,20,0.5)

dbinom(8,20,0.4)

n<-20
p<-0.4
x<-c(0:n)
y<-array(dbinom(x,n,p))
dimnames(y)<-list(freq=x)
barplot(y,main=paste("n=",n,", p=",p))

rbinom(5,20,0.4)

set.seed(1)
rbinom(5,20,0.4)

n <- 20
x <- 8
p <- x / n
sd <- sqrt(x * (n - x)/n ) / n
c(p - 1.96 * sd, p + 1.96 * sd)

dbinom(405,765,0.5)

dbinom(405,765,0.4)

binom.test(34537,67150,0.5)

#Section 3.3

p <- c(0.382, 0.219, 0.305, 0.094)
x <- c(39, 16, 27, 15)
dmultinom(x, prob = p)

binom.test(39, 97)

# Goodness of fit

x <- c(62, 52, 53, 38, 46, 49)
p0 <- c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6)
chisq.test(x, p = p0)

