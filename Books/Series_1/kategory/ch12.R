# Section 12.1
library(vcd)
head(DanishWelfare)

tabledata<-xtabs(Freq~Alcohol+Income,data=DanishWelfare)
tabledata

library(polycor)
polychor(tabledata,ML=TRUE)

library(polycor)
x <- c(1,1,2,2,1,3,3,3,1,1)
y <- c(3,3,1,1,2,2,2,1,2,2)
polychor(x, y, ML = TRUE)

polychor(x, y)

set.seed(0)
r1<-rnorm(100)
r2<-rnorm(100)
z<-r1+r2
w<-r1
cor(z,w)
x<-cut(z,breaks<-c(-100,-2,-1,0,1,2,100),label<-c(1:6),right=FALSE)
y<-cut(w,breaks<-c(-100,-2,-1,0,1,2,100),label<-c(1:6),right=FALSE)
polychor(x,y)

#section 12.2 
library(polycor)
x <- c(1,7,2,5,6,3,3,4,1,2)
y <- c(3,3,1,1,2,2,2,1,2,2)
polyserial(x, y, ML = TRUE)

polyserial(x, y)

#Section 12.3
individualdata <-data.frame(lapply(DanishWelfare,function(i) rep(i,DanishWelfare[,"Freq"])))
head(individualdata)
individualdata <-individualdata[-1]
head(individualdata)

polychor(individualdata$Alcohol, individualdata$Income)

library(polycor)
ans <- hetcor(individualdata, ML = TRUE)

ans

ans$correlations

factanal(individualdata, 1, covmat = ans$correlations)


