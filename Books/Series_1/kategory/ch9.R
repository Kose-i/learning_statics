#Section 9.1
library(vcd)
tabledata<-xtabs(Freq~Status+Urban,data=DanishWelfare)
tabledata

chisq.test(tabledata)

round(prop.table(tabledata,margin=2),3)

# Define mathtic presentation
x<-c(1,2,3)
y<-c(1,2,3,4,5)
plot(0:6, 0:6, type = "n")
for (i in 1:3) {
for (j in 1:5) {
points(x[i],y[j],cex=log(tabledata[i,j]),xlim=c(0,4))
}
}

library(MASS)
result<-corresp(tabledata,nf=1)
result

plot(-3:2, -3:2, type = "n")
for (i in 1:3) {
for (j in 1:5) {
points(result$rscore[i],result$cscore[j],cex=log(tabledata[i,j]),xlim=c(0,4))
}
}

plot(result)

result<-corresp(tabledata,nf=2)
result

biplot(result)

#Section 9.2
gakuryoku <-read.csv("gakuryoku1.csv",head=TRUE)
head(gakuryoku)

library(MASS)
result <- corresp(gakuryoku,nf=2)
result

biplot(result)




#Section 9.3
gakuryoku <-read.csv("gakuryoku2.csv",head=TRUE)
head(gakuryoku)

A<-factor(gakuryoku$A,level=c(1:4),labels=c("A1","A2","A3","A4"))
B<-factor(gakuryoku$B,level=c(1:4),labels=c("B1","B2","B3","B4"))
C<-factor(gakuryoku$C,level=c(1:4),labels=c("C1","C2","C3","C4"))
gakuryoku <-data.frame(A,B,C)

A<-factor(gakuryoku$A,level=c(1:4),labels="A")
B<-factor(gakuryoku$B,level=c(1:4),labels="B")
C<-factor(gakuryoku$C,level=c(1:4),labels="C")
gakuryoku <-data.frame(A,B,C)

summary(gakuryoku)

library(MASS)
result <- mca(gakuryoku)
result

result$rs
result$cs

plot(result)
