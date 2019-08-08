#Section 4.1 

binom.test(512,1385)$conf.int

binom.test(567,2419)$conf.int

Ctable <- matrix(c(512,1385,567,2419),nr=2)
chisq.test(Ctable)

chisq.test(Ctable,correct=FALSE)

library(MASS)
Ctable <- xtabs(~ Sex + W.Hnd, data = survey)
Ctable
chisq.test(Ctable)

Ctable <- matrix(c(2, 10, 3, 15), nr = 2)
chisq.test(Ctable)

table <-matrix(c(3, 1, 1, 3), nr = 2)
fisher.test(table)

fisher.test(table, alternative = "greater")

library(MASS)
Ctable <- xtabs(~ Sex + W.Hnd, data = survey)
fisher.test(Ctable)

library(MASS)
Ctable <- xtabs(~ Sex + W.Hnd, data = survey)
library(vcd)
assocstats(Ctable)

oddsratio(Ctable, log = FALSE)

oddsratio(Ctable)

#Section 4.2

library(vcd)
Ctable <- xtabs(~ Treatment + Improved, data = Arthritis)
Ctable
result <- chisq.test(Ctable)
result

result$expected
result$residual

Ctable <- xtabs(~ Treatment + Improved, data = Arthritis)
result <- chisq.test(Ctable)
v <- result$expected * (1- rowSums(Ctable)/sum(Ctable)) %o% (1-colSums(Ctable)/sum(Ctable))
stdresidual <- (Ctable - result$expected) / sqrt(v)

stdresidual

# Mantel Test
manteltrend.test <-function(x, score = c(1:dim(x)[2]))
{
ctotal <- colSums(x)                                      # Calculate sum of row
rtotal <- rowSums(x)                                      # Calculate sum of line
n <- sum(x)                                               # Calculate sum of data
expected <- (rtotal %o% ctotal)/n                        # Calculate expectation frequency
tscore <- (x-expected) %*% score
v <- n*(score^2 %*% ctotal) -(score %*% ctotal)^2
statistics <-n^2*(n-1)*tscore[2]^2/rtotal[1]/rtotal[2]/v # Calculate statistic
p.value<-pchisq(statistics,1,lower.tail=FALSE)           # Calculate value
result<-list(statistics=statistics,p_value=p.value)
return(result)
}

library(vcd)
Ctable <- xtabs(~ Treatment + Improved, data = Arthritis)
manteltrend.test(Ctable, score = c(0,1,2))

library(vcd)
Ctable <- xtabs(~ Treatment + Improved, data = Arthritis)
x <- rep(col(Ctable)[1,],Ctable[1,])
y <- rep(col(Ctable)[2,],Ctable[2,])
wilcox.test(x,y)

print("test******")
# function for Wilcoxon Rank Test
CategoricalWiltest<-function(x){
group1<-rep(c(1:dim(x)[2]),x[1,])
group2<-rep(c(1:dim(x)[2]),x[2,])
result<-wilcox.test(group1,group2,correct=FALSE)
return(result)
}

library(vcd)
Ctable <- xtabs(~Treatment + Improved, data = Arthritis)
CategoricalWiltest(Ctable)

# function Wilcoxon average Range score
Wilcoxon.score <- function(x){
ctotal <- colSums(x)
cumctotal <- cumsum(ctotal)
shiftcum <- c(0,cumctotal[1:length(cumctotal)-1])
score <- (cumctotal+shiftcum+1)/2
return(score)
}

gdresult <- c(2,3,9,15,18)
nsample <- rep(20,5)
prop.trend.test(gdresult, nsample, score = c(1,2,3,4,5)) 

Ctable <- matrix(c(2,3,9,15,18,18,17,11,5,2), byrow = TRUE, nc =5)
manteltrend.test(Ctable, score = c(1,2,3,4,5))

#Section 4.3

Ctable <- xtabs(Freq ~ Status + Alcohol, data = DanishWelfare)
Ctable
chisq.test(Ctable)

# Generally Mantel Test
extendedM.test <-function(x,score=c(1:dim(x)[2]))
{
ctotal <-colSums(x)                                       # Calculate sum of row
rtotal <-rowSums(x)                                       # Calculate sum of line
n <-sum(x)                                                # Calculate sum of data
expected <- (rtotal %o% ctotal)/n                         # Calculate expected frequency
tscore <- (x-expected) %*% score                          # Calculate sum of score
mean.score <-(score %*% ctotal)/n                         # Calculate means of score
v <-(score - mean.score)^2 %*% ctotal
statistics <-(n-1)*sum(tscore^2/rtotal)/v                 # Calculate statistics
p.value<-pchisq(statistics,dim(x)[1]-1,lower.tail=FALSE)  # Calculate p-value
result<-list(statistics=statistics,p_value=p.value)
return(result)
}

library(vcd)
Ctable <- xtabs(Freq ~ Status + Alcohol, data = DanishWelfare)
extendedM.test(Ctable, score = c(0,1,2))

# function for Kruskal Test
CategoricalKruskaltest<-function(x){
colv <-rep(col(x),x)
rowv <-rep(row(x),x)
return(kruskal.test(colv,rowv))
}

library(vcd)
Ctable<-xtabs(Freq~Status+Alcohol,data=DanishWelfare)
CategoricalKruskaltest(Ctable)

library(coin)
Ctable<-apply(jobsatisfaction,c(1,2),sum)
Ctable

lbl_test(as.table(Ctable))

class(Ctable)

lbl_test(as.table(Ctable), scores = list(Job.Satisfaction = c(1,2,3,4), Income = c(2.5, 10, 20, 30)))

Ctable <- xtabs(~ Treatment + Improved, data = Arthritis)
lbl_test(Ctable)

Kidsdish <- matrix(c(93,147,100,315), nr =2)
mcnemar.test(Kidsdish)

binom.test(147,247,0.5)

THdata <- matrix(c(13,17,2,4,35,2,0,7,2), nr = 3)
dimnames(THdata) <- list(grade3 =c(1,2,3), grade4 = c(1,2,3))
THdata
mcnemar.test(THdata)
