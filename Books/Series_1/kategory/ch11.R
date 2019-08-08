#Section 11.2
summary(PlantGrowth)

tapply(PlantGrowth$weight,PlantGrowth$group,mean)

lm(weight ~ group, data = PlantGrowth)

x <-c(1,2,1,0,2,3)

class(x)
summary(x)

x <- factor(x)
class(x)
summary(x)

x <-c(1,2,1,0,2,3)
x <-factor(x, levels = 0:3, labels = c("A", "B", "C", "D"))
x

result<-lm(weight~group,data=PlantGrowth)
score <-c(0,result$coefficients[2:3])
freq <-xtabs(~group,data=PlantGrowth)
newscore <- score - score %*% freq/sum(freq)
newscore

#Section 11.3
datatable<-read.csv("suryouka2data.csv",head=TRUE)
head(datatable)

datatable$income <-factor(datatable$income)
datatable$work <-factor(datatable$work)
datatable$newthing <-factor(datatable$newthing)
datatable$mass <-factor(datatable$mass)
summary(datatable)

library(MASS)
result<-lda(formula=mass~income+work+newthing,data=datatable)
result

predict(result)$x

predict(result)$posterior

predict(result)$class

predict(result)$x

predict(result)$posterior


table(datatable$mass,predict(result)$class,dnn=c("mass","class"))


library(vcd)
head(BrokenMarriage)

dataBM <-data.frame(lapply(BrokenMarriage, function(i) rep(i, BrokenMarriage[,"Freq"])))
head(dataBM)

lda(formula = broken ~ gender + rank, data = dataBM)
