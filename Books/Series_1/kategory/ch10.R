#Section 10.1
library(vcd)
head(OvaryCancer)

library(mvpart)
result <- rpart(survival~stage+operation+xray,weight=Freq,data=OvaryCancer)
result

library(rpart)
library(mvpart)

plot(result,uniform=TRUE,margin=0.2)
text(result,use.n=TRUE, all=TRUE)

#Section 10.2
library(mvpart)
result <- rpart(Species~.,data=iris)
result

plot(result,uniform=TRUE,margin=0.2)
text(result,use.n=TRUE)

predict(result,type="class")

iris$est <-predict(result,type="class")
xtabs(~Species+est,data=iris)

predict(result)

result$control

newresult<-prune(result,cp=0.1)
newresult

plot(newresult,uniform=TRUE,margin=0.2)
text(newresult,use.n=TRUE)

summary(newresult)

library(rpartOrdinal)
data(lowbwt)
head(lowbwt)

lowbwt$Category <- factor(ifelse(lowbwt$bwt<=2500,3,ifelse(lowbwt$bwt<=3000,2,
             ifelse(lowbwt$bwt<=3500,1,0))),ordered=TRUE)

library(rpart)
ord.rpart<-rpart(Category~age+lwt+race+smoke+ptl+ht+ui+ftv,data=lowbwt,method=ordinal)
ord.rpart

plot(ord.rpart,margin=0.1)
text(ord.rpart)

lowbwt$est<-predict(ord.rpart)
xtabs(~Category+est,data=lowbwt)








