#Section 8.1
library(vcd)
xtabs(Freq~Status+Urban,data=DanishWelfare)

library(vcd)
tabledata<-xtabs(Freq~Status+Urban,data=DanishWelfare)
result<-loglin(tabledata,margin=list(c(1),c(2)),param=TRUE,fit=TRUE)

result$param

round(result$fit,1)

result$lrt
result$pearson

result$df

1-pchisq(result$pearson,result$df)

chisq.test(tabledata)

result<-loglin(tabledata,margin=list(c(1,2)),param=TRUE,fit=TRUE)
result

# Analysis by using function loglm
library(MASS)
tabledata<-xtabs(Freq~Status+Urban,data=DanishWelfare)
result<-loglm(formula=~Status+Urban,data=tabledata)
result

summary(result)

result$coef

coef(result)

round(fitted(result),1)

round(residuals(result),1)

expected <-fitted(result)
pearson.residual<-(tabledata-expected)/sqrt(expected)
round(pearson.residual,1)
sum(pearson.residual^2)

#Section 8.2
library(MASS)
Modelindependent<-loglm(formula=~Gender+Admit+Dept,data=UCBAdmissions)
Modelindependent

ModelGA <-loglm(formula=~Gender+Admit+Dept+Gender:Admit,data=UCBAdmissions)
ModelGA

ModelGA <-update(Modelindependent, .~. + Gender:Admit)

extractAIC(ModelGA)
extractAIC(ModelGA, k = log(sum(UCBAdmissions)))

#Section 8.3
library(vcd)
NonResponse

library(MASS)
tabledata <- xtabs(Freq ~ response + residence, data = NonResponse)
result <-loglm(formula = ~response + residence + residence:response, data =tabledata)
result$para

glm(response ~ residence, family = binomial, weights = Freq, data = NonResponse)



