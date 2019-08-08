UCBAdmissions

GAdata <-apply(UCBAdmissions,c(2,1),sum)
GAdata

mosaicplot(GAdata)
chisq.test(GAdata)

mantelhaen.test(UCBAdmissions)

mantelhaen.test(UCBAdmissions,exact=TRUE)

# Estimate Common Odds ratio
library(epicalc)
mhor(mhtable = UCBAdmissions)

# Breslow and Day
library(vcd)
lor<-oddsratio(UCBAdmissions,log=FALSE)
lor

confint(lor)

plot(lor)

# Odds ratio Test
UCBBtoF <-UCBAdmissions[,,-1]
UCBBtoF
mhor(mhtable=UCBBtoF)

#Section 5.3
library(coin)
mantelhaen.test(jobsatisfaction)

lbl_test(jobsatisfaction)


