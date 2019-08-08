#Section 2.1

library(vcd)
data(Arthritis)

head(Arthritis)

summary(Arthritis)

summary(Arthritis$Age)

xtabs(~ Improved, data = Arthritis)

IMP <-xtabs(~ Improved, data = Arthritis)

(IMP <-xtabs(~Improved, data = Arthritis))

barplot(IMP)

IMP2 <-IMP[order(IMP, decreasing = TRUE)]
barplot(IMP2)

order(IMP, decreasing = TRUE)

pie(IMP)

pie(IMP, radius=1, col = gray(seq(0.6,1.0,length = 3)), clockwise = TRUE)

pie(IMP, radius=1, col = c("pink", "red", "purple"), clockwise = TRUE)

pie(IMP, radius=1, col = gray(seq(0.6,1.0,length = 3)), clockwise = TRUE, labels =c('No effect','Bit effect','Big effect'))

xtabs(~ Treatment + Improved, data = Arthritis)

Ctable <- xtabs(~ Treatment + Improved, data = Arthritis)
prop.table(Ctable, margin = 1)

prop.table(Ctable)

prop.table(Ctable, margin = 2)

round(2.65,1)

round(2.65001,1)

Ctable.p <- prop.table(Ctable, margin = 1)
round(Ctable.p,3)

library(vcd)
Ctable <- xtabs(~ Treatment + Improved, data = Arthritis)
Ctable.p <- prop.table(Ctable, margin = 1)
barplot(aperm(Ctable.p), horiz = T , axes = F, col = gray(seq(0.6,1.0,length = 3)) , xlab ="Improved(None Some Marked)")
axis(1, seq(0, 1, by=0.2), paste(seq(0, 100, by =20), "%"))

aperm(Ctable.p)

seq(0, 1, by =0.2)

Ctable <- xtabs(~Treatment+Improved, data = Arthritis)
mosaicplot(Ctable, col = TRUE)

mosaicplot(~Improved + Treatment, data = Arthritis, col = TRUE)

xtabs(~Treatment + Improved + Sex, data = Arthritis)

xtabs(~I(Sex:Treatment) + Improved, data = Arthritis)

Ctable2 <- xtabs(~I(Sex:Treatment) + Improved, data = Arthritis)
Ctable2.p <- prop.table(Ctable2, margin = 1)
round(Ctable2.p,3)

Ctable3 <- xtabs(~Treatment + Sex + Improved, data = Arthritis)
mosaicplot(Ctable3, col = TRUE)

#Section 2.2 

library(vcd)
data(DanishWelfare)
head(DanishWelfare)

summary(DanishWelfare)

xtabs(Freq ~ Alcohol, data = DanishWelfare)

xtabs(Freq ~ Status + Alcohol, data = DanishWelfare)

Ctable4 <- xtabs(Freq ~ Status + Alcohol, data = DanishWelfare)
mosaicplot(Ctable4, col = TRUE)

mosaicplot(Freq ~ Status + Alcohol, data = DanishWelfare, col = TRUE)

#Section 2.3

dtable <- matrix(c(2,18,3,17,9,11,15,5,18,2), nr = 2)
dimnames(dtable) <- list(effect = list("ON","OFF"),dose =list("1", "2", "3", "4", "5"))
class(dtable) <- "table"
dtable

dose_effect <- data.frame(dtable)
dose_effect

DW <- data.frame(lapply(DanishWelfare, function(i) rep(i,DanishWelfare[,"Freq"])))

head(DW)

DW2 <-DW[-1]
head(DW2)

library(vcd)
xtabs(Freq ~ Income, data = DanishWelfare)

levels(DanishWelfare$Income)

levels(DanishWelfare$Income) <- c("0-100", "0-100", "100<", "100<")
levels(DanishWelfare$Income)

data(DanishWelfare)
levels(DanishWelfare$Income)

summary(Arthritis$Age)

Arthritis$CatAge <-cut(Arthritis$Age,breaks=c(20,30,40,50,60,70,80),labels=c(20,30,40,50,60,70),right=FALSE)
Arthritis$CatAge

head(Arthritis)

UCBAdmissions 

UCdata <-apply(UCBAdmissions,c(1,2),sum)
UCdata

UCBAdmissions[,"Male",]

UCBAdmissions[,"Male", -1]

#Section 2.4

library(vcd)
data(Arthritis)
hist(Arthritis$Age)

hist(Arthritis$Age, breaks = c(20,30,40,50,60,70,80), col = "blue")

hist(Arthritis$Age, breaks = c(20,30,40,50,60,70,80), plot = FALSE)$counts

boxplot(Arthritis$Age)

boxplot(Age~Treatment, data = Arthritis)

hist(Arthritis$Age,  breaks = c(20,30,40,50,60,70,80), xlab = 'age', ylab = 'count', main = 'Distribution age',
xlim = c(10,90), ylim =c(0,40))
