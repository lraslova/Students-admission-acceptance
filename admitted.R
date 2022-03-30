#Data
library(lmtest)
library(stargazer)
library(sandwich)
library(olsrr)
library(tidyverse)
library(aod)
library(ggplot2)
library(mfx)
library(car)
library(lme4)

Admission <- read_excel("Admission.xlsx")
#data("Admission")
head(Admission)
summary(Admission)
str(Admission)
attach(Admission)

mean(gre)
mean(gpa)

gpa_cen<- gpa - 3.4
gre_cen<- gre - 588


#Logit - https://www2.karlin.mff.cuni.cz/~pesta/NMFM404/logistic.html
sapply(Admission, sd)

xtabs(~admit + rank, data = Admission)

#nastavení faktoru pro kategoriální proměnnou rank
Admission$rank <- factor(Admission$rank)
Admission$ses <- factor(Admission$ses)
Admission$Race <- factor(Admission$Race)
Admission$Gender_Male <- factor(Admission$Gender_Male)
 

#logit funkce
logit <- glm(admit ~ gre_cen + gpa_cen + rank + ses + Race + Gender_Male, data = Admission, family = "binomial")
summary(logit)


#intervaly stability
confint.default(logit)


#Mezní efekty - ručně
Logit.MEM <- dlogis(x.bar %*% logit$coef) * logit$coef
Logit.AME <- mean(dlogis(X %*% logit$coef)) * logit$coef

OLS <- lm(admit ~ gre_cen + gpa_cen + rank + ses + Race + Gender_Male, data=Admission, x=TRUE)
X <- OLS$x
x.bar <- apply(X, 2, mean)
print(x.bar)

cbind(OLS=OLS$coef, Logit.MEM, Logit.AME)

#Mezní efekty - balíček
Logit.MEM.mfx <- logitmfx(logit, data=Admission)
Logit.AME.mfx <- logitmfx(logit, data=Admission, atmean=FALSE)

Logit<-print(Logit.AME.mfx$mfxest)

#LPM
LPM <- lm(admit ~ gre_cen + gpa_cen + rank + ses + Race + Gender_Male, data = Admission, na.action=na.exclude)
summary(LPM)

bptest(LPM, studentize=FALSE)

LPM.robust.VCE <- vcovHC(LPM, type="HC1")  # Robustní odhad matice VCE.
LPM.robust.se <- sqrt(diag(LPM.robust.VCE))

reg1_robust <- coeftest(reg1, vcov = vcovHC, type = "HC0")
reg1_robust

#probit
probit <- glm(admit ~ gre_cen + gpa_cen + rank + ses + Race + Gender_Male, data = Admission, family=binomial(link=probit))
summary(probit)

Probit.MEM.mfx <- probitmfx(admit ~ gre + gpa + rank + ses + Race + Gender_Male, data = Admission)
Probit.AME.mfx <- probitmfx(admit ~ gre + gpa + rank + ses + Race + Gender_Male, data = Admission, atmean=FALSE)
Probit<-print(Probit.AME.mfx$mfxest)


#srovnání modelů
stargazer(list(logit,probit),title="Vystup", digits=4, type="text", out = "models.html", column.labels=c("logit", "probit"))
srovnani <- stargazer(list(logit, probit, Logit, Probit), column.labels = c("logit", "probit"), type = "text", out = "models.html")

getwd()

#srovnání mezních efektů
#stargazer(list(Logit.MEM.mfx$mfxest,Probit.MEM.mfx$mfxest),title="Vystup", digits=4, type="text", column.labels=c("logit", "probit"))
cbind(logit=Logit.AME.mfx$mfxest[, 1], probit=Probit.AME.mfx$mfxest[, 1])


#export do wordu



#Regrese klasika

#Grafy
hist(gre)
hist(log(gre), frequency=F)
lines(density(log(gre)), col = 10)

hist(gpa)
hist(log(gpa))
lines(density(log(gpa)), col = 10)

#centrování
mean(gpa)
mean(gre)

gpa_cen<- gpa - mean(gpa)
hist(gpa_cen)
lines(density(gpa_cen), col=10)
hist(log(gpa_cen))

#rovnice

reg <- lm(gre ~ gpa_cen + rank + ses + Race + Gender_Male +gpa_cen:Gender_Male, data = Admission)
summary(reg)
bptest(reg, studentize=FALSE)
vif(reg)

reg1 <- lm(gre ~ gpa_cen + rank + ses + Race + Gender_Male, data = Admission)
summary(reg1)


exp()

linearHypothesis(logit, c("ses2=0", "ses3=0"), test ="F")
waldtest(logit,glm(admit ~ gre_cen + gpa_cen + rank + Race + Gender_Male, data = Admission, family = "binomial"))
