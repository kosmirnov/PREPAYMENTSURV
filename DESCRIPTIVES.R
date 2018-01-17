if (!(require(survminer))) install.packages ("survminer")
if (!(require(survival))) install.packages ("survival")
if (!(require(stargazer))) install.packages ("stargazer")
if (!(require(dplyr))) install.packages ("dplyr")
if (!(require(data.table))) install.packages ("data.table")
if (!(require(ggplot2))) install.packages ("ggplot2")


gc()


options(scipen=999)
options(digits=5)
#options(contrasts = c("contr.treatment", "contr.treatment"))


load("C:/Users/Kons/OneDrive/MASTERARBEIT/DATA/PREPAYMENTSURV/FNMA.Rda")

FNMA$OPTION.CAT <- cut(FNMA$OPTION.LAG1, breaks=c(-Inf,0,0.75, Inf), labels=c("OTM","ATM","ITM"))
FNMA <- within(FNMA, OPTION.CAT <- relevel(OPTION.CAT, ref = 2))
FNMA$OPTION.BI <- cut(FNMA$OPTION.LAG1, breaks=c(-Inf,0.5, Inf), labels=c("OTM","ITM"))


FNMA$LTV <- (FNMA$LAST_UPB/FNMA$HOUSEPRICE.DEV)*100
FNMA$LTV.CAT <- cut(FNMA$LTV, breaks=c(-Inf,65,80, Inf), labels=c("1","2","3"))
FNMA <- within(FNMA, LTV.CAT <- relevel(LTV.CAT, ref = 2))
FNMA <- na.omit(FNMA)


FNMA$CSCORE.CAT <- cut(FNMA$CSCORE, breaks=c(-Inf,750, Inf), labels=c("1","2"))
FNMA$OCC_STAT[FNMA$OCC_STAT=="S"]<- "P"



## Kaplan-Meier estimates ##
fit1 <- survfit(Surv(START,STOP,PREPAID)~1, data=FNMA)
ggsurvplot(fit1, conf.int = TRUE, pval=TRUE, fun="pct", risk.table = TRUE, linetype = "strata", legend.title="Survival Curve")
ggsurvplot(fit1, conf.int = TRUE, pval=TRUE, fun="cumhaz", risk.table = TRUE, linetype = "strata", legend.title="Survival Curve")

fit1.table <- summary(fit1)
fit1.table<- as.data.table(cbind(fit1.table$time,fit1.table$n.risk,fit1.table$n.event,fit1.table$n.censor,fit1.table$surv))
colnames(fit1.table) <- c("time","N.risk","N.event","N.censor","SurvivalProb")
fit1.table$SMM <- fit1.table$N.event/fit1.table$N.risk
fit1.table$CPR <- 1-(1-fit1.table$SMM)^12

ggplot(fit1.table, aes(x=time, y=CPR)) +
  geom_line()


fit2 <- survfit(Surv(START,STOP,PREPAID)~strata(OPTION.BI), data=FNMA)
ggsurvplot(fit2, data=FNMA)

fit2.table <- summary(fit2)
fit2.table<- as.data.table(cbind(fit2.table$time,fit2.table$n.risk,fit2.table$n.event,fit1.table$n.censor,fit2.table$surv,fit2.table$strata))
colnames(fit2.table) <- c("time","N.risk","N.event","SurvivalProb","Strata")
fit2.table$SMM <- fit2.table$N.event/fit2.table$N.risk
fit2.table$CPR <- 1-(1-fit2.table$SMM)^12
ggplot(fit2.table, aes(x=time, y=CPR, group=Strata)) +
  geom_line(aes(color=Strata))




fit3 <- survfit(Surv(START,STOP,PREPAID)~strata(LTV.CAT), data=FNMA)
ggsurvplot(fit3, data=FNMA,conf.int = TRUE)

fit3.table <- summary(fit3)
fit3.table<- as.data.table(cbind(fit3.table$time,fit3.table$n.risk,fit3.table$n.event,fit3.table$surv,fit3.table$strata))
colnames(fit3.table) <- c("time","N.risk","N.event","SurvivalProb","Strata")
fit3.table$SMM <- fit3.table$N.event/fit3.table$N.risk
fit3.table$CPR <- 1-(1-fit3.table$SMM)^12
ggplot(fit3.table, aes(x=time, y=SMM, group=Strata)) +
  geom_line(aes(color=Strata))












