if (!(require(survminer))) install.packages ("survminer")
if (!(require(survival))) install.packages ("survival")
if (!(require(stargazer))) install.packages ("stargazer")
if (!(require(dplyr))) install.packages ("dplyr")
if (!(require(data.table))) install.packages ("data.table")
if (!(require(ggplot2))) install.packages ("ggplot2")

# load SMM/CPR Calculator function
source("SMM.R")

gc()
options(scipen=999)
options(digits=5)

load("C:/Users/Kons/OneDrive/MASTERARBEIT/DATA/PREPAYMENTSURV/FNMA.Rda")

### Further Data adjustments##
FNMA$OPTION.BI <- cut(FNMA$OPTION.LAG1, breaks=c(-Inf,0.75, Inf), labels=c("OTM","ITM"))
FNMA$LTV <- (FNMA$LAST_UPB/FNMA$HOUSEPRICE.DEV)*100
FNMA$LTV.CAT <- cut(FNMA$LTV, breaks=c(-Inf,65,80, Inf), labels=c("<65","65-80",">80"))
FNMA <- within(FNMA, LTV.CAT <- relevel(LTV.CAT, ref = 2))
FNMA$OCC_STAT[FNMA$OCC_STAT=="S"]<- "P"
FNMA$OCC_STAT <- factor(FNMA$OCC_STAT)
FNMA <- within(FNMA, OCC_STAT <- relevel(OCC_STAT, ref = 2))
FNMA$VOL.CAT <- cut(FNMA$ORIG_AMT, breaks=c(-Inf,150000,300000, Inf), labels=c("<150", "150-300",">300"))
FNMA <- within(FNMA, VOL.CAT <- relevel(VOL.CAT, ref = 2))
FNMA <- na.omit(FNMA)





###Kaplan-Meier estimates###

fit.null <- survfit(Surv(START,STOP,PREPAID)~1, data=FNMA)
ggsurvplot(fit.null, risk.table = TRUE,  palette=c("#86BC25"), censor=FALSE, conf.int = TRUE, xlab="Month", ggtheme=theme_bw(), legend=c("none"))
SMM.null<- SMM(fit.null)
ggplot(SMM.null, aes(x=time, y=CPR)) +
  geom_line(aes(),colour="#86BC25")+
  theme_bw()

fit.VOL <- survfit(Surv(START,STOP,PREPAID)~strata(VOL.CAT), data=FNMA)
ggsurvplot(fit.VOL, risk.table = TRUE,  palette=c("#86BC25","#0076A8","#BBBCBC"), 
           censor=FALSE, conf.int = TRUE, xlab="Month", ggtheme=theme_bw(),
           legend=c(0.9, 0.8),legend.labs = c("150k$-300k$","<150k$",">300k$"),legend.title="Credit Volume")
SMM.VOL<- SMM(fit.VOL)
#@Jordan: Can u change the legend and the colors to the same as in the plot above=
ggplot(SMM.VOL, aes(x=time, y=CPR, group=Strata)) +
  geom_line(aes(color=Strata))+
  theme_bw()


fit.LTV <- survfit(Surv(START,STOP,PREPAID)~strata(LTV.CAT), data=FNMA)
SMM.LTV<- SMM(fit.LTV)
#@Jordan: Can u change the legend and the colors to the same as in the plot above=
ggplot(SMM.LTV, aes(x=time, y=CPR, group=Strata)) +
  geom_line(aes(color=Strata))+
  theme_bw()
ggsurvplot(fit.LTV, risk.table = TRUE,  palette=c("#86BC25","#0076A8","#BBBCBC"), 
           censor=FALSE, conf.int = TRUE, xlab="Month", ggtheme=theme_bw(),
           legend=c(0.9, 0.8),legend.labs = c("65-80","<65",">80"),legend.title="LTV")

fit.OPTION <- survfit(Surv(START,STOP,PREPAID)~strata(OPTION.BI), data=FNMA)
SMM.OPTION<- SMM(fit.OPTION)
#@Jordan: Can u change the legend and the colors to the same as in the plot above=
ggplot(SMM.OPTION, aes(x=time, y=CPR, group=Strata)) +
  geom_line(aes(color=Strata))+
  theme_bw()
ggsurvplot(fit.OPTION, risk.table = TRUE,  palette=c("#86BC25","#0076A8"), 
           censor=FALSE, conf.int = TRUE, xlab="Month", ggtheme=theme_bw(),
           legend=c(0.9, 0.8),legend.labs = c("OTM","ITM"),legend.title="LTV")
