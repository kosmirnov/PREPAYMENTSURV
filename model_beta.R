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

FNMA$OPTION.BI <- cut(FNMA$OPTION.LAG1, breaks=c(-Inf,0.5, Inf), labels=c("OTM","ITM"))

FNMA$LTV <- (FNMA$LAST_UPB/FNMA$HOUSEPRICE.DEV)*100
FNMA$LTV.CAT <- cut(FNMA$LTV, breaks=c(-Inf,65,80, Inf), labels=c("<65","65-80",">80"))
FNMA <- within(FNMA, LTV.CAT <- relevel(LTV.CAT, ref = 2))

FNMA$DTI.CAT <- cut(FNMA$LTV, breaks=c(-Inf,40, Inf), labels=c("<40",">=40"))

FNMA$VOL.CAT <- cut(FNMA$ORIG_AMT, breaks=c(-Inf,150000,300000, Inf), labels=c("<150,000$","150,000$-300,000$",">300,000$"))
FNMA <- within(FNMA, VOL.CAT <- relevel(VOL.CAT, ref = 2))

FNMA$CSCORE.CAT <- cut(FNMA$CSCORE, breaks=c(-Inf,775, Inf), labels=c("<775",">775"))

FNMA$OCC_STAT[FNMA$OCC_STAT=="S"]<- "P"
FNMA$OCC_STAT <- factor(FNMA$OCC_STAT)
FNMA <- within(FNMA, OCC_STAT <- relevel(OCC_STAT, ref = 2))


### Regression Models ##
## continous model suggests that all analysed variable are strongly significant. Tough, variables like DTI and CSCORE seem not to have a strong impact tough.
FINAL_cont<-coxph(Surv(START,STOP,PREPAID)~OPTION+CSCORE+LTV+OCC_STAT+DTI+VOL.CAT+UNEMP+YEAR, data=FNMA)
summary(FINAL_cont)
zph_cont <- cox.zph(FINAL_cont)
zph_cont

ggcoxzph(zph_cont,var=c("OPTION","VOL.CAT<150,000$","LTV","DTI"),  resid=FALSE,se=TRUE, title=FALSE)

## PH Assumptions are tough violated. As you can see OPTION shows that for the pure interest-driven incentive, this assumption
ggcoxzph(zph_cont,var=c("OPTION","LTV","VOL.CAT>200000"),  resid=FALSE,se=TRUE)
# is truly heavily violated. This is in line with the theory. The function basically represents the Burnout
# However, for LTV PH assumption is violated fomrally. Having a closer looks shows that the time-effect is truly neglible.
extractAIC(FINAL_cont)

## For deeper analysis, use a discrete model...
FINAL_disc<-coxph(Surv(START,STOP,PREPAID)~OPTION.BI+LTV.CAT+VOL.CAT+OCC_STAT+CSCORE+DTI+UNEMP+YEAR, data=FNMA)
summary(FINAL_disc)
zph_disc <- cox.zph(FINAL_disc)
zph_disc
## burn out##
ggcoxzph(zph_disc,var=c("OPTION.BIITM","LTV.CAT>80", subtitle="test"),  resid=FALSE,se=TRUE)
extractAIC(FINAL_disc)

## adding time-depended factors. All factors were adjusted with time-depended covariates beta(t)X(t).
FINAL_Full<-coxph(Surv(START,STOP,PREPAID)~OPTION.BI+CSCORE+CSCORE:START+LTV.CAT+LTV.CAT:START+OCC_STAT+OCC_STAT:START+DTI+UNEMP+VOL.CAT+VOL.CAT:START+OPTION.BI:START, data=FNMA)
summary(FINAL_Full)
ass <- cox.zph(FINAL_Full)
ass
ggcoxzph(ass,var=c("OPTION.BIITM"),  resid=FALSE,se=TRUE)
ggforest(FINAL_Full)


####
extractAIC(FINAL_Full)


