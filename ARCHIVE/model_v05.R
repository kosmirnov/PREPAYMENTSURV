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

FNMA <- within(FNMA, OPTION.CAT <- relevel(OPTION.CAT, ref = 2))
FNMA$OPTION.BI <- cut(FNMA$OPTION.LAG1, breaks=c(-Inf,0.5, Inf), labels=c("OTM","ITM"))

#continous model
FNMA$LTV <- (FNMA$LAST_UPB/FNMA$HOUSEPRICE.DEV)*100
FNMA$LTV.CAT <- cut(FNMA$LTV, breaks=c(-Inf,75, Inf), labels=c("<65",">80"))
FNMA <- within(FNMA, LTV.CAT <- relevel(LTV.CAT, ref = 2))

FNMA$DTI.CAT <- cut(FNMA$LTV, breaks=c(-Inf,40, Inf), labels=c("<40",">=40"))

FNMA$VOL.CAT <- cut(FNMA$ORIG_AMT, breaks=c(-Inf,200000, Inf), labels=c("<200000",">200000"))

FNMA$CSCORE.CAT <- cut(FNMA$CSCORE, breaks=c(-Inf,775, Inf), labels=c("<775",">775"))

FNMA$OCC_STAT[FNMA$OCC_STAT=="S"]<- "P"
FNMA$OCC_STAT <- factor(FNMA$OCC_STAT)
FNMA <- within(FNMA, OCC_STAT <- relevel(OCC_STAT, ref = 2))


FINAL_cont<-coxph(Surv(START,STOP,PREPAID)~OPTION+LTV+ORIG_AMT+CSCORE+DTI+UNEMP+REGION+YEAR, data=FNMA)
summary(FINAL_cont)

FINAL_disc<-coxph(Surv(START,STOP,PREPAID)~OPTION.BI+LTV.CAT+VOL.CAT+CSCORE.CAT+DTI.CAT+UNEMP+REGION+YEAR, data=FNMA)
summary(FINAL_disc)
zph_disc <- cox.zph(FINAL_disc)
zph_disc


FINAL<-coxph(Surv(START,STOP,PREPAID)~OPTION.BI+LTV.CAT+VOL.CAT+CSCORE.CAT+DTI.CAT+UNEMP+REGION+YEAR+OPTION.BI:START, data=FNMA)
summary(FINAL_disc)
zph_disc <- cox.zph(FINAL_disc)
zph_disc

FINAL_Full<-coxph(Surv(START,STOP,PREPAID)~OPTION.BI+CSCORE+CSCORE:START+LTV.CAT+LTV.CAT:START+OCC_STAT+OCC_STAT:START+OPTION.BI:START+DTI.CAT+DTI.CAT:START+UNEMP+VOL.CAT+VOL.CAT:START, data=FNMA)
summary(FINAL_Full)
ass <- cox.zph(FINAL_Full)
ass
ggcoxzph(ass,var=c("OPTION.BIITM","LTV.CAT<65","LTV.CAT>80","CSCORE"),  resid=FALSE,se=TRUE)
ggcoxzph(ass,var=c("LTV","OPTION.BIITM:START"),  resid=FALSE,se=TRUE)




extractAIC(FINAL_Full)
anova(FINAL_NoPH,FINAL_BETA)

stargazer()


#FNMA <- within(FNMA, OPTIONCAT.GLENN <- relevel(OPTIONCAT.GLENN, ref = 4))

# Lagged Variables model has better AIC, but OTM is not significant
# Categorize have better properties plus no issues to handle nonlinearities, so choose Option 3

# never get a S-Shape for some reason, but I honestly do not really now what this pspline function actually does...


# OLD OLD rest is irrelevant so far






# Cox-Regressions
Option.test1<-coxph(Surv(START,STOP,PREPAID)~OPTIONCAT+SHILLER+MSAREG+PURPOSE+OCC_STAT+PROP_TYP+OLTV+DTI+CSCORE+REGION+ORIG_AMT, data=FNMA)
summary(Option.test1)

Option.test2<-coxph(Surv(START,STOP,PREPAID)~pspline(OPTION)+PURPOSE+DTi+CSCORE+REGION, data=FNMA)
summary(Option.test2)
termplot(Option.test1)

ggcoxfunctional(Surv(START,STOP,ZEROBAL==1)~OPTIONTRESH, data=FNMA)
ggcoxdiagnostics(Option.test1)
ggco


Option.test2 <- coxph(Surv(START,STOP,ZEROBAL), data=FNMA)


residuals <- resid(Option.test2, type="martingale")
b <- residuals[seq(1, length(residuals), 1000)]
a <- FNMA$OLTV
a <- a[seq(1,length(a),1000)]

plot(0,0,xlab="Waiting time",
     ylab="Martingale residuals")
lines(lowess(b~a)$x,lowess(b~a)$y)

test=loess(b~a)



summary(Option.test1)
summary(Option.test2)


coxsnellres=FNMA$ZEROBAL-resid(Option.test1,type="martingale")

## Then using NA method to estimate the cumulative hazard function for residuals;
fitres=survfit(coxph(Surv(coxsnellres,FNMA$ZEROBAL)~1,method='breslow'),type='aalen')
plot(fitres$time,-log(fitres$surv),type='s',xlab='Cox-Snell Residuals', 
     ylab='Estimated Cumulative Hazard Function',
     main='Figure 11.1 on page 356')
abline(0,1,col='red',lty=2)

## Alternatively, one may use
fit4=survfit(Surv(coxsnellres,bmt$d3)~1)
Htilde=cumsum(fit4$n.event/fit4$n.risk)
plot(fit4$time,Htilde,type='s',col='blue')
abline(0,1,col='red',lty=2)










# OLD #
FNMA$CSCORECAT <- car::recode(FNMA$CSCORE,"lo:649=0; 650:749=1; 750:hi=2")
FNMA$OPTIONBI <- car::recode(FNMA$OPTIONTRESH,"lo:0=0; 0.0001:hi=1")
FNMA$OPTIONCAT <- car::recode(FNMA$OPTIONTRESH,"lo:-1=-2; -0.9999:-0.5=-1; -0.4999:0=0; ; 0.0001:0.5=1; 0.5:1=2; 1.0001:hi=3")
FNMA$OPTIONCAT <- as.factor(FNMA$OPTIONCAT)
FNMA$OPTIONBI <- as.factor(FNMA$OPTIONBI)
Option.test1<-coxph(Surv(START,STOP,ZEROBAL)~OPTIONTRESH, data=FNMA)
Option.test2<-coxph(Surv(START,STOP,ZEROBAL)~OPTIONBI, data=FNMA)

Option.test3<-coxph(Surv(START,STOP,ZEROBAL)~OPTIONBI+CSCORECAT + DTI + OLTV + PROP_TYP + MONTH, data=FNMA)
Option.test4<-coxph(Surv(START,STOP,ZEROBAL)~OPTIONBI+strata(CSCORECAT) + DTI + OLTV + PROP_TYP + MONTH, data=FNMA)
Option.test6<-coxph(Surv(START,STOP,ZEROBAL)~OPTIONCAT+CSCORECAT , data=FNMA)

anova(Option.test3,Option.test4)
summary(Option.test1)
summary(Option.test2)
summary(Option.test3)
summary(Option.test4)
summary(Option.test5)

anova(Option.test3,Option.test4)
anova(Option.test5,Option.test4)
Month.test1<-coxph(Surv(START,STOP,ZEROBAL)~MONTH+OPTIONBI+CSCORECAT, data=FNMA, )
summary(Month.test1)

cox.ass1 <- cox.zph(Option.test4)
cox.ass1 <- cox.zph(Option.test6)
ggcoxzph(cox.ass1)


save(FNMA, file="FNMA_REG.Rda")


cox.ass <- cox.zph(Option.test5)
ggcpxzph(cox.ass)

ggcoxdiagnostics(Option.test, type = "dfbeta",
               linear.predictions = FALSE, ggtheme = theme_bw())

stargazer(Option.test5, type="latex")
fit <- survfit(Option.test5)
ggsurvplot(fit, conf.int = TRUE,
           ggtheme = theme_minimal()
           
           
           
           coxph.fit2 <- coxph(Surv(futime, fustat) ~ age + ecog.ps, data=ovarian)
           ggcoxdiagnostics(coxph.fit2, type = "deviance")