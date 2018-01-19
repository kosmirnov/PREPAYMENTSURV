if (!(require(survminer))) install.packages ("survminer")
if (!(require(survival))) install.packages ("survival")
if (!(require(stargazer))) install.packages ("stargazer")
if (!(require(dplyr))) install.packages ("dplyr")
if (!(require(data.table))) install.packages ("data.table")
if (!(require(ggplot2))) install.packages ("ggplot2")



gc()


options(scipen=999)
options(digits=5)



load("C:/Users/Kons/OneDrive/MASTERARBEIT/DATA/PREPAYMENTSURV/FNMA.Rda")

FNMA$SEASON <- as.factor(FNMA$SEASON)
FNMA <- within(FNMA, SEASON <- relevel(SEASON, ref = 1))
FNMA$SEASON <- factor(FNMA$SEASON, levels = c("Januar", "Februar", "März", "April", "Mai", "Juni","Juli","August","September","Oktober","November","Dezember"))
FNMA$OPTION <- (FNMA$ORIG_RT-FNMA$MINTEREST)
#FNMA$CANCELFEE <- ((FNMA$FEE*FNMA$LAST_UPB)/100)/FNMA$LAST_UPB
## lagged cat
FNMA[, OPTION.LAG1:=c(NA, OPTION[-.N]), by=LOAN_ID]
FNMA$OPTIONCAT <- cut(FNMA$OPTION.LAG1, breaks=c(-Inf,0, 1.5, Inf), labels=c("OTM","ATM","ITM"))
FNMA <- within(FNMA, OPTIONCAT <- relevel(OPTIONCAT, ref = 2))

#shiller growth##
FNMA[, SHILLER.LAG1:=c(NA, SHILLER[-.N]), by=LOAN_ID]
FNMA$SHILLER.LEVEL <- FNMA$SHILLER - FNMA$SHILLER.LAG1
FNMA$SHILLER.LEVEL <- (FNMA$SHILLER.LEVEL/FNMA$SHILLER.LAG1)

FNMA$OLTV = FNMA$OLTV/100
FNMA$HOUSEPRICE= FNMA$ORIG_AMT/FNMA$OLTV
FNMA$EQUITY = FNMA$HOUSEPRICE - FNMA$ORIG_AMT
FNMA$INTEREST.MONTHLY = FNMA$LAST_UPB*FNMA$ORIG_RT/12/100

FNMA<- na.omit(FNMA)

FNMA <- FNMA %>% 
 group_by(LOAN_ID) %>% 
 mutate(HOUSEPRICE.DEV = HOUSEPRICE*cumprod(1+SHGROWTH))%>%
 ungroup()%>%
 as.data.table()

FNMA$ROE <- ((FNMA$HOUSEPRICE.DEV - FNMA$HOUSEPRICE - FNMA$INTEREST.MONTHLY)/FNMA$EQUITY)*100



FNMA$ROE.SIGN <- sign(FNMA$ROE)
FNMA$ROE.SIGN[FNMA$ROE.SIGN==-1] <- 0
FNMA$ROE.SIGN <- as.character(FNMA$ROE.SIGN)



FNMA$ROECAT <- cut(FNMA$ROE, breaks=c(-Inf,-50,-30,-15,15,30,50, Inf), labels=c("DDOTM","DOTM","OTM","ATM","ITM","DITM","DDITM"))
FNMA <- within(FNMA, ROECAT <- relevel(ROECAT, ref = 4))

FNMA$ROECAT2 <- cut(FNMA$ROE, breaks=c(-Inf,-30,30, Inf), labels=c("OTM","ATM","ITM"))
FNMA <- within(FNMA, ROECAT2 <- relevel(ROECAT2, ref = 2))


# final model
#
FNMA$CSCORE.CAT <- cut(FNMA$CSCORE, breaks=c(-Inf,650,700,750, Inf), labels=c("RLOW","LOW","OK","GOOD"))
FNMA <- within(FNMA, CSCORE.CAT <- relevel(CSCORE.CAT, ref = 3))
count_cscore <- count(FNMA,FNMA$CSCORE.CAT)


final<-coxph(Surv(START,STOP,PREPAID)~OPTIONCAT+ROE.SIGN+REGION+strata(CSCORE.CAT)+DTI+UNEMP+PROP_TYP+OCC_STAT+SEASON, data=FNMA)
summary(final)
extractAIC(cscore1)
ass2<- cox.zph(final)

ggsurvplot(survfit(cscore1))

#val_eod <- aggregate(FNMA$START, by = list(FNMA$LOAN_ID), FUN = tail, n = 1)
#FNMA <- inner_join(FNMA, val_eod, by = c("LOAN_ID" = "Group.1"))
#names(FNMA)[names(FNMA)=="x"] <- "LOAN_AGE"




FNMA <- FNMA %>% 
  group_by(LOAN_ID) %>% 
  mutate(TAXCOST = sum(TAXCOST.MONTHLY))%>%
  ungroup()%>%
  as.data.table()

FNMA$NETPROFIT = FNMA$HOUSEPRICE.DEV - FNMA$ORIG_AMT -FNMA$TAXCOST -FNMA$EQUITY
FNMA$PROFIT <- FNMA$HOUSEPRICE.DEV-FNMA$HOUSEPRICE


#FNMA$SH.SIGN <- sign(FNMA$SHILLER.MEAN)
#FNMA$SH.SIGN[FNMA$SH.SIGN==-1] <- 0
#FNMA$SH.SIGN <- as.character(FNMA$SH.SIGN)


#FNMA_AGG <-FNMA[, .SD[.N], by ="LOAN_ID"] 

#FNMA$CANCELFEE= FNMA$FEE*FNMA$LAST_UPB


#FNMA$SHILLERCAT <- cut(FNMA$SHILLER.MEAN, breaks=c(-Inf,-0.75,0,0.75, Inf), labels=c("DOTM","OTM","ATM","ITM"))
#FNMA <- within(FNMA, SHILLERCAT <- relevel(SHILLERCAT, ref = 3))

shiler.count <- count(FNMA,FNMA$SHILLERCAT)
shiller.count <- count(FNMA,FNMA$SHI)




option_spline<-coxph(Surv(START,STOP,PREPAID)~pspline(OPTION, df=4)+pspline(ROE, df=4)+REGION+DTI+CSCORE+UNEMP+PROP_TYP+OCC_STAT+SEASON, data=FNMA)
termplot(option_spline)
  
#relevel baseline to FNMA OPTIONCAT 
# check for functional form


optioncat1<-coxph(Surv(START,STOP,PREPAID)~OPTIONCAT+SHGROWTH+REGION+DTI+CSCORE+OLTV+UNEMP+PROP_TYP+OCC_STAT+SEASON+Loan.Age, data=FNMA)
summary(optioncat1)# OTM= 0.728, ATM= 0.888, AIC=387075
extractAIC(optioncat1)
optioncat1<-coxph(Surv(START,STOP,PREPAID)~OPTION+SHILLER+REGION+DTI+CSCORE+OLTV+UNEMP+PROP_TYP+OCC_STAT+SEASON+Loan.Age, data=FNMA)



shiller1<-coxph(Surv(START,STOP,PREPAID)~OPTIONCAT+SHILLER.LEVEL+REGION+DTI+CSCORE+OLTV+UNEMP+PROP_TYP+OCC_STAT+SEASON+Loan.Age, data=FNMA)
summary(shiller1)




# glenn
breaks<- seq(-1,4,0.25)
breaklabel<- c("-1","-0.75","-0.5","-0.25","0","0.25","0.5","0.75","1","1.25","1.5","1.75","2","2.25","2.5","2.75","3","3.25","3.5","3.75")
FNMA$OPTIONCAT.GLENN <- cut(FNMA$OPTION.LAG, breaks=breaks,labels=breaklabel) 
GLENN <- count(FNMA,FNMA$OPTIONCAT.GLENN)

glennreg<-coxph(Surv(START,STOP,PREPAID)~OPTIONCAT.GLENN+CSCORE+DTI+OLTV+REGION, data=FNMA)

#FNMA <- within(FNMA, OPTIONCAT.GLENN <- relevel(OPTIONCAT.GLENN, ref = 4))

# Lagged Variables model has better AIC, but OTM is not significant
# Categorize have better properties plus no issues to handle nonlinearities, so choose Option 3

# never get a S-Shape for some reason, but I honestly do not really now what this pspline function actually does...


# OLD OLD rest is irrelevant so far

#create shiller growth dummies to determine if house price growth is growing or not



FNMA$START = FNMA$START-1
FNMA$STOP = FNMA$STOP-1

option.cat1 <-coxph(Surv(START,STOP,PREPAID)~pspline(OPTION_LAG1), data=FNMA)
summary(option.cat1)
extractAIC(option.cat1)
termplot(option.cat1)

#regressions
option1 <-coxph(Surv(START,STOP,PREPAID)~OPTIONCAT_LAG1, data=FNMA)
summary(option1)
extractAIC(option1)

option.cat2 <-coxph(Surv(START,STOP,PREPAID)~OPTIONCAT_LAG1+SHILLER, data=FNMA)
summary(option.cat2)
extractAIC(option.cat2)


option.cat3 <-coxph(Surv(START,STOP,PREPAID)~OPTIONCAT_LAG1+SHILLER+MSAREG+CSCORE+OLTV+DTI, data=FNMA)
summary(option.cat3)
extractAIC(option.cat3)




option.cat2 <-coxph(Surv(START,STOP,PREPAID)~OPTIONCAT_LAG1, data=FNMA)
summary(option.cat2)
extractAIC(option.cat2)
# take cat lag 1 

option.lag1<-coxph(Surv(START,STOP,PREPAID)~OPTION_LAG1, data=FNMA)
summary(option.lag1)
extractAIC(option.lag1)
ggsurvplot(survfit(option.lag1), data =FNMA , risk.table = TRUE, fun="haz")

# shiller 

step



# 1. test if OPTIONCAT is significant

step(coxph(Surv(START,STOP,PREPAID)~OPTIONCAT_LAG1+GSIGN+REGION+OLTV+DTI+CSCORE+PURPOSE+PROP_TYP, data=FNMA))









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