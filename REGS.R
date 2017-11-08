if (!(require(survminer))) install.packages ("survminer")
if (!(require(survival))) install.packages ("survival")
if (!(require(stargazer))) install.packages ("stargazer")
if (!(require(dplyr))) install.packages ("dplyr")
if (!(require(Hmisc))) install.packages ("Hmisc")



gc()


options(scipen=999)
options(digits=5)



load("C:/Users/Kons/OneDrive/MASTERARBEIT/DATA/FNMA.Rda")

FNMA$OPTION <- (FNMA$ORIG_RT-FNMA$MINTEREST)
FNMA$CANCELFEE <- ((FNMA$FEE*FNMA$LAST_UPB)/100)/FNMA$LAST_UPB

FNMA$OPTION_LAG1 <- Lag(FNMA$OPTION,1)
FNMA$OPTIONCAT <- cut(FNMA$OPTION, breaks=c(-Inf,-0.25, 0.25, Inf), labels=c("OTM","ATM","ITM"))
FNMA$OPTIONCAT_LAG1 <- cut(FNMA$OPTION_LAG1, breaks=c(-Inf,-0.25, 0.25, Inf), labels=c("OTM","ATM","ITM"))



#FNMA<- na.omit(FNMA)
#relevel baseline to FNMA OPTIONCAT 
FNMA <- within(FNMA, OPTIONCAT <- relevel(OPTIONCAT, ref = 2))
FNMA <- within(FNMA, OPTIONCAT_LAG1 <- relevel(OPTIONCAT_LAG1, ref = 2))
FNMA$SEASON <- as.factor(FNMA$SEASON)
FNMA <- within(FNMA, SEASON <- relevel(SEASON, ref = 1))
FNMA$SEASON <- factor(FNMA$SEASON, levels = c("Januar", "Februar", "März", "April", "Mai", "Juni","Juli","August","September","Oktober","November","Dezember"))



option1<-coxph(Surv(START,STOP,PREPAID)~OPTION+CANCELFEE+REGION+DTI+CSCORE+OLTV+UNEMP+PROP_TYP+OCC_STAT+SEASON+SHILLER, data=FNMA)
option2<-coxph(Surv(START,STOP,PREPAID)~OPTION_LAG1+CANCELFEE+REGION+DTI+CSCORE+OLTV+UNEMP+PROP_TYP+OCC_STAT+SEASON+SHILLER, data=FNMA)
option3<-coxph(Surv(START,STOP,PREPAID)~OPTIONCAT+CANCELFEE+REGION+DTI+CSCORE+OLTV+UNEMP+PROP_TYP+OCC_STAT+SEASON+SHILLER, data=FNMA)
option4<-coxph(Surv(START,STOP,PREPAID)~OPTIONCAT_LAG1+CANCELFEE+REGION+DTI+CSCORE+OLTV+UNEMP+PROP_TYP+OCC_STAT+SEASON+SHILLER, data=FNMA)


summary(option1)
summary(option2)
summary(option3)
summary(option4)

extractAIC(option1)
extractAIC(option2)
extractAIC(option3)
extractAIC(option4)

# Lagged Variables model has better AIC, but OTM is not significant
# Categorize have better properties plus no issues to handle nonlinearities, so choose Option 3


psplines1<-coxph(Surv(START,STOP,PREPAID)~pspline(OPTION)+REGION+DTI+CSCORE+OLTV+UNEMP+PROP_TYP+OCC_STAT+SHILLER+SEASON, data=FNMA)
termplot(psplines1)
# never get a S-Shape for some reason, but I honestly do not really now what this pspline function actually does...


# OLD OLD rest is irrelevant so far

#create shiller growth dummies to determine if house price growth is growing or not
FNMA$GSIGN <- sign(FNMA$SHGROWTH)
FNMA$GSIGN[FNMA$GSIGN==-1] <- 0
FNMA$GSIGN <- as.character(FNMA$GSIGN)



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