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


FNMA[, OPTION.LAG1:=c(NA, OPTION[-.N]), by=LOAN_ID]
FNMA[, OPTION.LAG2:=c(NA, OPTION.LAG1[-.N]), by=LOAN_ID]
FNMA[, OPTION.LAG3:=c(NA, OPTION.LAG2[-.N]), by=LOAN_ID]
FNMA[, OPTION.LAG4:=c(NA, OPTION.LAG3[-.N]), by=LOAN_ID]
FNMA[, OPTION.LAG5:=c(NA, OPTION.LAG4[-.N]), by=LOAN_ID]
FNMA[, OPTION.LAG6:=c(NA, OPTION.LAG5[-.N]), by=LOAN_ID]

#FNMA<- na.omit(FNMA)
lag1<-coxph(Surv(START,STOP,PREPAID)~OPTION, data=FNMA)
lag2<-coxph(Surv(START,STOP,PREPAID)~OPTION.LAG1, data=FNMA)
lag3<-coxph(Surv(START,STOP,PREPAID)~OPTION.LAG2, data=FNMA)
lag4<-coxph(Surv(START,STOP,PREPAID)~OPTION.LAG3, data=FNMA)
lag5<-coxph(Surv(START,STOP,PREPAID)~OPTION.LAG4, data=FNMA)
lag6<-coxph(Surv(START,STOP,PREPAID)~OPTION.LAG5, data=FNMA)
lag7<-coxph(Surv(START,STOP,PREPAID)~OPTION.LAG6, data=FNMA)


summary(lag1)
summary(lag2)
summary(lag3)
summary(lag4)
summary(lag5)
summary(lag6)
summary(lag7)

extractAIC(lag1)
extractAIC(lag2)
extractAIC(lag3)
extractAIC(lag3)
extractAIC(lag4)
extractAIC(lag5)
extractAIC(lag6)
extractAIC(lag7)



## lagged cat


FNMA$OPTIONCAT <- cut(FNMA$OPTION, breaks=c(-Inf,0, 0.75, Inf), labels=c("OTM","ATM","ITM"))
FNMA$OPTIONCAT.LAG1 <- cut(FNMA$OPTION.LAG1, breaks=c(-Inf,0, 0.75, Inf), labels=c("OTM","ATM","ITM"))
FNMA$OPTIONCAT.LAG2 <- cut(FNMA$OPTION.LAG2, breaks=c(-Inf,0, 0.75, Inf), labels=c("OTM","ATM","ITM"))
FNMA$OPTIONCAT.LAG3 <- cut(FNMA$OPTION.LAG3, breaks=c(-Inf,0, 0.75, Inf), labels=c("OTM","ATM","ITM"))
FNMA$OPTIONCAT.LAG4 <- cut(FNMA$OPTION.LAG4, breaks=c(-Inf,0, 0.75, Inf), labels=c("OTM","ATM","ITM"))
FNMA$OPTIONCAT.LAG5 <- cut(FNMA$OPTION.LAG5, breaks=c(-Inf,0, 0.75, Inf), labels=c("OTM","ATM","ITM"))
FNMA$OPTIONCAT.LAG6 <- cut(FNMA$OPTION.LAG6, breaks=c(-Inf,0, 0.75, Inf), labels=c("OTM","ATM","ITM"))

FNMA <- within(FNMA, OPTIONCAT <- relevel(OPTIONCAT, ref = 2))
FNMA <- within(FNMA, OPTIONCAT.LAG1 <- relevel(OPTIONCAT.LAG1, ref = 2))
FNMA <- within(FNMA, OPTIONCAT.LAG2 <- relevel(OPTIONCAT.LAG2, ref = 2))
FNMA <- within(FNMA, OPTIONCAT.LAG3 <- relevel(OPTIONCAT.LAG3, ref = 2))
FNMA <- within(FNMA, OPTIONCAT.LAG4 <- relevel(OPTIONCAT.LAG4, ref = 2))
FNMA <- within(FNMA, OPTIONCAT.LAG5 <- relevel(OPTIONCAT.LAG5, ref = 2))
FNMA <- within(FNMA, OPTIONCAT.LAG6 <- relevel(OPTIONCAT.LAG6, ref = 2))


##gg barplot

#relevel baseline to FNMA OPTIONCAT 






##
option<-coxph(Surv(START,STOP,PREPAID)~OPTION+REGION+DTI+CSCORE+OLTV+UNEMP+PROP_TYP+OCC_STAT+SEASON+SHILLER+Loan.Age, data=FNMA)
option1<-coxph(Surv(START,STOP,PREPAID)~OPTION.LAG1+REGION+DTI+CSCORE+OLTV+UNEMP+PROP_TYP+OCC_STAT+SEASON+SHILLER+Loan.Age, data=FNMA)
option2<-coxph(Surv(START,STOP,PREPAID)~OPTION.LAG2+REGION+DTI+CSCORE+OLTV+UNEMP+PROP_TYP+OCC_STAT+SEASON+SHILLER+Loan.Age, data=FNMA)
option3<-coxph(Surv(START,STOP,PREPAID)~OPTION.LAG3+REGION+DTI+CSCORE+OLTV+UNEMP+PROP_TYP+OCC_STAT+SEASON+SHILLER+Loan.Age, data=FNMA)
option4<-coxph(Surv(START,STOP,PREPAID)~OPTION.LAG4+REGION+DTI+CSCORE+OLTV+UNEMP+PROP_TYP+OCC_STAT+SEASON+SHILLER+Loan.Age, data=FNMA)
option5<-coxph(Surv(START,STOP,PREPAID)~OPTION.LAG5+REGION+DTI+CSCORE+OLTV+UNEMP+PROP_TYP+OCC_STAT+SEASON+SHILLER+Loan.Age, data=FNMA)
option6<-coxph(Surv(START,STOP,PREPAID)~OPTION.LAG6+REGION+DTI+CSCORE+OLTV+UNEMP+PROP_TYP+OCC_STAT+SEASON+SHILLER+Loan.Age, data=FNMA)



summary(option)#not significant
summary(option1)# p 0.0095, beta= 0.063553, AIC=387249
summary(option2)# p 0.0017, beta= 0.0767, AIC= 387251 <--- fits best stastisscally and economically
summary(option3)#p. 0.0048, beta= 0.069, AIC= 387257
summary(option4)#not significant
summary(option5)#not significant
summary(option6)#not significant

extractAIC(option)
extractAIC(option2)
extractAIC(option3)
extractAIC(option4)
extractAIC(option5)
extractAIC(option6)




#now try regression with categorized variables

optioncat<-coxph(Surv(START,STOP,PREPAID)~OPTIONCAT+REGION+DTI+CSCORE+OLTV+UNEMP+PROP_TYP+OCC_STAT+SEASON+SHILLER+Loan.Age, data=FNMA)
optioncat1<-coxph(Surv(START,STOP,PREPAID)~OPTIONCAT.LAG1+REGION+DTI+CSCORE+OLTV+UNEMP+PROP_TYP+OCC_STAT+SEASON+SHILLER+Loan.Age, data=FNMA)
optioncat2<-coxph(Surv(START,STOP,PREPAID)~OPTIONCAT.LAG2+REGION+DTI+CSCORE+OLTV+UNEMP+PROP_TYP+OCC_STAT+SEASON+SHILLER+Loan.Age, data=FNMA)
optioncat3<-coxph(Surv(START,STOP,PREPAID)~OPTIONCAT.LAG3+REGION+DTI+CSCORE+OLTV+UNEMP+PROP_TYP+OCC_STAT+SEASON+SHILLER+Loan.Age, data=FNMA)
optioncat4<-coxph(Surv(START,STOP,PREPAID)~OPTIONCAT.LAG4+REGION+DTI+CSCORE+OLTV+UNEMP+PROP_TYP+OCC_STAT+SEASON+SHILLER+Loan.Age, data=FNMA)
optioncat5<-coxph(Surv(START,STOP,PREPAID)~OPTIONCAT.LAG5+REGION+DTI+CSCORE+OLTV+UNEMP+PROP_TYP+OCC_STAT+SEASON+SHILLER+Loan.Age, data=FNMA)
optioncat6<-coxph(Surv(START,STOP,PREPAID)~OPTIONCAT.LAG6+REGION+DTI+CSCORE+OLTV+UNEMP+PROP_TYP+OCC_STAT+SEASON+SHILLER+Loan.Age, data=FNMA)


summary(optioncat)#...
summary(optioncat1)# OTM= 0.728, ATM= 0.888, AIC=387075
summary(optioncat2)# OTM= 0.771, ATM= 0.899, AIC=387129
summary(optioncat3)# OTM= 0.795, ATM= 0.936, AIC=387165
summary(optioncat4)#
summary(optioncat5)#
summary(optioncat6)#

extractAIC(option)
extractAIC(optioncat1)
extractAIC(optioncat2)
extractAIC(optioncat3)
extractAIC(optioncat4)
extractAIC(optioncat5)
extractAIC(optioncat6)


cat1_count <- count(FNMA,FNMA$OPTIONCAT)
### so far from AIC point of view, AIC for model = "optioncat1" fits the best.
## adjusting categories..

cat2 = c(-Inf,-0.25, 0.25, Inf)
FNMA$OPTIONCAT2 <- cut(FNMA$OPTION, breaks=cat2, labels=c("OTM","ATM","ITM"))
FNMA$OPTIONCAT2.LAG1 <- cut(FNMA$OPTION.LAG1, breaks=cat2, labels=c("OTM","ATM","ITM"))
FNMA$OPTIONCAT2.LAG2 <- cut(FNMA$OPTION.LAG2, breaks=cat2, labels=c("OTM","ATM","ITM"))
FNMA$OPTIONCAT2.LAG3 <- cut(FNMA$OPTION.LAG3, breaks=cat2, labels=c("OTM","ATM","ITM"))

FNMA <- within(FNMA, OPTIONCAT2.LAG1 <- relevel(OPTIONCAT2.LAG1, ref = 2))
FNMA <- within(FNMA, OPTIONCAT2.LAG2 <- relevel(OPTIONCAT2.LAG2, ref = 2))
FNMA <- within(FNMA, OPTIONCAT2.LAG3 <- relevel(OPTIONCAT2.LAG3, ref = 2))
#distribution
cat2_count <- count(FNMA,FNMA$OPTIONCAT2)

optioncat21<-coxph(Surv(START,STOP,PREPAID)~OPTIONCAT2.LAG1+REGION+DTI+CSCORE+OLTV+UNEMP+PROP_TYP+OCC_STAT+SEASON+SHILLER+Loan.Age, data=FNMA)
optioncat22<-coxph(Surv(START,STOP,PREPAID)~OPTIONCAT2.LAG2+REGION+DTI+CSCORE+OLTV+UNEMP+PROP_TYP+OCC_STAT+SEASON+SHILLER+Loan.Age, data=FNMA)
optioncat23<-coxph(Surv(START,STOP,PREPAID)~OPTIONCAT2.LAG3+REGION+DTI+CSCORE+OLTV+UNEMP+PROP_TYP+OCC_STAT+SEASON+SHILLER+Loan.Age, data=FNMA)

summary(optioncat21)# AIC=387056
summary(optioncat22)# AIC=387093
summary(optioncat23)# AIC=387149

extractAIC(optioncat21)
extractAIC(optioncat22)
extractAIC(optioncat23)

## choose between lag 1 and lag 2







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