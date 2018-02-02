if (!(require(survminer))) install.packages ("survminer")
if (!(require(survival))) install.packages ("survival")
if (!(require(stargazer))) install.packages ("stargazer")
if (!(require(dplyr))) install.packages ("dplyr")
if (!(require(data.table))) install.packages ("data.table")
if (!(require(ggplot2))) install.packages ("ggplot2")

# load SMM/CPR Calculator function
setwd("C:/Users/Kons/OneDrive/MASTERARBEIT/DATA/PREPAYMENTSURV")

source("SMM.R")


gc()
options(scipen=999)
options(digits=5)

load("C:/Users/Kons/OneDrive/MASTERARBEIT/DATA/PREPAYMENTSURV/FNMA.Rda")




###################################################Kaplan-Meier estimates####################################################

### NULL MODEL: Kaplan Meier ###
fit.null <- survfit(Surv(START,STOP,PREPAID)~1, data=FNMA)
SMM.null<- SMM(fit.null)
# CPR plot
ggplot(SMM.null, aes(x=Month, y=CPR)) +
  geom_line(aes(),colour="#86BC25",size=1)+
  theme_bw()+
  scale_x_continuous(breaks = seq(0, 65, by = 10))+
  scale_y_continuous(breaks = seq(0, 0.4, by = 0.05),labels = scales::percent)
ggsave(file = "CPR_null.jpeg")
#Survival Curve plot
ggsurv.NULL <- ggsurvplot(fit.null, risk.table = TRUE,  palette=c("#86BC25"), censor=FALSE, conf.int = TRUE, xlab="Month", 
                          ggtheme=theme_bw(), legend=c("none"),break.x.by = 10,break.y.by = 0.1)
ggsurv.NULL
ggsave(file = "surv_null.jpeg", print(ggsurv.NULL))


##### YIELD: Kaplan Meier ####
fit.YIELD <- survfit(Surv(START,STOP,PREPAID)~strata(YIELD.CAT), data=FNMA)
SMM.YIELD<- SMM(fit.YIELD)
#CPR plot
ggplot(SMM.YIELD, aes(x=Month, y=CPR, group=Strata)) +
  geom_line(aes(color=Strata),size=1)+
  theme_bw()+
  scale_x_continuous(breaks = seq(0, 65, by = 10))+
  scale_y_continuous(breaks = seq(0, 0.4, by = 0.05),labels = scales::percent)+
  theme(legend.position = c(0.9, 0.8)) +
  scale_color_manual(name = "Yield Break (at 0.75%):",
                     breaks = c(1,2),
                     labels = c("Out of Money","In the Money"),
                     values = c("#86BC25","#0076A8"))
ggsave(file = "CPR_YIELD.jpeg")
#survival curves for YIELD
ggsurv.YIELD<- ggsurvplot(fit.YIELD, risk.table = TRUE,  palette=c("#86BC25","#0076A8"), 
                          censor=FALSE, conf.int = TRUE, xlab="Month", ggtheme=theme_bw(),
                          legend=c(0.9, 0.8),legend.labs = c("OTM","ITM"),legend.title="YIELD",
                          break.x.by = 10,break.y.by = 0.1)
ggsurv.YIELD
ggsave(file = "surv_YIELD.jpeg", print(ggsurv.YIELD))





##### VOL: Kaplan Meier ####
fit.VOL <- survfit(Surv(START,STOP,PREPAID)~strata(VOL.CAT), data=FNMA)
SMM.VOL<- SMM(fit.VOL)
#CPR plot
ggplot(SMM.VOL, aes(x=Month, y=CPR, group=Strata)) +
  geom_line(aes(color=Strata),size=1)+
  theme_bw()+
  scale_x_continuous(breaks = seq(0, 65, by = 10))+
  scale_y_continuous(breaks = seq(0, 0.6, by = 0.05),labels = scales::percent)+
  theme(legend.position = c(0.9, 0.8)) +
  scale_color_manual(name = "Credit Volume",
                     breaks = c(1,2),
                     labels = c("<= 350,000$",">350,000$"),
                     values = c("#86BC25","#0076A8"))
ggsave(file = "CPR_vol.jpeg")
#Survival Curve plot
ggsurv.VOL<- ggsurvplot(fit.VOL, risk.table = TRUE,  palette=c("#86BC25","#0076A8"), 
                          censor=FALSE, conf.int = TRUE, xlab="Month", ggtheme=theme_bw(),
                          legend=c(0.9, 0.8),legend.labs = c("<= 350,000$",">350,000$"),legend.title="Credit Volume",
                          break.x.by = 10,break.y.by = 0.1)
ggsurv.VOL
ggsave(file = "surv_vol.jpeg", print(ggsurv.VOL))




##### LTV: Kaplan Meier ####
fit.LTV <- survfit(Surv(START,STOP,PREPAID)~strata(LTV.CAT), data=FNMA)
SMM.LTV<- SMM(fit.LTV)
#CPR plot
ggplot(SMM.LTV, aes(x=Month, y=CPR, group=Strata)) +
  geom_line(aes(color=Strata),size=1)+
  theme_bw()+
  scale_x_continuous(breaks = seq(0, 65, by = 10))+
  scale_y_continuous(breaks = seq(0, 0.6, by = 0.05),labels = scales::percent)+
  theme(legend.position = c(0.9, 0.8)) +
  scale_color_manual(name = "LTV",
                     breaks = c(1,2,3),
                     labels = c("<60","60-75",">75"),
                     values = c("#86BC25","#0076A8","#BBBCBC"))
ggsave(file = "CPR_LTV.jpeg")
#Survival Curve plot
ggsurv.LTV<- ggsurvplot(fit.LTV, risk.table = TRUE,  palette=c("#86BC25","#0076A8","#BBBCBC"), 
                        censor=FALSE, conf.int = TRUE, xlab="Month", ggtheme=theme_bw(),
                        legend=c(0.9, 0.8),legend.labs = c("<60","60-75",">75"),legend.title="LTV",
                        break.x.by = 10,break.y.by = 0.1)
ggsurv.LTV
ggsave(file = "surv_LTV.jpeg", print(ggsurv.LTV))


##### TAX: Kaplan Meier ####

fit.tax <- survfit(Surv(START,STOP,PREPAID)~strata(TAX), data=FNMA)
SMM.tax<- SMM(fit.tax)
#CPR plot
ggplot(SMM.tax, aes(x=Month, y=CPR, group=Strata)) +
  geom_line(aes(color=Strata),size=1)+
  theme_bw()+
  scale_x_continuous(breaks = seq(0, 65, by = 10))+
  scale_y_continuous(breaks = seq(0, 0.4, by = 0.05),labels = scales::percent)+
  theme(legend.position = c(0.9, 0.8)) +
  scale_color_manual(name = "NY vs LA:",
                     breaks = c(1,2),
                     labels = c("LA","NY"),
                     values = c("#86BC25","#0076A8"))
ggsave(file = "CPR_tax.jpeg")
#survival curves for tax
ggsurv.tax<- ggsurvplot(fit.tax, risk.table = TRUE,  
                        censor=FALSE, conf.int = TRUE, xlab="Month", ggtheme=theme_bw(),
                        break.x.by = 10,break.y.by = 0.1)
ggsurv.tax





### Cox-PH Models ###
## continous model suggests that all analysed variable are strongly significant. Tough, variables like DTI and CSCORE seem not to have a strong impact tough.
FINAL_cont<-coxph(Surv(START,STOP,PREPAID)~OPTION+CSCORE+LTV+OCC_STAT+DTI+VOL.CAT+UNEMP+YEAR, data=FNMA)
summary(FINAL_cont)
extractAIC(FINAL_cont)
zph_cont <- cox.zph(FINAL_cont)
zph_cont
#first schoenfeld plots for continous
zphplot_cont<- ggcoxzph(zph_cont,var=c("OPTION","LTV","DTI"),  resid=FALSE,se=TRUE, title=FALSE, ggtheme=theme_bw(),xlab="Month")
length(zphplot_cont)
zphplot_cont[[1]] <- zphplot_cont[[1]] + labs(title = 'YIELD (p=0.0000)') 
zphplot_cont[[2]] <- zphplot_cont[[2]] + labs(title = 'LTV (p=0.0000)') 
zphplot_cont[[3]] <- zphplot_cont[[3]] + labs(title = 'DTI (p=0.6502)') 
ggsave(file = "zph_cont.jpeg", print(zphplot_cont))


## For deeper analysis, use a discrete model in which the most important factors were categorzied
FINAL_disc<-coxph(Surv(START,STOP,PREPAID)~YIELD.CAT+LTV.CAT+VOL.CAT+OCC_STAT+CSCORE+DTI+UNEMP+YEAR, data=FNMA)
summary(FINAL_disc)
extractAIC(FINAL_disc)
zph_disc <- cox.zph(FINAL_disc)
zph_disc
zphplot_disc <- ggcoxzph(zph_disc,var=c("YIELD.CATITM","LTV.CAT>75","VOL.CAT>350k"),  resid=FALSE,se=TRUE, title=FALSE, ggtheme=theme_bw(),xlab="Month")
length(zphplot_disc)
zphplot_disc[[1]] <- zphplot_disc[[1]] + labs(title = 'ITM YIELDs: Burnout (p=0.0002)') 
zphplot_disc[[2]] <- zphplot_disc[[2]] + labs(title = 'LTV>75 (p=0.0006)') 
zphplot_disc[[3]] <- zphplot_disc[[3]] + labs(title = 'VOL.CAT>350k (p=0.0000)') 
ggsave(file = "zph_disc.jpeg", print(zphplot_disc))

## adding time-depended factors. All factors were adjusted with time-depended covariates beta(t)X(t).



FINAL_time<-coxph(Surv(START,STOP,PREPAID)~YIELD.CAT+CSCORE+CSCORE:START+LTV.CAT+LTV.CAT:START+OCC_STAT+OCC_STAT:START+DTI+UNEMP+VOL.CAT+VOL.CAT:START+YIELD.CAT:START+UNEMP+TAX, data=FNMA)
summary(FINAL_time)
extractAIC(FINAL_time)
zph_time <- cox.zph(FINAL_time)
zph_time
zphplot_time <- ggcoxzph(zph_time,var=c("YIELD.CATITM","LTV.CAT>75","VOL.CAT>350k"),  resid=FALSE,se=TRUE, title=FALSE, ggtheme=theme_bw(),xlab="Month")
length(zphplot_time)
zphplot_time[[1]] <- zphplot_time[[1]] + labs(title = 'ITM YIELD: independed of time (p=0.9344)') 
zphplot_time[[2]] <- zphplot_time[[2]] + labs(title = 'LTV>75: independed of time (p=0.1239)') 
zphplot_time[[3]] <- zphplot_time[[3]] + labs(title = 'VOL.CAT>350k: independed of time (p=0.0460)') 
zphplot_time
ggsave(file = "zph_time.jpeg", print(zphplot_time))






