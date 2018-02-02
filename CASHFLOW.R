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
#2. Create count Variables
AGG <- FNMA %>%
  group_by(MONTH) %>%
  summarise(LAST_UPB=sum(LAST_UPB))

AGG <- subset(AGG,MONTH>="Okt 2010")
AGG<-data.table(AGG)
AGG[, i := sequence(.N)]
AGG[46,2] <- AGG[45,2] + (AGG[47,2]- AGG[45,2])/2



AGG$WAC <- mean(FNMA$ORIG_RT)/100/12
AGG$MPAY <- AGG$LAST_UPB*((AGG$WAC*(1+AGG$WAC)^(360-AGG$i+1))/((1+AGG$WAC)^(360-AGG$i+1)-1))
AGG$INT <- AGG$LAST_UPB*AGG$WAC
AGG$SPRIN <- AGG$MPAY - AGG$INT
AGG$SBAL <- AGG$LAST_UPB - AGG$SPRIN

shift<- data.table(diff(AGG$LAST_UPB))
AGG$TPRIN <- shift*-1
AGG$PREPAY <- AGG$TPRIN - AGG$SPRIN
AGG$SMM <- AGG$PREPAY/(AGG$LAST_UPB-AGG$SPRIN)
AGG$CPR <- 1-(1-AGG$SMM)^12
AGG <- AGG[-nrow(AGG),]


#correcting the outlier @ Mai 2015, NC,  by linear interpolation





ggplot(AGG, aes(x=MONTH, y=CPR)) +
  geom_line(aes(),colour="#86BC25",size=1)+
  theme_bw()+
  scale_x_continuous(breaks = seq(0, 65, by = 10))+
  scale_y_continuous(breaks = seq(0, 0.4, by = 0.05),labels = scales::percent)
