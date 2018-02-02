if (!(require(survminer))) install.packages ("survminer")
if (!(require(survival))) install.packages ("survival")
if (!(require(stargazer))) install.packages ("stargazer")
if (!(require(dplyr))) install.packages ("dplyr")
if (!(require(data.table))) install.packages ("data.table")
if (!(require(ggplot2))) install.packages ("ggplot2")
if (!(require(zoo))) install.packages ("zoo")




setwd("C:/Users/Kons/OneDrive/MASTERARBEIT/DATA/PREPAYMENTSURV")


gc()
options(scipen=999)
options(digits=5)

load("C:/Users/Kons/OneDrive/MASTERARBEIT/DATA/PREPAYMENTSURV/FNMA.Rda")

FNMA$MINTEREST <- FNMA$MINTEREST/100
### YIELD ### 
# 1. Real MINTEREST
FNMA$MONTH <- as.numeric(FNMA$MONTH)
ggplot(FNMA, aes(x=MONTH, y=MINTEREST, group=REGION)) +
  geom_line(aes(color=REGION),size=1)+
  theme_bw()+
  theme(legend.position = c(0.9, 0.8)) +
  scale_color_manual(values = c("#86BC25","#0076A8","#BBBCBC","#E3E48D","#A0DCFF"))+
  scale_x_yearmon(breaks = seq(from = min(FNMA$MONTH), 
                               to = max(FNMA$MONTH)))+ 
  scale_y_continuous(breaks = seq(0, 0.05, by = 0.005),labels = scales::percent)+
  xlab("Months")+
  ylab("Average Mortgage Rate")
  ggsave(file = "des_YIELD.jpeg")

# 2. Yield spread
YIELD.SPREAD_AGG <- FNMA %>%
  group_by(MONTH,REGION) %>%
  dplyr::summarize(MEANS.AGG = mean(YIELD.SPREAD))
YIELD.SPREAD_AGG$MEANS.AGG <- YIELD.SPREAD_AGG$MEANS.AGG/100

ggplot(YIELD.SPREAD_AGG, aes(x=MONTH, y=MEANS.AGG, group=REGION)) +
  geom_line(aes(color=REGION),size=1)+
  theme_bw()+
  theme(legend.position = c(0.1, 0.8)) +
  scale_color_manual(values = c("#86BC25","#0076A8","#BBBCBC","#E3E48D","#A0DCFF"))+
  scale_x_yearmon(breaks = seq(from = min(FNMA$MONTH), 
                               to = max(FNMA$MONTH)))+
  scale_y_continuous(breaks = seq(-0.03, 0.02, by = 0.002),labels = scales::percent)+
  xlab("Months")+
  ylab("Yield Spread")
ggsave(file = "des_YIELD.SPREAD.jpeg")

# 3. House-Price development 
HP <- subset(FNMA,MSAREG=="NY" | MSAREG=="LA" | MSAREG=="LV"| MSAREG=="CHI")


# hier vll noch average plot

ggplot(HP, aes(x=MONTH, y=SHGROWTH, group=MSAREG)) +
  geom_line(aes(color=MSAREG),size=1)+
  theme_bw()+
  theme(legend.position = c(0.9, 0.8)) +
  scale_color_manual(values = c("#86BC25","#0076A8","#BBBCBC","#E3E48D","#A0DCFF"))+
  scale_x_yearmon(breaks = seq(from = min(FNMA$MONTH), 
                               to = max(FNMA$MONTH)))+
  scale_y_continuous(breaks = seq(-0.04, 0.05, by = 0.01),labels = scales::percent)+
  xlab("Months")+
  ylab("Shiller-Case Growth")
ggsave(file = "des_SHG.jpeg")

#4. LTV
LTV.AGG <- HP %>%
  group_by(MONTH,MSAREG) %>%
  dplyr::summarize(MEANS.AGG = mean(LTV))

LTV.AGG$MEANS.AGG <- LTV.AGG$MEANS.AGG/100

ggplot(LTV.AGG, aes(x=MONTH, y=MEANS.AGG, group=MSAREG)) +
  geom_line(aes(color=MSAREG),size=1)+
  theme_bw()+
  theme(legend.position = c(0.9, 0.8)) +
  scale_color_manual(values = c("#86BC25","#0076A8","#BBBCBC","#E3E48D","#A0DCFF"))+
  scale_x_yearmon(breaks = seq(from = min(FNMA$MONTH), 
                               to = max(FNMA$MONTH)))+
  scale_y_continuous(breaks = seq(0.45, 0.85, by = 0.2),labels = scales::percent)+
  xlab("Months")+
  ylab("LTV")+
  ggplot(LTV.NATION, aes(x=MONTH, y=MEANS.AGG))
  
ggsave(file = "des_LTV.jpeg")


### add these two to the plot above
SHG.NATION <- FNMA %>%
  group_by(MONTH) %>%
  dplyr::summarize(MEANS.NATION = mean(SHGROWTH))

LTV.NATION <- FNMA %>%
  group_by(MONTH) %>%
  dplyr::summarize(MEANS.NATION = mean(LTV))