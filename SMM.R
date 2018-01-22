SMM <- function(fit)
{
  fit1.table <- summary(fit)
  fit1.table<- as.data.table(cbind(fit1.table$time,fit1.table$n.risk,fit1.table$n.event,fit1.table$surv,fit1.table$std.err,fit1.table$upper,fit1.table$lower,fit1.table$strata))
  if(ncol(fit1.table) < 8) {
    colnames(fit1.table) <- c("time","N.risk","N.event","SurvivalProb","std.err","upper","lower")
  } else {
    colnames(fit1.table) <- c("time","N.risk","N.event","SurvivalProb","std.err","upper","lower","Strata")}
  fit1.table$SMM <- fit1.table$N.event/fit1.table$N.risk
  fit1.table$CPR <- 1-(1-fit1.table$SMM)^12
  return(fit1.table)
  
}

