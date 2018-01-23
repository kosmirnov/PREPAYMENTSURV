SMM <- function(fit.table)
{
  fit.table <- summary(fit.table)
  fit.table<- as.data.table(cbind(fit.table$time,fit.table$n.risk,fit.table$n.event,fit.table$surv,fit.table$std.err,fit.table$upper,fit.table$lower,fit.table$strata))
  if(ncol(fit.table) < 8) {
    colnames(fit.table) <- c("Month","N.risk","N.event","SurvivalProb","std.err","upper","lower")
  } else {
    colnames(fit.table) <- c("Month","N.risk","N.event","SurvivalProb","std.err","upper","lower","Strata")
    fit.table$Strata <- as.factor(fit.table$Strata)}
  fit.table$SMM <- fit.table$N.event/fit.table$N.risk
  fit.table$CPR <- 1-(1-fit.table$SMM)^12
  return(fit.table)
  
}

