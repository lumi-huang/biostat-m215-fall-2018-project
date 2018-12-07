
library(survival)
library(KMsurv)

##---------------------log-rank test

log.rank = function(time, event, data, list){ # list : number of selected column
  surv = Surv(time, event, type = "right")
  result = data.frame("Log-rank_p" = NA, "Gehan_p" = NA)
  
  for (i in 1:length(list)){
    cova = data[,list[i]]
    surv.test = survdiff(surv ~ cova, rho = 0)
    surv.test2 = survdiff(surv ~ cova, rho = 1)
    result = rbind(result, round(c(1-pchisq(as.numeric(surv.test[5]),length(unique(cova))-1), 1-pchisq(as.numeric(surv.test2[5]),length(unique(cova))-1)),4))
  }
  
  result = result[-1,]
  result = data.frame("variable" = colnames(data)[list], result)
  rownames(result) = 1:nrow(result)
  result
}

##---------------------

# Example
breast_final = data.frame(breast_final)
log.rank(time = breast_final$SRV_TIME_MON, event = breast_final$delta, data = breast_final, list = c(1,3,4,6,9:15))

breast_final2 = breast_final
breast_final2$Agemean = as.integer(breast_final$Age>mean(breast_final$Age))
breast_final2$AgeDXmean = as.integer(breast_final$Age>mean(breast_final$AGE_DX))

log.rank(time = breast_final$SRV_TIME_MON, event = breast_final$delta, data = breast_final2, list = c(1,3,4,6,9:17))


##---------------------KM curve

KMcurve = function(time, event, covariate, conf=TRUE, main=NULL) {
  surv = Surv(time, event, type="right")
  if (missing(covariate) || (length(unique(covariate))==1)) {
    surv.res = survfit(surv ~ 1)
    plot(surv.res, conf.int=conf, main = "Product−Limit Survival Estimates", 
         ylab = "Survival Probability", xlab = "Time")
  } else {
    surv.res = survfit(surv ~ covariate)
    plot(surv.res, conf.int=conf, main="Product−Limit Survival Estimator", ylab="Survival Probability", xlab="Time", 
         col=1:length(unique(covariate)), lty=1:length(unique(covariate)))
  }
}


##---------------------

# Example
KMcurve(time=breast_final$SRV_TIME_MON, event=breast_final$delta, breast_final$stage, conf = FALSE)
legend("bottomleft", c("Stage 0", "Stage 1","Stage 2","Stage 3","Stage 4"), lty = 1:5, col = 1:5, cex=0.9)



##---------------------
# To test significance of single variable using Cox model

single.cox = function(time, event, covariate, co.type) {
  
  p = c()
  
  for (i in 1:ncol(covariate)){
    surv.sub = Surv(time=time, event=event, type="right")
    if (co.type[i]=="N") { # continuous variable
      formul = paste("surv.sub ~ covariate[,", i, "]", sep="")
      fit = coxph(eval(parse(text=formul)), ties = "breslow")
    } else { # categorical variable
      formul = paste("surv.sub ~ factor(covariate[,", i, "])", sep="")
      fit = coxph(eval(parse(text=formul)), ties = "breslow")
    }
    p = rbind(p, t(round(summary(fit)$logtest, 3))) # LRT
  }
  
  return(p)
  
}

##---------------------

# Example
co.type = c(rep("C", 4), "N") # "C" means categorical variable / "N" means numeric variable
single.cox(breast2$SRV_TIME_MON, breast2$STAT_REC, X[,1:5], co.type)
