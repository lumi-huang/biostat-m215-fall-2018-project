
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

# Example
co.type = c(rep("C", 4), "N") # "C" means categorical variable / "N" means numeric variable
single.cox(breast2$SRV_TIME_MON, breast2$STAT_REC, X[,1:5], co.type)

