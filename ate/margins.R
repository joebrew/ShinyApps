#Calculate marginal effects for predictors in binomial-response GLMs
#with standard errors estimated via bootstrap. usable link functions
#are probit and logit, others not implemented. mfxboot.glm originally from:
#  Alan Fernihough, 2011.
#	"Simple Logit and Probit Marginal Effects in R,"
#	Working Papers 201122, School Of Economics, University College Dublin.
#Subsequent modifications by William Brannon, Analyst Institute.
require(nnet)

mfxboot <-
  function(what, ...)
    UseMethod("mfxboot")

mfxboot.formula <-
  function(modform, method, data, iter=100)
  {
    x <- glm(modform, family=binomial(link=method), data)
    
    if( method %in% c("logit", "probit") )
      invisible(mfxboot.glm(x, iter))
    else if( method == "multinom" )
    {
      x <- multinom(modform, data=data)
      invisible(mfxboot.multinom(x, iter))
    }
    else
      stop( paste("unknown model type: ", method, sep="") )
  }

mfxboot.glm <-
  function(model, iter=100, data)
  {
    if( model$family$family != "binomial" )
      #if( model$family != "binomial" )
      stop("can only handle GLMs with a binomial response")
    else
    {
      method <- model$family$link
      modform <- model$formula
    }
    
    # get marginal effects
    pdf <- ifelse(method=="probit",
                  mean(dnorm(predict(model, type = "link"))),
                  mean(dlogis(predict(model, type = "link"))))
    marginal.effects <- pdf*coef(model)
    
    # start bootstrap
    bootvals <- matrix(rep(NA,iter*length(coef(model))), nrow=iter)
    set.seed(1111)
    
    for(i in 1:iter)
    {
      samp1 <- data[sample(1:dim(data)[1],replace=T,dim(data)[1]),]
      #joe's version
      #samp1 <- df[sample(1:dim(df)[1],replace=T,dim(df)[1]),]
      
      x1 <- glm(modform, family=binomial(link=method),samp1)
      pdf1 <- ifelse(method=="probit",
                     mean(dnorm(predict(x1, type = "link"))),
                     mean(dlogis(predict(x1, type = "link"))))
      bootvals[i,] <- pdf1*coef(x1)
    }
    
    se <- apply(bootvals,2,sd)
    res <- cbind(marginal.effects, se)
    
    if( names(model$coefficients[1])=="(Intercept)" )
      res <- res[2:nrow(res),]
    
    colnames(res) <- c("marginal.effect","se")
    
    ret <- structure(list(effects=as.data.frame(res), call=model$call), class=c("mfxglm", "mfxboot", "data.frame"))
    invisible(ret)
  }

mfxboot.multinom <-
  function(model, iter=100)
  {
    .NotYetImplemented()
  }

confint.mfxboot <-
  function(obj, level=0.95)
  {
    df <- obj$effects
    
    bounds <- c( (1 - level) / 2, 1 - (1 - level) / 2)
    dn <- paste(format(bounds * 100, trim=TRUE, scientific=FALSE), "%")
    z <- qnorm(bounds)
    
    cf <- as.matrix(df$marginal.effect)
    cf <- sapply(z, function(x) cf + x * df$se)
    
    dimnames(cf) <- list(rownames(df), dn)
    
    as.data.frame(cf)
  }

summary.mfxboot <-
  function(obj, conf.level=0.95, digits=3)
  {
    s <- obj$effects
    
    s$zval <- s$marginal.effect / s$se
    s$pval <- 2 * pnorm(-abs(s$zval))
    names(s) <- c("Estimate", "SE", "z-value", "Pr(>|z|)")
    
    ci <- confint(obj, level=conf.level)
    
    ret <- list(effects=s, ci=ci, call=obj$call)
    class(ret) <- "summary.mfxboot"
    
    ret
  }

print.summary.mfxboot <-
  function(x)
  {
    cat("Call: \n")
    print(x$call)
    cat("\n")
    
    cat("Coefficients: \n")
    printCoefmat(x$effects)
    cat("\n")
    
    cat("Confidence intervals: \n")
    printCoefmat(x$ci)
    cat("\n")
    
    invisible(x)
  }

print.mfxboot <-
  function(x)
    print(x$effects)
