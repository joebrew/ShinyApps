# Universe size (total n)
# total.n 

# Outcome collection rate (%) (e.g., survey response rate)
# dv.variable.contact.rate

# Percent (%) in treatment group
# percent.in.treatment

# Turnout/support/action rate in control (%)
# baseline.action.support.rate

# Contact (i.e., treatment application) rate (%)
# treatment.application.rate

# Predictive power of individual-level covariates (R-squared)
# r.squared

# Desired power of experiment
# power

# Confidence interval to use (2-tailed)
# confidence.interval

# Show details
# checkbox

PowerFun<- function(total.n = 20000, 
                    dv.variable.contact.rate = 100,
                    percent.in.treatment = 50,
                    baseline.action.support.rate = 50,
                    treatment.application.rate = 100,
                    r.squared = 0,
                    power = 80,
                    confidence.interval = 90,
                    #number.of.clusters = 1,
                    #intra.cluster.correlation.coefficient = 0.1,
                    checkbox = FALSE){
  
  # SPREADSHEET CONVERSION
  b3 <- as.numeric(total.n) 
  b4 <- as.numeric(dv.variable.contact.rate / 100)
  b7 <- as.numeric(percent.in.treatment / 100)
  b8 <- as.numeric(baseline.action.support.rate / 100)
  b9 <- as.numeric(treatment.application.rate / 100)
  b11 <- as.numeric(r.squared)
  b12 <- as.numeric(power / 100)
  b13 <- as.numeric(confidence.interval / 100)
  #b19 <- as.numeric(number.of.clusters)
  #b21 <- as.numeric(intra.cluster.correlation.coefficient)
  
  # SPREADSHEET CALCULATIONS
  b5 <- b3*b4
  b14 <- sqrt((((b8*(1-b8))*(1-b11))/((b7*(1-b7))*b5)))
  b16 <- (qnorm(b12) + qnorm(((b13/2)+0.5)))*b14
  b17 <- b16/b9
  #b20 <- b3/b19
  #b22 <- sqrt((1+((b20-1)*b21)))
  #b23 <- b14*b22
  #b25 <- (qnorm(b12) + qnorm(((b13/2) + 0.5)))*b23
  #b26 <- b25 / b9
  
  # MY NAMES
  myNames <- c("b3", "b4", "b5",
               "b7", "b8", "b9",
               "b11", "b12", "b13", "b14",
               "b16", "b17")#, 
  #"b19", "b20", "b21",
  #"b22", "b23", "b25", "b26")
  
  # MY VALUES
  myValues <- c(b3, b4, b5,
                b7, b8, b9,
                b11, b12, b13, b14,
                b16, b17)#, 
  #b19, b20, b21,
  #b22, b23, b25, b26)
  
  # MY FULL NAMES
  myFullNames <- c("total.n", "dv.variable.contact.rate", "Number.of.outcome.measurements",
                   "percent.in.treatment", "baseline.action.support.rate", "treatment.application.rate",
                   "r.squared", "power", "confidence.interval", "S.E.",
                   "ITT.MDE", "TOT.MDE")#, 
  #"number.of.clusters", "Average.cluster.size", "intra.cluster.correlation.coefficient",
  #"Square.root.of.variance.inflation.factor", "Adjusted.S.E.", "Adj.ITT.MDE", "Adj.TOT.MDE")
  
  #   # BIND INTO DATAFRAME
  
  x <- data.frame(myNames)
  names(x) <- "myNames"
  x$myValues <- myValues
  
  
  # x <- myValues
  # names(x) <- myNames
  # x <- data.frame(x)
  
  #   x <- as.data.frame(rbind(myValues))
  #    names(x) <- myNames
  #   #x <- as.data.frame(rbind(myFullNames,  myValues))
  #   
  #   # RETURN
  return(x)
  
  # setNames(x, myNames)
  
  
  # # BIND INTO LIST
  # 
  # x <- as.list(setNames(myValues, myNames))
  
  
  
}

#   
# 
# x <- PowerFun(total.n = 20000, 
#          dv.variable.contact.rate = 100,
#          percent.in.treatment = 50,
#          baseline.action.support.rate = 50,
#          treatment.application.rate = 100,
#          r.squared = 0,
#          power = 80,
#          confidence.interval = 90,
#          number.of.clusters = 1,
#          intra.cluster.correlation.coefficient = 0.1,
#          checkbox = FALSE)

#   
# 