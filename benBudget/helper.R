BudgetFun <- function(tu,
                      rent,
                      flight,
                      christmas,
                      food,
                      other,
                      euro=TRUE){
  costEuros <- 
    tu +
    (rent*10) +
    (flight) +
    (christmas)+
    (food*300) +
    (other*300) 
  
  costDollars <- 
    costEuros*1.3718
  
  ifelse(euro, 
         costEuros,
         costDollars)
}


PlotBudgetFun <- function(tu,
                          rent,
                          flight,
                          christmas,
                          food,
                          other){
  
  Tuition <- tu
  Rent <- rent*10
  Travel <- flight + christmas
  Food <- food*300
  Other <- other*300
  
  df <- as.data.frame(cbind(c(Tuition, Rent, Travel, Food, Other),
                            c("Tuition", "Rent", "Travel", "Food", "Other"),
                            c("One-time", "Monthly", "One-time", "Monthly", "Monthly"),
                            c(tu, rent, flight+christmas, food*30, other*30)))
  colnames(df) <- c("Amount", "Category", "Freq", "Amount2")
  
  
  
  df$Amount2 <- as.numeric(as.character(df$Amount2))
  
  df$Amount <- as.numeric(as.character(df$Amount))
  df <- df[order(df$Amount),]
  #df$Amount <- ifelse(euros, df$Amount*1.3718, df$Amount)
  
  
  
  
  #EUR
  mybp <- barplot(df$Amount,
                  names.arg=df$Category,
                  ylim=c(0, max(df$Amount) + 1000),
                  border="darkgrey",
                  yaxt="n",
                  xlab="Category",
                  ylab="Euros")
  axis(side=2, at=seq(0,20000,1000),
       labels=seq(0,20000,1000),
       las=1,
       cex.axis=0.8)
  
  abline(h=seq(0,20000,1000),
         col=adjustcolor("black", alpha.f=0.1))
  
  text(x=mybp[,1],
       y=df$Amount + 400,
       labels=paste0(round(df$Amount/sum(df$Amount)*100, digits=1), "%"))
  
 # text(x=mybp[,1],
#       y=df$Amount -400,
#       labels=paste(df$Amount2, df$Freq),
#       cex=0.6)
  
}



