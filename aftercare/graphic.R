source("helper.R")


teacherPay <- 14
studentCost <- 7

for (i in ts$day){
  for (j in c(30,60,90,120)){
    income <- Hourfy(ts[which(ts$day == i),paste0("x", j)])*studentCost
    expense <- Hourfy(j)*teacherPay
    roi <- income - expense
    
    ts[which(ts$day == i), paste0("roi", j)] <- roi
    
    
  }
}

ts2 <- ts[, grepl("roi|day", colnames(ts))]


colnames(ts2) <- c("day", "3:30", "4:00", "4:30", "5:00")
ts2

# BEST SOLUTION - TUE-THU UNTIL 5PM
bp <- barplot(ts2[,"5:00"], names.arg= ts2$day, ylab="Revenue/Loss", 
              border = NA, col=adjustcolor("darkgreen", alpha.f=0.6),
              main = "Revenue/loss for HP with after school until 5:00pm")
abline(h=0)
text(bp[,1], 5, labels=paste0("$",ts2[,"5:00"]))
