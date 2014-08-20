library(RColorBrewer)

BudgetFun <- function(tot,
                      n.messages = 1, 
                      cost.eip.mail = 2.00,
                      cost.post.eip.mail = 1.50,
                      phone.contact = 10,
                      n.interviews = 2000,
                      phone.cost = 4.00,
                      effect.without.eip = 3.5,
                      effect.with.eip.1 = 10,
                      effect.with.eip.2 = 13){
  
  # SPREADSHEET COPY
  b4 <- tot
  b5 <- n.messages
  b10 <- cost.eip.mail
  b11 <- cost.post.eip.mail
  b13 <- phone.contact/100
  b14 <- n.interviews
  b15 <- phone.cost
  b18 <- effect.without.eip/100
  b19 <- effect.with.eip.1/100
  b20 <- effect.with.eip.2/100
  
  #
  b16 <- b14/b13
  
  # WITHOUT EIP
  b25 <- b4
  b26 <- b25 / b11
  b27 <- b26 * b18
  
  # WITH EIP
  b30 <- b4
  b31 <- b16*b5*b10
  b32 <- b16*(b5+1)*b13*b15
  b33 <- 15250
  b34 <- b30 - sum(c(b31, b32, b33))
  b35 <- b34/ b11
  b36 <- ifelse(b5 == 1,
                (b35*b19)+(b16*b18),
                (b35*b20)+(b16*b5*b18))
  
  #
  b38 <- (b36-b27)/b27*100
  
  #########
  myNames <- c("b4",
               "b5",
               "b10",
               "b11",
               "b13",
               "b14",
               "b15",
               "b16",
               "b18",
               "b19",
               "b20",
               "b25",
               "b26",
               "b27",
               #####
               "b30",
               "b31",
               "b32",
               "b33",
               "b34",
               "b35",
               "b36",
               "b38")
  
  myFullNames <- c("Total budget",
                   "Number of messages (treatment conditions)",
                   "Cost of EIP mail per target",
                   "Cost of post-EIP mail per target",
                   "Phone survey contact rate",
                   "Number of interviews per condition",
                   "Cost of phone survey per complete",
                   "Number of mail targets per condition: ",
                   "Persuasive effect of mail without EIP",
                   "Persuasive effect of mail with targeting EIP (one treatment)",
                   "Persusasive effect of mail with messaging EIP (2 + treatments)",
                   "Total budget: ",
                   "Number of targets afforded: ",
                   "Persuaded voters: ",
                   #####
                   "Total budget: ",
                   "Total cost of EIP mail: ",
                   "Total cost of phone surveys: ",
                   "Total cost of analysis and roboscreen: ",
                   "Total budget after EIP",
                   "Number of targets afforded post-EIP",
                   "Persuaded voters: ",
                   "Percent increase in persuaded voters: ")
  
  myValues <- c(b4,
                b5,
                b10,
                b11,
                b13*100,
                b14,
                b15,
                b16,
                b18*100,
                b19*100,
                b20*100,
                b25,
                b26,
                b27,
                #####
                b30,
                b31,
                b32,
                b33,
                b34,
                b35,
                b36,
                b38)
  #x <- as.list(setNames(myValues, myNames))
  #return(x)
  
  #paste(myFullNames, myValues, "\n")
  x <- as.data.frame(cbind(myFullNames, myValues))
  x$myValues <- as.numeric(as.character(x$myValues))
  x$myValues <- round(x$myValues, digits=2)
  names(x) <- c(" ", " ")
  return(x)
  
}

# x <- BudgetFun(tot = 300000,
#           n.messages = 1, 
#           cost.eip.mail = 2.00,
#           cost.post.eip.mail = 1.50,
#           phone.contact = 10,
#           n.interviews = 2000,
#           phone.cost = 4.00,
#           effect.without.eip = 3.5,
#           effect.with.eip.1 = 10,
#           effect.with.eip.2 = 13)



BudgetFunList <- function(tot,
                      n.messages = 1, 
                      cost.eip.mail = 2.00,
                      cost.post.eip.mail = 1.50,
                      phone.contact = 10,
                      n.interviews = 2000,
                      phone.cost = 4.00,
                      effect.without.eip = 3.5,
                      effect.with.eip.1 = 10,
                      effect.with.eip.2 = 13){
  
  # SPREADSHEET COPY
  b4 <- tot
  b5 <- n.messages
  b10 <- cost.eip.mail
  b11 <- cost.post.eip.mail
  b13 <- phone.contact/100
  b14 <- n.interviews
  b15 <- phone.cost
  b18 <- effect.without.eip/100
  b19 <- effect.with.eip.1/100
  b20 <- effect.with.eip.2/100
  
  #
  b16 <- b14/b13
  
  # WITHOUT EIP
  b25 <- b4
  b26 <- b25 / b11
  b27 <- b26 * b18
  
  # WITH EIP
  b30 <- b4
  b31 <- b16*b5*b10
  b32 <- b16*(b5+1)*b13*b15
  b33 <- 15250
  b34 <- b30 - sum(c(b31, b32, b33))
  b35 <- b34/ b11
  b36 <- ifelse(b5 == 1,
                (b35*b19)+(b16*b18),
                (b35*b20)+(b16*b5*b18))
  
  #
  b38 <- (b36-b27)/b27*100
  
  #########
  myNames <- c("b4",
               "b5",
               "b10",
               "b11",
               "b13",
               "b14",
               "b15",
               "b16",
               "b18",
               "b19",
               "b20",
               "b25",
               "b26",
               "b27",
               #####
               "b30",
               "b31",
               "b32",
               "b33",
               "b34",
               "b35",
               "b36",
               "b38")
  
  myFullNames <- c("Total budget",
                   "Number of messages (treatment conditions)",
                   "Cost of EIP mail per target",
                   "Cost of post-EIP mail per target",
                   "Phone survey contact rate",
                   "Number of interviews per condition",
                   "Cost of phone survey per complete",
                   "Number of mail targets per condition: ",
                   "Persuasive effect of mail without EIP",
                   "Persuasive effect of mail with targeting EIP (one treatment)",
                   "Persusasive effect of mail with messaging EIP (2 + treatments)",
                   "Total budget: ",
                   "Number of targets afforded: ",
                   "Persuaded voters: ",
                   #####
                   "Total budget: ",
                   "Total cost of EIP mail: ",
                   "Total cost of phone surveys: ",
                   "Total cost of analysis and roboscreen: ",
                   "Total budget after EIP",
                   "Number of targets afforded post-EIP",
                   "Persuaded voters: ",
                   "Percent increase in persuaded voters: ")
  
  myValues <- c(b4,
                b5,
                b10,
                b11,
                b13*100,
                b14,
                b15,
                b16,
                b18*100,
                b19*100,
                b20*100,
                b25,
                b26,
                b27,
                #####
                b30,
                b31,
                b32,
                b33,
                b34,
                b35,
                b36,
                b38)
  x <- as.list(setNames(myValues, myNames))
  return(x)
  
#   #paste(myFullNames, myValues, "\n")
#   x <- as.data.frame(cbind(myFullNames, myValues))
#   x$myValues <- as.numeric(as.character(x$myValues))
#   x$myValues <- round(x$myValues, digits=2)
#   names(x) <- c(" ", " ")
#   return(x)
  
}
