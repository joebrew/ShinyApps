library(shiny)



#########################
# SOURCE IN HELPER FUNCTION
##############
source("BudgetFun.R")




shinyServer(function(input, output) {
  
  output$a <- renderPlot({
    x <- BudgetFunList(tot = as.numeric(input$b4),
                       n.messages = input$b5, 
                       cost.eip.mail = input$b10,
                       cost.post.eip.mail = input$b11,
                       phone.contact = input$b13,
                       n.interviews = as.numeric(input$b14),
                       phone.cost = input$b15,
                       effect.without.eip = input$b18,
                       effect.with.eip.1 = input$b19,
                       effect.with.eip.2 = input$b20)
    
    temp <- colorRampPalette(c("darkred", "darkgreen"))(200)
    
    color <- adjustcolor(temp[ceiling(x$b38)], alpha.f=0.2)
    my_color <- ifelse(x$b38 < 200,
                       color,
                       "darkgreen")
    
    par(mfrow=c(2,2))
    par(mar=c(5,7,1,1))
    bp <- barplot(c(x$b31, x$b32, x$b33),
                  names.arg=c("EIP mail",
                              "Phone surveys",
                              "Analysis and\nroboscreen"),
                  las=1,
                  horiz=TRUE,
                  xlab="Dollars",
                  border=FALSE,
                  col=adjustcolor("grey", alpha.f=0.2))
    title(main="Cost breakdown", 
          cex.main=1, 
          col=adjustcolor("black", alpha.f=0.5))
    
    text(x=0, y=bp[,1],  pos=4,
         col=adjustcolor("black", alpha.f=0.7),
         labels=paste("$", round(c(x$b31, x$b32, x$b33), digits=2)),
         cex=1)
    
    par(mar=c(5,3,1,1))
    
    bp <- barplot(x$b38,
                  col = my_color,
                  border=FALSE,
                  ylim=c(0, x$b38 + 5))
    title(main="% increase in persuaded voters", 
          cex.main=1, 
          col=adjustcolor("black", alpha.f=0.5))
    
    
    text(x=bp[,1], y=ifelse(x$b38 > 0, 10, -10),
         col=adjustcolor("black", alpha.f=0.5),
         labels=paste(round(x$b38, digits=2), "%"),
         cex=1.5)
    #box("plot")
    
    abline(h=100, col=adjustcolor("darkgreen", alpha.f=0.4), lwd=2)
    abline(h=0, col=adjustcolor("darkred", alpha.f=0.4), lwd=2)
    
    bp <- barplot(x$b34,
                  col = adjustcolor("grey", alpha.f=0.2),
                  border=FALSE,
                  ylim=c(0, x$b34 + 5)) 
    
    title(main="Total budget after EIP",
          cex.main=1, 
          col=adjustcolor("black", alpha.f=0.5))
    
    
    text(x=bp[,1], y=0.1*x$b34,
         col=adjustcolor("black", alpha.f=0.5),
         labels=paste("$", round(x$b34, digits=2)),
         cex=1.5)
    
    bp <- barplot(x$b36,
                  col = adjustcolor("grey", alpha.f=0.2),
                  border=FALSE)
    title(main="Persuaded voters", 
          cex.main=1, 
          col=adjustcolor("black", alpha.f=0.5))
    
    
    text(x=bp[,1], y=0.1*x$b36,
         col=adjustcolor("black", alpha.f=0.5),
         labels=paste(round(x$b36, digits=0)),
         cex=1.5)
  })
  
  output$b <- renderTable({
    BudgetFun(tot = as.numeric(input$b4),
              n.messages = input$b5, 
              cost.eip.mail = input$b10,
              cost.post.eip.mail = input$b11,
              phone.contact = input$b13,
              n.interviews = as.numeric(input$b14),
              phone.cost = input$b15,
              effect.without.eip = input$b18,
              effect.with.eip.1 = input$b19,
              effect.with.eip.2 = input$b20)
  },include.rownames=FALSE)
  
  
  # tot
  # n.messages
  # cost.eip.mail
  # cost.post.eip.mail
  # phone.contact
  # n.interviews
  # phone.cost
  # effect.without.eip
  # effect.with.eip.1
  # effect.with.eip.2
  
#   
#   output$distPlot <- renderPlot({
#     
#     barplot((1:10)^3)
#   })
#   
#   # NUMBER OF MAIL TARGETS PER CONDITION
#   output$n.targets <- renderText({
#     as.numeric(input$n.interviews) / input$phone.contact
#   })
#   
#   #TOTAL BUDGET 
#   output$total.budget <- renderText({
#     paste0("$", as.numeric(input$tot))
#   })
#   
#   ################################## BEGIN WITHOUT EIP
#   
#   # WITHOUT EIP - NUMBER OF TARGETS AFFORED
#   output$n.targets.afforded <- renderText({
#     as.numeric(input$tot) / as.numeric(input$cost.post.eip.mail)
#   })
#   
#   # WITHOUT EIP - PERSUADED VOTERS
#   output$persuaded.voters <- renderText({
#     
#     (as.numeric(input$tot) / as.numeric(input$cost.post.eip.mail)) * # B26
#       (input$effect.without.eip/100)
#   })
#   ################################### END WITHOUT EIP
#   
#   ################################## BEGIN WITH EIP
# 
#   # WITH EIP - TOTAL COST OF EIP MAIL
#   output$cost.eip.mail <- renderText({
#    (as.numeric(input$n.interviews)/as.numeric(input$phone.contact)) *   # b14 / b13 = b16
#      as.numeric(input$n.messages)
#   })
#   
#   # WITH EIP - TOTAL COST OF PHONE SURVEYS
#   output$cost.eip.mail <- renderText({
#     (as.numeric(input$n.interviews)/as.numeric(input$phone.contact)) *   # b14 / b13 = b16
#       (as.numeric(input$n.messages) + 1) * #b5 +1
#       (as.numeric(input$phone.contact)/100)  * # b13
#       (as.numeric(input$phone.cost))  # b15
#   })
#   
#   # WITH EIP - TOTAL COST OF ANALYSIS AND ROBOSCREEN
#   output$cost.analysis <- renderText({
#     15250
#   })
#   
#   # WITH EIP - TOTAL BUDGET AFTER EIP
#   output$budget.after.eip <- renderText({
#     
#     (as.numeric(input$tot)) - #b30 
#       sum(c( 
#         #b31
#         (as.numeric(input$n.interviews)/as.numeric(input$phone.contact)) *   # b14 / b13 = b16
#           as.numeric(input$n.messages), 
#         #b32 
#         (as.numeric(input$n.interviews)/as.numeric(input$phone.contact)) *   # b14 / b13 = b16
#           (as.numeric(input$n.messages) + 1) * #b5 +1
#           (as.numeric(input$phone.contact)/100)  * # b13
#           (as.numeric(input$phone.cost))  # b15
#         , 
#         #b33
#         15250
#             )) 
#   })
#   
#   # WITH EIP - NUMBER OF TARGETS AFFORDED POST-EIP
#   output$n.targes.post.eip <- renderText({
#     # b34 - b11
#     
#     # b34
#     (    (as.numeric(input$tot)) - #b30 
#            sum(c( 
#              #b31
#              (as.numeric(input$n.interviews)/as.numeric(input$phone.contact)) *   # b14 / b13 = b16
#                as.numeric(input$n.messages), 
#              #b32 
#              (as.numeric(input$n.interviews)/as.numeric(input$phone.contact)) *   # b14 / b13 = b16
#                (as.numeric(input$n.messages) + 1) * #b5 +1
#                (as.numeric(input$phone.contact)/100)  * # b13
#                (as.numeric(input$phone.cost))  # b15
#              , 
#              #b33
#              15250
#            )) ) -
#       #b11
#       (input$cost.post.eip.mail)
#  
#     
#   })
# 
#   
#   # tot
#   # n.messages
#   # cost.eip.mail
#   # cost.post.eip.mail
#   # phone.contact
#   # n.interviews
#   # phone.cost
#   # effect.without.eip
#   # effect.with.eip.1
#   # effect.with.eip.2
#   
#   # WITH EIP - PERSUADED VOTERS
#   output$persuaded.voters.w <- renderText({
#     
#     # if(b5 = 1){
#     if(input$n.messages == 1){
#       
#       # (b35*b19) + (b16+b18)
#       
#       #b35
#       ((
#         # b34 - b11
#         
#         # b34
#         (    (as.numeric(input$tot)) - #b30 
#                sum(c( 
#                  #b31
#                  (as.numeric(input$n.interviews)/as.numeric(input$phone.contact)) *   # b14 / b13 = b16
#                    as.numeric(input$n.messages), 
#                  #b32 
#                  (as.numeric(input$n.interviews)/as.numeric(input$phone.contact)) *   # b14 / b13 = b16
#                    (as.numeric(input$n.messages) + 1) * #b5 +1
#                    (as.numeric(input$phone.contact)/100)  * # b13
#                    (as.numeric(input$phone.cost))  # b15
#                  , 
#                  #b33
#                  15250
#                )) ) -
#           #b11
#           (input$cost.post.eip.mail)
#         
#         )*
#         #b19
#         (input$effect.with.eip.1)) +
#         #b16
#         (((as.numeric(input$n.interviews)/as.numeric(input$phone.contact)) ) +
#         #b18
#         (input$effect.without.eip))
#       
#     }else{
#       #b35
#       ((    # b34
#         (    (as.numeric(input$tot)) - #b30 
#                sum(c( 
#                  #b31
#                  (as.numeric(input$n.interviews)/as.numeric(input$phone.contact)) *   # b14 / b13 = b16
#                    as.numeric(input$n.messages), 
#                  #b32 
#                  (as.numeric(input$n.interviews)/as.numeric(input$phone.contact)) *   # b14 / b13 = b16
#                    (as.numeric(input$n.messages) + 1) * #b5 +1
#                    (as.numeric(input$phone.contact)/100)  * # b13
#                    (as.numeric(input$phone.cost))  # b15
#                  , 
#                  #b33
#                  15250
#                )) ) -
#           #b11
#           (input$cost.post.eip.mail)
#         
#       ) *
#         #b20
#         (input$effect.with.eip.2)) +
#         #b16
#         (((as.numeric(input$n.interviews)/as.numeric(input$phone.contact)) )*
#         #b5
#         (input$n.messages) *
#         #b18
#         (effect.without.eip))
#       
#     }
#   })
#   
#   
#   ################################### END WITH EIP
#   
#   # PERCENT INCREASE IN PERSUADED VOTERS
#   output$p.increase.persuaded.voters <- renderText({
# 
#     # (b36 - b27) / b27
#     
#     #b36
#     ((    # if(b5 = 1){
#       if(input$n.messages == 1){
#         
#         # (b35*b19) + (b16+b18)
#         
#         #b35
#         ((
#           # b34 - b11
#           
#           # b34
#           (    (as.numeric(input$tot)) - #b30 
#                  sum(c( 
#                    #b31
#                    (as.numeric(input$n.interviews)/as.numeric(input$phone.contact)) *   # b14 / b13 = b16
#                      as.numeric(input$n.messages), 
#                    #b32 
#                    (as.numeric(input$n.interviews)/as.numeric(input$phone.contact)) *   # b14 / b13 = b16
#                      (as.numeric(input$n.messages) + 1) * #b5 +1
#                      (as.numeric(input$phone.contact)/100)  * # b13
#                      (as.numeric(input$phone.cost))  # b15
#                    , 
#                    #b33
#                    15250
#                  )) ) -
#             #b11
#             (input$cost.post.eip.mail)
#           
#         )*
#           #b19
#           (input$effect.with.eip.1)) +
#           #b16
#           (((as.numeric(input$n.interviews)/as.numeric(input$phone.contact)) ) +
#              #b18
#              (input$effect.without.eip))
#         
#       }else{
#         #b35
#         ((    # b34
#           (    (as.numeric(input$tot)) - #b30 
#                  sum(c( 
#                    #b31
#                    (as.numeric(input$n.interviews)/as.numeric(input$phone.contact)) *   # b14 / b13 = b16
#                      as.numeric(input$n.messages), 
#                    #b32 
#                    (as.numeric(input$n.interviews)/as.numeric(input$phone.contact)) *   # b14 / b13 = b16
#                      (as.numeric(input$n.messages) + 1) * #b5 +1
#                      (as.numeric(input$phone.contact)/100)  * # b13
#                      (as.numeric(input$phone.cost))  # b15
#                    , 
#                    #b33
#                    15250
#                  )) ) -
#             #b11
#             (input$cost.post.eip.mail)
#           
#         ) *
#           #b20
#           (input$effect.with.eip.2)) +
#           #b16
#           (((as.numeric(input$n.interviews)/as.numeric(input$phone.contact)) )*
#              #b5
#              (input$n.messages) *
#              #b18
#              (effect.without.eip))
#         
#       }) -
#       #b27
#       (    (as.numeric(input$tot) / as.numeric(input$cost.post.eip.mail)) * # B26
#              (input$effect.without.eip/100))) / 
#       #b27
#       *(    (as.numeric(input$tot) / as.numeric(input$cost.post.eip.mail)) * # B26
#               (input$effect.without.eip/100))
#     
#   })
#   
  
  
  ######### END
})



