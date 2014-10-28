
#########################

##############


# Define server logic for slider examples
shinyServer(function(input, output) {

  
#############################
output$b5 <- renderText({ 
  if(input$checkbox){
  as.numeric(input$total.n) * as.numeric(input$dv.variable.contact.rate) / 100  
  } 
})
 
#############################
output$b14 <- renderText({ 
  if(input$checkbox){
  sqrt(
    (
      (
        (
          input$baseline.action.support.rate*
            (
              1-input$baseline.action.support.rate
            )
        )*
          (
            1-input$r.squared
          )
      )/
        (
          (
            input$percent.in.treatment*
              (
                1-input$percent.in.treatment
              )
          )*
            (
              as.numeric(
                input$total.n
              ) * 
                as.numeric(
                  input$dv.variable.contact.rate
                ) / 
                100
            ) 
        )
    )
  )
  }
})

#############################
output$b16 <- renderText({ 
  
  if(input$checkbox){
 (
  qnorm(input$power/100) +
    qnorm(((input$confidence.interval/100/2)+0.5))
)*
  
  
  (sqrt(
    (
      (
        (
          input$baseline.action.support.rate*
            (
              1-input$baseline.action.support.rate
            )
        )*
          (
            1-input$r.squared
          )
      )/
        (
          (
            input$percent.in.treatment*
              (
                1-input$percent.in.treatment
              )
          )*
            (
              as.numeric(
                input$total.n
              ) * 
                as.numeric(
                  input$dv.variable.contact.rate
                ) / 
                100
            ) 
        )
    )
  ))
  }
})

#############################
output$b17 <- renderText({ 
  if(input$checkbox){
    
  (
    qnorm(input$power/100) +
      qnorm(((input$confidence.interval/100/2)+0.5))
  )*
    
    
    (sqrt(
      (
        (
          (
            input$baseline.action.support.rate*
              (
                1-input$baseline.action.support.rate
              )
          )*
            (
              1-input$r.squared
            )
        )/
          (
            (
              input$percent.in.treatment*
                (
                  1-input$percent.in.treatment
                )
            )*
              (
                as.numeric(
                  input$total.n
                ) * 
                  as.numeric(
                    input$dv.variable.contact.rate
                  ) / 
                  100
              ) 
          )
      )
    ))/ 
  input$treatment.application.rate *100
  }
})


#############################
output$b20 <- renderText({ 
  
  if(input$checkbox){
    if(input$number.of.clusters > 0){
  as.numeric(input$total.n) / as.numeric(input$number.of.clusters)
  }} else{NULL}
  
})

#############################
output$b22 <- renderText({ 
  if(input$checkbox){
    if(input$number.of.clusters > 0){
    
  
  sqrt((1+(( (as.numeric(input$total.n) / as.numeric(input$number.of.clusters))  -1)*
             as.numeric(input$intra.cluster.correlation.coefficient))))}
  }
  
})

#############################
output$b23 <- renderText({ 
  if(input$checkbox){
    if(input$number.of.clusters > 0){
    
  
  (  sqrt(
    (
      (
        (
          input$baseline.action.support.rate*
            (
              1-input$baseline.action.support.rate
            )
        )*
          (
            1-input$r.squared
          )
      )/
        (
          (
            input$percent.in.treatment*
              (
                1-input$percent.in.treatment
              )
          )*
            (
              as.numeric(
                input$total.n
              ) * 
                as.numeric(
                  input$dv.variable.contact.rate
                ) / 
                100
            ) 
        )
    )
  ))*
    (sqrt((1+(( (as.numeric(input$total.n) / as.numeric(input$number.of.clusters))  -1)*
                as.numeric(input$intra.cluster.correlation.coefficient)))))
  }}
  
})

#############################
output$b25 <- renderText({ 
  if(input$number.of.clusters > 0){
  
  if(input$checkbox){
    
  
  (
    qnorm(input$power/100) +
      qnorm(((input$confidence.interval/100/2)+0.5))
  )*
    (  sqrt(
      (
        (
          (
            input$baseline.action.support.rate*
              (
                1-input$baseline.action.support.rate
              )
          )*
            (
              1-input$r.squared
            )
        )/
          (
            (
              input$percent.in.treatment*
                (
                  1-input$percent.in.treatment
                )
            )*
              (
                as.numeric(
                  input$total.n
                ) * 
                  as.numeric(
                    input$dv.variable.contact.rate
                  ) / 
                  100
              ) 
          )
      )
    ))*
    (sqrt((1+(( (as.numeric(input$total.n) / as.numeric(input$number.of.clusters))  -1)*
                as.numeric(input$intra.cluster.correlation.coefficient)))))
  }}
})

#############################
output$b26 <- renderText({ 
  
  
  if(input$checkbox){
    if(input$number.of.clusters > 0){
    
  
  (  (
    qnorm(input$power/100) +
      qnorm(((input$confidence.interval/100/2)+0.5))
  )*
    (  sqrt(
      (
        (
          (
            input$baseline.action.support.rate*
              (
                1-input$baseline.action.support.rate
              )
          )*
            (
              1-input$r.squared
            )
        )/
          (
            (
              input$percent.in.treatment*
                (
                  1-input$percent.in.treatment
                )
            )*
              (
                as.numeric(
                  input$total.n
                ) * 
                  as.numeric(
                    input$dv.variable.contact.rate
                  ) / 
                  100
              ) 
          )
      )
    ))*
    (sqrt((1+(( (as.numeric(input$total.n) / as.numeric(input$number.of.clusters))  -1)*
                as.numeric(input$intra.cluster.correlation.coefficient)))))) / 
    input$treatment.application.rate
  
  }}
  
})

#############################################






















#####################################################


#############################
output$zb5 <- renderText({ 
  
  if(input$zcheckbox){
    
  as.numeric(input$ztotal.n) * as.numeric(input$zdv.variable.contact.rate) / 100  
  
  }
})

#############################
output$zb14 <- renderText({ 
  
  if(input$zcheckbox){
    
  
  #s.e.#########
  sqrt(((( (input$zstandard.deviation.of.observations.across.units/100 )^2)*
           (1-input$zr.squared))/
          (( (input$zpercent.in.treatment/100)*
              (1- (input$zpercent.in.treatment/100) ))*
             (as.numeric(input$ztotal.n) * as.numeric(input$zdv.variable.contact.rate) / 100))))
  }
  ##############
  

  
 
})

#############################
output$zb16 <- renderText({ 
  if(input$zcheckbox){
    

  
  (
    qnorm(input$zpower/100) +
      qnorm(((input$zconfidence.interval/100/2)+0.5))
  )*
    
    #s.e.#########
  sqrt(((( (input$zstandard.deviation.of.observations.across.units/100 )^2)*
           (1-input$zr.squared))/
          (( (input$zpercent.in.treatment/100)*
               (1- (input$zpercent.in.treatment/100) ))*
             (as.numeric(input$ztotal.n) * as.numeric(input$zdv.variable.contact.rate) / 100))))
  ##############
  }
    
})

#############################
output$zb17 <- renderText({ 
  
  if(input$zcheckbox){
    
  
  
  ####################### zb16
 ((qnorm(input$zpower/100) +
      qnorm(((input$zconfidence.interval/100/2)+0.5))
  )*
    
  sqrt(((( (input$zstandard.deviation.of.observations.across.units/100 )^2)*
           (1-input$zr.squared))/
          (( (input$zpercent.in.treatment/100)*
               (1- (input$zpercent.in.treatment/100) ))*
             (as.numeric(input$ztotal.n) * as.numeric(input$zdv.variable.contact.rate) / 100)))))/
   
  #######################
 (input$ztreatment.application.rate/100)

  }
  
})


#############################
output$zb20 <- renderText({ 
  
  if(input$zcheckbox){
    if(input$znumber.of.clusters > 0){
    
  
  as.numeric(input$ztotal.n) / as.numeric(input$znumber.of.clusters)
  }}
  
})

#############################
output$zb22 <- renderText({ 
  if(input$zcheckbox){
    if(input$znumber.of.clusters > 0){
    
  
  sqrt((1+(( (as.numeric(input$ztotal.n) / as.numeric(input$znumber.of.clusters)) -1)*
            (as.numeric(input$zintra.cluster.correlation.coefficient) ) )))
  }}
  
})

#############################
output$zb23 <- renderText({ 
  
  if(input$zcheckbox){
    if(input$znumber.of.clusters > 0){
    

  
  (
    #s.e.#########
    sqrt(((( (input$zstandard.deviation.of.observations.across.units/100 )^2)*
             (1-input$zr.squared))/
            (( (input$zpercent.in.treatment/100)*
                 (1- (input$zpercent.in.treatment/100) ))*
               (as.numeric(input$ztotal.n) * as.numeric(input$zdv.variable.contact.rate) / 100))))
    ##############
    ) * 
    (  sqrt((1+(( (as.numeric(input$ztotal.n) / as.numeric(input$znumber.of.clusters)) -1)*
                  (as.numeric(input$zintra.cluster.correlation.coefficient) ) ))))
  
  }}
 
})

#############################
output$zb25 <- renderText({ 
  
  if(input$zcheckbox){
    if(input$znumber.of.clusters > 0){
    
  
  
  
  (
    qnorm(input$zpower/100) +
      qnorm(((input$zconfidence.interval/100/2)+0.5))
  )*
    
    # B 23#########
  (
    (
      #s.e.#########
      sqrt(((( (input$zstandard.deviation.of.observations.across.units/100 )^2)*
               (1-input$zr.squared))/
              (( (input$zpercent.in.treatment/100)*
                   (1- (input$zpercent.in.treatment/100) ))*
                 (as.numeric(input$ztotal.n) * as.numeric(input$zdv.variable.contact.rate) / 100))))
      ##############
    ) * 
      (  sqrt((1+(( (as.numeric(input$ztotal.n) / as.numeric(input$znumber.of.clusters)) -1)*
                    (as.numeric(input$zintra.cluster.correlation.coefficient) ) ))))
    )
 
  }}
})

#############################
output$zb26 <- renderText({ 
  
  if(input$zcheckbox){
    if(input$znumber.of.clusters > 0){
    
  
  (  (
    qnorm(input$zpower/100) +
      qnorm(((input$zconfidence.interval/100/2)+0.5))
  )*
    
    # B 23#########
  (
    (
      #s.e.#########
      sqrt(((( (input$zstandard.deviation.of.observations.across.units/100 )^2)*
               (1-input$zr.squared))/
              (( (input$zpercent.in.treatment/100)*
                   (1- (input$zpercent.in.treatment/100) ))*
                 (as.numeric(input$ztotal.n) * as.numeric(input$zdv.variable.contact.rate) / 100))))
      ##############
    ) * 
      (  sqrt((1+(( (as.numeric(input$ztotal.n) / as.numeric(input$znumber.of.clusters)) -1)*
                    (as.numeric(input$zintra.cluster.correlation.coefficient) ) ))))
  )) /
    (input$ztreatment.application.rate)*100  
  
  }}
})


#############################
output$dplot <- renderPlot({ 
  
  


itt.mde.d <- 
  (
    qnorm(input$power/100) +
      qnorm(((input$confidence.interval/100/2)+0.5))
  )*
  
  
  (sqrt(
    (
      (
        (
          input$baseline.action.support.rate*
            (
              1-input$baseline.action.support.rate
            )
        )*
          (
            1-input$r.squared
          )
      )/
        (
          (
            input$percent.in.treatment*
              (
                1-input$percent.in.treatment
              )
          )*
            (
              as.numeric(
                input$total.n
              ) * 
                as.numeric(
                  input$dv.variable.contact.rate
                ) / 
                100
            ) 
        )
    )
  ))
tot.mde.d <- 
  (
    qnorm(input$power/100) +
      qnorm(((input$confidence.interval/100/2)+0.5))
  )*
  
  
  (sqrt(
    (
      (
        (
          input$baseline.action.support.rate*
            (
              1-input$baseline.action.support.rate
            )
        )*
          (
            1-input$r.squared
          )
      )/
        (
          (
            input$percent.in.treatment*
              (
                1-input$percent.in.treatment
              )
          )*
            (
              as.numeric(
                input$total.n
              ) * 
                as.numeric(
                  input$dv.variable.contact.rate
                ) / 
                100
            ) 
        )
    )
  ))/ 
  input$treatment.application.rate *100

adj.itt.mde.d <-  (
  qnorm(input$power/100) +
    qnorm(((input$confidence.interval/100/2)+0.5))
)*
  (  sqrt(
    (
      (
        (
          input$baseline.action.support.rate*
            (
              1-input$baseline.action.support.rate
            )
        )*
          (
            1-input$r.squared
          )
      )/
        (
          (
            input$percent.in.treatment*
              (
                1-input$percent.in.treatment
              )
          )*
            (
              as.numeric(
                input$total.n
              ) * 
                as.numeric(
                  input$dv.variable.contact.rate
                ) / 
                100
            ) 
        )
    )
  ))*
  (sqrt((1+(( (as.numeric(input$total.n) / as.numeric(input$number.of.clusters))  -1)*
              as.numeric(input$intra.cluster.correlation.coefficient)))))

adj.tot.mde.d <-   (  (
  qnorm(input$power/100) +
    qnorm(((input$confidence.interval/100/2)+0.5))
)*
  (  sqrt(
    (
      (
        (
          input$baseline.action.support.rate*
            (
              1-input$baseline.action.support.rate
            )
        )*
          (
            1-input$r.squared
          )
      )/
        (
          (
            input$percent.in.treatment*
              (
                1-input$percent.in.treatment
              )
          )*
            (
              as.numeric(
                input$total.n
              ) * 
                as.numeric(
                  input$dv.variable.contact.rate
                ) / 
                100
            ) 
        )
    )
  ))*
  (sqrt((1+(( (as.numeric(input$total.n) / as.numeric(input$number.of.clusters))  -1)*
              as.numeric(input$intra.cluster.correlation.coefficient)))))) / 
  input$treatment.application.rate



myCols <- adjustcolor(c( "darkgreen", "grey"), alpha.f=0.5)

bp.elements <- c(itt.mde.d,
                 tot.mde.d,
                 adj.itt.mde.d,
                 adj.tot.mde.d)
bp.names <- c("ITT",
              "TOT",
              "ADJ ITT",
              "ADJ TOT")

if(input$number.of.clusters == 0){
  bp.elements <- bp.elements[1:2]
  bp.names <- bp.names[1:2]
} else{
  bp.elements <- bp.elements[3:4]
  bp.names <- bp.names[3:4]
}



mybp <- barplot(bp.elements,
        col=myCols,
        names.arg=bp.names,
        ylab="Minimal detectable effect",
        main=NULL,
        border=FALSE)
text(x=mybp[,1], y=bp.elements,
     pos= ifelse(bp.elements >= 0.95*max(bp.elements, na.rm=TRUE),1,3),
     labels=paste(round(bp.elements*100, digits=1), "%"),
     cex=1.5)
box("outer")




  
})

#############################
output$cplot <- renderPlot({ 
  
    itt.mde.d <- 
      
      (
        qnorm(input$zpower/100) +
          qnorm(((input$zconfidence.interval/100/2)+0.5))
      )*
      
      #s.e.#########
    sqrt(((( (input$zstandard.deviation.of.observations.across.units/100 )^2)*
             (1-input$zr.squared))/
            (( (input$zpercent.in.treatment/100)*
                 (1- (input$zpercent.in.treatment/100) ))*
               (as.numeric(input$ztotal.n) * as.numeric(input$zdv.variable.contact.rate) / 100))))
    ##############
    
    tot.mde.d <- 
      ####################### zb16
      ((qnorm(input$zpower/100) +
          qnorm(((input$zconfidence.interval/100/2)+0.5))
      )*
        
        sqrt(((( (input$zstandard.deviation.of.observations.across.units/100 )^2)*
                 (1-input$zr.squared))/
                (( (input$zpercent.in.treatment/100)*
                     (1- (input$zpercent.in.treatment/100) ))*
                   (as.numeric(input$ztotal.n) * as.numeric(input$zdv.variable.contact.rate) / 100)))))/
      
      #######################
    (input$ztreatment.application.rate/100)
    
    adj.itt.mde.d <- 
      
      (
        qnorm(input$zpower/100) +
          qnorm(((input$zconfidence.interval/100/2)+0.5))
      )*
      
      # B 23#########
    (
      (
        #s.e.#########
        sqrt(((( (input$zstandard.deviation.of.observations.across.units/100 )^2)*
                 (1-input$zr.squared))/
                (( (input$zpercent.in.treatment/100)*
                     (1- (input$zpercent.in.treatment/100) ))*
                   (as.numeric(input$ztotal.n) * as.numeric(input$zdv.variable.contact.rate) / 100))))
        ##############
      ) * 
        (  sqrt((1+(( (as.numeric(input$ztotal.n) / as.numeric(input$znumber.of.clusters)) -1)*
                      (as.numeric(input$zintra.cluster.correlation.coefficient) ) ))))
    )
    
    adj.tot.mde.d <- 
      (  (
        qnorm(input$zpower/100) +
          qnorm(((input$zconfidence.interval/100/2)+0.5))
      )*
        
        # B 23#########
      (
        (
          #s.e.#########
          sqrt(((( (input$zstandard.deviation.of.observations.across.units/100 )^2)*
                   (1-input$zr.squared))/
                  (( (input$zpercent.in.treatment/100)*
                       (1- (input$zpercent.in.treatment/100) ))*
                     (as.numeric(input$ztotal.n) * as.numeric(input$zdv.variable.contact.rate) / 100))))
          ##############
        ) * 
          (  sqrt((1+(( (as.numeric(input$ztotal.n) / as.numeric(input$znumber.of.clusters)) -1)*
                        (as.numeric(input$zintra.cluster.correlation.coefficient) ) ))))
      )) /
      (input$ztreatment.application.rate)*100  
    
    
    
    myCols <- adjustcolor(c("darkgreen", "grey"), alpha.f=0.5)
    
    bp.elements <- c(itt.mde.d,
                     tot.mde.d,
                     adj.itt.mde.d,
                     adj.tot.mde.d)
    bp.names <- c("ITT",
                  "TOT",
                  "ADJ ITT",
                  "ADJ TOT")
    
    if(input$znumber.of.clusters == 0){
      bp.elements <- bp.elements[1:2]
      bp.names <- bp.names[1:2]
    } else{
      bp.elements <- bp.elements[3:4]
      bp.names <- bp.names[3:4]
    }
    
    mybp <- barplot(bp.elements,
            col=myCols,
            names.arg=bp.names,
            ylab="Minimal detectable effect",
            main=NULL, 
            border=FALSE)
    text(x=mybp[,1], y=bp.elements,
         pos= ifelse(bp.elements >= .95*max(bp.elements, na.rm=TRUE),1,3),
         labels=paste(round(bp.elements*100, digits=1), "%"),
         cex=1.5)
    
    box("outer")
  
  
  
})







#################################
# DETAILS
#################################



#############################
output$Details <- renderText({ 
  if(input$checkbox){
      
      
   print("Details")

    }
  
})

#############################
output$Number.of.outcome.measurements <- renderText({ 
  if(input$checkbox){
    
    
    print("Number of outcome measurements")
    
  }
  
})

#############################
output$S.E. <- renderText({ 
  if(input$checkbox){
    
    
    print("S.E.:")
    
  }
  
})

#############################
output$ITT.MDE <- renderText({ 
  if(input$checkbox){
    
    
    print("ITT MDE:")
    
  }
  
})

#############################
output$TOT.MDE <- renderText({ 
  if(input$checkbox){
    
    
    print("ToT MDE:")
    
  }
  
})

#############################
output$Average.cluster.size <- renderText({ 
  if(input$checkbox){
    if(input$number.of.clusters > 0){
      
      print("Average cluster size:")
      
      
    } else {NULL}
    
    
    
  }
  
})

#############################
output$Square.root.of.variance.inflation.factor <- renderText({ 
  if(input$checkbox){
    if(input$number.of.clusters > 0){
      
      print("Square root of variance inflation factor:")
      
      
    } else {NULL}
    
    
    
  }
  
})

#############################
output$Adjusted.S.E. <- renderText({ 
  if(input$checkbox){
    if(input$number.of.clusters > 0){
      
      print("Adjusted S.E.:")
      
      
    } else {NULL}
    
  }
  
})

#############################
output$Adj.ITT.MDE <- renderText({ 
  if(input$checkbox){
    if(input$number.of.clusters > 0){
      
      print("Adjusted ITT MDE:")
      
      
    } else {NULL}

  }
  
})

#############################
output$Adj.TOT.MDE <- renderText({ 
  if(input$checkbox){
    if(input$number.of.clusters > 0){
      
      print("Adjusted ToT MDE:")
      
      
    } else {NULL}

  }
  
})

  









#################################
# Z DETAILS
#################################



#############################
output$zDetails <- renderText({ 
  if(input$zcheckbox){
    
    
    print("Details")
    
  }
  
})

#############################
output$zNumber.of.outcome.measurements <- renderText({ 
  if(input$zcheckbox){
    
    
    print("Number of outcome measurements")
    
  }
  
})

#############################
output$zS.E. <- renderText({ 
  if(input$zcheckbox){
    
    
    print("S.E.:")
    
  }
  
})

#############################
output$zITT.MDE <- renderText({ 
  if(input$zcheckbox){
    
    
    print("ITT MDE:")
    
  }
  
})

#############################
output$zTOT.MDE <- renderText({ 
  if(input$zcheckbox){
    
    
    print("ToT MDE:")
    
  }
  
})

#############################
output$zAverage.cluster.size <- renderText({ 
  if(input$zcheckbox){
    if(input$znumber.of.clusters > 0){
      
      print("Average cluster size:")
      
      
    } else {NULL}
    
    
    
  }
  
})

#############################
output$zSquare.root.of.variance.inflation.factor <- renderText({ 
  if(input$zcheckbox){
    if(input$znumber.of.clusters > 0){
      
      print("Square root of variance inflation factor:")
      
      
    } else {NULL}
    
    
    
  }
  
})

#############################
output$zAdjusted.S.E. <- renderText({ 
  if(input$zcheckbox){
    if(input$znumber.of.clusters > 0){
      
      print("Adjusted S.E.:")
      
      
    } else {NULL}
    
  }
  
})

#############################
output$zAdj.ITT.MDE <- renderText({ 
  if(input$zcheckbox){
    if(input$znumber.of.clusters > 0){
      
      print("Adjusted ITT MDE:")
      
      
    } else {NULL}
    
  }
  
})

#############################
output$zAdj.TOT.MDE <- renderText({ 
  if(input$zcheckbox){
    if(input$znumber.of.clusters > 0){
      
      print("Adjusted ToT MDE:")
      
      
    } else {NULL}
    
  }
  
})




})