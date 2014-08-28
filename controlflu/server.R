library(shiny)
suppressPackageStartupMessages(library(googleVis))


shinyServer(function (input, output) {
  
  source("helper.R")
  
  ########
  mydata <- reactive({
    ir[which(ir$team2014 == as.numeric(as.character(input$team))),]
    })
  
  #######
  mydata13 <- reactive({
    mydata()[which(mydata()$year == 2013),]
    })
  
  #######
  output$plot1 <- renderPlot({
    TeamFun(team = as.numeric(input$team), data = mydata(), bar=FALSE)
    })
  
  #######
  output$plot2 <- renderPlot({
    par(mar=c(9,4,1,1))

    TeamFun(team = as.numeric(input$team), data = mydata(), bar=TRUE)
  })
  
  #######
  output$plot3 <- renderPlot({
    TeamFun(team = as.numeric(input$team), data = mydata(), cf=TRUE, bar=FALSE)
  })
  
  #######
  output$plot4 <- renderPlot({
    par(mar=c(9,4,1,1))
    
    TeamFun(team = as.numeric(input$team), data = mydata(), cf=TRUE, bar=TRUE)
  })
  
  #######
  output$plot5 <- renderPlot({
    
    x <- ir[which(as.numeric(ir$team2014) == as.numeric(input$team) ),]
    x <- unique(sort(x$school[which(!is.na(x$immRate[which(x$year == 2013)]))]))
    x <- x[which(x != "NEWBERRY ELEM.")] #NOT SURE WHY NEWBERRY ELEM DOESN'T WORK
    x <- x[1:3]
    
    par(mar=c(5,4,4,1))
    par(mfrow=c(ceiling(length(x)/3), 3))
    
    for (i in x){
      GradeFun(i, 2013)
    }
    
  })
  
  #######
  output$plot6 <- renderPlot({
    
    x <- ir[which(as.numeric(ir$team2014) == as.numeric(input$team) ),]
    x <- unique(sort(x$school[which(!is.na(x$immRate[which(x$year == 2013)]))]))
    x <- x[which(x != "NEWBERRY ELEM.")] #NOT SURE WHY NEWBERRY ELEM DOESN'T WORK
    x <- x[4:6]
    
    par(mar=c(5,4,4,1))
    par(mfrow=c(ceiling(length(x)/3), 3))
    
    for (i in x){
      GradeFun(i, 2013)
    }
    
  })
  
  
  
  

  ########
  output$text1 <- renderText({
    paste("Team", input$team, "immunization rate overview")
  })
  
  ########
  output$text2 <- renderText({
    paste("Team", input$team, "consent form return rate overview")
  })
  

  ########
  output$table1 <- renderDataTable({
    x <- as.data.frame(mydata())
    x[which(x$team2014 == input$team),]
    x <- x[,c("school", "year", "immRate", "cfrr", "totMem")]
    x <- x[order(x$school),]
  })
  
  ########
  output$table2 <- renderDataTable({
    x <- as.data.frame(mydata13())
    x[which(x$team2014 == input$team),]
    x <- x[,c("school", "year", "immRate", "doses", "totMem")]
    x <- x[order(x$school),]
  })
  

  
  ########
  output$motionchart1 <- renderGvis({
    
    gvisMotionChart(data = mydata(), 
                    idvar = "school", 
                    timevar = "year",
                    xvar = "year",
                    yvar = "immRate",
                    colorvar = "cfrr",
                    sizevar = "totMem")

  })
  
  ########
  output$motionchart2 <- renderGvis({
    
    gvisMotionChart(data = ir, #mydata(), 
                    idvar = "school", 
                    timevar = "year",
                    xvar = "year",
                    yvar = "immRate",
                    colorvar = "team2014",
                    sizevar = "totMem")
    
  })
  
  ########
  output$motionchart3 <- renderGvis({
    
    gvisMotionChart(data = mydata(), 
                    idvar = "school", 
                    timevar = "year",
                    xvar = "year",
                    yvar = "immRate",
                    colorvar = "p.vfc",
                    sizevar = "totMem")
    
  })


  
  
#     plot(M)
#     
#     return (gvisMotionChart(cast.1,"jurisdiccion",timevar="Anio",xvar="ipcf_mean",yvar="ginis",date.format="%Y"))
#     

  
#   output$edades <- renderGvis({
#     
#     cast.map <- aggregate(data()[,4],list(Jurisdiccion=data()$jurisdiccion),mean)
#     names(cast.map)[2] <- "promedio"
#     cast.map$promedio <- round(cast.map$promedio,3)
#     
#     map <- gvisGeoChart(cast.map,colorvar="promedio",locationvar="Jurisdiccion",
#                         options=list(region="AR",displayMode="regions", resolution="provinces",
#                                      title="Average age per Province",
#                                      titlePosition='out',
#                                      titleTextStyle="{color:'black',fontName:'Courier',fontSize:14}",
#                                      height=500, width=400))
#     
#     quant<-cut(data()$ch06,quantile(data()$ch06,(0:4)/4),include.lowest=TRUE,na.rm=TRUE)
#     sset <- cbind(data(),quant)
#     tab <- with(sset,prop.table(table(jurisdiccion,quant),1))
#     tab <- as.data.frame.matrix(tab)
#     tab <- tab*100
#     tab <- round(tab,2)
#     tab <- cbind(row.names(tab),tab)
#     colnames(tab)[1] <- "Provincia"
#     bar.pl <- gvisColumnChart(tab,xvar=colnames(tab)[1],yvar=colnames(tab)[2:5],
#                               options=list(title="Age cuartiles per province (in %)",
#                                            titlePosition='out',
#                                            hAxis="{slantedText:'true',slantedTextAngle:45}",
#                                            titleTextStyle="{color:'black',fontName:'Courier',fontSize:14}",
#                                            height=500, width=400))
#     
#     merge <- gvisMerge(map,bar.pl,horizontal=TRUE,tableOptions="cellspacing=10")
#     
#     return(merge)
#     
#   })
#   
#   output$tabla1 <- renderGvis({
#     
#     t1 <- with(data(), prop.table(table(jurisdiccion,ocupacion),1))
#     t1 <- as.data.frame.matrix(t1)
#     t1 <- t1*100
#     t1 <- round(t1,1)
#     t1 <- cbind(row.names(t1),t1)
#     colnames(t1)[1] <- "Provincia"
#     t1.pl <- gvisTable(t1,options=list(page='enable',height=300,width=800))
#     return(t1.pl)
#   })
#   
#   output$tabla2 <- renderGvis({
#     t2 <- with(data(), prop.table(table(jurisdiccion,caes_recode2),1))
#     t2 <- as.data.frame.matrix(t2)
#     t2 <- t2*100
#     t2 <- round(t2,1)
#     t2 <- cbind(row.names(t2),t2)
#     colnames(t2) <- tolower(colnames(t2))
#     colnames(t2)[1] <- "Provincia"
#     t2.pl <- gvisTable(t2,options=list(page='enable',height=300,width=800))
#     
#     return(t2.pl)
#   })
#   
#   output$linech <- renderGvis({
#     tab <- with(data(),prop.table(table(jurisdiccion,nivel_ed),1))
#     tab <- as.data.frame.matrix(tab)
#     tab <- tab*100
#     tab <- round(tab,2)
#     tab <- cbind(row.names(tab),tab)
#     colnames(tab)[1] <- "Provincia"
#     tab <- tab[,-grep("Sin instruccion",colnames(tab))]
#     order <- c("Provincia","Primaria Incompleta","Primaria Completa","Secundaria Incompleta","Secundaria Completa",
#                "Universitaria Incompleta", "Universitaria Completa")
#     
#     index <- lapply(order, function(x) grep(x,names(tab)))
#     
#     tab <- tab[,c(as.numeric(index))]
#     
#     tab.s <- cbind(tab[,1],sum(tab[,2:7]),sum(tab[,3:7]),sum(tab[,4:7])
#                    ,sum(tab[,5:7]),sum(tab[,6:7]),sum(tab[,7]))
#     
#     tab.2 <- data.frame(rep(0,nrow(tab)),rep(0,nrow(tab)),rep(0,nrow(tab))
#                         ,rep(0,nrow(tab)),rep(0,nrow(tab)),rep(0,nrow(tab)))
#     
#     names(tab.2) <- names(tab)[2:7]
#     
#     for ( i in 1:6){
#       for (j in 1:6){
#         if(i >= j){
#           tab.2[,j] <- tab.2[,j]+tab[,i+1]
#         } 
#       }
#     }
#     tab.pl <- as.data.frame(t(tab.2))
#     colnames(tab.pl) <- tab$Provincia
#     tab.pl <- cbind(Nivel_ed=names(tab)[2:7],tab.pl)
#     
#     #### Area under curve ####
#     
#     areas <- rbind(rep(100,ncol(tab.pl)),tab.pl)
#     
#     areas[,2:ncol(areas)] <- areas[,2:ncol(areas)]/100
#     
#     ed.coef <- (areas[1,-c(1)]+areas[2,-c(1)])/2
#     limit <- nrow(areas)-1
#     
#     for (i in 2:limit){
#       j <- i +1
#       ed.coef <- ed.coef + (areas[i,-c(1)]+areas[j,-c(1)])/2
#     }
#     
#     ed.coef <- ed.coef/limit
#     
#     ed.coef <- t(ed.coef)
#     ed.coef <- round(ed.coef,4)
#     ed.coef <- cbind(colnames(areas)[-c(1)],ed.coef)
#     
#     ed.coef <- as.data.frame(ed.coef)
#     colnames(ed.coef)<-c("Provincia","Education Completeness")
#     
#     #### Plots ###
#     
#     line.pl <- gvisLineChart(tab.pl,xvar=colnames(tab.pl)[1],yvar=colnames(tab.pl)[-1],
#                              options=list(
#                                title="Education Curve",
#                                titlePosition='out',
#                                hAxis="{slantedText:'true',slantedTextAngle:45}",
#                                titleTextStyle="{color:'black',fontName:'Courier'}",
#                                legend="{color:'black',fontName:'Courier'}",
#                                fontSize="10",
#                                chartArea="{left:40,top:30,width:'70%',height:'75%'}",            
#                                height=550, width=600))
#     
#     t1.ed <- gvisTable(ed.coef,
#                        options=list(page='enable',fontSize="10",height=300,width=275))
#     
#     ed.output <- gvisMerge(line.pl,t1.ed,horizontal=TRUE)
#     
#     return(ed.output)
#   })
  
  #outputOptions(output, "motionchart", suspendWhenHidden = FALSE)


#   outputOptions(output, "edades", suspendWhenHidden = FALSE)
#   outputOptions(output, "tabla1", suspendWhenHidden = FALSE)
#   outputOptions(output, "tabla2", suspendWhenHidden = FALSE)
#   outputOptions(output, "linech", suspendWhenHidden = FALSE)
})