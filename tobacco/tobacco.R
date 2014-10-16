library(Hmisc)
library(RColorBrewer)
library(car)

#################
# ## Setwd
# if ( Sys.info()["sysname"] == "Linux" ){
#   setwd("/home/joebrew/Documents/ShinyApps/tobacco")
# } else {
#   setwd("C:/Users/BrewJR/Documents/ShinyApps/tobacco")
# }
# 
# mywd <- getwd()


# Read in data
#comb <- read.csv("comb1.csv", stringsAsFactors = FALSE)
comb <- read.csv("combined_corrected.csv", stringsAsFactors = FALSE)
locations <- read.csv("locations.csv", stringsAsFactors = FALSE)
comb <- cbind(comb,locations)

# MAKE CRAZY LOCATIONS NA
comb$lat[which(comb$lat > 32)] <- NA
comb$lat[which(comb$lat < 20)] <- NA
comb$lon[which(comb$lon > - 75)] <- NA
comb$lon[which(comb$lon < -89)] <- NA


comb[,1] <- NULL
# Recode all 77, 88, 99
for (i in 1:ncol(comb)){
  var <-  comb[,i]
  var[which(var %in% c(77,88,99))] <- NA
  var[which(var %in% c("77", "88", "99"))] <- NA
  comb[,i] <- var
}

# Selectively recode
yesnos <- c(7,8, 12:31, 33:35)
for (i in yesnos){
  comb[,i] <- Recode(comb[,i],
                     "1 = 'Yes';
                     2 = 'No'")
}

# Size category
comb[,5] <- Recode(comb[,5],
                   "1 = 'Small'; 
                   2 = 'Large'")

# Type of insurance coverage
comb[,9] <- Recode(comb[,9],
                   "1 = 'Self'; 
                   2 = 'Fully';
                   0 = NA;
                   ' ' = NA; 
                   '' = NA; 
                   '1- ' = NA;  
                   '1-' = 'Self';
                   '2l' = 'Fully'")

# Employer sector
comb[,11] <- Recode(comb[,11],
                    "1 = 'Government';
                    2 = 'Healthcare';
                    3 = 'Business';
                    4 = 'County School District';
                    5 = 'City Municipality';
                    6 = 'County Municipality'")

# Extent of TFG policy coverage
comb[,32] <- Recode(comb[,32], "
                    1 = '100% SFG';
                    2 = '100% TFG';
                    3 = 'SFIW';
                    4 = 'SFIWO'")

# Insurance carrier
comb[,10] <- Recode(comb[,10], 
                    "'1' = 'UHC';
                    '2' = 'Aetna';
                    '3' = 'Avmed';
                    '4' = 'BCBS';
                    '5' = 'Capital';
                    '6' = 'Florida Blue';
                    '7' = 'Cigna';
                    '8' = 'Health First';
                    '9' = 'Humana';
                    '10' = 'Self-insured';
                    '11' = 'Other';
                    '12' = 'Combination'")

# FIX THE SECTOR ISSUE
# library(gdata)
# school_districts <- read.xls("tobacco.xlsm", sheet = 4)
# city <- read.xls("tobacco.xlsm", sheet = 5)
# county <- read.xls("tobacco.xlsm", sheet = 6)
# 
# # school districts
# comb[,11][which(comb[,3] %in% as.character(school_districts[,3]))] <-
#   "County School District"
# 
# # city municipalities
# comb[,11][which(comb[,3] %in% as.character(city[,3]))] <-
#   "City Municipality" 
# 
# # county municipalities
# comb[,11][which(comb[,3] %in% as.character(county[,3]))] <-
#   "County Municipality" 
# 
# # visualize
# bp <- barplot(table(comb[,11]), cex.names = 0.55, las = 1, border = "darkgrey")
# abline(h=seq(0,1000,100),
#        col = adjustcolor("darkgreen", alpha.f = 0.2))
# text(x = bp[,1],
#      y = 150,
#      pos = 1,
#      labels = paste0(100*round(prop.table(table(comb[,11])), digits = 4), "%"),
#      cex = 0.5)
# 
# #write.csv(comb, "combined_corrected.csv")

# Read in names of questions
comb_names <- read.csv("comb1.csv", header = FALSE)
comb_names <- comb_names[1,]

###################
# SIMPASYM FUNCTION FOR CALCULATIN C.I. OF PROPORTIONS
###################
simpasym <- function(n, p, z=1.96, cc=TRUE){
  out <- list()
  if(cc){
    out$lb <- p - z*sqrt((p*(1-p))/n) - 0.5/n
    out$ub <- p + z*sqrt((p*(1-p))/n) + 0.5/n
  } else {
    out$lb <- p - z*sqrt((p*(1-p))/n)
    out$ub <- p + z*sqrt((p*(1-p))/n)
  }
  out
}

###################
# WRITE FUNCTION FOR PLOTTING BARS
###################
BarFun <- function(var, by_var = NULL, recode_var = NULL, ref = NULL,
                   cex.names = 1, las = 1, legend = FALSE, rain = FALSE,
                   border = "black", percent = TRUE,
                   legend.cex = 0.8, legend.title = NULL,
                   err.cex = 0.8){
  
  #   var <- comb[,"Insurance.Carrier"]
  #   by_var = comb[,5]
  #   ref <- NULL
  #   recode_var <- NULL
  #   cex.names = 1
  #   las = 1
  #   percent = TRUE
  #   border = "black"
  #   rain = TRUE
  #   legend = TRUE
  # Ensure it's treated as a character
  var <- as.character(var)
  
  # Make var a factor and relevel so "unknown" comes first
  var <- factor(var)
  if(!is.null(by_var)){by_var <- factor(by_var)}
  if(is.null(ref)){
    var <- var
  } else {
    var <- relevel(var, ref = ref)
  }
  
  # Make a table
  if(is.null(by_var)){
    var_table <- table(var)
    var_names <- names(var_table)
    var_vals <- as.numeric(var_table)
    var_prop <- prop.table(var_table) * 100
    
    # Calculate confidence intervals
    ci <- simpasym(n=sum(var_table), 
                   p=var_prop / 100, 
                   z=1.96, cc=TRUE) 
    lb <- ci$lb * sum(var_table)
    ub <- ci$ub * sum(var_table)
    
  } else {
    #var_table <- table(var, by_var)
    var_table <- table(by_var, var)
    var_names <- levels(var)
    var_vals <- matrix(var_table, ncol = length(levels(by_var)))
    var_prop <- prop.table(var_table, 2) * 100
    
    # Calculate confidence intervals
    ci <- simpasym(n= rep(apply(var_table, 1, sum),2), #sum(var_table), 
                   p=var_prop / 100, 
                   z=1.96, cc=TRUE) 
    lb <- ci$lb * rep(apply(var_table, 1, sum),2) #var_table#sum(var_table)
    ub <- ci$ub * rep(apply(var_table, 1, sum),2)#var_table#sum(var_table)
    
  }
  
  
  # Assing positions based on relative value compared to others
  var_pos <- ifelse(var_prop < 0.5 * max(var_prop),
                    3, 1)
  
  
  
  # Create color vector (first is always red if there are unknowns)
  #   if( var_table["Unknown"] > 0 ){
  #     my_colors <- c("Red", colorRampPalette(c("darkblue", "darkgreen"))(length(levels(var)) -1))
  #   } else {
  #   }
  
  if(is.null(by_var)){
    my_colors <- colorRampPalette(c("darkblue", "darkgreen"))(length(levels(var)))
    
    if(rain){
      my_colors <- rainbow(length(levels(var)))
    }
    
  }else{
    my_colors <- colorRampPalette(c("darkblue", "darkgreen"))(length(levels(by_var)))
    if(rain){
      my_colors <- rainbow(length(levels(by_var)))
    }
  }
  
  my_colors <- adjustcolor(my_colors, alpha.f = 0.6)
  
  
  # Make barplot
  if(is.null(by_var)){
    
    if(percent){
      
      bp <- barplot(var_prop,
                    ylim = c(0, max(ci$ub)*120),
                    col = my_colors,
                    border = border,
                    ylab = "Percent",
                    cex.names = cex.names,
                    las = las, 
                    beside = TRUE)
      if(legend){
        legend(x = "topright",
               fill = my_colors,
               bty = "n",
               ncol = round(length(my_colors) / 2),
               legend = levels(var),
               cex = legend.cex,
               title = legend.title)
      }
      
      # Add point estimate text
      #   text(x = bp[,1],
      #        y = var_vals,
      #        labels = paste0(round(var_prop, digits = 1), "%"),
      #        pos = var_pos)
      
      # Add error bars
      errbar(x=bp[,1],
             y=var_prop,
             yplus=ci$ub*100,
             yminus=ci$lb*100,
             add=TRUE,
             type="n",
             errbar.col=adjustcolor("darkred", alpha.f=0.6),
             lwd=2)
      
      # Add text of point estimate with c.i.'s
      text(x=bp[,1], 
           y= ifelse(var_pos == 1, 
                     var_prop - (.1*max(var_prop)),
                     var_prop + (.1*max(var_prop))),
           labels=paste0(" ", round(var_prop, digits=1),
                         "%",
                         "\n(",
                         round(ci$lb*100, digits=1),
                         "%",
                         "-",
                         round(ci$ub*100, digits=1),
                         "%",
                         ")"
           ),
           cex=err.cex,
           col=adjustcolor("black", alpha.f=0.7))
      
      abline(h = 0)
      
    }else {
      
      bp <- barplot(var_table,
                    ylim = c(0, max(ub)*1.2),
                    col = my_colors,
                    border = border,
                    ylab = "Percent",
                    cex.names = cex.names,
                    las = las, 
                    beside = TRUE)
      
      if(legend){
        legend(x = "topright",
               fill = my_colors,
               bty = "n",
               ncol = round(length(my_colors) / 2),
               legend = levels(var),
               cex = legend.cex,
               title = legend.title)
      }
      # Add error bars
      errbar(x=bp[,1],
             y=var_table,
             yplus=ub,
             yminus=lb,
             add=TRUE,
             type="n",
             errbar.col=adjustcolor("darkred", alpha.f=0.6),
             lwd=2)
      
      # Add text of point estimate with c.i.'s
      text(x=bp[,1], 
           y= ifelse(var_pos == 1, 
                     var_table - (.1*max(var_table)),
                     var_table + (.1*max(var_table))),
           labels=paste0(" ", round(var_prop, digits=1),
                         "%",
                         "\n(",
                         round(ci$lb*100, digits=1),
                         "%",
                         "-",
                         round(ci$ub*100, digits=1),
                         "%",
                         ")"
           ),
           cex=err.cex,
           col=adjustcolor("black", alpha.f=0.7))
      
      abline(h = 0)
      
    }
    
    # begin by_var
    
  } else{
    
    if(percent){
      bp <- barplot(var_prop,
                    ylim = c(0, max(ci$ub)*120),
                    col = my_colors,
                    border = border,
                    ylab = "Percent",
                    cex.names = cex.names,
                    las = las,
                    beside = TRUE)
      
      if(legend){
        legend(x = "topright",
               fill = my_colors,
               bty = "n",
               ncol = round(length(my_colors) / 2),
               legend = levels(by_var),
               cex = legend.cex,
               title = legend.title)
      }
      
      # Add error bars
      errbar(x=bp,
             y=var_prop,
             yplus=ci$ub*100,
             yminus=ci$lb*100,
             add=TRUE,
             type="n",
             errbar.col=adjustcolor("darkred", alpha.f=0.6),
             lwd=2)
      
      # Add text of point estimate with c.i.'s
      text(x=bp, 
           y= ifelse(var_pos == 1, 
                     var_prop - (.1*max(var_prop)),
                     var_prop + (.1*max(var_prop))),
           labels=paste0(" ", round(var_prop, digits=1),
                         "%",
                         "\n(",
                         round(ci$lb*100, digits=1),
                         "%",
                         "-",
                         round(ci$ub*100, digits=1),
                         "%",
                         ")"
           ),
           cex=err.cex,
           col=adjustcolor("black", alpha.f=0.7))
      
      abline(h = 0)
      
    } else{
      
      bp <- barplot(var_table,
                    ylim = c(0, max(ub)*1.2),
                    col = my_colors,
                    border = border,
                    ylab = "Percent",
                    cex.names = cex.names,
                    las = las,
                    beside = TRUE)
      
      if(legend){
        legend(x = "topright",
               fill = my_colors,
               bty = "n",
               ncol = round(length(my_colors) / 2),
               legend = levels(by_var),
               cex = legend.cex,
               title = legend.title)
      }
      
      # Add error bars
      errbar(x=bp,
             y=var_table,
             yplus=ub,
             yminus=lb,
             add=TRUE,
             type="n",
             errbar.col=adjustcolor("darkred", alpha.f=0.6),
             lwd=2)
      
      # Add text of point estimate with c.i.'s
      text(x=bp, 
           y= ifelse(var_pos == 1, 
                     var_table - (.1*max(var_table)),
                     var_table + (.1*max(var_table))),
           labels=paste0(" ", round(var_prop, digits=1),
                         "%",
                         "\n(",
                         round(ci$lb*100, digits=1),
                         "%",
                         "-",
                         round(ci$ub*100, digits=1),
                         "%",
                         ")"
           ),
           cex=err.cex,
           col=adjustcolor("black", alpha.f=0.7))
      
      abline(h = 0)
    }
  } 
}

# ###############
# # GEOCODING
# ###############
# 
# # Having already geocoded, just read in locations
# locations <- read.csv("locations.csv")
# 
# # merge with comb
# comb <- cbind(comb, locations)
# 
# #THE BELOW ONLY NEEDS TO BE RUN ONCE:
# places <- paste0(comb$Name.of.Employer,
#                 ", ", 
#                 comb$County.Reporting,
#                 " ", "county",
#                 ", ",
#                 "Florida")
# 
# library(RCurl)
# library(RJSONIO)
# library(plyr)
# 
# url <- function(address, return.call = "json", sensor = "false") {
#   root <- "http://maps.google.com/maps/api/geocode/"
#   u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
#   return(URLencode(u))
# }
# 
# geoCode <- function(address,verbose=FALSE) {
#   if(verbose) cat(address,"\n")
#   u <- url(address)
#   doc <- getURL(u)
#   x <- fromJSON(doc,simplify = FALSE)
#   if(x$status=="OK") {
#     lat <- x$results[[1]]$geometry$location$lat
#     lng <- x$results[[1]]$geometry$location$lng
#     location_type <- x$results[[1]]$geometry$location_type
#     formatted_address <- x$results[[1]]$formatted_address
#     return(c(lat, lng, location_type, formatted_address))
#   } else {
#     return(c(NA,NA,NA, NA))
#   }
# }
# 
# 
# locations <- ldply(places, function(x) geoCode(x))
# names(locations) <- c("lat","lon","location_type", "forAddress")
# # 
# # #write.csv(locations, "locations.csv")
# 
# ###############
# # MAP FUNCTION
# ###############
# par(mfrow = c(1,1))
# par(mar=c(1,1,1,1))
# library(maps)
# mymap <- map("county", "fl",
#              fill = TRUE,
#              col = "grey",
#              border = "darkgrey")
# 
# size_col <- ifelse(comb[,5] == 1, "blue", 
#                    ifelse(comb[,5] == 2, "red",
#                           "darkgreen"))
# size_pch <- ifelse(comb[,5] == 1, 16,
#                    ifelse(comb[,5] == 2, 15,
#                           17))
# 
# # points(comb$lon, comb$lat, col = adjustcolor("black", alpha.f = 0.6),
# #        cex = 0.5)
# points(comb$lon, comb$lat, 
#        col = adjustcolor(size_col, alpha.f = 0.2),
#        pch = size_pch,
#        cex = 0.5)
# 
# legend(x = "left",
#        pch = c(16,15,17),
#        col = adjustcolor(c("blue", "red", "darkgreen"), alpha.f = 0.4),
#        legend = c("Small (<50)", "Large (50+)", "Unknown"),
#        bty = "n",
#        cex = 0.75,
#        title = "Business size")
# 
# 
# 


# 
# ########## MAP STUFF
library(rgdal)
# Make geographic version of comb
# comb_geo <- comb[which(!is.na(comb$lon) &
#                          !is.na(comb$lat)),]
# coordinates(comb_geo) <- ~ lon + lat

# Clean up county column
comb$County <- Recode(comb$County,
                          " 'Alachua ' = 'Alachua';
                          'Baker ' = 'Baker';
                          'Bay ' = 'Bay';
                          'Bradford ' = 'Bradford';
                          'Charlotte County Public Schools' = 'Charlotte';
                          'Desoto' = 'DeSoto';
                          'Dixe' = 'Dixie';
                          'Escambia ' = 'Escambia';
                          'Gulf ' = 'Gulf';
                          'Hernando ' = 'Hernando';
                          'Leon ' = 'Leon';
                          'Levy ' = 'Levy';
                          'Liberty ' = 'Liberty';
                          'Orange ' = 'Orange';
                          'Osceola ' = 'Osceola';
                          'Pasco ' = 'Pasco';
                          'Pinellas ' = 'Pinellas';
                          'Saint Johns' = 'St. Johns';
                          'Saint Lucie' = 'St. Lucie';
                          'Wakulla ' = 'Wakulla'")

# Read in map
library(rgdal)
fl <- readOGR("FCTY2", "FCTY2")
fl$County <- fl$NAME

# ##########
# # MAP FUNCTION
# ##########
library(RColorBrewer)
library(classInt)
MapFun <- function(var, color = "Blues", percent = TRUE,
                   legend_title = NULL){
  plotvar <- var
  nclr <- 5
  plotclr <- brewer.pal(nclr, color)
  #class <- classIntervals(plotvar, nclr, style = "quantile", dataPrecision=0) #use "equal" instead
  #class <- classIntervals(0:100, nclr, style="equal")
  class <- classIntervals(plotvar, style = "equal", dataPrecision=0)
  colcode <- findColours(class, plotclr)
  if(percent){
    legcode <- paste0(gsub(",", " - ", gsub("[[]|[]]|[)]", "", names(attr(colcode, "table")))), "%")
  } else {
    legcode <- paste0(gsub(",", " - ", gsub("[[]|[]]|[)]", "", names(attr(colcode, "table")))))
    
  }
  
  plot(fl, border="darkgrey", col=colcode)
  
  if(is.null(legend_title)){mytitle <- NA}else{mytitle <- legend_title}
  legend("left", # position
         legend = legcode, #names(attr(colcode, "table")), 
         fill = attr(colcode, "palette"), 
         cex = 1.0, 
         border=NA,
         bty = "n",
         title = mytitle)
}


# Define titles
map_titles <- c(
  "County",
  "Interview date" ,
  "Employer" ,
  "Number of employees" ,
  "Large business (50+)?" ,
  "Number of tobacco users" ,
  "Does employer hire tobacco users?" ,
  "Does employer provide health insurance?" ,
  "Type of insurance" ,
  "Insurance carrier" ,
  "Employer sector" ,
  "Any tobacco cessation coverage?" ,
  "Patch NRT OTC covered?" ,
  "Gum NRT OTC covered?" ,
  "Lozenge NRT OTC covered?" ,
  "OTC covered only?" ,
  "Inhaler NRT Rx covered?" ,
  "Nose spray Rx covered?" ,
  "Bupropion Rx covered?" ,
  "varenicline Rx covered?" ,
  "Covered Rx only?" ,
  "Covered both OTC and Rx?" ,
  "Individual counseling covered?" ,
  "Phone counseling covered?" ,
  "Group counseling covered?" ,
  "Web based counseling covered?" ,
  "Covered some types of counseling?" ,
  "Covered all types of counseling?" ,
  "Covered at least 2 quit attempts per year?" ,
  "Combination counseling and meds covered?" ,
  "Any barriers to coverage of meds?" ,
  "Extent of TFG policy coverage",
  "E-cigs restricted by policy" ,
  "Sliver FTCA" ,
  "Gold FTCA" 
)

# Define which ones can be easily plotted
factor_vars <- c(5, 7, 8, 12:31, 33:35)


# APP-SPECIFIC PLOTTING FUNCTION
ChoroFun <- function(var_index){
  
  if(var_index == 5){
    comb[,var_index] <- Recode(comb[,var_index],
                               "'Large' = 'Yes';
                               'Small' = 'No'")
  }
  
  # Yes/No or big/small variables
  if(var_index %in% factor_vars){
    
    
#     # Get overall distribution and most prevalent name
#     temp_table <- table(comb[,var_index])
#     #temp_table <- rev(sort(temp_table)) # this would screw up since others aren't sorted
#     temp_name <- names(temp_table)[1]
#     
#     # Create a list in order to populate with values for each county
#     myvar <- list()
#     
#     # Loop through each county getting the distribution
#     for (i in unique(fl$County)){
#       x <- 100*prop.table(table(comb[which(comb$County == i),var_index]))
#       myvar[[i]] <- ceiling(as.numeric(x[1]))
#       #myvar
#     }
#     
#     # Put list into vector mode
#     mapvar <- as.numeric(unlist(myvar))
#     
    
    # Create a list in order to populate with values for each county
    myvar <- list()
    
    # Loop through each county getting the distribution
    for (i in unique(fl$County)){
      x <- 100*prop.table(table(comb[which(comb$County == i),var_index]))
      if(is.na(x["Yes"])){
        y <- 0
      } else{
        y <- x["Yes"]
      }
      myvar[[i]] <- ceiling(as.numeric(y))
      #myvar
    }
    
    # Put list into vector mode
    mapvar <- as.numeric(unlist(myvar))
        
    # Plot
    MapFun(var = mapvar, color = "Blues", percent = FALSE,
           legend_title = paste0("% Yes"))
    title(map_titles[var_index])
    
  } else { # FOR ALL OTHER VARIABLES
    plot(fl,  col = adjustcolor("yellow", alpha.f =0.4), 
         border = adjustcolor("black", alpha.f=0.2),
         proj = "aitoff")

    text(x = -83, y = 29, cex = 3, labels = "Cannot map multi-level\n categorical data, dude.")
    
  }
  
}




