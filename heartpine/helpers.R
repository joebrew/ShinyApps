get_september_price <- function(df){
  
  final_price <- vector(mode = "numeric",
                        length = nrow(df))
  
  # Loop through each row to get price for that day
  for (i in 1:nrow(df)){
    
    # Get number of minutes they stayed each day
    temp_minutes <- (df$time_hour[i] * 60) +  # hour
      df$time_minute[i] -                     # minute
      180                                            # minus the 3 o'clock thing
    
    # Subtract 30 more minutes for 4-6 graders (since they didn't start timing until 3:30)
    if(df$grade[i] >=4 ){
      temp_minutes <- temp_minutes - 30
    }

    # Turn to price
    price <- 0
    if(temp_minutes == 0){
      price <- 0
    } else  if(temp_minutes <= 15){
      price <- 5
    } else if(temp_minutes <= 30){
      price <- 10
    } else {
      price <- 10 + (temp_minutes - 30) 
    }
    final_price[i] <- price
  }
  
  
  # Return price
  return(final_price)
}
# ###################################

get_other_price <- function(df){
  
  final_price <- vector(mode = "numeric",
                        length = nrow(df))
  # loop through each row to get the price for that day
  for (i in 1:nrow(df)){
    price <- 0
    if(df$pre_paid[i] == "yes"){
      if(df$time_hour[i] == 4){
        price <- 5 
      }
      if(df$time_hour[i] == 5){
        price <- 12
      }
    } else{ # non-pre-paid
      if(df$time_hour[i] == 4){
        price <- 7 
      }
      if(df$time_hour[i] == 5){
        price <- 15        
      }
    }
    # Add minutes for late
    temp_minutes <- df$time_minute[i]
    late_fee <- 0
    if(temp_minutes == 0){
      late_fee <- 0
    } else  if(temp_minutes <= 15){
      late_fee <- 5
    } else if(temp_minutes <= 30){
      late_fee <- 10
    } else {
      late_fee <- 10 + (temp_minutes - 30) 
    }    
    final_price[i] <- price + late_fee
  }
  return(final_price)
}

# ###################################
get_new_price <- function(df){
  
  final_price <- vector(mode = "numeric",
                        length = nrow(df))
  # loop through each row to get the price for that day
  for (i in 1:nrow(df)){
    price <- 0
    if(df$pre_paid[i] == "yes"){
        price <- 5 
    } else{ # non-pre-paid
        price <- 7 
      }
    # Add minutes for late
    temp_minutes <- df$time_minute[i]
    late_fee <- 0
    if(temp_minutes == 0){
      late_fee <- 0
    } else  if(temp_minutes <= 15){
      late_fee <- 5
    } else if(temp_minutes <= 30){
      late_fee <- 10
    } else {
      late_fee <- 10 + (temp_minutes - 30) 
    }    
    final_price[i] <- price + late_fee
  }
  return(final_price)
}

# ###################################
# Function to plot
visualize <- function(name, data = df, show_vals = FALSE){
  data <- data[which(data$name == name),]
  plot_data <- data.frame(date = seq(as.Date("2014-09-01"),
                                     as.Date(Sys.Date()), 
                                     1))
  plot_data <- left_join(x = plot_data,
                         y = data,
                         by = "date")
  time <- plot_data$time_hour + (plot_data$time_minute/60)
  label_days <- as.numeric(format(plot_data$date, "%d"))
  label_months <- format(plot_data$date[which(label_days == 15)], "%b")
  money <- plot_data$
  
  bp <- barplot(time, col = "black",
                ylim = c(0, max(time, na.rm = T) * 1.1))
  rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = 
         "lightgrey")
  
  # Draw month lines
  if(data$grade[1] < 4){
    abline(h = 3, col = adjustcolor("red", alpha.f = 0.6))
  } else{
    abline(h = 3.5, col = adjustcolor("red", alpha.f = 0.6))
  }

  
  # Plot data
  bp <- barplot(time, col = adjustcolor("darkgreen", alpha.f = 0.6), add = T,
                border = adjustcolor("darkgreen", alpha.f = 0.6))
  axis(side = 1,
       at = bp[!is.na(time),1],
       label_days[which(!is.na(time))],
       tick= FALSE,
       line = -1,
       cex.axis = 0.85)
  axis(side = 1,
       at = bp[which(label_days == 15),1],
       labels = label_months,
       line = 0,
       tick = FALSE)
  
  # TOTALS
  title(main = paste0("Total: $", sum(data$price, na.rm = TRUE), "\n",
                      "Paid: $", paid(name), "   |   ",
                      "Due: $", sum(data$price, na.rm = TRUE) - paid(name)),
        col = adjustcolor("darkred", alpha.f = 0.7),
        cex.main = 1.4)
  
  # Add values
  if(show_vals){
    
    # dollars on each bar
    text(x =  bp[!is.na(time),1],
         y = time[which(!is.na(time))],
         labels = data$price,
         pos = 3,
         col = adjustcolor("black", alpha.f = 0.6),
         cex = 0.8)
  }
  
  #   # Draw supposed pickup time
  abline(v = bp[which(format(plot_data$date, "%d") == "01"),],
         lty = 1,
         col = adjustcolor("white", alpha.f = 0.5))


  
}