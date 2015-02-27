#####
# MAKE FAKE DATA
#####
df <- data.frame(
  x1 = (1:1000) * rnorm(n = 1000, mean = 500, sd = 200),
  sex = c(sample(c('male', 'female', 'female'), 500, replace = TRUE),
          sample(c('male', 'male', 'female'), 500, replace = TRUE)),
  y = seq(0,100, length = 1000))

# Look at it
#head(df)


change_female <- function(data = df, p_female = 50){
  
  female_inds <- which(data$sex == 'female')
  females <- data[sample(female_inds, round(p_female*0.01*nrow(data)),
                         replace = TRUE),]
  
  male_inds <- which(data$sex == 'male')
  males <- data[sample(male_inds, round((100-p_female)*
                                          0.01*nrow(data)),
                       replace = TRUE),]
  
  return(rbind(males, females))
}

#####
# MAKE SIMPLE LINEAR MODEL TO PREDICT Y
#####
fit <- lm(y ~ x1 + sex, 
          data = df)

# GET 50 FEMALES
df <- change_female(p_female = 50)



#####
# FUNCTION TO GET PREDICTED PORTFOLIO VALUE
# VIA BOOTSRAPPING
#####
portfolio_value <- function(data = df, model = fit, n = 1000){
  # Predict on each data point
  predicted <- predict(model, data)
  # Create empty matrix for bootstraps
  magic_number <- round(0.63 * n)
  mat <- matrix(nrow = nrow(data),
                ncol = magic_number)
  # Bootstrap sample with replacement from original data
  for (i in 1:nrow(data)){
    new_data <- sample(predicted, magic_number, replace = TRUE)
    mat[i,] <- new_data
  }
  vals <- apply(mat, 2, sum)
  return(vals)
}

#####
# BOOTSTRAPPED PORTFOLIO VALUE DISTRIBUTION
# WITH NO CHANGES TO THE ORIGINAL DATA
#####
# x <- portfolio_value(data = df, model = fit, n = 1000)
# 
# # Original distribution
# hist(x, freq = FALSE, breaks = 50, col = "grey", border = NA,
#      xlim = c(43000, 57000),
#      main = "Possible bootsrapped portfolio values",
#      xlab = "Dollars")
# 
# # Bootstrapped distributions at varying number of bootstraps
# ns <- round(seq(50,1000, length = 20))
# cols <- colorRampPalette(c('blue', 'lightblue'))(100)
# for (i in 1:length(ns)){
#   x <- portfolio_value(data = df, model = fit, n = ns[i])
#   lines(density(x), col = adjustcolor(cols[i], alpha.f = 0.5))
#   Sys.sleep(0.1)
# }
# # Note: distribution remains unchanged by n bootstraps
# 
# # Add 2.5 and 97.5 lines
# abline( v= quantile(x, probs = c(0.025, 0.975)),
#        col = adjustcolor("darkred", alpha.f = 0.4), lwd = 3)
# 
# #####
# # NOW CHANGE PERCENT FEMALE
# #####
# # Current distribution in data
# table(df$sex)

# 
# # Original distribution
# table(df$sex)
# 
# #####
# # Now increase percent female
# #####
# new_df <- change_female(data = df, p_female = 70)
# 
# # new distribution
# table(new_df$sex)
# 
# ####
# # PREDICT AND RESAMPLE ON *NEW* DATA
# # USING ORIGINAL LINEAR MODEL
# #####
# ns <- round(seq(50,1000, length = 20))
# cols <- colorRampPalette(c('red', 'yellow'))(20)
# for (i in 1:length(ns)){
#   x <- portfolio_value(data = new_df, model = fit, n = ns[i])
#   lines(density(x), col = adjustcolor(cols[i], alpha.f = 0.5))
#   Sys.sleep(0.5)
# }
# 
# #####
# # Now DECREASE percent female
# #####
# new_df <- change_female(data = df, p_female = 30)
# 
# # new distribution
# table(new_df$sex)
# 
# # Resample on new data, using original model
# ns <- round(seq(50,1000, length = 20))
# cols <- colorRampPalette(c('darkgreen', 'green'))(20)
# for (i in 1:length(ns)){
#   x <- portfolio_value(data = new_df, model = fit, n = ns[i])
#   lines(density(x), col = adjustcolor(cols[i], alpha.f = 0.5))
#   Sys.sleep(0.5)
# }
