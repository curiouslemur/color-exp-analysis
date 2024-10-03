### NOTE ### 
# In the paper, the proposed fit as in the paper is: sd = 1.4*xbar*(1 - xbar)
# where xbar is the mean association across all people for this color and concept
###### Fitting function for the sd. Model s = a.xbar.(1-xbar)
####======== function to get the value of the coeff a
getFitSd <- function(data){
  d1 <- data[1,]; a_start = d1$sd_sample / (d1$xbar * (1 - d1$xbar))
  fit <- nls( sd_sample ~ a * xbar * (1-xbar), data = data, start = list(a = a_start))
  return(fit)
}

####======== function to get the values for xbars, sd_samples, and sd_fits for each pair of concepts and colors
getXvalues <- function(data, concept_pairs, color_pairs, sd_fit_coef){
  res <- data.frame()
  # Loop through all concept pairs and color pairs to populate the result dataframe
  for (i in 1:nrow(concept_pairs)) {
    for (j in 1:nrow(color_pairs)) {
      concept1 <- concept_pairs[i, 1]; 
      concept2 <- concept_pairs[i, 2]
      color1 <- color_pairs[j, 1]; color2 <- color_pairs[j, 2]
      
      # Combine into a new row
      new_row <- data.frame(
        concept1 = concept1, 
        concept2 = concept2,
        color1 = color1, 
        color2 = color2,
        xbar1 = data$xbar[data$concept == concept1 & data$color == color1],
        xbar2 = data$xbar[data$concept == concept1 & data$color == color2],
        xbar3 = data$xbar[data$concept == concept2 & data$color == color1],
        xbar4 = data$xbar[data$concept == concept2 & data$color == color2], 
        
        sd_sample1 = data$sd_sample[data$concept == concept1 & data$color == color1],
        sd_sample2 = data$sd_sample[data$concept == concept1 & data$color == color2],
        sd_sample3 = data$sd_sample[data$concept == concept2 & data$color == color1],
        sd_sample4 = data$sd_sample[data$concept == concept2 & data$color == color2]
      )
      
      new_row <- mutate(new_row, 
                        sd_fit1 = sd_fit_coef * xbar1 * (1 - xbar1),
                        sd_fit2 = sd_fit_coef * xbar2 * (1 - xbar2),
                        sd_fit3 = sd_fit_coef * xbar3 * (1 - xbar3), 
                        sd_fit4 = sd_fit_coef * xbar4 * (1 - xbar4))
      
      # Add the new row to the result dataframe
      res <- rbind(res, new_row)
    }
  }
  return(res)
}

####======== function to get values of deltaS (either using sd_sample or fitted sd)
getDeltaS <- function(data){
  data <- mutate(data, 
                 ProbDelta_sample = pnorm( deltaX / sqrt(sd_sample1^2 + sd_sample2^2 + sd_sample3^2 + sd_sample4^2)),  
                 ProbDelta_fit = pnorm( deltaX / sqrt(sd_fit1^2 + sd_fit2^2 + sd_fit3^2 + sd_fit4^2)))  
  data <- mutate(data, 
                 deltaS_sample = abs(2 * ProbDelta_sample - 1),
                 deltaS_fit = abs(2 * ProbDelta_fit - 1))
  return(data)
}