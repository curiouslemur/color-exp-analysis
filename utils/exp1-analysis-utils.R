### NOTE ###
# In the paper schloss2021semantic, the proposed fit for standard deviation is: sd = 1.4*xbar*(1 - xbar)
# where xbar is the mean association across all people for each {color and concept} pair
###### Fitting function for the sd. Model s = a.xbar.(1-xbar)
#### ======== function to get the value of the coeff a
getFitSd <- function(data) {
  d1 <- data[1, ]
  a_start <- d1$sd_sample / (d1$xbar * (1 - d1$xbar))
  fit <- nls(sd_sample ~ a * xbar * (1 - xbar), data = data, start = list(a = a_start))
  return(fit)
}

#### using the sd = a * xbar ^ b model for fitting the standard deviation
##### returns alpha a and beta b values
getFitSd2 <- function(data) {
  fit <- nls(sd_sample ~ a * xbar^b, data = data, start = list(a = 1, b = 1))
  return(fit)
}

#### ======== function to get the values for xbars, sd_samples, and sd_fits for each pair of concepts and colors
# the resulting dataframe is a representation of the bigram in the paper
getXvalues <- function(data, concept_pairs, color_pairs, sd_fit_coef){
  res <- data.frame()
  # Loop through all concept pairs and color pairs to populate the res (for result) dataframe
  for (i in 1:nrow(concept_pairs)) {
    for (j in 1:nrow(color_pairs)) {
      concept1 <- concept_pairs[i, 1]; concept2 <- concept_pairs[i, 2]
      color1 <- color_pairs[j, 1]; color2 <- color_pairs[j, 2]
  
        # Combine into a new row
        new_row <- data.frame(
          concept1 = concept1, concept2 = concept2,
          color1 = color1, color2 = color2,
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
      # print(nrow(res))
    }
  }
  return(res)
}

## !!! This is by far a more optimized version of the above function
getXvalues_opt <- function(data, concept_pairs, color_pairs, sd_fit_coef){
  df1 <- merge(concept_pairs, color_pairs)
  # lookup_df = data # %>% select(concept, color, xbar)
  
  extended_DF <- df1 %>%
    left_join(data, by = c("concept1" = "concept", "color1" = "color")) %>%
    rename(xbar1 = xbar, sd_sample1 = sd_sample) %>%
    left_join(data, by = c("concept1" = "concept", "color2" = "color")) %>%
    rename(xbar2 = xbar, sd_sample2 = sd_sample) %>%
    left_join(data, by = c("concept2" = "concept", "color1" = "color")) %>%
    rename(xbar3 = xbar, sd_sample3 = sd_sample) %>%
    left_join(data, by = c("concept2" = "concept", "color2" = "color")) %>%
    rename(xbar4 = xbar, sd_sample4 = sd_sample)
  
  x_opt <- mutate(extended_DF, 
                  sd_fit1 = sd_fit_coef * xbar1 * (1 - xbar1),
                  sd_fit2 = sd_fit_coef * xbar2 * (1 - xbar2),
                  sd_fit3 = sd_fit_coef * xbar3 * (1 - xbar3), 
                  sd_fit4 = sd_fit_coef * xbar4 * (1 - xbar4)) %>% 
    mutate(deltaX = (xbar1 + xbar4) - (xbar2 + xbar3))
  
  return(x_opt)
}

#### ======== function to get values of deltaS (either using sd_sample or fitted sd)
getDeltaS <- function(data) {
  data <- mutate(data,
    ProbDelta_sample = pnorm(deltaX / sqrt(sd_sample1^2 + sd_sample2^2 + sd_sample3^2 + sd_sample4^2)),
    ProbDelta_fit = pnorm(deltaX / sqrt(sd_fit1^2 + sd_fit2^2 + sd_fit3^2 + sd_fit4^2))
  )
  data <- mutate(data,
    deltaS_sample = abs(2 * ProbDelta_sample - 1),
    deltaS_fit = abs(2 * ProbDelta_fit - 1)
  )
  return(data)
}

