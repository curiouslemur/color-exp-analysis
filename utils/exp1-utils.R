library(tidyverse)
library(dplyr)
# library(RColorBrewer)
library(readr)
# library(tidytext)
library(lme4)
library(pwr)
library(gridExtra)
library(grid)

# install.packages("pacman")

pacman::p_load(tidyverse,
               dplyr,
               RColorBrewer,
               reader,
               tidytext)

theme_studyBackground <- function(){
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = backgroundColor), 
        panel.grid.major = element_line(size = 0.5, linetype = 'dotted', colour = "grey"),
        panel.grid.minor = element_line(size = 0.5, linetype = 'dotted', colour = "#79797a")) 
}

#### ---------- Get the correlation matrix for each pair of concept within the same category
##### Params: data: dataframe: | category | concept | code | mean.rating |
within_cat_corr <- function (df_cat, category){
  # category = "emotion"
  df_cat_ <- df_cat %>% filter(cat == category) 
  df_cat_conc <- unique(df_cat_$concept)
  
  ## building pivoted dataframe | cat | code | angry | happy | love | sad |
  df_cat_ <- df_cat_ %>% 
    pivot_wider(id_cols = c("cat", "code"), names_from = "concept", values_from = "mean_rating")
  
  #### calculating pairwise correlations within a category
  df_cat_ <- df_cat_ %>% select(all_of(df_cat_conc))
  num_cols <- ncol(df_cat_)
  df_cat_ <- as.data.frame(df_cat_)
  # Create an empty matrix to store p-values
  p_values <- matrix(NA, nrow = num_cols, ncol = num_cols)
  
  # Loop through each pair of columns
  for (i in 1:(num_cols - 1)) {
    for (j in (i + 1):num_cols) {
      # Calculate correlation test
      correlation_test <- cor.test(df_cat_[, i], df_cat_[, j])
      
      # Store the p-value
      p_values[i, j] <- correlation_test$p.value
      p_values[j, i] <- correlation_test$p.value
    }
  }
  
  # Apply Bonferroni correction
  adjusted_p_values <- p.adjust(as.vector(p_values), method = "bonferroni")
  
  # Reshape the adjusted p-values to a matrix
  adjusted_p_matrix <- matrix(adjusted_p_values, nrow = num_cols, ncol = num_cols)
  
  # Print the correlation matrix and adjusted p-values
  print("Correlation Matrix:")
  print(cor(df_cat_))
  
  print("----------------------------------")
  
  print("Adjusted P-values (Bonferroni Correction):")
  print(adjusted_p_matrix)
}


#### ---------- Comparing the differences between the means of each groups across concepts and colors (t-test at conf.level = 0.95)
##### Params -- data: dataframe with at least these col: color, ans, concept, country, rating
#####        -- conceptListEn: List of concepts to use for facet_wrap

ci_t.test <- function (data, conceptListEn, title ) {
  allDf <- data 
  
  ## pivot df_weights to separate weight for each country into two columns
  ## Note: This might not be the right model since the design is mixed-design anova. 
  
  df_pivot <- pivot_wider(
    data = allDf,
    id_cols = c("concept", "color"),
    names_from = "country",
    values_from = "rating",
    values_fn = list # adding this line to address duplicated values / rows
  ) 
  
  # here in t.test(x,y): x are scores for MDG and y are scores for USA
  t.res <- df_pivot %>% group_by(concept, color) %>% 
    summarise(t = round(t.test(as.vector(unlist(mdg)), as.vector(unlist(usa)))$statistic, digits = 4),
              df = round(t.test(as.vector(unlist(mdg)), as.vector(unlist(usa)))$parameter, digits = 4),
              p.value = round(t.test(as.vector(unlist(mdg)), as.vector(unlist(usa)))$p.value, digits = 4),
              lci = round(t.test(as.vector(unlist(mdg)), as.vector(unlist(usa)))$conf.int[1], digits = 4),
              uci = round(t.test(as.vector(unlist(mdg)), as.vector(unlist(usa)))$conf.int[2], digits = 4),
              meanMdg = round(t.test(as.vector(unlist(mdg)), as.vector(unlist(usa)))$estimate[1], digits = 4),
              meanUsa = round(t.test(as.vector(unlist(mdg)), as.vector(unlist(usa)))$estimate[2], digits = 4),
              .groups = 'drop')
  
  t.res <- full_join(t.res, bcp37hex, by = 'color')
  t.res$meanDiff <- t.res$meanMdg - t.res$meanUsa
  # t.test(as.vector(unlist(df_pivot$mdg[1])), as.vector(unlist(df_pivot$usa[1])))
  # a <- apply(df_pivot[,3:4], c(1,2), function(x){as.vector(unlist(x))})
  
  ## Concepts and codes for which the t.test results are significant
  t.res.sig <- filter(t.res, p.value <= 0.05)
  
  ## Plotting
  t.res$color = fct_rev(factor(t.res$color, levels = color_codes))
  t.res <- t.res %>% mutate(ciColor = if_else(p.value <= 0.05, hex, '#ffffff55'))
  
  
  r <- t.res %>% 
    # filter(p.value <= 0.05) %>%
    ggplot(aes(color, meanDiff, color = ciColor)) +
    # ggplot(aes(color, meanDiff, color = hex)) + 
    
    geom_pointrange(aes(ymin = lci, ymax = uci)) +
    geom_hline(yintercept = 0, color = 'gray') +
    scale_color_identity() +
    scale_y_continuous(limits = c(-1.5,1.5)) +
    coord_flip() + 
    facet_wrap(.~factor(concept, levels=conceptListEn)) + 
    theme(
      # panel.background = element_rect(fill = "#e0dede"),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 10),
      strip.text = element_text(size = 16))+
    labs(title =paste("95% CI for the difference between the mean for MDG - USA", title, sep = ': '), y = "", x = "color code in BCP37 library"); r
}


#### ---------- Function to translate concept names from French into English
##### Params --
####
translateConcepts <- function(df, concept_fr, concept_en){
  df$concept <- replace(df$conceptFr, 
                        df$conceptFr %in% concept_fr, 
                        concept_en[match(df$conceptFr[df$conceptFr %in% concept_fr], concept_fr)])
  return(df)
}

#### ----------- Function to plot the association weights (sorted)
plotWeight_Err <- function(data, title){
  data$con = factor(data$concept, levels=conceptListEn3) # to order the facet strips 
  p1 <- data %>% 
    # filter(concept %in% c("mango", "tree", "angry", "happy", "death")) %>%
    ggplot(aes(reorder_within(color, weight, concept), weight, fill = hex, color = barStroke)) + 
    geom_bar(stat = 'identity',size = 0.15, width = 0.80) + 
    geom_errorbar( aes(x=reorder_within(color, weight, concept), ymin=weight-se, ymax=weight+se), width=0.4, colour="black", alpha=0.6, size=0.25)+
    scale_fill_identity() + scale_color_identity() +
    scale_x_reordered() +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
    facet_wrap(~ con, scales = "free_x", ncol = 1) +
    labs(x = "", title = title) + 
    theme_light() +
    theme(legend.position = "none",
          strip.text = element_text(size = 12, color = "black"), 
          strip.background = element_blank()); 
  return(p1)
}


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



