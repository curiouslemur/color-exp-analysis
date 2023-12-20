# library(tidyverse)
# library(dplyr)
# library(RColorBrewer)
# library(readr)
# library(tidytext)

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
