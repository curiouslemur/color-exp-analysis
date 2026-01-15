library(readr)
library(dplyr)
library(tidyr)
library(purrr)

source(file = "./utils/stimuli.R")
source(file = "./utils/exp1-analysis-utils.R")

alpha_mg = 1.54; alpha_us = 1.38

# loading datasets
dataPath <- "data/exp1/csv/2026/"
mgdem <- read_csv(paste(dataPath, "mg-dem.csv", sep = "", show_col_types = FALSE))
mgdf <- read_csv(paste(dataPath, "mg-df.csv", sep = "", show_col_types = FALSE))
mgdf_w <- read_csv(paste(dataPath, "mg-df-summary.csv", sep = "", show_col_types = FALSE))

usdem <- read_csv(paste(dataPath, "us-dem.csv", sep = "", show_col_types = FALSE))
usdf <- read_csv(paste(dataPath, "us-df.csv", sep = "", show_col_types = FALSE))
usdf_w <- read_csv(paste(dataPath, "us-df-summary.csv", sep = "", show_col_types = FALSE)); rm(dataPath)

# Compute landscapes from the summary data that have weight (mean_rating) ---
## note: alpha_fit is set to 1.4 in schloss's paper
## alpha_mg and alpha_us are calculated in exp1-analysis.Rmd
mg_landscape <- make_pairwise_landscape(mgdf_w, alpha_fit = alpha_mg)
us_landscape <- make_pairwise_landscape(usdf_w, alpha_fit = alpha_us)
# head(mg_landscape, 10)

# Save outputs: pairwise_sem_dis == pairwise_semantic_discriminability 
write_csv(mg_landscape, "output/mg_pairwise_sem_dis_alpha_mg.csv")
write_csv(us_landscape, "output/us_pairwise_sem_dis_alph_us.csv")

#### exporting data necessary for visualization
mg_landscape %>% select(country, concept_a, concept_b, color_1, color_2, semantic_distance) %>% 
  write_csv("output/mg_pairwise_sem_dis_alpha_mg_vis.csv")
us_landscape %>% select(country, concept_a, concept_b, color_1, color_2, semantic_distance) %>% 
  write_csv("output/us_pairwise_sem_dis_alpha_us_vis.csv")
