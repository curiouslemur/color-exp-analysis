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

##======================================================================

# Computing landscapes from the summary data that have weight (mean_rating) ---
## note: alpha_fit is set to 1.4 in schloss's paper
## alpha_mg and alpha_us are calculated in exp1-analysis.Rmd
mg_landscape <- make_pairwise_landscape(mgdf_w, alpha_fit = alpha_mg)
us_landscape <- make_pairwise_landscape(usdf_w, alpha_fit = alpha_us)
# head(mg_landscape, 10)

# Save outputs: pairwise_sem_dis == pairwise_semantic_discriminability 
write_csv(mg_landscape, "output/mg_pairwise_sem_dis_alpha_mg.csv")
write_csv(us_landscape, "output/us_pairwise_sem_dis_alpha_us.csv")

#### exporting data necessary for heatmap vis
mg_landscape %>% select(country, concept_a, concept_b, color_1, color_2, mu_D, semantic_distance) %>% 
  write_csv("output/mg_pairwise_sem_dis_alpha_mg_vis.csv")
us_landscape %>% select(country, concept_a, concept_b, color_1, color_2, mu_D, semantic_distance) %>% 
  write_csv("output/us_pairwise_sem_dis_alpha_us_vis.csv")

##======================================================================
## To categorize a DelatS as low or high, we need a threshold tL for low, and tH for high.
## in our case, we use the values at 0.25 quantile for tL, and 0.75 quantile for tH
## another option would be to hardcode the thresholds

##======================================================================
## Populating the four categories in the quadrants will help identify the colors and concepts that can be used 
## in follow-up experiments. 
## We propose the following ways below

# names of the four categories
CAT_NAMES <- c("MG_high__US_low", "MG_high__US_high", "MG_low__US_low", "MG_low__US_high")

# mg_landscape <- read_csv("output/mg_pairwise_sem_dis_alpha_mg.csv", show_col_types = FALSE)
# us_landscape <- read_csv("output/us_pairwise_sem_dis_alpha_us.csv", show_col_types = FALSE)

## take tagged df out of pick function
x <- load_and_join_tagged(mg_deltaS, us_deltaS, thresholds, low_q, high_q)
write_csv(x$tagged, "output/joined_n_tagged_quadrant.csv")

# 1) Pick one random 2x2 set at random one per quadrant
res_random   <- pick_random1_per_quadrant(x, low_q = 0.25, high_q = 0.75, seed = 42)
nrow(res_random$results)
View(res_random$results)
# write_csv(res_random$results, "output/res_random.csv")

# 2) Deterministic: Pick "most extreme" per quadrant
# res_extreme  <- pick_most_extreme1_per_quadrant(mg_landscape, us_landscape, low_q = 0.25, high_q = 0.75)
res_extreme  <- pick_most_extreme1_per_quadrant(x, low_q = 0.25, high_q = 0.75)
View(res_extreme$results)
# write_csv(res_extreme$results, "output/res_extreme.csv")

# 3) Return ALL qualifying color-pairs per quadrant
res_all      <- pick_all_per_quadrant(x, low_q = 0.25, high_q = 0.75)
View(res_all$results %>% filter(concept_a %in% c("banana"), concept_b %in% c("happy"), category %in% c("MG_high__US_high")))
# write_csv(res_all$results, "output/res_all.csv")

# 4) Return TOP K per quadrant (ranked by "extremeness")
res_topK     <- pick_topK_per_quadrant(x, K = 5, low_q = 0.25, high_q = 0.75)
View(res_topK$results  %>% filter(concept_a %in% c("banana"), concept_b %in% c("happy"), category %in% c("MG_high__US_high")))
# write_csv(res_topK$results, "output/res_topK.csv")
