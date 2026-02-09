library(readr)
library(dplyr)
library(tidyr)
library(purrr)

source(file = "./utils/stimuli.R")
source(file = "./utils/exp1-deltaS-utils.R")

alpha_mg = 1.54; alpha_us = 1.38
alpha_both = 1.47

#----------------------------------
# R file for computing deltaS
#----------------------------------

# loading datasets
dataPath <- "data/exp1/csv/"
mgdem <- read_csv(paste(dataPath, "mg-dem.csv", sep = ""), show_col_types = FALSE)
mgdf <- read_csv(paste(dataPath, "mg-df.csv", sep = ""), show_col_types = FALSE)
mgdf_w <- read_csv(paste(dataPath, "mg-df-summary.csv", sep = ""), show_col_types = FALSE)

usdem <- read_csv(paste(dataPath, "us-dem.csv", sep = ""), show_col_types = FALSE)
usdf <- read_csv(paste(dataPath, "us-df.csv", sep = ""), show_col_types = FALSE)
usdf_w <- read_csv(paste(dataPath, "us-df-summary.csv", sep = ""), show_col_types = FALSE); rm(dataPath)

##======================================================================

# Computing landscapes from the summary data that have weight (mean_rating) ---
## note: alpha_fit is set to 1.4 in schloss's paper
## alpha_mg and alpha_us are calculated in exp1-analysis.Rmd
mg_landscape <- make_pairwise_landscape(mgdf_w, alpha_both)
us_landscape <- make_pairwise_landscape(usdf_w, alpha_both)
### !!! NOTE that DeltaS (from exp1-analysis.Rmd) and landscape dataframes 
### yield the same delta S and delta X. a win!

# Save outputs: pairwise_sem_dis == pairwise_semantic_discriminability 
# write_csv(mg_landscape, "output/mg_pairwise_sem_dis_alpha_both-signed.csv")
# write_csv(us_landscape, "output/us_pairwise_sem_dis_alpha_both-signed.csv")

#### exporting data necessary for heatmap vis
# mg_landscape %>% select(country, concept_a, concept_b, color_1, color_2, A_to_C1, A_to_C2, B_to_C1, B_to_C2, mu_D, semantic_distance) %>%
#   write_csv("docs/deltaS-vis-2026/data/mg_pairwise_sem_dis_alpha_mg_vis.csv")
# us_landscape %>% select(country, concept_a, concept_b, color_1, color_2, A_to_C1, A_to_C2, B_to_C1, B_to_C2, mu_D, semantic_distance) %>%
#   write_csv("docs/deltaS-vis-2026/data/us_pairwise_sem_dis_alpha_us_vis.csv")


#--------------------------------------------------------------------
# 2026-02-04 following zoom update 
# Code for exporting delat (landscape) file for exploratory filtering & sorting
# BEGIN >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
selCols = c('concept_a', 'concept_b', 'color_1', 'color_2', 
            'A_to_C1', 'A_to_C2', 'B_to_C1', 'B_to_C2', 'p_gt0', 
            'mu_D', 'semantic_distance')
colsNewNames = c('conA', 'conB', 'col1', 'col2', 
                 'x1', 'x2', 'x3', 'x4', 'p_gt0',
                 'dX', 'dS')

# expMg == export for MG
expMg <- mg_landscape %>% select(selCols)
names(expMg) <- paste0(colsNewNames, '_mg')
expMg <- expMg %>% rename(conA = conA_mg, conB = conB_mg, col1 = col1_mg, col2 = col2_mg)

expUs <- us_landscape %>% select(selCols)
names(expUs) <- paste0(colsNewNames, '_us')
expUs <- expUs %>% rename(conA = conA_us, conB = conB_us, col1 = col1_us, col2 = col2_us)

expBoth <- left_join(expUs, expMg, by = c('conA', 'conB', 'col1', 'col2')) %>% 
  mutate(dS_diff = round(dS_us - dS_mg, digits = 4))
# write_csv(expBoth, "output/exp-ds-mg-us-both.csv")


#---------------------------------------------------------------------
tL = 0.3; tH = 0.7

hus_lmg <- highUS_lowMG(expBoth, tL, tH); 
lus_hmg <- lowUS_highMG(expBoth, tL, tH)

conPairs_hus_lmg <- hus_lmg %>% distinct(conA, conB); conPairs_lus_hmg <- lus_hmg %>% distinct(conA, conB)
conPairs_overlap <- conPairs_lus_hmg %>% semi_join(conPairs_hus_lmg, by = c("conA", "conB"))

hus_lmg_overlap <- hus_lmg %>% semi_join(conPairs_overlap, by = c("conA", "conB"))
lus_hmg_overlap <- lus_hmg %>% semi_join(conPairs_overlap, by = c("conA", "conB"))

sheet_url <- "https://docs.google.com/spreadsheets/d/1YlpES6Sn_Uimo3ACYpuK0xZM35DmqJSIKCCZMTeE_Vg/edit?usp=sharing"

hus_lmg_overlap %>% 
  select(-c('x1_us', 'x2_us', 'x3_us', 'x4_us', 'p_gt0_us', 'x1_mg', 'x2_mg', 'x3_mg', 'x4_mg', 'p_gt0_mg')) %>%
  # filter(abs(dS_diff) >= abs(tH-tL)) %>% 
  filter(abs(dS_diff) >= 0.5) %>% 
  sheet_write(ss = sheet_url, 
              # sheet = paste0("Lus_Lmg_ovrlp-", round(max(hus_lmg_overlap$dS_mg), digits = 2),"-", round(min(hus_lmg_overlap$dS_us), digits = 2))
              sheet = paste0("hUs_lMg_ovrlp-", tL, "-", tH, "  dS>0.5")
  )
lus_hmg_overlap %>% 
  select(-c('x1_us', 'x2_us', 'x3_us', 'x4_us', 'p_gt0_us', 'x1_mg', 'x2_mg', 'x3_mg', 'x4_mg', 'p_gt0_mg')) %>%
  # filter(abs(dS_diff)>= abs(tH-tL)) %>% 
  filter(abs(dS_diff) >= 0.5) %>% 
  sheet_write(ss = sheet_url, 
            # sheet = paste0("Hus_Hmg_ovrlp-", round(max(lus_hmg_overlap$dS_us), digits = 2),"-", round(min(lus_hmg_overlap$dS_mg), digits = 3))
            sheet = paste0("lUs_hMg_ovrlp-", tL, "-", tH, "  dS>0.5")
  )
# sheet_write(lus_hmg_overlap, ss = sheet_url, sheet = paste0("lus_hmg_overlap-",tL,"-",tH))
# sheet_write(hus_lmg, ss = sheet_url, sheet = paste0("hus_lmg-",tL,"-",tH))
# END <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<



##======================================================================
## To categorize a DelatS as low or high, we need a threshold tL for low, and tH for high.
## set tL as .3 and tH as .7

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

