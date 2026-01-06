library(readr)
library(dplyr)
library(tidyr)

# ----------------------------
# Helpers
# ----------------------------

# Canonicalize concept-pair and color-pair ordering so joins are stable (alphabetically ordered names)
canon_pairs <- function(df, ds_col, ds_name) {
  df %>%
    mutate(
      concept_lo = pmin(concept_a, concept_b),
      concept_hi = pmax(concept_a, concept_b),
      color_lo   = pmin(color_1, color_2),
      color_hi   = pmax(color_1, color_2)
    ) %>%
    transmute(concept_lo, concept_hi, color_lo, color_hi, !!ds_name := .data[[ds_col]])
}

compute_thresholds_quantile <- function(mg_ds, us_ds, low_q = 0.25, high_q = 0.75) {
  list(
    mg_low  = quantile(mg_ds, low_q,  na.rm = TRUE),
    mg_high = quantile(mg_ds, high_q, na.rm = TRUE),
    us_low  = quantile(us_ds, low_q,  na.rm = TRUE),
    us_high = quantile(us_ds, high_q, na.rm = TRUE)
  )
}

tag_quadrants <- function(joined, thr) {
  joined %>%
    mutate(
      mg_level = case_when(
        deltaS_mg >= thr$mg_high ~ "high",
        deltaS_mg <= thr$mg_low  ~ "low",
        TRUE ~ "mid"
      ),
      us_level = case_when(
        deltaS_us >= thr$us_high ~ "high",
        deltaS_us <= thr$us_low  ~ "low",
        TRUE ~ "mid"
      ),
      category = case_when(
        mg_level == "high" & us_level == "low"  ~ "MG_high__US_low",
        mg_level == "high" & us_level == "high" ~ "MG_high__US_high",
        mg_level == "low"  & us_level == "low"  ~ "MG_low__US_low",
        mg_level == "low"  & us_level == "high" ~ "MG_low__US_high",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(category))
}

add_extremeness <- function(df) {
  df %>%
    mutate(
      extremeness = case_when(
        category == "MG_high__US_low"  ~ (deltaS_mg - deltaS_us),
        category == "MG_low__US_high"  ~ (deltaS_us - deltaS_mg),
        category == "MG_high__US_high" ~ (deltaS_mg + deltaS_us),
        category == "MG_low__US_low"   ~ -(deltaS_mg + deltaS_us),
        TRUE ~ NA_real_
      )
    )
}

load_and_join_tagged <- function(mg_path, us_path, thresholds = NULL, low_q = 0.25, high_q = 0.75) {
  mg_raw <- read_csv(mg_path, show_col_types = FALSE)
  us_raw <- read_csv(us_path, show_col_types = FALSE)
  
  mg <- canon_pairs(mg_raw, ds_col = "deltaS", ds_name = "deltaS_mg")
  us <- canon_pairs(us_raw, ds_col = "deltaS", ds_name = "deltaS_us")
  
  if (is.null(thresholds)) {
    thresholds <- compute_thresholds_quantile(mg$deltaS_mg, us$deltaS_us, low_q = low_q, high_q = high_q)
  }
  
  joined <- mg %>% inner_join(us, by = c("concept_lo","concept_hi","color_lo","color_hi"))
  
  tagged <- joined %>% tag_quadrants(thresholds)
  
  list(tagged = tagged, thresholds = thresholds)
}

DESIRED_CATS <- c("MG_high__US_low", "MG_high__US_high", "MG_low__US_low", "MG_low__US_high")

# ----------------------------
# 1) Random one per quadrant
# ----------------------------
pick_random1_per_quadrant <- function(mg_path, us_path,
                                      thresholds = NULL, low_q = 0.25, high_q = 0.75, seed = 1) {
  set.seed(seed)
  x <- load_and_join_tagged(mg_path, us_path, thresholds, low_q, high_q)
  
  picked <- x$tagged %>%
    group_by(concept_lo, concept_hi, category) %>%
    slice_sample(n = 1) %>% ungroup()
  
  results <- x$tagged %>%
    distinct(concept_lo, concept_hi) %>%
    tidyr::expand(concept_lo, concept_hi, category = DESIRED_CATS) %>%
    left_join(picked, by = c("concept_lo","concept_hi","category")) %>%
    select(concept_lo, concept_hi, category, color_lo, color_hi, deltaS_mg, deltaS_us) %>% 
    mutate(thr_mg_low = thresholds$mg_low, thr_mg_high = thresholds$mg_high,
           thr_us_low = thresholds$us_low, thr_us_high = thresholds$us_high)
  
  list(thresholds = x$thresholds, results = results)
}

# ----------------------------
# 2) Deterministic "most extreme" per quadrant
# ----------------------------
pick_most_extreme1_per_quadrant <- function(mg_path, us_path,
                                            thresholds = NULL, low_q = 0.25, high_q = 0.75) {
  x <- load_and_join_tagged(mg_path, us_path, thresholds, low_q, high_q)
  
  picked <- x$tagged %>%
    add_extremeness() %>%
    group_by(concept_lo, concept_hi, category) %>%
    arrange(desc(extremeness), .by_group = TRUE) %>%
    slice_head(n = 1) %>%
    ungroup()
  
  results <- x$tagged %>%
    distinct(concept_lo, concept_hi) %>%
    tidyr::expand(concept_lo, concept_hi, category = DESIRED_CATS) %>%
    left_join(picked %>% select(-mg_level, -us_level), by = c("concept_lo","concept_hi","category")) %>%
    select(concept_lo, concept_hi, category, color_lo, color_hi, deltaS_mg, deltaS_us, extremeness) %>% 
    mutate(thr_mg_low = thresholds$mg_low, thr_mg_high = thresholds$mg_high,
           thr_us_low = thresholds$us_low, thr_us_high = thresholds$us_high)
  
  list(thresholds = x$thresholds, results = results)
}

# ----------------------------
# 3) Return ALL qualifying color-pairs per quadrant
# ----------------------------
pick_all_per_quadrant <- function(mg_path, us_path,
                                  thresholds = NULL, low_q = 0.25, high_q = 0.75) {
  x <- load_and_join_tagged(mg_path, us_path, thresholds, low_q, high_q)
  
  results <- x$tagged %>%
    filter(category %in% DESIRED_CATS) %>%
    select(concept_lo, concept_hi, category, color_lo, color_hi, deltaS_mg, deltaS_us) %>% 
    mutate(thr_mg_low = thresholds$mg_low, thr_mg_high = thresholds$mg_high,
           thr_us_low = thresholds$us_low, thr_us_high = thresholds$us_high)
  
  list(thresholds = x$thresholds, results = results)
}

# ----------------------------
# 4) Return TOP K per quadrant (ranked by "extremeness")
# ----------------------------
pick_topK_per_quadrant <- function(mg_path, us_path,
                                   K = 5,
                                   thresholds = NULL, low_q = 0.25, high_q = 0.75) {
  x <- load_and_join_tagged(mg_path, us_path, thresholds, low_q, high_q)
  
  results <- x$tagged %>%
    add_extremeness() %>%
    group_by(concept_lo, concept_hi, category) %>%
    arrange(desc(extremeness), .by_group = TRUE) %>%
    slice_head(n = K) %>%
    ungroup() %>%
    select(concept_lo, concept_hi, category, color_lo, color_hi, deltaS_mg, deltaS_us, extremeness) %>% 
    mutate(thr_mg_low = thresholds$mg_low, thr_mg_high = thresholds$mg_high,
           thr_us_low = thresholds$us_low, thr_us_high = thresholds$us_high)
  
  list(thresholds = x$thresholds, results = results)
}

# ----------------------------
# Example usage
# ----------------------------
mg_file <- "cg/mg_pairwise_AB_Cvalues_with_deltaS.csv"
us_file <- "cg/us_pairwise_AB_Cvalues_with_deltaS.csv"

res_random   <- pick_random1_per_quadrant(mg_file, us_file, low_q = 0.25, high_q = 0.75, seed = 42)
res_extreme  <- pick_most_extreme1_per_quadrant(mg_file, us_file, low_q = 0.25, high_q = 0.75)
res_all      <- pick_all_per_quadrant(mg_file, us_file, low_q = 0.25, high_q = 0.75)
res_topK     <- pick_topK_per_quadrant(mg_file, us_file, K = 5, low_q = 0.25, high_q = 0.75)

# res_random$results
View(res_extreme$results)
View(res_all$results %>% filter(concept_lo %in% c("banana"), concept_hi %in% c("happy"), category %in% c("MG_high__US_high")))
View(res_topK$results  %>% filter(concept_lo %in% c("banana"), concept_hi %in% c("happy"), category %in% c("MG_high__US_high")))
