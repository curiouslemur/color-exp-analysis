library(readr)
library(dplyr)
library(tidyr)
library(purrr)

mg_path <- "cg/mg_pairwise_AB_Cvalues_with_deltaS.csv"
us_path <- "cg/us_pairwise_AB_Cvalues_with_deltaS.csv"

# ---- Helper: compute thresholds dynamically (quantile-based) ----
# You can swap this out for fixed numeric thresholds if you want.
compute_thresholds <- function(mg_df, us_df, low_q  = 0.25, high_q = 0.75) {
  list(
    mg_low  = quantile(mg_df$deltaS_mg, low_q,  na.rm = TRUE),
    mg_high = quantile(mg_df$deltaS_mg, high_q, na.rm = TRUE),
    us_low  = quantile(us_df$deltaS_us, low_q,  na.rm = TRUE),
    us_high = quantile(us_df$deltaS_us, high_q, na.rm = TRUE)
  )
}

# ---- Main: pick 1 color-pair per category for each concept-pair ----
# Categories (4 quadrants):
#   1) MG high, US low
#   2) MG high, US high
#   3) MG low,  US low
#   4) MG low,  US high
pick_color_pairs_per_concept_pair <- function(mg_path, us_path, thresholds = NULL, 
                                              low_q = 0.25, high_q = 0.75, seed = 1) {
  set.seed(seed)
  
  mg <- read_csv(mg_path, show_col_types = FALSE) %>%
    select(concept_a, concept_b, color_1, color_2, deltaS_mg = deltaS)
  
  us <- read_csv(us_path, show_col_types = FALSE) %>%
    select(concept_a, concept_b, color_1, color_2, deltaS_us = deltaS)
  
  # Canonicalize concept-pair and color-pair ordering so joins are stable
  canon <- function(df) {
    df %>%
      mutate(
        concept_lo = pmin(concept_a, concept_b),
        concept_hi = pmax(concept_a, concept_b),
        color_lo   = pmin(color_1, color_2),
        color_hi   = pmax(color_1, color_2)
      ) %>%
      select(concept_lo, concept_hi, color_lo, color_hi, everything(),
             -concept_a, -concept_b, -color_1, -color_2)
  }
  
  mg_c <- canon(mg)
  us_c <- canon(us)
  
  if (is.null(thresholds)) {
    thresholds <- compute_thresholds(mg_c, us_c, low_q = low_q, high_q = high_q)}
  
  joined <- mg_c %>%
    inner_join(us_c, by = c("concept_lo","concept_hi","color_lo","color_hi"))
  
  # Assign category for each row
  tagged <- joined %>%
    mutate(
      mg_level = case_when(
        deltaS_mg >= thresholds$mg_high ~ "high",
        deltaS_mg <= thresholds$mg_low  ~ "low",
        TRUE ~ "mid"
      ),
      us_level = case_when(
        deltaS_us >= thresholds$us_high ~ "high",
        deltaS_us <= thresholds$us_low  ~ "low",
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
  
  # For each concept-pair, pick one color-pair in each category (if available)
  desired_cats <- c("MG_high__US_low", "MG_high__US_high", "MG_low__US_low", "MG_low__US_high")
  
  picked <- tagged %>%
    group_by(concept_lo, concept_hi, category) %>%
    slice_sample(n = 1) %>%
    ungroup()
  
  # Ensure exactly 4 rows per concept pair (fill missing categories with NA)
  out <- expand_grid(
    concept_lo = sort(unique(tagged$concept_lo)),
    concept_hi = sort(unique(tagged$concept_hi))
  ) %>%
    distinct() %>%
    tidyr::expand(concept_lo, concept_hi, category = desired_cats) %>%
    left_join(picked, by = c("concept_lo","concept_hi","category")) %>%
    select(concept_lo, concept_hi, category, color_lo, color_hi, deltaS_mg, deltaS_us) %>% 
    mutate(thr_mg_low = thresholds$mg_low, thr_mg_high = thresholds$mg_high,
           thr_us_low = thresholds$us_low, thr_us_high = thresholds$us_high)
  
  list(
    thresholds = thresholds,
    results = out
  )
}

# ---- Run (quantile-based thresholds by default) ----
res <- pick_color_pairs_per_concept_pair(
  mg_path, us_path,
  low_q = 0.25, high_q = 0.75,  # dynamic thresholds
  seed = 42
)

res$thresholds
res$results %>% arrange(concept_lo, concept_hi, category) %>% print(n = 40)

## Export all data about pairs of concepts and the pairs of colors in each category of the quadrant
write_csv(res$results , "cg/deltaS-recompute-con-col-pairs-all.csv")

## keep only concept-pairs where ALL 4 categories exist (no missing rows)
complete_concept_pairs <- res$results %>%
  group_by(concept_lo, concept_hi) %>%
  filter(
    n() == 4,
    # and none of the picked color pairs is missing (i.e., category not found)
    !any(is.na(color_lo) | is.na(color_hi) | is.na(deltaS_mg) | is.na(deltaS_us))
  ) %>%
  ungroup()

complete_concept_pairs <- complete_concept_pairs 
## Export the data without NAs
write_csv(complete_concept_pairs, "cg/deltaS-recompute-con-col-pairs-complete.csv")


## ---- If you prefer FIXED numeric thresholds instead of quantiles ----
# fixed_thr <- list(mg_low=0.2, mg_high=0.6, us_low=0.2, us_high=0.6)
# res_fixed <- pick_color_pairs_per_concept_pair(mg_path, us_path, thresholds = fixed_thr, seed = 42)
# res_fixed$results
