## Utility file with helper functions for the 2026 analysis

# Pairwise semantic discriminability landscape (using Schloss-style uncertainty model)
# ----------------------------------------------------------------------------
# This version follows the paper's assumption:
#   Each association x_i ~ Normal(mean = xbar_i, sd = sigma_i),
#   where sigma_i = 1.4 * xbar_i * (1 - xbar_i) with xbar_i in [0, 1].
#
# For a given (concept_a=A, concept_b=B, color_1=C1, color_2=C2):
#   D = (A->C1 + B->C2) - (A->C2 + B->C1)
# If we assume independence between the four x_i's, then:
#   D ~ Normal(mu_D, var_D)
#   p_gt0 = P(D > 0) = pnorm(mu_D / sqrt(var_D))
#   semantic_distance = |p_gt0 - p_swapped| = |2*p_gt0 - 1|

# Function for computing the semantic distance landscape
## Returns a table with columns: 
### country | concept_a | concept_b | color_1 | color_2 | ... | deltaS
make_pairwise_landscape <- function(summary_df, alpha_fit = 1.4, eps = 1e-12) {
  xbar <- summary_df$mean_rating
  
  concepts <- sort(unique(summary_df$concept)); concepts
  colors   <- sort(unique(summary_df$color)); colors
  nc <- length(concepts); nk <- length(colors)
  
  summary_df$concept <- factor(summary_df$concept, levels = concepts)
  summary_df$color   <- factor(summary_df$color, levels = colors)
  
  # Lookup matrices for xbar
  mean_mat <- matrix(NA_real_, nrow = nc, ncol = nk, dimnames = list(concepts, colors))
  idx <- cbind(as.integer(summary_df$concept), as.integer(summary_df$color))
  mean_mat[idx] <- summary_df$mean_rating
  
  # Schloss-style sigma_i = 1.4 * xbar_i * (1 - xbar_i)
  # Clamp to avoid exactly 0 variance and ties to dominate numerically
  sigma_mat <- alpha_fit * mean_mat * (1 - mean_mat)
  # sigma_mat_ <- pmax(sigma_mat, eps)
  
  # All unordered pairs (canonical) of concepts and colors
  concept_pairs <- t(combn(seq_len(nc), 2))  # [a,b]
  color_pairs   <- t(combn(seq_len(nk), 2))  # [c1,c2]
  
  # Cross join indices
  grid <- expand.grid(
    cp = seq_len(nrow(concept_pairs)),
    kp = seq_len(nrow(color_pairs)))
  
  a  <- concept_pairs[grid$cp, 1]
  b  <- concept_pairs[grid$cp, 2]
  c1 <- color_pairs[grid$kp, 1]
  c2 <- color_pairs[grid$kp, 2]
  
  # Means needed for mu_D
  m_a_c1 <- mean_mat[cbind(a, c1)] # this is x1: conceptA -> color1 in the paper's bivariate figure
  m_a_c2 <- mean_mat[cbind(a, c2)] # x2: conceptA -> color2
  m_b_c1 <- mean_mat[cbind(b, c1)] # x3: conceptB -> color1
  m_b_c2 <- mean_mat[cbind(b, c2)] # x4: conceptB -> color2
  
  # Sigmas needed for var_D (assuming independence)
  s_a_c1 <- sigma_mat[cbind(a, c1)]
  s_a_c2 <- sigma_mat[cbind(a, c2)]
  s_b_c1 <- sigma_mat[cbind(b, c1)]
  s_b_c2 <- sigma_mat[cbind(b, c2)]
  
  # mu_D stands for delta_X
  mu_D  <- (m_a_c1 + m_b_c2) - (m_a_c2 + m_b_c1)
  var_D <- (s_a_c1^2 + s_b_c2^2) + (s_a_c2^2 + s_b_c1^2)
  denom <- sqrt(var_D)
  
  z <- mu_D / denom
  
  # p_gt0 stands for p greater than 0
  p_gt0 <- case_when(
    denom > 0 ~ stats::pnorm(z),
    is.na(mu_D) | is.na(var_D) ~ NA_real_
    # mu_D > 0 ~ 1, mu_D < 0 ~ 0, TRUE ~ 0.5
  )
  
  # p_gt0 <- stats::pnorm(z)
  p_swapped <- 1 - p_gt0
  
  semantic_distance <- abs(p_gt0 - p_swapped)  # == abs(2*p_gt0 - 1)
  
  data.frame(
    country = unique(summary_df$country)[1],
    concept_a = concepts[a], concept_b = concepts[b],
    color_1 = colors[c1], color_2 = colors[c2],
    # four x bars
    A_to_C1 = m_a_c1, B_to_C2 = m_b_c2, 
    A_to_C2 = m_a_c2, B_to_C1 = m_b_c1,
    # sigmas 
    sigma_A_C1 = s_a_c1, sigma_B_C2 = s_b_c2,
    sigma_A_C2 = s_a_c2, sigma_B_C1 = s_b_c1,
    # Delta x and variances
    mu_D = mu_D,
    var_D = var_D,
    p_gt0 = p_gt0,
    p_swapped = p_swapped,
    semantic_distance = semantic_distance,
    stringsAsFactors = FALSE )
}


# Helper functions for categorizing the different semantic distances in the landscapes as high and low
# ----------------------------------------------------------------------------

# Canonicalize concept-pair and color-pair ordering so joins are stable (alphabetically ordered names)
# df is the semantic distance landscape dataset
# ds_name should be deltaS_mg / deltaS_us : need to distinguish the two column names

canon_pairs <- function(df, ds_col, ds_name) {
  df %>%
    mutate(
      concept_lo = pmin(concept_a, concept_b),
      concept_hi = pmax(concept_a, concept_b),
      color_lo   = pmin(color_1, color_2),
      color_hi   = pmax(color_1, color_2)) %>%
    transmute(concept_lo, concept_hi, color_lo, color_hi, !!ds_name := .data[[ds_col]]) %>% 
    rename(concept_a = concept_lo, concept_b = concept_hi, color_a = color_lo, color_b = color_hi)
}

compute_thresholds_quantile <- function(mg_ds, us_ds, low_q = 0.25, high_q = 0.75) {
  list(
    mg_low  = quantile(mg_ds, low_q,  na.rm = TRUE),
    mg_high = quantile(mg_ds, high_q, na.rm = TRUE),
    us_low  = quantile(us_ds, low_q,  na.rm = TRUE),
    us_high = quantile(us_ds, high_q, na.rm = TRUE))
}

tag_quadrants <- function(joined, thr) {
  joined %>%
    mutate(
      mg_level = case_when(
        deltaS_mg >= thr$mg_high ~ "high",
        deltaS_mg <= thr$mg_low  ~ "low", TRUE ~ "mid"),
      us_level = case_when(
        deltaS_us >= thr$us_high ~ "high",
        deltaS_us <= thr$us_low  ~ "low", TRUE ~ "mid"),
      category = case_when(
        mg_level == "high" & us_level == "low"  ~ "MG_high__US_low",
        mg_level == "high" & us_level == "high" ~ "MG_high__US_high",
        mg_level == "low"  & us_level == "low"  ~ "MG_low__US_low",
        mg_level == "low"  & us_level == "high" ~ "MG_low__US_high", TRUE ~ NA_character_)
    ) %>% filter(!is.na(category))
}

# function to define how extreme the difference between teh two deltaS is
add_extremeness <- function(df) {
  df %>%
    mutate(
      extremeness = case_when(
        category == "MG_high__US_low"  ~ (deltaS_mg - deltaS_us),
        category == "MG_low__US_high"  ~ (deltaS_us - deltaS_mg),
        category == "MG_high__US_high" ~ (deltaS_mg + deltaS_us),
        category == "MG_low__US_low"   ~ -(deltaS_mg + deltaS_us),
        TRUE ~ NA_real_))
}

# function to join us and mg deltaS datasets and tag which category each line belongs to
load_and_join_tagged <- function(mg_deltaS, us_deltaS, thresholds = NULL, low_q = 0.25, high_q = 0.75) {
  
  # separately tag deltaS for mg and us datasets
  mg <- canon_pairs(mg_deltaS, ds_col = "semantic_distance", ds_name = "deltaS_mg")
  us <- canon_pairs(us_deltaS, ds_col = "semantic_distance", ds_name = "deltaS_us")
  
  if (is.null(thresholds)) {
    thresholds <- compute_thresholds_quantile(mg$deltaS_mg, us$deltaS_us, low_q = low_q, high_q = high_q)
  }
  
  # join both deltaS datasets
  joined <- mg %>% inner_join(us, by = c("concept_a","concept_b","color_a","color_b")) 
  # tag categories
  tagged <- joined %>% tag_quadrants(thresholds)
  list(tagged = tagged, thresholds = thresholds)
}

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

# ----------------------------
# 1) Pick one random 2x2 set at random one per quadrant
# ----------------------------
# pick_random1_per_quadrant <- function(mg_deltaS, us_deltaS, thresholds = NULL, low_q = 0.25, high_q = 0.75, seed = 1) {
pick_random1_per_quadrant <- function(joined_n_tagged, thresholds = NULL, low_q = 0.25, high_q = 0.75, seed = 1) {
    
  set.seed(seed)
  # x <- load_and_join_tagged(mg_deltaS, us_deltaS, thresholds, low_q, high_q)
  
  x <- joined_n_tagged
  picked <- x$tagged %>%
    group_by(concept_a, concept_b, category) %>%
    slice_sample(n = 1) %>% ungroup()
  
  results <- picked %>% 
    mutate(thr_mg_low = thresholds$mg_low, thr_mg_high = thresholds$mg_high,
           thr_us_low = thresholds$us_low, thr_us_high = thresholds$us_high)
  
  list(thresholds = x$thresholds, results = results)
}

# ----------------------------
# 2) Deterministic: Pick "most extreme" per quadrant
# ----------------------------
# pick_most_extreme1_per_quadrant <- function(mg_deltaS, us_deltaS, thresholds = NULL, low_q = 0.25, high_q = 0.75) {
pick_most_extreme1_per_quadrant <- function(joined_n_tagged, thresholds = NULL, low_q = 0.25, high_q = 0.75) {
    
  # x <- load_and_join_tagged(mg_deltaS, us_deltaS, thresholds, low_q, high_q)
  x <- joined_n_tagged
  
  picked <- x$tagged %>% 
    add_extremeness() %>%
    group_by(concept_a, concept_b, category) %>%
    arrange(desc(extremeness), .by_group = TRUE) %>%
    slice_head(n = 1) %>% ungroup()
  
  results <- picked %>% 
    mutate(thr_mg_low = thresholds$mg_low, thr_mg_high = thresholds$mg_high,
           thr_us_low = thresholds$us_low, thr_us_high = thresholds$us_high)
  
  list(thresholds = x$thresholds, results = results)
}

# ----------------------------
# 3) Return ALL qualifying color-pairs per quadrant
# ----------------------------
pick_all_per_quadrant <- function(joined_n_tagged,
                                  thresholds = NULL, low_q = 0.25, high_q = 0.75) {
  # x <- load_and_join_tagged(mg_deltaS, us_deltaS, thresholds, low_q, high_q)
  x <- joined_n_tagged
  
  results <- x$tagged %>%
    filter(category %in% CAT_NAMES) %>%
    select(concept_a, concept_b, category, color_a, color_b, deltaS_mg, deltaS_us) %>% 
    mutate(thr_mg_low = thresholds$mg_low, thr_mg_high = thresholds$mg_high,
           thr_us_low = thresholds$us_low, thr_us_high = thresholds$us_high)
  
  list(thresholds = x$thresholds, results = results)
}

# ----------------------------
# 4) Return TOP K per quadrant (ranked by "extremeness")
# ----------------------------
pick_topK_per_quadrant <- function(joined_n_tagged, K = 5,
                                   thresholds = NULL, low_q = 0.25, high_q = 0.75) {
  # x <- load_and_join_tagged(mg_deltaS, us_deltaS, thresholds, low_q, high_q)
  x <- joined_n_tagged
  results <- x$tagged %>% add_extremeness() %>%
    group_by(concept_a, concept_b, category) %>%
    arrange(desc(extremeness), .by_group = TRUE) %>%
    slice_head(n = K) %>%
    ungroup() %>%
    select(concept_a, concept_b, category, color_a, color_b, deltaS_mg, deltaS_us, extremeness) %>% 
    mutate(thr_mg_low = thresholds$mg_low, thr_mg_high = thresholds$mg_high,
           thr_us_low = thresholds$us_low, thr_us_high = thresholds$us_high)
  
  list(thresholds = x$thresholds, results = results)
}

