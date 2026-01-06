library(readr)
library(dplyr)
library(tidyr)
library(clue)

# -----------------------
# Inputs (your scenario)
# -----------------------
concepts <- c("happy", "sick", "banana")
colors   <- c("BK", "LH", "MC")
k <- length(concepts)

mg_sum <- read_csv("cg/mg_concept_color_summary.csv", show_col_types = FALSE)
us_sum <- read_csv("cg/us_concept_color_summary.csv", show_col_types = FALSE)

# -----------------------
# Build xbar and sigma (paper model)
# sigma = 1.4 * xbar * (1-xbar)
# -----------------------
build_xbar_sigma <- function(summary_df, concepts, colors, sd_scale = 1.4) {
  df <- summary_df %>%
    filter(concept %in% concepts, color %in% colors) %>%
    select(concept, color, mean_rating)
  
  grid <- expand_grid(concept = concepts, color = colors) %>%
    left_join(df, by = c("concept", "color"))
  
  if (any(is.na(grid$mean_rating))) {
    missing <- grid %>% filter(is.na(mean_rating))
    stop("Missing mean_rating for: ",
         paste0(missing$concept, "×", missing$color, collapse = ", "))
  }
  
  xbar <- grid$mean_rating
  if (any(xbar < -1e-6 | xbar > 1 + 1e-6)) {
    stop("mean_rating must be in [0,1].")
  }
  
  xbar_mat <- matrix(xbar, nrow = k, ncol = k, byrow = TRUE,
                     dimnames = list(concepts, colors))
  sigma_mat <- sd_scale * xbar_mat * (1 - xbar_mat)
  dimnames(sigma_mat) <- dimnames(xbar_mat)
  
  list(xbar = xbar_mat, sigma = sigma_mat)
}

# -----------------------
# Run Monte Carlo and compute:
# - confusion_matrix: P(concept -> color)
# - pairwise_p: P(D_{A,B} > 0)
# - pairwise_S: 2p-1 (signed)
# - pairwise_SD: |2p-1|
# -----------------------
run_semantic_model <- function(summary_df, concepts, colors,
                               n_sims = 20000,
                               sd_scale = 1.4,
                               clamp_01 = TRUE,
                               seed = 1) {
  stopifnot(length(concepts) == length(colors))
  k <- length(concepts)
  
  mats <- build_xbar_sigma(summary_df, concepts, colors, sd_scale)
  xbar  <- mats$xbar
  sigma <- mats$sigma
  
  set.seed(seed)
  
  # Confusion counts: rows=concepts, cols=colors
  conf_counts <- matrix(0L, nrow = k, ncol = k, dimnames = list(concepts, colors))
  
  # Pairwise win counts: rows/cols=concepts
  # pair_win[A,B] = # sims where current assigned colors for (A,B) beat the swap
  pair_win <- matrix(0L, nrow = k, ncol = k, dimnames = list(concepts, concepts))
  
  pair_idx <- combn(seq_len(k), 2)
  
  xbar_vec  <- as.vector(xbar)
  sigma_vec <- as.vector(sigma)
  
  for (s in seq_len(n_sims)) {
    samp_vec <- rnorm(k*k, mean = xbar_vec, sd = sigma_vec)
    samp_mat <- matrix(samp_vec, nrow = k, ncol = k, byrow = TRUE,
                       dimnames = list(concepts, colors))
    
    if (clamp_01) samp_mat <- pmin(pmax(samp_mat, 0), 1)
    
    # maximize total association (requires nonnegative -> clamp ensures this)
    perm <- as.integer(clue::solve_LSAP(samp_mat, maximum = TRUE))
    assigned_colors <- colors[perm]
    names(assigned_colors) <- concepts
    
    # update confusion counts
    for (i in seq_len(k)) {
      conf_counts[concepts[i], assigned_colors[i]] <- conf_counts[concepts[i], assigned_colors[i]] + 1L
    }
    
    # update pairwise swap-win counts
    for (p in seq_len(ncol(pair_idx))) {
      i <- pair_idx[1, p]
      j <- pair_idx[2, p]
      A <- concepts[i]; B <- concepts[j]
      CA <- assigned_colors[A]
      CB <- assigned_colors[B]
      
      # D = (A->CA + B->CB) - (A->CB + B->CA)
      D <- (samp_mat[A, CA] + samp_mat[B, CB]) - (samp_mat[A, CB] + samp_mat[B, CA])
      
      if (D > 0) {
        pair_win[A, B] <- pair_win[A, B] + 1L
        pair_win[B, A] <- pair_win[B, A] + 1L
      }
    }
  }
  
  p_pair <- pair_win / n_sims
  diag(p_pair) <- NA_real_
  
  S_pair  <- 2 * p_pair - 1           # signed
  SD_pair <- abs(S_pair)              # semantic distance
  
  list(
    inputs = list(n_sims = n_sims, sd_scale = sd_scale, clamp_01 = clamp_01, seed = seed),
    xbar = xbar,
    sigma = sigma,
    confusion_matrix = conf_counts / n_sims,
    pairwise_p = p_pair,
    pairwise_S = S_pair,
    pairwise_SD = SD_pair
  )
}

# -----------------------
# Run for each group
# -----------------------
mg_res <- run_semantic_model(mg_sum, concepts, colors, n_sims = 20000, seed = 42)
us_res <- run_semantic_model(us_sum, concepts, colors, n_sims = 20000, seed = 42)

# -----------------------
# Make a tidy between-group comparison table (for reporting)
# -----------------------
# pairwise_long <- function(mat, value_name) {
#   as.data.frame(as.table(mat)) %>%
#     rename(concept_a = Var1, concept_b = Var2, !!value_name := Freq) %>%
#     filter(!is.na(.data[[value_name]])) %>%
#     # keep each unordered pair once
#     rowwise() %>%
#     mutate(a = min(concept_a, concept_b), b = max(concept_a, concept_b)) %>%
#     ungroup() %>%
#     select(concept_a = a, concept_b = b, !!value_name) %>%
#     distinct()
# }

pairwise_long <- function(mat, value_name) {
  as.data.frame(as.table(mat), stringsAsFactors = FALSE) %>%
    rename(concept_a = Var1, concept_b = Var2, !!value_name := Freq) %>%
    mutate(
      concept_a = as.character(concept_a),
      concept_b = as.character(concept_b)
    ) %>%
    filter(!is.na(.data[[value_name]])) %>%
    # keep each unordered pair once (canonicalize order)
    mutate(
      a = pmin(concept_a, concept_b),
      b = pmax(concept_a, concept_b)
    ) %>%
    filter(a != b) %>%  # drop diagonal if any slipped through
    select(concept_a = a, concept_b = b, !!value_name) %>%
    distinct()
}

mg_tbl <- pairwise_long(mg_res$pairwise_p,  "p_mdg")  %>%
  left_join(pairwise_long(mg_res$pairwise_S,  "S_mdg"),  by = c("concept_a","concept_b")) %>%
  left_join(pairwise_long(mg_res$pairwise_SD, "SD_mdg"), by = c("concept_a","concept_b"))

us_tbl <- pairwise_long(us_res$pairwise_p,  "p_us") %>%
  left_join(pairwise_long(us_res$pairwise_S,  "S_us"),  by = c("concept_a","concept_b")) %>%
  left_join(pairwise_long(us_res$pairwise_SD, "SD_us"), by = c("concept_a","concept_b"))

between_groups <- mg_tbl %>%
  inner_join(us_tbl, by = c("concept_a","concept_b")) %>%
  mutate(
    delta_SD = SD_us - SD_mdg,     # + means more separable in US
    delta_S  = S_us  - S_mdg,      # signed difference (captures "flip" risk)
    flip = (S_us > 0 & S_mdg < 0) | (S_us < 0 & S_mdg > 0)
  ) %>%
  arrange(desc(abs(delta_S)))

between_groups
write_csv(between_groups, "cg/between_group_pairwise_semantic_distance.csv")

# -----------------------
# Optional: compare confusion structure concept -> color
# -----------------------
conf_long <- function(conf_mat, prefix) {
  as.data.frame(as.table(conf_mat)) %>%
    rename(concept = Var1, color = Var2, !!prefix := Freq)
}

conf_delta <- conf_long(us_res$confusion_matrix, "p_us") %>%
  inner_join(conf_long(mg_res$confusion_matrix, "p_mdg"), by = c("concept","color")) %>%
  mutate(delta = p_us - p_mdg) %>%
  arrange(concept, desc(abs(delta)))

conf_delta
# write_csv(conf_delta, "between_group_confusion_delta.csv")

# Quick sanity checks: named matrices
mg_res$xbar
mg_res$sigma
mg_res$pairwise_SD
us_res$pairwise_SD
