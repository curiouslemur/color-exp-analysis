# k×k semantic discriminability via Monte Carlo + assignment (Schloss-style sigma)
# ------------------------------------------------------------------------------

library(readr)
library(dplyr)
library(tidyr)
library(clue)   # solve_LSAP()

# Build mean (xbar) and sigma matrices for a chosen concept set and color set
build_xbar_sigma <- function(summary_df, concepts, colors, sd_scale = 1.4) {
  # summary_df = us_sum
  df <- summary_df %>%
    filter(concept %in% concepts, color %in% colors) %>%
    select(concept, color, mean_rating)
  
  # Ensure full grid exists
  grid <- expand_grid(concept = concepts, color = colors) %>%
    left_join(df, by = c("concept", "color"))
  
  if (any(is.na(grid$mean_rating))) {
    missing <- grid %>% filter(is.na(mean_rating))
    stop("Missing mean_rating for some concept-color pairs:\n", paste0(missing$concept, " × ", missing$color, collapse = ", "))}
  
  # xbar in [0,1]
  xbar <- grid$mean_rating
  if (any(xbar < -1e-6 | xbar > 1 + 1e-6)) {stop("mean_rating must be in [0,1]. If your data is 0–100, divide by 100 first.")}
  
  k <- length(concepts)
  xbar_mat <- matrix(xbar, nrow = k, ncol = k, byrow = TRUE, dimnames = list(concepts, colors))
  
  sigma_mat <- sd_scale * xbar_mat * (1 - xbar_mat)
  
  list(xbar = xbar_mat, sigma = sigma_mat)
}

# Run Monte Carlo and compute assignment distribution + metrics
semantic_discriminability_k <- function(summary_df, concepts, colors,
                                        n_sims = 5000, sd_scale = 1.4, clamp_01 = TRUE, seed = 1) {
  stopifnot(length(concepts) == length(colors))
  k <- length(concepts)
  
  mats <- build_xbar_sigma(summary_df, concepts, colors, sd_scale = sd_scale)
  xbar <- mats$xbar
  sigma <- mats$sigma
  
  set.seed(seed)
  
  # Store assignment keys and confusion counts
  ## assign_key: the full mapping (assignment) produced on each Monte Carlo simulation. 
  ## assign_key tracks which complete k↔k mapping happened each simulation
  assign_key <- character(n_sims)
  
  ## conf_counts: counts of how often each concept gets assigned to each color across simulations.
  ## conf_counts aggregates per-concept assignment frequencies across simulations.
  conf_counts <- matrix(0L, nrow = k, ncol = k, dimnames = list(concepts, colors))
  
  xbar_vec  <- as.vector(xbar)
  sigma_vec <- as.vector(sigma)
  
  for (s in seq_len(n_sims)) {
    # Sample all edges
    samp_vec <- rnorm(k * k, mean = xbar_vec, sd = sigma_vec)
    samp_mat <- matrix(samp_vec, nrow = k, ncol = k, byrow = TRUE)
    
    # Optionally clamp to [0,1] (useful because Normal can go outside bounds)
    if (clamp_01) samp_mat <- pmin(pmax(samp_mat, 0), 1)
    
    # Max-sum assignment: solve_LSAP minimizes cost, so use cost = -score
    perm <- as.integer(solve_LSAP(samp_mat, maximum = TRUE))  # perm[i] = assigned column for row i
    
    # Key for this mapping (concept order is fixed)
    # Example: "2,5,1,3,4" meaning row1->col2, row2->col5, row3->col1, row4->col3, row5->col4 ...
    assign_key[s] <- paste(perm, collapse = ",")
    
    # Update confusion counts
    for (i in seq_len(k)) {
      conf_counts[i, perm[i]] <- conf_counts[i, perm[i]] + 1L
    }
  }
  
  # Assignment distribution
  tab <- sort(table(assign_key), decreasing = TRUE)
  probs <- as.numeric(tab) / n_sims
  
  p_best <- probs[1]
  p_second <- if (length(probs) >= 2) probs[2] else 0
  margin <- p_best - p_second
  
  # Entropy over assignments (bits)
  entropy_bits <- -sum(probs * log2(probs))
  
  # Optional: normalize by log2(k!) (max possible entropy if all permutations equally likely)
  log2_kfact <- log2(factorial(k))
  entropy_norm <- entropy_bits / log2_kfact  # ~0 (very stable) to ~1 (very ambiguous)
  
  # Decode top assignments into explicit mappings
  decode_perm <- function(key) as.integer(strsplit(key, ",", fixed = TRUE)[[1]])
  
  top_df <- tibble(
    assignment_key = names(tab),
    prob = probs) %>%
    mutate(rank = row_number()) %>%
    # slice_head(n = min(20, n())) %>%
    head(20) %>%
    rowwise() %>%
    mutate(
      perm = list(decode_perm(assignment_key)),
      mapping = paste0(concepts, "→", colors[unlist(perm)], collapse = " | ")) %>%
    ungroup() %>%
    select(rank, prob, mapping, assignment_key)
  
  # Confusion probabilities (concept -> color)
  conf_prob <- conf_counts / n_sims
  conf_long <- as.data.frame(as.table(conf_prob)) %>%
    rename(concept = Var1, color = Var2, prob = Freq) %>%
    arrange(concept, desc(prob))
  
  list(
    inputs = list(concepts = concepts, colors = colors, n_sims = n_sims,
                  sd_scale = sd_scale, clamp_01 = clamp_01, seed = seed),
    metrics = tibble(
      k = k,
      n_sims = n_sims,
      p_best = p_best,
      p_second = p_second,
      margin = margin,
      entropy_bits = entropy_bits,
      entropy_norm = entropy_norm
    ),
    top_assignments = top_df,
    confusion = conf_long
  )
}

# ---------------------------
# Application: the colors and concepts for the palettes need to be defined
# ---------------------------

# Read your 0–1 summaries
mg_sum <- read_csv("cg/mg_concept_color_summary.csv", show_col_types = FALSE)
us_sum <- read_csv("cg/us_concept_color_summary.csv", show_col_types = FALSE)

# Choose a palette scenario (k concepts, k colors)
concepts <- c("sick","happy","banana")
colors   <- c("BK","LH","MC")

# Run for each group
mg_res <- semantic_discriminability_k(mg_sum, concepts, colors, n_sims = 10000, seed = 42)
us_res <- semantic_discriminability_k(us_sum, concepts, colors, n_sims = 10000, seed = 42)

# Compare (semantic discriminability gap)
mg_res$metrics
us_res$metrics
gap_entropy_bits <- us_res$metrics$entropy_bits - mg_res$metrics$entropy_bits
gap_pbest        <- us_res$metrics$p_best - mg_res$metrics$p_best

# Inspect the most likely mappings
mg_res$top_assignments
us_res$top_assignments

# Inspect confusion probabilities
mg_res$confusion
us_res$confusion

