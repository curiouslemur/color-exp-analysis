# k×k semantic discriminability via Monte Carlo + assignment (Schloss-style sigma)
# Version with concept/color NAMES in matrices + named assignment keys
# ------------------------------------------------------------------------------

library(readr)
library(dplyr)
library(tidyr)
library(clue)   # solve_LSAP()

# Build mean (xbar) and sigma matrices for a chosen concept set and color set
build_xbar_sigma <- function(summary_df, concepts, colors, sd_scale = 1.4) {
  df <- summary_df %>%
    filter(concept %in% concepts, color %in% colors) %>%
    select(concept, color, mean_rating)
  
  # Ensure full grid exists
  grid <- expand_grid(concept = concepts, color = colors) %>% left_join(df, by = c("concept", "color"))
  
  if (any(is.na(grid$mean_rating))) {
    missing <- grid %>% filter(is.na(mean_rating))
    stop("Missing mean_rating for some concept-color pairs:\n",
      paste0(missing$concept, " × ", missing$color, collapse = ", ")) }
  
  xbar <- grid$mean_rating
  if (any(xbar < -1e-6 | xbar > 1 + 1e-6)) {stop("mean_rating must be in [0,1]. If your data is 0–100, divide by 100 first.")}
  
  k <- length(concepts)
  
  # IMPORTANT: dimnames = (concept names, color names)
  xbar_mat <- matrix(
    xbar, nrow = k, ncol = k, byrow = TRUE,
    dimnames = list(concepts, colors))
  
  sigma_mat <- sd_scale * xbar_mat * (1 - xbar_mat)
  dimnames(sigma_mat) <- dimnames(xbar_mat)
  
  list(xbar = xbar_mat, sigma = sigma_mat)
}

# Run Monte Carlo and compute assignment distribution + metrics
semantic_discriminability_k_named <- function(summary_df, concepts, colors,
                                              n_sims = 5000, sd_scale = 1.4, clamp_01 = TRUE, seed = 1) {
  stopifnot(length(concepts) == length(colors))
  k <- length(concepts)
  
  mats <- build_xbar_sigma(summary_df, concepts, colors, sd_scale = sd_scale)
  xbar  <- mats$xbar
  sigma <- mats$sigma
  
  set.seed(seed)
  
  # Human-readable assignment keys (mapping strings, not indices)
  assign_key <- character(n_sims)
  
  # Confusion counts: rows=concepts, cols=colors (named)
  conf_counts <- matrix(0L, nrow = k, ncol = k, dimnames = list(concepts, colors))
  
  # Vectorize for rnorm, but keep names for clarity/debugging
  xbar_vec  <- as.vector(xbar)
  sigma_vec <- as.vector(sigma)
  
  for (s in seq_len(n_sims)) {
    
    # Sample all edges
    samp_vec <- rnorm(k * k, mean = xbar_vec, sd = sigma_vec)
    samp_mat <- matrix(samp_vec, nrow = k, ncol = k, byrow = TRUE)
    
    # IMPORTANT: attach concept/color names to sampled matrix too
    dimnames(samp_mat) <- list(concepts, colors)
    
    # Optionally clamp to [0,1] (keeps nonnegative for solve_LSAP with maximum=TRUE)
    if (clamp_01) samp_mat <- pmin(pmax(samp_mat, 0), 1)
    
    # Max-sum assignment directly (requires nonnegative entries)
    perm <- as.integer(clue::solve_LSAP(samp_mat, maximum = TRUE))
    names(perm) <- concepts
    
    assigned_colors <- colors[perm]
    names(assigned_colors) <- concepts
    
    # Store key as a readable mapping string (stable order: concepts vector)
    assign_key[s] <- paste0(concepts, "→", assigned_colors, collapse = " | ")
    
    # Update confusion counts using NAMES (not numeric indices)
    for (i in seq_along(concepts)) {
      conf_counts[concepts[i], assigned_colors[i]] <- conf_counts[concepts[i], assigned_colors[i]] + 1L
    }
  }
  
  # Assignment distribution (keys are readable already)
  tab   <- sort(table(assign_key), decreasing = TRUE)
  probs <- as.numeric(tab) / n_sims
  
  p_best   <- probs[1]
  p_second <- if (length(probs) >= 2) probs[2] else 0
  margin   <- p_best - p_second
  
  entropy_bits <- -sum(probs * log2(probs))
  entropy_norm <- entropy_bits / log2(factorial(k))
  
  # Top assignments table: mapping is just the table name
  top_df <- tibble(
    rank = seq_along(probs),
    prob = probs,
    mapping = names(tab)
  ) %>%
    head(20)
  
  # Confusion probabilities (concept -> color)
  conf_prob <- conf_counts / n_sims
  conf_long <- as.data.frame(as.table(conf_prob)) %>%
    rename(concept = Var1, color = Var2, prob = Freq) %>%
    arrange(concept, desc(prob))
  
  list(
    inputs = list(concepts = concepts, colors = colors, n_sims = n_sims,
                  sd_scale = sd_scale, clamp_01 = clamp_01, seed = seed),
    xbar = xbar,                 # named matrix
    sigma = sigma,               # named matrix
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
    confusion_matrix = conf_prob, # named matrix
    confusion = conf_long
  )
}

# ---------------------------
# Example usage
# ---------------------------

mg_sum <- read_csv("cg/mg_concept_color_summary.csv", show_col_types = FALSE)
us_sum <- read_csv("cg/us_concept_color_summary.csv", show_col_types = FALSE)

concepts <- c("sick","happy","banana")
colors   <- c("BK","LH","MC")

# Run for each group
mg_res1 <- semantic_discriminability_k_named(mg_sum, concepts, colors, n_sims = 10000, seed = 42)
us_res1 <- semantic_discriminability_k_named(us_sum, concepts, colors, n_sims = 10000, seed = 42)

# mg_res$xbar              # shows concept/color names
# mg_res$sigma             # shows concept/color names
# mg_res$confusion_matrix  # shows concept/color names
# mg_res$top_assignments   # mapping strings, no indices

# Compare (semantic discriminability gap)
mg_res1$metrics
us_res1$metrics
gap_entropy_bits <- us_res$metrics$entropy_bits - mg_res$metrics$entropy_bits
gap_pbest        <- us_res$metrics$p_best - mg_res$metrics$p_best

# Inspect the most likely mappings
mg_res$top_assignments
us_res$top_assignments

# Inspect confusion probabilities
mg_res$confusion
us_res$confusion

