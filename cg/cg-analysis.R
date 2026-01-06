# Pairwise semantic discriminability landscape (using Schloss-style uncertainty model)
# ----------------------------------------------------------------------------
# This version follows the paper's assumption:
#   Each association x_i ~ Normal(mean = xbar_i, sd = sigma_i),
#   where sigma_i = 1.4 * xbar_i * (1 - xbar_i)
# with xbar_i in [0, 1].
#
# For a given (concept_a=A, concept_b=B, color_1=C1, color_2=C2):
#   D = (A->C1 + B->C2) - (A->C2 + B->C1)
# If we assume independence between the four x_i's, then:
#   D ~ Normal(mu_D, var_D)
#   p_correct = P(D > 0) = pnorm(mu_D / sqrt(var_D))
#   semantic_distance = |p_correct - p_swapped| = |2*p_correct - 1|

library(readr)

make_pairwise_landscape <- function(summary_df, sd_scale = 1.4, eps = 1e-12) {
  # summary_df = us_sum; 
  
  # Ensure xbar is in [0,1] (your regenerated summaries are already 0–1)
  xbar <- summary_df$mean_rating
  if (any(xbar < -1e-6 | xbar > 1 + 1e-6, na.rm = TRUE)) {
    stop("mean_rating must be in [0,1]. If your file is 0–100, divide by 100 first.")
  }
  
  concepts <- sort(unique(summary_df$concept))
  colors   <- sort(unique(summary_df$color))
  nc <- length(concepts)
  nk <- length(colors)
  
  summary_df$concept <- factor(summary_df$concept, levels = concepts)
  summary_df$color   <- factor(summary_df$color, levels = colors)
  
  # Lookup matrices for xbar
  mean_mat <- matrix(NA_real_, nrow = nc, ncol = nk, dimnames = list(concepts, colors))
  idx <- cbind(as.integer(summary_df$concept), as.integer(summary_df$color))
  mean_mat[idx] <- summary_df$mean_rating
  
  # Schloss-style sigma_i = 1.4 * xbar_i * (1 - xbar_i)
  # Clamp to avoid exactly 0 variance if you don't want ties to dominate numerically
  sigma_mat <- sd_scale * mean_mat * (1 - mean_mat)
  sigma_mat <- pmax(sigma_mat, eps)
  
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
  m_a_c1 <- mean_mat[cbind(a, c1)]
  m_a_c2 <- mean_mat[cbind(a, c2)]
  m_b_c1 <- mean_mat[cbind(b, c1)]
  m_b_c2 <- mean_mat[cbind(b, c2)]
  
  # Sigmas needed for var_D (assuming independence)
  s_a_c1 <- sigma_mat[cbind(a, c1)]
  s_a_c2 <- sigma_mat[cbind(a, c2)]
  s_b_c1 <- sigma_mat[cbind(b, c1)]
  s_b_c2 <- sigma_mat[cbind(b, c2)]
  
  mu_D  <- (m_a_c1 + m_b_c2) - (m_a_c2 + m_b_c1)
  var_D <- (s_a_c1^2 + s_b_c2^2) + (s_a_c2^2 + s_b_c1^2)
  
  z <- mu_D / sqrt(var_D)
  p_correct <- stats::pnorm(z)
  p_swapped <- 1 - p_correct
  
  semantic_distance <- abs(p_correct - p_swapped)  # == abs(2*p_correct - 1)
  
  data.frame(
    country = unique(summary_df$country)[1],
    concept_a = concepts[a],
    concept_b = concepts[b],
    color_1 = colors[c1],
    color_2 = colors[c2],
    # helpful to keep the four means too
    A_to_C1 = m_a_c1,
    B_to_C2 = m_b_c2,
    A_to_C2 = m_a_c2,
    B_to_C1 = m_b_c1,
    sigma_A_C1 = s_a_c1,
    sigma_B_C2 = s_b_c2,
    sigma_A_C2 = s_a_c2,
    sigma_B_C1 = s_b_c1,
    mu_D = mu_D,
    var_D = var_D,
    p_correct = p_correct,
    p_swapped = p_swapped,
    semantic_distance = semantic_distance,
    stringsAsFactors = FALSE
  )
}

# --- Read your 0–1 summaries ---
mg_sum <- read_csv("cg/mg_concept_color_summary.csv", show_col_types = FALSE)
us_sum <- read_csv("cg/us_concept_color_summary.csv", show_col_types = FALSE)

# --- Compute landscapes ---
mg_landscape <- make_pairwise_landscape(mg_sum, sd_scale = 1.4)
us_landscape <- make_pairwise_landscape(us_sum, sd_scale = 1.4)

# --- Write outputs ---
write_csv(mg_landscape, "cg/mg_pairwise_semantic_discriminability.csv")
write_csv(us_landscape, "cg/us_pairwise_semantic_discriminability.csv")

# Peek
head(mg_landscape, 10)
