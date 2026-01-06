library(readr)
library(dplyr)

compute_deltaS <- function(df) {
  df %>%
    mutate(
      # Means (already in your columns)
      mu1 = A_to_C1,  # x1
      mu2 = A_to_C2,  # x2
      mu3 = B_to_C1,  # x3
      mu4 = B_to_C2,  # x4
      
      # Paper’s noise model: sigma_i = 1.4 * mu_i * (1 - mu_i)
      s1 = 1.4 * mu1 * (1 - mu1),
      s2 = 1.4 * mu2 * (1 - mu2),
      s3 = 1.4 * mu3 * (1 - mu3),
      s4 = 1.4 * mu4 * (1 - mu4),
      
      # Δx mean and std
      delta_mu = (mu1 + mu4) - (mu2 + mu3),
      denom    = sqrt(s1^2 + s2^2 + s3^2 + s4^2),
      
      # P(Δx > 0) under Normal; handle the degenerate denom=0 case
      p_gt0 = case_when(
        is.na(delta_mu) | is.na(denom) ~ NA_real_,
        denom > 0 ~ pnorm(delta_mu / denom),
        delta_mu > 0 ~ 1,
        delta_mu < 0 ~ 0,
        TRUE ~ 0.5
      ),
      
      # ΔS = |P(Δx>0) - P(Δx<0)| = |2*P(Δx>0) - 1|
      deltaS = abs(2 * p_gt0 - 1)
    ) %>%
    select(-mu1, -mu2, -mu3, -mu4, -s1, -s2, -s3, -s4, -delta_mu, -denom, -p_gt0)
}

# --- Apply to your two files and write new CSVs ---

mg_df <- read_csv("cg/mg_pairwise_AB_Cvalues.csv", show_col_types = FALSE) %>% compute_deltaS()
us_df <- read_csv("cg/us_pairwise_AB_Cvalues.csv", show_col_types = FALSE) %>% compute_deltaS()

write_csv(mg_df, "cg/mg_pairwise_AB_Cvalues_with_deltaS.csv")
write_csv(us_df, "cg/us_pairwise_AB_Cvalues_with_deltaS.csv" )

# quick sanity check
mg_df %>% select(country, concept_a, concept_b, color_1, color_2, deltaS) %>% head(10)
us_df %>% select(country, concept_a, concept_b, color_1, color_2, deltaS) %>% head(10)
