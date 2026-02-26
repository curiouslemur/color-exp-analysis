library(tidyverse)
library(dplyr)
library(readr)
# library(tidytext)
library(lme4)
library(pwr)
library(gridExtra)
library(grid)
library(jsonlite)
library(purrr)

#### List of concepts used for experiment 1 (final set of 14 concepts)
conceptListEn = c('banana', 'mango', 'peach', 'death', 'justice', 'peace', 'safety',
                   'angry', 'happy', 'sad', 'sick', 'lightning', 'sandstorm', 'tree')
conceptListFr = c('banane', 'mangue', 'pêche (fruit)', 'mort', 'justice', 'paix', 'sécurité',
                   'en colère', 'heureux', 'triste', 'malade', 'foudre', 'tempête de sable', 'arbre')

#### getComplete status
getPercentComplete <- function(df){
  df %>% mutate(
  percentComplete = na_if(percentComplete, "undefined"),
  percentComplete = na_if(percentComplete, "null"),
  percentComplete = as.numeric(percentComplete))}

#### function to get the demographic data (from consent) and ishihara response status
getConsentInfo <- function(df) {
  tmp <- df %>%
    filter(trialId == "consent") %>%
    mutate(answer_json = map(answer, ~ fromJSON(.x))) %>%
    filter(map_lgl(answer_json, ~ any(names(.x) %in% c(
      "prolific_pid", "expLang", "consented", "signedAtISO",
      "countryTaking", "yearsThere", "countryLongest",
      "nativeLanguage", "otherLangauges", 
      "age", "gender", "profession", "colorBlindness"
    )))) %>% 
    transmute(
      participantId,
      demo = answer_json
    ) %>%
    unnest_wider(demo) %>%
    # otherLanguages as a string (if it's a vector)
    mutate(
      otherLanguages = map_chr(otherLanguages, ~ {
        if (is.null(.x)) NA_character_ else paste(.x, collapse = "; ")
      })
    ) %>% group_by(participantId) %>%
    slice(1) %>%
    ungroup()
  return(tmp)
}

safe_fromJSON <- function(x) {
  if (is.na(x) || x == "undefined" || !str_starts(x, "\\{")) return(NULL)
  tryCatch(jsonlite::fromJSON(x), error = function(e) NULL)
} 

getIshihara <- function(df){
  tmp <- df %>%
    filter(str_detect(trialId, "^ishihara_plate_")) %>%
    transmute(
      participantId,
      plate = trialId,
      ans = map(answer, safe_fromJSON)
    ) %>%
    mutate(
      isCorrect = map(ans, ~{
        if (is.null(.x)) {NA} 
        else if (!"isCorrect" %in% names(.x)) {NA} 
        else {.x$isCorrect}
      }),
      isCorrect = as.logical(unlist(isCorrect))
    ) %>%
    select(participantId, plate, isCorrect) %>%
    distinct(participantId, plate, .keep_all = TRUE) %>%
    filter(str_detect(plate, "^ishihara_plate_(10|[0-9])$")) %>%
    pivot_wider(
      names_from = plate,
      values_from = isCorrect
    )
  return(tmp)
}

getDemographics <- function(df){
  tmp_consent <- getConsentInfo(df)
  tmp_ish <- getIshihara(df)
  
  tmp <- tmp_consent %>%
    left_join(tmp_ish, by = "participantId") %>%
    # (optional) guarantee all 0..10 columns exist, even if missing in data
    mutate(across(
      .cols = all_of(paste0("ishihara_plate_", 0:10)),
      .fns  = ~ .x
    ))
  return(tmp)
}


#### Function to get a parsed ('clean') data.
getCleanData <- function(response_df){
  tmp <- response_df %>%
    mutate(parsed = map(answer, fromJSON)) %>%
    transmute(
      participantId,
      trialId,
      answer,
      conA = map_chr(parsed, ~ .x$conceptA$text),
      conA_col_ans = map_chr(parsed, ~ .x$conceptA$chosenColorCode),
      conB = map_chr(parsed, ~ .x$conceptB$text),
      conB_col_ans = map_chr(parsed, ~ .x$conceptB$chosenColorCode),
      # need: conB_col_expected
      rt = map_dbl(parsed, ~ as.numeric(.x$responseTime)),
      mgLevel = map_chr(parsed, ~ .x$mgLevel),
      usLevel = map_chr(parsed, ~ .x$usLevel),
      leftColCode = map_chr(parsed, ~ .x$leftColorCode), # the position in which the color appears in the stimulus
      rightColCode = map_chr(parsed, ~ .x$rightColorCode), # the position in which the color appears in the stimulus
      # swapped = 1 / 0,
      leftHeight = map_dbl(parsed, ~ as.numeric(.x$leftHeight)),
      rightHeight = map_dbl(parsed, ~ as.numeric(.x$rightHeight)))
  return(tmp)
}


# color text used in attention check
color_code_family_en <- c(
  SR="red", LR="red", MR="red", DR="red",
  SO="orange", LO="orange", MO="orange", DO="orange",
  SY="yellow", LY="yellow", MY="yellow", DY="yellow",
  SH="chartreuse", LH="chartreuse", MH="chartreuse", DH="chartreuse",
  SG="green", LG="green", MG="green", DG="green",
  SC="cyan", LC="cyan", MC="cyan", DC="cyan",
  SB="blue", LB="blue", MB="blue", DB="blue",
  SP="purple", LP="purple", MP="purple", DP="purple",
  BK="black", A1="gray", A2="gray", A3="gray", WH="white",
  PK="pink", BR="brown", GR="gray", GD="gold"
)

color_code_family_fr <- c(
  SR="rouge", LR="rouge", MR="rouge", DR="rouge",
  SO="orange", LO="orange", MO="orange", DO="orange",
  SY="jaune", LY="jaune", MY="jaune", DY="jaune",
  SH="chartreuse", LH="chartreuse", MH="chartreuse", DH="chartreuse",
  SG="vert", LG="vert", MG="vert", DG="vert",
  SC="cyan", LC="cyan", MC="cyan", DC="cyan",
  SB="bleu", LB="bleu", MB="bleu", DB="bleu",
  SP="violet", LP="violet", MP="violet", DP="violet",
  BK="noir", A1="gris", A2="gris", A3="gris", WH="blanc",
  PK="rose", BR="brun", GR="gris", GD="dore"
)

# For MG/French keys like concept_doré
normalize_fr <- function(x) {
  x %>%
    tolower() %>%
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>%  # doré -> dore
    str_squish()
}

# helper for parsing json
`%||%` <- function(x, y) if (is.null(x)) y else x

get_ishihara_fail_ids <- function(df, fail_threshold = 2) {
  df %>%
    filter(responseId == "ishiharaResponse") %>%
    mutate(
      parsed = map(answer, ~ tryCatch(fromJSON(.x), error = function(e) NULL)),
      isCorrect = map_lgl(parsed, ~ {
        if (is.null(.x)) return(NA)
        val <- .x$isCorrect %||% NA
        if (is.na(val)) return(NA)
        as.logical(val)
      })
    ) %>%
    group_by(participantId) %>%
    summarise(
      n_ishihara = sum(!is.na(isCorrect)),
      n_failed = sum(isCorrect == FALSE, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(n_failed > fail_threshold) %>%
    pull(participantId)
}

score_attention_from_parsed <- function(x, normalize_keys = FALSE) {
  if (is.null(x)) return(NA)
  
  conceptA_id <- x$conceptA$id %||% NA_character_
  conceptB_id <- x$conceptB$id %||% NA_character_
  conceptA_chosen <- x$conceptA$chosenColorCode %||% NA_character_
  conceptB_chosen <- x$conceptB$chosenColorCode %||% NA_character_
  
  assignments_raw <- x$assignments %||% NULL
  if (is.null(assignments_raw)) return(NA)
  
  assignments_list <- as.list(assignments_raw)
  
  if (normalize_keys) {
    # MG/French: normalize both assignment keys and concept ids
    names(assignments_list) <- normalize_fr(names(assignments_list))
    conceptA_id <- normalize_fr(conceptA_id)
    conceptB_id <- normalize_fr(conceptB_id)
  }
  
  conceptA_expected <- assignments_list[[conceptA_id]] %||% NA_character_
  conceptB_expected <- assignments_list[[conceptB_id]] %||% NA_character_
  
  if (is.na(conceptA_expected) || is.na(conceptB_expected) ||
      is.na(conceptA_chosen) || is.na(conceptB_chosen)) {
    return(NA)
  }
  
  (conceptA_chosen == conceptA_expected) &&
    (conceptB_chosen == conceptB_expected)
}

get_attention_rows <- function(df, normalize_keys = FALSE, trial_pattern = "^attention_") {
  df %>%
    filter(
      tolower(responseId) == "choice",
      str_detect(trialId, trial_pattern)
    ) %>%
    mutate(
      parsed = map(answer, ~ tryCatch(fromJSON(.x), error = function(e) NULL)),
      attention_correct = map_lgl(parsed, ~ score_attention_from_parsed(.x, normalize_keys = normalize_keys))
    )
}

get_attention_fail_ids <- function(df, fail_threshold = 1, normalize_keys = FALSE, trial_pattern = "^attention_") {
  get_attention_rows(df, normalize_keys = normalize_keys, trial_pattern = trial_pattern) %>%
    group_by(participantId) %>%
    summarise(
      n_attention = sum(!is.na(attention_correct)),
      n_failed = sum(attention_correct == FALSE, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(n_failed > fail_threshold) %>%
    pull(participantId)
}

#### helpers for analysis
addStimuliInfo <- function(df, stimuli, suf){
  tmp <- df %>%
    left_join(
      stimuli %>% 
        select(conA, conB, col1, col2,
               paste0("x1", suf), paste0("x2", suf), paste0("x3", suf), paste0("x4", suf), 
               paste0("p_gt0", suf), paste0("dX", suf), paste0("dS", suf), dS_diff) %>%
        distinct(conA, conB, .keep_all = TRUE),
      by = c("conA", "conB"))  %>% 
    mutate(
      category = paste(mgLevel, usLevel, sep = "-")
    )
  colnames(tmp) <- gsub(suf, "", colnames(tmp))
  return(tmp)
}

addAccuracy <- function(df){
  tmp <- df %>%
    mutate(
      accuracy = case_when(
        dX > 0 &
          col1 == conA_col_ans &
          col2 == conB_col_ans ~ 1,
        
        dX < 0 &
          col1 == conB_col_ans &
          col2 == conA_col_ans ~ 1,
        
        TRUE ~ 0)
    )
}

overall_acc_by_group <- function(us_df_acc, mg_df_acc, mg_level, us_level) {
  bind_rows(
    us_df_acc %>% filter(mgLevel == mg_level, usLevel == us_level),
    mg_df_acc %>% filter(mgLevel == mg_level, usLevel == us_level)
  ) %>%
    group_by(group) %>%
    summarise(
      n = n(),
      n_correct = sum(accuracy == 1, na.rm = TRUE),
      pct_correct = mean(accuracy == 1, na.rm = TRUE) * 100,
      .groups = "drop"
    )
}

pair_acc <- function(us_df_acc, mg_df_acc, mg_level, us_level) {
  bind_rows(
    us_df_acc %>% filter(mgLevel == mg_level, usLevel == us_level),
    mg_df_acc %>% filter(mgLevel == mg_level, usLevel == us_level)
  ) %>%
    group_by(group, conA, conB, col1, col2) %>%
    summarise(
      dX = dX[which(!is.na(dX))[1]],
      dS = dS[which(!is.na(dS))[1]],
      n = n(),
      n_correct = sum(accuracy == 1, na.rm = TRUE),
      pct_correct = mean(accuracy == 1, na.rm = TRUE) * 100,
      .groups = "drop"
    ) %>%
    pivot_wider(
      id_cols = c(conA, conB, col1, col2),
      names_from = group,
      values_from = c(dX, dS, n, n_correct, pct_correct),
      names_glue = "{.value}_{group}"
    )
}
