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

