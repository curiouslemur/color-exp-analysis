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

##### List of concepts used for experiment 1 (final set of 14 concepts)
conceptListEn = c('banana', 'mango', 'peach', 'death', 'justice', 'peace', 'safety',
                   'angry', 'happy', 'sad', 'sick', 'lightning', 'sandstorm', 'tree')
conceptListFr = c('banane', 'mangue', 'pêche (fruit)', 'mort', 'justice', 'paix', 'sécurité',
                   'en colère', 'heureux', 'triste', 'malade', 'foudre', 'tempête de sable', 'arbre')

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

