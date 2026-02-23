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
