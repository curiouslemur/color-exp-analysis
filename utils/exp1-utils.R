# library(tidyverse)
# library(dplyr)
# library(RColorBrewer)
# library(readr)
# library(tidytext)

# install.packages("pacman")

pacman::p_load(tidyverse,
               dplyr,
               RColorBrewer,
               reader,
               tidytext)

theme_studyBackground <- function(){
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = backgroundColor), 
        panel.grid.major = element_line(size = 0.5, linetype = 'dotted', colour = "grey"),
        panel.grid.minor = element_line(size = 0.5, linetype = 'dotted', colour = "#79797a")) 
}

#### ---------- Boostrap CI functions centered on refCountry
# Params:
## data: the data for one country
getMinDiffData <- function(data, conceptList, country, countryRef){ 
  a = data.frame()
  for (i in 1:length(conceptList)){
    # print(i)
    dataTmpCountry <- filter(data, concept == conceptList[i])
    dataTmpCountryRef <- filter(en3, wepEn == dataTmpLang$wepEn[1])
    
    ciTmp1 = boot.ci(boot(data=dataTmpLang$val, statistic=mean.fun, R=1000, sim="ordinary"))
    ciTmp2 = data.frame(y = var(dataTmpLang$val),
                        ymin = round( ciTmp1$bca[,4] , 2),
                        ymax = round( ciTmp1$bca[,5] , 2))
    
    aTmp <- data.frame(
      wep = wepList[i],
      wepEn = dataTmpLang$wepEn[1],
      meanLang = mean(dataTmpLang$val),
      meanEn = mean(dataTmpEn$val),
      ymin = ciTmp2$ymin- mean(dataTmpEn$val),
      ymax = ciTmp2$ymax- mean(dataTmpEn$val)
    )
    a <- rbind(a, aTmp)
  }
  a$meanDiff <- a$meanLang - a$meanEn
  a$lang <- language
  return(a)
}

#### ---------- Boostrap CI functions centered on English - FROM Language exp. w2v word2vis
getMinDiffData <- function(data, wepList, language){
  a = data.frame()
  for (i in 1:length(wepList)){
    # print(i)
    dataTmpLang <- filter(data, wep == wepList[i])
    dataTmpEn <- filter(en3, wepEn == dataTmpLang$wepEn[1])
    
    ciTmp1 = boot.ci(boot(data=dataTmpLang$val, statistic=mean.fun, R=1000, sim="ordinary"))
    ciTmp2 = data.frame(y = var(dataTmpLang$val),
                        ymin = round( ciTmp1$bca[,4] , 2),
                        ymax = round( ciTmp1$bca[,5] , 2))
    
    aTmp <- data.frame(
      wep = wepList[i],
      wepEn = dataTmpLang$wepEn[1],
      meanLang = mean(dataTmpLang$val),
      meanEn = mean(dataTmpEn$val),
      ymin = ciTmp2$ymin- mean(dataTmpEn$val),
      ymax = ciTmp2$ymax- mean(dataTmpEn$val)
    )
    a <- rbind(a, aTmp)
  }
  a$meanDiff <- a$meanLang - a$meanEn
  a$lang <- language
  return(a)
}