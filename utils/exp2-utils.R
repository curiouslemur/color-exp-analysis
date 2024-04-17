## function to generate df for candidate colors and concepts
genCandidates <- function(df, concepts, nTopColor){
  tmp <- data.frame(); k = 1
  for (i in 1:(length(concepts)-1)){
    c1 <- concepts[i]
    c1df <- filter(df, concept == c1)
    m1 <- min(tail(sort(c1df$weight), nTopColor))
    # print(paste("i: ", i))
    # j = 5
    for (j in (i+1):(length(concepts))){
      c2 <- concepts[j]
      c2df <- filter(df, concept == c2)
      m2 <- min(tail(sort(c2df$weight), nTopColor)) # median weight, or mean, or any arbitrary threshold
      
      # colors highly rated in c1df 
      c1high <- filter(c1df, weight >= m1, weight >= 0.35)$color
      # from c1up, subset that are lowly associated for c2 (i.e. in c2df)
      c2low <- filter(c2df, color %in% c1high, round(weight, digits = 2) <= 0.25)$color
      c1high <- intersect(c1high, c2low)
      
      c2high <- filter(c2df, weight >= m2, weight >= 0.35)$color
      c1low <- filter(c1df, color %in% c2high, round(weight, digits = 2) <= 0.25)$color
      c2high <- intersect(c2high, c1low)
      
      c1df_ <- filter(c1df, color %in% union(c1high, c2high))
      c1df_$type <- "con1"
      
      c2df_ <- filter(c2df, color %in% union(c1high, c2high))
      c2df_$type <- "con2"
      
      cPair <- rbind(c1df_, c2df_)
      cPair$pairId = k
      
      tmp <- rbind(tmp, cPair)
      k <- k+1
    }
  }
  return(tmp)
}

## function to generate list of plots of the candidate colors and concepts
getListPlotCandidates <- function(tmp, conceptsOrd){
  tmp_ <- tmp %>% group_by(pairId) %>%
    filter(!max(weight) < 0.5) %>%
    mutate(nColors = n()/2, .groups = 'drop') %>% 
    filter(nColors >= 4)  # nColors == nombre de couleur unique dans le pair candidat
  
  tmp_$con = factor(tmp_$concept, levels=conceptsOrd) # to order the facet strips 
  tmp_ <- tmp_ %>% drop_na()
  
  plotList <- list(); l = 1
  for (p in unique(tmp_$pairId)){
    # p = unique(tmp_$pairId)[1]
    plot <- filter(tmp_, pairId == p) %>%
      ggplot(aes(reorder_within(color, weight, con), weight, fill = hex, color = 'black')) +
      geom_bar(stat = 'identity',size = 0.15, width = 0.80) + 
      # geom_errorbar( aes(x=reorder_within(color, weight, concept), ymin=weight-se, ymax=weight+se), width=0.4, colour="black", alpha=0.6, size=0.25)+
      
      scale_fill_identity() + scale_color_identity() + scale_x_reordered() +
      scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
      facet_wrap(~ con, scales = "free_x", ncol = 1) +
      labs(x = "", title = "") + theme_light() +
      theme(legend.position = "none",
            strip.text = element_text(size = 12, color = "black"), strip.background = element_blank(),
            plot.background = element_rect(color = "azure4", linewidth = 0.5),
            panel.spacing = unit(0.25, 'cm'))
    plotList[[l]] = plot
    l <- l+1
  }
  return(plotList)
}
