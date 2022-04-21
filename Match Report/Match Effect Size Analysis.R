
# # POSITION LEVEL ANALYSIS -----------------------------------------------------------
# 
# ## Match Effect Size / Rank ---------------------------------------------------------
#   
#   positionMatchCohensD <- teamData %>% 
#     group_by(type, opponent, match_date, position) %>%
#     summarise(.groups = 'keep', across(one_of(metricNameFull), .fns = ~ sum(.x) / sum(dur))) %>% 
#     pivot_longer(cols = one_of(metricNameFull), names_to = 'metric') %>% 
#     group_by(type, position, metric) %>% 
#     summarise(.groups = 'keep', 
#       mean = mean(value), 
#       sd = sd(value)
#     ) %>% 
#     pivot_wider(names_from = type, values_from = c(mean, sd)) %>% 
#     select(position, metric, mean_comparison, mean_current, sd = sd_comparison) %>% 
#     mutate(
#       d = (mean_current - mean_comparison)/sd,
#       r = effectsize::interpret_cohens_d(d)
#     ) 
#   
#   #Rank
#   positionMatchRank <- teamData %>% 
#     group_by(type, opponent, match_date, position) %>%
#     summarise(.groups = 'keep', across(one_of(metricNameFull), .fns = ~ sum(.x) / sum(dur))) %>% 
#     group_by(position) %>% 
#     mutate(
#       n = n(),
#       across(one_of(metricNameFull), ~rank(-.x))
#     ) %>% 
#     filter(type == 'current') %>% 
#     select(-type, -opponent, -match_date) %>% 
#     pivot_longer(cols = one_of(metricNameFull), values_to = 'rank', names_to = 'metric')
#   
#   positionMatch <-  left_join(positionMatchCohensD, positionMatchRank, by = c("metric", "position")) %>% 
#     mutate(dur = matchDur %>% filter(match_date == matchDate) %>% pull(full_match))
#   
# ## Half Effect Size / Rank ---------------------------------------
#   
#   #Cohens 
#   positionHalfCohensD <- teamData %>% 
#     group_by(type, opponent, match_date, position, match_half) %>%
#     summarise(.groups = 'keep', across(one_of(metricNameFull), .fns = ~ sum(.x) / sum(dur))) %>% 
#     pivot_longer(cols = one_of(metricNameFull), names_to = 'metric') %>% 
#     group_by(type, match_half, position, metric) %>% 
#     summarise(.groups = 'keep', 
#       mean = mean(value), 
#       sd = sd(value)
#     ) %>% 
#     pivot_wider(names_from = type, values_from = c(mean, sd)) %>% 
#     select(position, match_half, metric, mean_comparison, mean_current, sd = sd_comparison) %>% 
#     mutate(
#       d = (mean_current - mean_comparison)/sd,
#       r = effectsize::interpret_cohens_d(d)
#     ) 
#   
#   positionHalfRank <- teamData %>% 
#     group_by(type, opponent, match_date, match_half, position) %>%
#     summarise(.groups = 'keep', across(one_of(metricNameFull), .fns = ~ sum(.x) / sum(dur))) %>% 
#     group_by(match_half, position) %>% 
#     mutate(
#       n = n(),
#       across(one_of(metricNameFull), ~rank(-.x))
#     ) %>% 
#     filter(type == 'current') %>% 
#     select(-type, -opponent, -match_date) %>% 
#     pivot_longer(cols = one_of(metricNameFull), values_to = 'rank', names_to = 'metric')
#   
#   positionHalf <-  left_join(positionHalfCohensD, positionHalfRank, by = c("metric", "match_half", "position")) %>% 
#     left_join(
#       x = .,
#       y = matchDur %>%
#         filter(match_date == matchDate) %>% 
#         select(`FIRST HALF` = first_half, `SECOND HALF` = second_half) %>% 
#         pivot_longer(cols = c(`FIRST HALF`, `SECOND HALF`), names_to = 'match_half', values_to = 'dur'),
#       by = c('match_half')
#     )
#   
#   
# 
# ## Half Difference ---------------------------------------------------------
# 
#   positionHalfDiffCohensD <- teamData %>% 
#     group_by(type, match_date, match_half, position) %>%
#     summarise(.groups = 'keep', across(one_of(metricNameFull), .fns = ~ sum(.x) / sum(dur))) %>% 
#     pivot_longer(cols = one_of(metricNameFull), names_to = 'metric') %>% 
#     pivot_wider(names_from = match_half, values_from = value) %>% 
#     rename(first_half = `FIRST HALF`, second_half = `SECOND HALF`) %>% 
#     mutate(pct = second_half / first_half) %>% 
#     group_by(type, position, metric) %>% 
#     select(type, position, metric, pct) %>% 
#     summarise(.groups = 'keep', 
#       across(.cols = c("pct"), mean, .names = "mean"), 
#       across(.cols = c("pct"), sd, .names = "sd") 
#     ) %>% 
#     pivot_wider(names_from = type, values_from = c(mean, sd)) %>% 
#     select(position, metric, mean_comparison, mean_current, sd = sd_comparison) %>% 
#     mutate(
#       d = (mean_current - mean_comparison)/sd,
#       r = effectsize::interpret_cohens_d(d)
#     )
#   
#   positionHalfDiffRank <- teamData %>% 
#     group_by(type, opponent, match_date, match_half, position) %>%
#     summarise(.groups = 'keep', across(one_of(metricNameFull), .fns = ~ sum(.x) / sum(dur))) %>% 
#     pivot_longer(cols = one_of(metricNameFull), names_to = 'metric') %>% 
#     pivot_wider(names_from = match_half, values_from = value) %>% 
#     rename(first_half = `FIRST HALF`, second_half = `SECOND HALF`) %>% 
#     mutate(pct = second_half / first_half) %>% 
#     select(type,position, match_date, opponent, metric, pct) %>%
#     group_by(position, metric) %>% 
#     mutate(
#       n = n(),
#       rank = rank(-pct)
#     ) %>% 
#     filter(type == 'current') %>% 
#     select(position, metric, n, pct, rank)
#   
#   positionHalfDiff <- left_join(positionHalfDiffCohensD, positionHalfDiffRank, by = c('metric', 'position'))
#   

# TEAM VISUALIZATIONS ------------------------------------------------------

  ## Data Tables ----------------------------------------------------------
  #Matches Compared To 
  matchComparisonNames <- comparisonMatch %>% pull(match_name) %>% unique() 

  #Current Match
  matchCurrentName <- currentMatch %>% pull(match_name) %>% unique()

  ## !!! Add visReactable fun ---------
  teamMatchDT <- teamMatch %>% 
    mutate(match_half = 'FULL MATCH') %>% 
    select(match_half, everything()) %>% 
    bind_rows(., teamHalf) %>% 
    nest(data = -match_half) %>% 
    mutate(
      dt = map(.x = data, .f = function(x){
        x %>% 
          ungroup() %>% 
          mutate(
            metric_name = metric,
            metric_value = round(mean_current*dur,2),
            metric_value_min = round(mean_current,2),
            metric_comparison_min = paste0(round(mean_comparison,2), " ± ", round(sd,2)),
            effect_size = paste0(round(d,1), " ", r),
            rank = paste0(rank, "/", n)
          ) %>% 
          select(metric_name, metric_value, metric_value_min, metric_comparison_min, effect_size, rank) %>% 
          reactable(style = list(fontSize = "14px"))
      })
    )
  
  teamHalfDiffDT <- teamHalfDiff %>% 
    ungroup() %>% 
    mutate(
      metric_name = metric,
      metric_pct_diff = round(mean_current,2),
      metric_comparison = paste0(round(mean_comparison,2), " ± ", round(sd,2)),
      effect_size = paste0(round(d,1), " ", r),
      rank = paste0(rank, "/", n)
    ) %>% 
    select(metric_name, metric_pct_diff, metric_comparison, effect_size, rank) %>% 
    reactable(style = list(fontSize = "14px"))
  

  ## Plotly ----------------------------------------------------------------------------
  teamMatchGGData1 <- bind_rows(
    x = teamData %>% 
      mutate(match_name = paste0(opponent," : ",match_date)) %>% 
      group_by(type, position, name_display, match_name) %>%
      summarise(.groups = 'keep', 
                dur = sum(dur),
                across(one_of(metricNameFull), .fns = sum)) %>% 
      filter(dur > 75) %>% 
      group_by(type, match_name) %>%
      summarise(.groups = 'keep', 
                across(one_of(metricNameFull), .fns  = ~ sum(.x) / sum(dur))
      ) %>% 
      mutate(match_half = 'FULL MATCH') %>% 
      pivot_longer(cols = one_of(metricNameFull), names_to = 'metric'),
    y = teamData %>% 
      mutate(match_name = paste0(opponent," : ",match_date)) %>% 
      group_by(type, position, name_display, match_name, match_half) %>%
      summarise(.groups = 'keep', 
                dur = sum(dur),
                across(one_of(metricNameFull), .fns = sum)) %>% 
      filter(dur > 30) %>% 
      group_by(type, match_name, match_half) %>%
      summarise(.groups = 'keep', 
                across(one_of(metricNameFull), .fns  = ~ sum(.x) / sum(dur))
      ) %>% 
      pivot_longer(cols = one_of(metricNameFull), names_to = 'metric')
    )  %>% 
    mutate(
      type = ifelse(type == 'comparison', 'Comparative Matches', currentMatch %>% pull(match_name) %>% unique()) %>% factor(., levels = c('Comparative Matches', currentMatch %>% pull(match_name) %>% unique())),
      match_half = factor(match_half, levels = c('FULL MATCH', 'FIRST HALF', 'SECOND HALF')),
      metric = factor(metric, levels = c("dist", "hml", "hsr", "exp", "spr"))
    ) 
  
  teamMatchGGData2 <- teamMatchGGData1 %>% 
    filter(type == 'Comparative Matches') %>% 
    group_by(match_half, metric) %>% 
    summarise(.group = 'keep',
              n = n(), 
              mn = mean(value),
              sd = sd(value)
    ) %>% 
    ungroup() %>% 
    mutate(
      mnMinusSd = mn-sd,
      mnPlusSd = mn+sd
    )
  
  teamMatchGGData <- left_join(teamMatchGGData1, teamMatchGGData2, by = c("match_half", "metric"))

  namedLimits <- c("Normal Range", "Comparative Matches", matchCurrentName)
  
  gg <- ggplot(data = teamMatchGGData) + 
    scale_x_discrete() +
    geom_rect(
      aes(
        xmin = as.numeric(match_half)-.07,
        xmax = as.numeric(match_half)+.07,
        ymin = mnMinusSd,
        ymax = mnPlusSd,
        fill = "Normal Range",
        shape = "Normal Range",
        color = "Normal Range",
        size = "Normal Range",
      ),
      alpha = .25
    ) +
    geom_point(
      aes(
        x = match_half,
        y = value,
        text = paste0(match_name, "<br>",round(value,1)),
        shape = type,
        fill = type,
        color = type,
        size = type
      )
    ) +
    facet_wrap(~metric,nrow = 5, scales = 'free_y', labeller = as_labeller(metricLabelLongPerMinute)) +
    scale_color_manual(
      name = 'Legend',
      guide = "none",
      limits = namedLimits,
      values = c(
        alpha('#828282',0), #Normal Range 
        alpha('#212844',.1), #Comparative Matches
        alpha('#212844',1)
      )
    ) +
    scale_fill_manual(
      name = 'Legend',
      limits = namedLimits,
      values = c(
        alpha('#828282',.8), #Normal Range
        alpha('#212844',.1), #comparative Matches
        alpha('#212844',1)
      )
    ) +
    scale_size_manual(
      name = 'Legend',
      limits = namedLimits,
      guide = "none",
      values=c(
        NA, #Normal Range
        2.5, #comparative Matches
        3
      )
    ) +
    scale_shape_manual(
      guide = "none",
      name = 'Legend',
      limits = namedLimits,
      values = c(
        NA, #Normal Range
        16, #comparative Matches
        23
      ),
    ) +
    theme_bw() +
    theme(
      axis.title.y=element_blank(),
      axis.title.x=element_blank(),
      strip.background =element_rect(fill="#212844"),
      strip.text = element_text(colour = 'white')
    ) 
  
  teamMatchPP <- ggplotly(gg,height = 600, tooltip = "text") %>% 
    layout(legend = list(title = NA, orientation = 'h', x = 0, y = 1.1)) %>% 
    config(displaylogo = FALSE) 
  ## Save --------------------------------------------------------------------

  saveRDS(teamMatchDT, 'Match Report/vis/teamMatchDT.rds')
  
  saveRDS(teamMatchPP, 'Match Report/vis/teamMatchPP.rds')
  
  saveRDS(teamHalfDiffDT, 'Match Report/vis/teamHalfDiffDT.rds')

  
  # POSITION VISUALIZATIONS ------------------------------------------------------
  
  ## Data Tables ----------------------------------------------------------
  #Matches Compared To
  comparisonMatch %>% pull(match_name) %>% unique() 
  comparisonMatch %>% pull(match_name) %>% unique() %>% length()
  
  # !!!! add vis reactable ------ 
  #Current Match
  currentMatch %>% pull(match_name) %>% unique()
  
  #Format Full Match Table
  teamMatchDT <- positionMatch %>% 
    mutate(
      value = round(mean_current,2),
      comparison = paste0(round(mean_comparison,2), " ± ", round(sd,2)),
      effect_size = paste0(round(d,1), " ", r)
    ) %>% 
    select(position, metric, value, comparison, effect_size, rank)  %>% 
    nest(data = -position) %>% 
    mutate(
      dt = map(.x = data, .f = function(.x){
        DT::datatable(.x, 
                      extensions = 'Buttons',
                      options = list(
                        dom = '',
                        pageLength = 5
                      )
        )
      })
    )
    
  
  #First Half
  positionHalfDT <- positionHalf %>% 
    ungroup() %>% 
    mutate(
      value = round(mean_current,2),
      comparison = paste0(round(mean_comparison,2), " ± ", round(sd,2)),
      effect_size = paste0(round(d,1), " ", r)
    ) %>% 
    select(match_half, position, metric, value, comparison, effect_size, rank) %>% 
    nest(data = -c(position, match_half)) %>% 
    mutate(dt = map(.x = data, .f = function(.x){
      DT::datatable(.x, 
                    extensions = 'Buttons',
                    options = list(
                      dom = '',
                      pageLength = 5
                    )
      )
    }))

  
  #Half Percent Difference
  positionHalfDiffDT <- positionHalfDiff %>% 
    ungroup() %>% 
    mutate(
      value = round(pct,2),
      comparison = paste0(round(mean_comparison,2), " ± ", round(sd,2)),
      effect_size = paste0(round(d,1), " ", r)
    ) %>% 
    select(position, metric, value, comparison, effect_size, rank) %>% 
    nest(data = -position) %>% 
    mutate(dt = map(.x = data, .f = function(.x){
      DT::datatable(.x, 
                    extensions = 'Buttons',
                    options = list(
                      dom = '',
                      pageLength = 5
                    )
      )
    }))
  
  positionHalfDiffDT$dt
    
  
  ## Plotly ----------------------------------------------------------------------------
  #Full Match Comparison -- 
  positionMatchGGData <- bind_rows(
    x = teamData %>% 
      mutate(match_name = paste0(opponent," : ",match_date)) %>% 
      group_by(type, position, match_name) %>%
      summarise(.groups = 'keep',
                across(one_of(metricNameFull), .fns = ~ sum(.x) / sum(dur))
      ) %>% 
      pivot_longer(cols = one_of(metricNameFull), names_to = 'metric') %>% 
      mutate(match_half = 'FULL MATCH') , 
    y = teamData %>% 
      mutate(match_name = paste0(opponent," : ",match_date)) %>% 
      group_by(type, match_name, position, match_half) %>%
      summarise(.groups = 'keep',
                across(one_of(metricNameFull), .fns = ~ sum(.x) / sum(dur))
      ) %>% 
      pivot_longer(cols = one_of(metricNameFull), names_to = 'metric')
  )  %>% 
    mutate(
      type = ifelse(type == 'comparison', 'Comparative Matches', currentMatch %>% pull(match_name) %>% unique()) %>% factor(., levels = c('Comparative Matches', currentMatch %>% pull(match_name) %>% unique())),
      match_half = factor(match_half, levels = c('FULL MATCH', 'FIRST HALF', 'SECOND HALF')),
      metric = factor(metric, levels = c("dist", "hml", "hsr", "exp", "spr"))
    ) 
  
 positionMatchPP <-  positionMatchGGData %>% 
    nest(data = -position) %>% 
    mutate(pp = map(.x = data, .f = function(.x){

      gg <-  ggplot(data = .x) + 
        geom_point(
          aes(
            x = match_half, 
            y = value, 
            text = paste0(match_name, "<br>",round(value,1)),
            shape = type, 
            fill = type,
            color = type
          )
        ) +
        facet_wrap(~metric,nrow = 5, scales = 'free_y') +
        scale_color_manual(values=c(alpha('#212844',.1), alpha('#212844',1))) +
        scale_fill_manual(values= c(alpha('#212844',.1), alpha('#212844',1))) +
        scale_shape_manual(values = c(16,23)) + 
        theme(axis.title.y=element_blank())
      
      ggplotly(gg, tooltip = "text") %>% 
        layout(legend = list(orientation = 'h', x = 0, y = 1.1)) %>% 
        config(displaylogo = FALSE)
      
    }))
    
  positionMatchPP$pp
  
  
  # ATHLETE VISUALIZATIONS ------------------------------------------------------
  
  ## Data Tables ----------------------------------------------------------

  #Format Full Match Table
  athleteMatchDT <- athleteMatch %>%
    ungroup() %>% 
    mutate(match_half = "FULL MATCH") %>% 
    bind_rows(., athleteHalf) %>% 
    mutate(
      metric_name = case_when(
        metric == "dist" ~ "TD",
        metric == "hml" ~ "TID",
        metric == "hsr" ~ "HSR",
        metric == "exp" ~ "Ac/DcD",
        metric == "spr" ~ "SprD",
      ),
      metric_unit = case_when(
        metric == "dist" ~ "m",
        metric == "hml" ~ "m > 25.5 w/kg",
        metric == "hsr" ~ "m > 5.5 m/s",
        metric == "exp" ~ "m > ±3 m/s²",
        metric == "spr" ~ "m > 7.0 m/s"
      ),
      value_min = value,
      value_total = value*dur,
      pct_dif_effect = paste0(round((value-mean_comparison)/mean_comparison * 100,0), "% ", r),
      rank2 = glue("{rank}/{n}")
    ) %>%
    mutate(comparison2 = ifelse(metric == "spr",
          glue("{round(mean_comparison,1)} ± {round(sd,1)}"),
          glue("{round(mean_comparison,0)} ± {round(sd,0)}")
      )
    ) %>% 
    mutate(
      pct_dif_effect = ifelse(match_half == 'FULL MATCH',
                              ifelse(dur <= 75, NA,  pct_dif_effect),
                              ifelse(dur <= 30, NA, pct_dif_effect)
      ),
      rank2 = ifelse(match_half == 'FULL MATCH',
                     ifelse(dur <= 75, NA, rank2),
                     ifelse(dur <= 30, NA,  rank2)
      )
    ) %>% 
    select(
      match_half,
      position,
      name_display,
      metric_name, 
      metric_unit, 
      dur, 
      value_total, 
      value_min, 
      comparison2, 
      rank2, 
      pct_dif_effect, 
      d
    ) %>% 
    nest(data = -c(position, match_half, name_display)) %>% 
    mutate(dt = map(.x = data, .f = ~visReactable(.x)))
  
  athleteMatchDT$dt[9][[1]]

  #!!!!Half Percent Difference -----
  athleteHalfDiffDT <- athleteHalfDiff %>% 
    ungroup() %>% 
    mutate(
      metric_name = case_when(
        metric == "dist" ~ "TD",
        metric == "hml" ~ "TID",
        metric == "hsr" ~ "HSR",
        metric == "exp" ~ "Ac/DcD",
        metric == "spr" ~ "SprD",
      ),
      metric_unit = case_when(
        metric == "dist" ~ "m",
        metric == "hml" ~ "m > 25.5 w/kg",
        metric == "hsr" ~ "m > 5.5 m/s",
        metric == "exp" ~ "m > ±3 m/s²",
        metric == "spr" ~ "m > 7.0 m/s"
      ),
      rank2 = ifelse(is.na(rank), NA, glue("{rank}/{n}")),
      pct_dif_effect = ifelse(is.na(pct),NA,paste0(round((pct-mean_comparison)/mean_comparison * 100,0), "% ", r)),
      comparison_pct = glue("{paste0(round(mean_comparison*100,0),'%')} ± {paste0(round(sd*100,0),'%')}")
    ) %>% 
    select(
      position, 
      name_display, 
      metric_name, 
      metric_unit, 
      pct, 
      comparison_pct, 
      rank2, 
      pct_dif_effect, 
      d
    ) %>% 
    nest(data = -c(name_display, position)) %>% 
    mutate(dt = map(.x = data, .f = ~visReactable(.x)))
  
    athleteHalfDiffDT$dt
  
  ## Plotly ----------------------------------------------------------------------------
  #Full Match Comparison -- 
  athleteMatchGGData <- bind_rows(
    x = teamData %>% 
      mutate(match_name = paste0(opponent," : ",match_date)) %>% 
      group_by(type, position, name_display, match_name) %>%
      summarise(.groups = 'keep', 
                dur = sum(dur),
                across(one_of(metricNameFull), .fns = ~ sum(.x) / sum(dur))) %>% 
      pivot_longer(cols = one_of(metricNameFull), names_to = 'metric') %>% 
      mutate(match_half = 'FULL MATCH'), #%>% 
      # filter(dur > 75), 
    y = teamData %>% 
      mutate(match_name = paste0(opponent," : ",match_date)) %>% 
      group_by(type, match_name, position, match_half, name_display) %>%
      summarise(.groups = 'keep',
                dur = sum(dur),
                across(one_of(metricNameFull), .fns = ~ sum(.x) / sum(dur))
      ) %>% 
      pivot_longer(cols = one_of(metricNameFull), names_to = 'metric'), #%>% 
      # filter(dur > 30)
  )  %>% 
  mutate(
    type = ifelse(type == 'comparison', 'Comparative Matches', currentMatch %>% pull(match_name) %>% unique()) %>% factor(., levels = c('Comparative Matches', currentMatch %>% pull(match_name) %>% unique())),
    match_half = factor(match_half, levels = c('FULL MATCH', 'FIRST HALF', 'SECOND HALF')),
    metric = factor(metric, levels = c("dist", "hml", "hsr", "exp", "spr"))
  ) 
  
  athleteMatchPP <- athleteMatchGGData %>% 
    filter(type != 'Comparative Matches') %>% 
    nest(data = -c(name_display)) %>% 
    mutate(data = map(.x = data, .f = function(.x){
      bind_rows(
        positionMatchGGData %>% 
          filter(type == 'Comparative Matches') %>% 
          filter(position == unique(.x$position)) %>% 
          group_by(type, position, metric, match_half) %>% 
          mutate(
            sd = sd(value, na.rm = T),
            mn = mean(value, na.rm = T),
            mnPlusSd = mn+sd,
            mnMinusSd = mn-sd
          ),
        .x
      )
    })) %>% 
    mutate(pp = map(.x = data, .f = function(.x){
      

      .x1 <- .x %>% 
        filter(if (match_half == 'FULL MATCH' & type != 'Comparative Matches') dur >= 75 else T) %>%
        filter(if (match_half != 'FULL MATCH' & type != 'Comparative Matches') dur >= 30 else T)
      
      .x2 <- .x %>% 
        ungroup() %>% 
        filter(type == 'Comparative Matches') %>% 
        select(metric, match_half, mnPlusSd, mnMinusSd) %>% 
        group_by(metric, match_half) %>% 
        slice(1) 
      
      metricLabeller <- c(
        `dist` = "Total Distance per Min (m)",
        `hml` = "Intensive Dist per Min (m > 25.5 w/kg)",
        `hsr` = "High Int Dist per Min (m > 5.5 m/s)",
        `exp` = "Acc/Dec Dist per Min (m > ±3 m/s²)",
        `spr` = "Sprint Dist per Min (m > 7.0 m/s)"
      )
      
      colorValues <- c(
          alpha('#828282',0), #Normal Range 
          alpha('#212844',.1), #Comparative Matches
          alpha('#212844',1)
        )

       fillValues <-  c(
            alpha('#828282',.8), #Normal Range
            alpha('#212844',.1), #comparative Matches
            alpha('#212844',1)
          )

        sizeValues <- c(
            NA, #Normal Range
            2.5, #comparative Matches
            3
          )
        
        shapeValues <- c(
            NA, #Normal Range
            16, #comparative Matches
            23
          )
        
      namedLimits <- c("Normal Range", "Comparative Matches", "Mexico v USA : 2022-03-24")
        
        if (length(unique(.x1$type)) ==1){
          
          colorValues <- colorValues[1:2]
          
          fillValues <- fillValues[1:2]
          
          sizeValues <- sizeValues[1:2]
          
          shapeValues <- shapeValues[1:2]
          
          namedLimits <- namedLimits[1:2]
          
        }
      

      gg <- ggplot(data = .x1) + 
        scale_x_discrete() +
        geom_rect(
          aes(
            xmin = as.numeric(match_half)-.07,
            xmax = as.numeric(match_half)+.07,
            ymin = mnMinusSd,
            ymax = mnPlusSd,
            fill = "Normal Range",
            shape = "Normal Range",
            color = "Normal Range",
            size = "Normal Range",
          ),
          alpha = .4
        ) +
        geom_point(
          aes(
            x = match_half,
            y = value,
            text = paste0(match_name, "<br>",round(value,1)),
            shape = type,
            fill = type,
            color = type,
            size = type
          )
        ) +
        facet_wrap(~metric,nrow = 5, scales = 'free_y', labeller = as_labeller(metricLabeller)) +
        scale_color_manual(
          name = 'Legend',
          guide = "none",
          limits = namedLimits,
          values = colorValues
        ) +
        scale_fill_manual(
          name = 'Legend',
          limits = namedLimits,
          values = fillValues
        ) +
        scale_size_manual(
          name = 'Legend',
          limits = namedLimits,
          guide = "none",
          values = sizeValues
        ) +
        scale_shape_manual(
          guide = "none",
          name = 'Legend',
          limits = namedLimits,
          values = shapeValues,
        ) +
        theme_bw() +
        theme(
          axis.title.y=element_blank(),
          axis.title.x=element_blank(),
          strip.background =element_rect(fill="#212844"),
          strip.text = element_text(colour = 'white')
        ) 
        
      ggplotly(gg,height = 600, tooltip = "text") %>% 
        layout(legend = list(title = NA, orientation = 'h', x = 0, y = 1.1)) %>% 
        config(displaylogo = FALSE) 
      
    }))
  
  athleteMatchPP$pp

  
     ## Save -----------------------------------------------------------------------------
  


  saveRDS(athleteMatchPP, "Match Report/vis/athleteMatchPP.rds")
  saveRDS(athleteHalfDiffDT, "Match Report/vis/athleteHalfDiffDT.rds")
  # saveRDS(athleteHalfDT, "Match Report/vis/athleteHalfDT.rds")
  saveRDS(athleteMatchDT,  "Match Report/vis/athleteMatchDT.rds")
  
  
  # MAIN REPORT ----------------------------------------------------
  
  ## Data Set Up -------------------------------------------------
  
  athleteHalfDiff
  athleteMatch
  
  mainReportDT1 <- matchData %>% 
    filter(match_date == matchDate) %>% 
    mutate(
      dist_min= dist / dur,
      hml_min= hml / dur, 
      min = dur 
    ) %>% 
    select(
      position, 
      match_half, 
      name_display, 
      min, 
      dur,
      vel_max,
      dist, 
      hml, 
      hsr, 
      spr,
      exp, 
      dist_min, 
      hml_min,
      acc, 
      dec, 
    ) %>% 
    pivot_longer(., cols = -c(position, match_half, name_display, dur), names_to = 'metric_name') %>% 
    pivot_wider(., names_from = match_half, values_from = c(dur, value))  %>% 
    rename(
      durFirstHalf = `dur_FIRST HALF`, 
      durSecondHalf = `dur_SECOND HALF`,
      valueFirstHalf = `value_FIRST HALF`,
      valueSecondHalf = `value_SECOND HALF`
    ) %>% 
    mutate(
      durFullMatch = max(durFirstHalf, na.rm = T)+max(durSecondHalf, na.rm = T),
      durAthMatch = durFirstHalf+durSecondHalf
    ) %>% 
    mutate(
      total = ifelse(metric_name == 'vel_max', pmax(`valueFirstHalf`,`valueSecondHalf`, na.rm = F), `valueFirstHalf`+`valueSecondHalf`), 
      pctDiff = round((`valueSecondHalf` - `valueFirstHalf`)/`valueFirstHalf`*100,0)
    ) %>% 
    mutate(
      pctDiff = ifelse(durFullMatch != durAthMatch, NA, pctDiff)
    )  %>% 
    select(-c(durFirstHalf, durSecondHalf, durFullMatch, durAthMatch)) %>% 
    left_join(
      x = ., 
      y = athleteMatchDT %>% select(-dt, -position) %>% unnest(data) %>% 
            select(match_half, name_display, metric_name, d) %>% 
            mutate(metric_name = case_when(
              metric_name == "Dist" ~ "dist",
              metric_name == "TID" ~ "hml",
              metric_name == "HID" ~ "hsr",
              metric_name == "Acc/Dec Dist" ~ "exp",
              metric_name == "Sprint Dist" ~ "spr"
            )) %>% 
            pivot_wider(names_from = match_half, values_from = d),
      by = c("name_display", "metric_name")
    ) %>% 
    rename(dFull = `FULL MATCH`, dFirst = `FIRST HALF`, dSecond = `SECOND HALF`) %>% 
    left_join(
      x = ., 
      y = athleteHalfDiff %>% select(name_display, metric_name = metric, dHalfDiff = d),
      by = c('name_display', 'metric_name')
    ) %>% 
    mutate(
      metric_name = case_when(
        metric_name == "min" ~ "Min",
        metric_name == "vel_max" ~ "Max spr",
        metric_name == "dist" ~ "TD",
        metric_name == "hml" ~ "TID",
        metric_name == "hsr" ~ "HSR",
        metric_name == "spr" ~ "Spr D",
        metric_name == "exp" ~ "Exp D",
        metric_name == "dist_min" ~ "TD/Min",
        metric_name == "hml_min" ~ "TID/Min",
        metric_name == "acc" ~ "Accels",
        metric_name == "dec" ~ "Decels"
      )
    ) %>% 
    nest(data = -name_display) 
  
  mainReportDTEmpty <- mainReportDT1 %>% 
    slice(1) %>% 
    unnest(cols = data) %>% 
    mutate(
      name_display = "", 
      position = NA,
      valueFirstHalf = NA, 
      valueSecondHalf = NA,
      total = NA,
      pctDiff = NA,
      dFull = NA,
      dFirst = NA, 
      dSecond = NA,
      dHalfDiff = NA
    ) %>% 
    nest(data = -c(name_display))
  
  mainReportDT <- mainReportDT1 %>% 
    bind_rows(., mainReportDTEmpty) %>% 
    mutate(
      dt = map(.x = data, .f = function(data){
        data %>% 
          # slice(1:9) %>% 
          reactable(data = ., 
            striped = TRUE,
            highlight = TRUE,
            bordered = TRUE,
            defaultPageSize = 11,
            # width = 260,
            # height = 345,
            theme = reactableTheme(
              headerStyle = list(color = '#212844', fontSize = '12px'),
              borderColor = "#B5B8BA",
              # stripedColor = "#E5E7F2",
              highlightColor = "#BCBEC7",
              cellPadding = "1px 1px",
              style = list(fontSize = '12px')
            ),
            defaultColDef = colDef(
              align = "center",
              width = 50,
              na = "-"
            ),
            columns = list(
              position = colDef(show = F),
              dFull = colDef(show = F),
              dFirst = colDef(show = F),
              dSecond = colDef(show = F),
              dHalfDiff = colDef(show = F),
              metric_name = colDef(
                show = T,
                name = '',
                width = 55
              ), 
              valueFirstHalf = colDef(
                show = T,
                name = '1st',
                cell = function(value, index){
                    if (is.na(value)) return("-")
                    ifelse(.$metric_name[index] == 'spr', round(value,1), round(value,0))
                }, 
                style = function(value, index){
                  
                  metric <- .[index,]$metric_name
                  metricName <- case_when(
                      metric == "TD" ~ "TD",
                      metric == "TID" ~ "TID",
                      metric == "HSR" ~ "HSR",
                      metric == "Spr D" ~ "Spr D",
                      metric == "Exp D" ~ "Exp D",
                      metric == "TD/Min" ~ "TD",
                      metric == "TID/Min" ~ "TID"
                    )
                  
                  dur <- filter(., metric_name == "Min") %>% pull(`valueFirstHalf`) %>% 
                    ifelse(is.na(.), 0, .)
                  
                  if (is.na(metricName) | dur < 30) {
                    color <- '#777777' #Black
                    fontWeight <- 'normal'
                    return(list(color = color, fontWeight = fontWeight))
                  }
                  
                  d <- filter(.,metric_name == metricName) %>% pull(dFirst)
                  
                  if (is.na(d)) {
                    color <- '#777777' #Black
                    fontWeight <- 'normal'
                    return(list(color = color, fontWeight = fontWeight))
                  }

                  if (d  >= .8) {
                    color <- '#008000' #Green
                    fontWeight <- 'bold'
                  } else if (d <= -.8) {
                    color <- '#800000' #Red
                    fontWeight <- 'bold'
                  } else {
                    color <- '#777777' #Black
                    fontWeight <- 'normal'
                  }
                  return(list(color = color, fontWeight = fontWeight))

                }
              ),
              valueSecondHalf = colDef(
                show = T,
                name = '2nd',
                cell = function(value, index){
                    if (is.na(value)) return("-")
                    ifelse(.$metric_name[index] == 'spr', round(value,1), round(value,0))
                }, 
                style = function(value, index){
                    
                    metric <- .[index,]$metric_name
                    metricName <- case_when(
                      metric == "TD" ~ "TD",
                      metric == "TID" ~ "TID",
                      metric == "HSR" ~ "HSR",
                      metric == "Spr D" ~ "Spr D",
                      metric == "Exp D" ~ "Exp D",
                      metric == "TD/Min" ~ "TD",
                      metric == "TID/Min" ~ "TID"
                    )
                    
                    dur <- filter(., metric_name == "Min") %>% pull(`valueSecondHalf`)  %>% 
                      ifelse(is.na(.), 0, .)
                    
                    if (is.na(metricName) | dur < 30) {
                      color <- '#777777' #Black
                      fontWeight <- 'normal'
                      return(list(color = color, fontWeight = fontWeight))
                    }
                    
                    d <- filter(.,metric_name == metricName) %>% pull(dSecond)
                    
                    if (is.na(d)) {
                      color <- '#777777' #Black
                      fontWeight <- 'normal'
                      return(list(color = color, fontWeight = fontWeight))
                    }
                    
                    if (d  >= .8) {
                      color <- '#008000' #Green
                      fontWeight <- 'bold'
                    } else if (d <= -.8) {
                      color <- '#800000' #Red
                      fontWeight <- 'bold'
                    } else {
                      color <- '#777777' #Black
                      fontWeight <- 'normal'
                    }
                    return(list(color = color, fontWeight = fontWeight))
                    
          }
              ),
              total = colDef(
                show = T,
                name = 'Total',
                cell = function(value, index){
                    if (is.na(value)) return("-")
                    ifelse(.$metric_name[index] == 'spr', round(value,1), round(value,0))
                },
                style = function(value, index){
                  
                  metric <- .[index,]$metric_name
                  metricName <- case_when(
                    metric == "TD" ~ "TD",
                    metric == "TID" ~ "TID",
                    metric == "HSR" ~ "HSR",
                    metric == "Spr D" ~ "Spr D",
                    metric == "Exp D" ~ "Exp D",
                    metric == "TD/Min" ~ "TD",
                    metric == "TID/Min" ~ "TID"
                  )
                  
                  dur <- filter(., metric_name == "Min") %>% pull(`total`) %>% 
                    ifelse(is.na(.), 0, .)
                  
                  if (is.na(metricName) | dur < 75) {
                    color <- '#777777' #Black
                    fontWeight <- 'normal'
                    return(list(color = color, fontWeight = fontWeight))
                  }
                  
                  d <- filter(.,metric_name == metricName) %>% pull(dFull)
                  
                  if (is.na(d)) {
                    color <- '#777777' #Black
                    fontWeight <- 'normal'
                    return(list(color = color, fontWeight = fontWeight))
                  }
                  
                  if (d  >= .8) {
                    color <- '#008000' #Green
                    fontWeight <- 'bold'
                  } else if (d <= -.8) {
                    color <- '#800000' #Red
                    fontWeight <- 'bold'
                  } else {
                    color <- '#777777' #Black
                    fontWeight <- 'normal'
                  }
                  return(list(color = color, fontWeight = fontWeight))
                  
                }
              ),
              pctDiff = colDef(
                show = T,
                name = '%Diff',
                format = colFormat(suffix = "%", separators = FALSE, digits = 0),
                style = function(value, index){
                  
                  metric <- .[index,]$metric_name
                  metricName <- case_when(
                    metric == "TD" ~ "TD",
                    metric == "TID" ~ "TID",
                    metric == "HSR" ~ "HSR",
                    metric == "Spr D" ~ "Spr D",
                    metric == "Exp D" ~ "Exp D",
                    metric == "TD/Min" ~ "TD",
                    metric == "TID/Min" ~ "TID"
                  )
                  
                  dur <- filter(., metric_name == "Min") %>% pull(`total`) %>% 
                    ifelse(is.na(.), 0, .)
                  
                  if (is.na(metricName) | dur < 75) {
                    color <- '#777777' #Black
                    fontWeight <- 'normal'
                    return(list(color = color, fontWeight = fontWeight))
                  }
                  
                  d <- filter(.,metric_name == metricName) %>% pull(dHalfDiff)
                  
                  if (is.na(d)) {
                    color <- '#777777' #Black
                    fontWeight <- 'normal'
                    return(list(color = color, fontWeight = fontWeight))
                  }
                  
                  if (d  >= .8) {
                    color <- '#008000' #Green
                    fontWeight <- 'bold'
                  } else if (d <= -.8) {
                    color <- '#800000' #Red
                    fontWeight <- 'bold'
                  } else {
                    color <- '#777777' #Black
                    fontWeight <- 'normal'
                  }
                  return(list(color = color, fontWeight = fontWeight))
                  
                }
              )
            )
          )
        
      })
    )
  
  saveRDS(mainReportDT, "Match Report/vis/mainReportDT.rds")
  
  
  
  
  
