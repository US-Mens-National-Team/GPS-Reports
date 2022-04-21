
# VISUALIZATIONS -------------------------------------------------

## Main Reactable ---------
visReactable <- function(data) {
  
  headerFun <- function(header, subHeader, tooltip) {
    tagList(
      tags$abbr(
        style = "text-decoration: underline; text-decoration-style: dotted; cursor: help",
        style = "cursor: help", 
        title = tooltip, 
        header
      ), 
      tags$div(
        style= "color: #999; font-size: 10px;",
        subHeader
      )
    )
    
  }
  
  data %>% 
    reactable(data = ., 
              striped = TRUE,
              highlight = TRUE,
              bordered = TRUE,
              theme = reactableTheme(
                headerStyle = list(color = '#212844', fontSize = '14px'),
                borderColor = "#B5B8BA",
                stripedColor = "#E5E7F2",
                highlightColor = "#BCBEC7",
                cellPadding = "8px 12px",
                style = list(fontSize = '12px'),
                searchInputStyle = list(width = "100%"), 
                cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center")
              ),
              defaultColDef = colDef(
                align = "center"
              ),
              columns = list(
                #COLUMNS
                metric = colDef(show = F),
                metric_label = colDef(
                  show = T,
                  name = 'Metric Name',
                  minWidth = 125,
                  maxWidth = 175,
                  html = T,
                  header = headerFun('Metric Name', 'unit', 'GPS metric name abbreviated \n \n See \'Report Glossary\' for more information'),
                  cell = function(value, index){
                    div(
                      div(value),
                      div(style = list(fontSize = 12, fontWeight = 200, color = '#999'), .$metric_unit[index])
                    )
                  }
                ),
                metric_unit = colDef(show = F),
                v = colDef(
                  show = T,
                  name = "Match Value",
                  minWidth = 125,
                  maxWidth = 175,
                  html = TRUE,
                  header = headerFun('Current Match Value', 'Metric / Min', 'Average GPS metric per minute for the reported match'),
                  # format = colFormat(digits = 2), 
                  cell = function(value, index){
                    if (.[index,]$metric == 'spr') {
                      round(value, 1)
                    } else {
                      round(value, 0)
                    }
                  }
                ),
                v_pct = colDef(
                  show = T,
                  name = "Metric Value",
                  minWidth = 125,
                  maxWidth = 175,
                  html = TRUE,
                  header = headerFun('Currrent Match Value', 'Half % Diff', 'Percentage difference between first and second half'),
                  format = colFormat(percent = T, digits = 0)
                ),
                msd = colDef(
                  show = T,
                  name = "Comparison Value",
                  minWidth = 125,
                  maxWidth = 175,
                  html = TRUE,
                  header = headerFun('Comparison Match Value', 'Mean±SD / Min', 'Average ± standard deviation (normal range) of comparison matches'),
                  format = colFormat(digits = 2)
                ),
                msd_pct = colDef(
                  show = T,
                  name = "Comparison Value",
                  minWidth = 125,
                  maxWidth = 175,
                  html = TRUE,
                  header = headerFun('Comparison Match Value', 'Half % Diff Mean±SD', 'Average ± standard deviation (normal range) of comparison match first half / second half'),
                  format = colFormat(digits = 2)
                ),
                rank2 = colDef(
                  show = T,
                  name = "Match Rank",
                  minWidth = 125,
                  maxWidth = 175,
                  html = TRUE,
                  header = headerFun("Match Rank", 'Match / Comparison Matches', "Rank of reported match / number of matches in the sample"),
                  format = colFormat(digits = 2)
                ),
                dr = colDef(
                  show = T,
                  name = "Effect Statistic",
                  minWidth = 125,
                  maxWidth = 175,
                  header = headerFun("Effect Statistic", 'Standard difference between comparison', 
                                     tooltip = " Standardized differences between the current match and comparison matches \n \n < 0.1 - Tiny \n 0.1 to 0.2 - Very small \n 0.2 to 0.5 - Small \n 0.5 to 0.8 - Medium \n 0.8 to 1.2 - Large \n 1.2 to 2 - Very large \n > 2 - Huge" 
                  ),
                  style = function(value, index){
                    if (is.na(.[index,"d"])) {
                      color <- '#777777' #Black
                      fontWeight <- 'normal'
                    } else if (.[index,"d"] >= .8) {
                      color <- '#008000' #Green
                      fontWeight <- 'bold'
                    } else if (.[index,"d"] <= -.8) {
                      color <- '#800000' #Red
                      fontWeight <- 'bold'
                    } else {
                      color <- '#777777' #Black
                      fontWeight <- 'normal'
                    }
                    list(color = color, fontWeight = fontWeight)
                  }
                ),
                dr2 = colDef(
                  show = F,
                  name = "Effect Statistic",
                  minWidth = 125,
                  maxWidth = 175,
                  style = function(value, index){
                    if (is.na(.[index,"d"])) {
                      color <- '#777777' #Black
                      fontWeight <- 'normal'
                    } else if (.[index,"d"] >= .8) {
                      color <- '#008000' #Green
                      fontWeight <- 'bold'
                    } else if (.[index,"d"] <= -.8) {
                      color <- '#800000' #Red
                      fontWeight <- 'bold'
                    } else {
                      color <- '#777777' #Black
                      fontWeight <- 'normal'
                    }
                    list(color = color, fontWeight = fontWeight)
                  }
                ),
                dr3 = colDef(
                  show = F,
                  name = "Effect Statistic",
                  minWidth = 125,
                  maxWidth = 175,
                  style = function(value, index){
                    if (is.na(.[index,"d"])) {
                      color <- '#777777' #Black
                      fontWeight <- 'normal'
                    } else if (.[index,"d"] >= .8) {
                      color <- '#008000' #Green
                      fontWeight <- 'bold'
                    } else if (.[index,"d"] <= -.8) {
                      color <- '#800000' #Red
                      fontWeight <- 'bold'
                    } else {
                      color <- '#777777' #Black
                      fontWeight <- 'normal'
                    }
                    list(color = color, fontWeight = fontWeight)
                  }
                ),
                d = colDef(
                  show = F
                )
              )
    )
}

# TEAM VIS ----------------------------------------------------------------

teamMatchDT <- bind_rows(teamMatch, teamHalf) %>% 
  ungroup() %>% 
  mutate(
    metric = factor(metric, levels = metricNameFull),
    metric_label = metricLabelShortPerMinute[metric] %>% unname(), 
    metric_unit = metricLabelUnits[metric] %>% unname(),
    msd = ifelse(metric == "spr",
             glue("{round(m,1)} ± {round(sd,1)}"),
             glue("{round(m,0)} ± {round(sd,0)}")
    ),
    rank2 = glue("{rank}/{n}"),
    dr =  paste0(round(d, 1)," (", r,")"),
    dr2 = paste0(round((v-m)/m * 100,0), "% ", r),
    dr3 = paste0(round((2*d+5.5),1), " ", r)
  ) %>% 
  arrange(metric) %>% 
  select(metric, match_half, metric_label, metric_unit, v, msd, rank2, dr, dr2, dr3, d) %T>% print() %>%  
  nest(data = -match_half) %>% 
  mutate(dt = map(.x = data, .f = ~visReactable(.x))) 

teamHalfDiffDT <- teamHalfDiff %>% 
  ungroup() %>% 
  mutate(
    metric = factor(metric, levels = metricNameFull),
    metric_label = metricLabelShortPerMinute[metric] %>% unname(), 
    metric_unit = metricLabelUnits[metric] %>% unname(),
    msd_pct = glue("{round(m_pct*100,0)}% ± {round(sd*100,0)}%"),
    rank2 = glue("{rank}/{n}"),
    dr =  paste0(round(d, 1)," (", r,")")
  ) %>% 
  select(metric, metric_label, metric_unit, v_pct, msd_pct, rank2, dr, d) %T>% print() %>%  
  visReactable()


## Plotly ----------------------------------------------------------------------------
  teamMatchGGData1 <- bind_rows(
    x = matchAnalysisData %>% 
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
    y = matchAnalysisData %>% 
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
    type = ifelse(type == 'comparison', 'Comparative Matches', currentMatchData %>% pull(match_name) %>% unique()) %>% factor(., levels = c('Comparative Matches', currentMatchData %>% pull(match_name) %>% unique())),
    match_half = factor(match_half, levels = c('FULL MATCH', 'FIRST HALF', 'SECOND HALF')),
    metric = factor(metric, levels = c("dist", "hml", "hsr", "exp", "spr"))
  ) 

teamMatchGGData2 <- teamMatchGGData1 %>% 
  filter(type == 'Comparative Matches') %>% 
  group_by(match_half, metric) %>% 
  summarise(.groups = 'keep',
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

namedLimits <- c("Normal Range", "Comparative Matches", currentMatchData %>% pull(match_name) %>% unique())

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
  config(displaylogo = FALSE) %T>% print() 
## Save --------------------------------------------------------------------

saveRDS(teamMatchDT, 'Match Report/vis/teamMatchDT.rds')

saveRDS(teamHalfDiffDT, 'Match Report/vis/teamHalfDiffDT.rds')

saveRDS(teamMatchPP, 'Match Report/vis/teamMatchPP.rds')


# # POSITION VISUALIZATIONS ------------------------------------------------------
# 
# ## Data Tables ----------------------------------------------------------
# #Matches Compared To
# comparisonMatch %>% pull(match_name) %>% unique() 
# comparisonMatch %>% pull(match_name) %>% unique() %>% length()
# 
# # !!!! add vis reactable ------ 
# #Current Match
# currentMatch %>% pull(match_name) %>% unique()
# 
# #Format Full Match Table
# teamMatchDT <- positionMatch %>% 
#   mutate(
#     value = round(mean_current,2),
#     comparison = paste0(round(mean_comparison,2), " ± ", round(sd,2)),
#     effect_size = paste0(round(d,1), " ", r)
#   ) %>% 
#   select(position, metric, value, comparison, effect_size, rank)  %>% 
#   nest(data = -position) %>% 
#   mutate(
#     dt = map(.x = data, .f = function(.x){
#       DT::datatable(.x, 
#                     extensions = 'Buttons',
#                     options = list(
#                       dom = '',
#                       pageLength = 5
#                     )
#       )
#     })
#   )
# 
# 
# #First Half
# positionHalfDT <- positionHalf %>% 
#   ungroup() %>% 
#   mutate(
#     value = round(mean_current,2),
#     comparison = paste0(round(mean_comparison,2), " ± ", round(sd,2)),
#     effect_size = paste0(round(d,1), " ", r)
#   ) %>% 
#   select(match_half, position, metric, value, comparison, effect_size, rank) %>% 
#   nest(data = -c(position, match_half)) %>% 
#   mutate(dt = map(.x = data, .f = function(.x){
#     DT::datatable(.x, 
#                   extensions = 'Buttons',
#                   options = list(
#                     dom = '',
#                     pageLength = 5
#                   )
#     )
#   }))
# 
# 
# #Half Percent Difference
# positionHalfDiffDT <- positionHalfDiff %>% 
#   ungroup() %>% 
#   mutate(
#     value = round(pct,2),
#     comparison = paste0(round(mean_comparison,2), " ± ", round(sd,2)),
#     effect_size = paste0(round(d,1), " ", r)
#   ) %>% 
#   select(position, metric, value, comparison, effect_size, rank) %>% 
#   nest(data = -position) %>% 
#   mutate(dt = map(.x = data, .f = function(.x){
#     DT::datatable(.x, 
#                   extensions = 'Buttons',
#                   options = list(
#                     dom = '',
#                     pageLength = 5
#                   )
#     )
#   }))
# 
# positionHalfDiffDT$dt
# 
# 
# ## Plotly ----------------------------------------------------------------------------
# #Full Match Comparison -- 
# positionMatchGGData <- bind_rows(
#   x = teamData %>% 
#     mutate(match_name = paste0(opponent," : ",match_date)) %>% 
#     group_by(type, position, match_name) %>%
#     summarise(.groups = 'keep',
#               across(one_of(metricNameFull), .fns = ~ sum(.x) / sum(dur))
#     ) %>% 
#     pivot_longer(cols = one_of(metricNameFull), names_to = 'metric') %>% 
#     mutate(match_half = 'FULL MATCH') , 
#   y = teamData %>% 
#     mutate(match_name = paste0(opponent," : ",match_date)) %>% 
#     group_by(type, match_name, position, match_half) %>%
#     summarise(.groups = 'keep',
#               across(one_of(metricNameFull), .fns = ~ sum(.x) / sum(dur))
#     ) %>% 
#     pivot_longer(cols = one_of(metricNameFull), names_to = 'metric')
# )  %>% 
#   mutate(
#     type = ifelse(type == 'comparison', 'Comparative Matches', currentMatch %>% pull(match_name) %>% unique()) %>% factor(., levels = c('Comparative Matches', currentMatch %>% pull(match_name) %>% unique())),
#     match_half = factor(match_half, levels = c('FULL MATCH', 'FIRST HALF', 'SECOND HALF')),
#     metric = factor(metric, levels = c("dist", "hml", "hsr", "exp", "spr"))
#   ) 
# 
# positionMatchPP <-  positionMatchGGData %>% 
#   nest(data = -position) %>% 
#   mutate(pp = map(.x = data, .f = function(.x){
#     
#     gg <-  ggplot(data = .x) + 
#       geom_point(
#         aes(
#           x = match_half, 
#           y = value, 
#           text = paste0(match_name, "<br>",round(value,1)),
#           shape = type, 
#           fill = type,
#           color = type
#         )
#       ) +
#       facet_wrap(~metric,nrow = 5, scales = 'free_y') +
#       scale_color_manual(values=c(alpha('#212844',.1), alpha('#212844',1))) +
#       scale_fill_manual(values= c(alpha('#212844',.1), alpha('#212844',1))) +
#       scale_shape_manual(values = c(16,23)) + 
#       theme(axis.title.y=element_blank())
#     
#     ggplotly(gg, tooltip = "text") %>% 
#       layout(legend = list(orientation = 'h', x = 0, y = 1.1)) %>% 
#       config(displaylogo = FALSE)
#     
#   }))
# 
# positionMatchPP$pp


# ATHLETE VISUALIZATIONS ------------------------------------------------------

## Data Tables ----------------------------------------------------------

#Format Full Match Table
athleteMatchDT <- bind_rows(athleteMatch, athleteHalf) %>%
  ungroup() %>% 
  mutate(
    metric = factor(metric, levels = metricNameFull),
    metric_label = metricLabelShortPerMinute[metric] %>% unname(), 
    metric_unit = metricLabelUnits[metric] %>% unname(),
    msd = ifelse(metric == "spr",
                 glue("{round(m,1)} ± {round(sd,1)}"),
                 glue("{round(m,0)} ± {round(sd,0)}")
    ),
    rank2 = ifelse(is.na(rank), NA, glue("{rank}/{n}")),
    dr =  ifelse(is.na(d), NA, paste0(round(d, 1)," (", r,")")) 
  ) %>% 
  select(match_half, position, athlete, metric, metric_label, metric_unit, v, msd, rank2, dr, d) %>% 
  nest(data = -c(position, match_half, athlete)) %>% 
  mutate(dt = map(.x = data, .f = ~visReactable(.x))) 


#Half Percent Difference -----
athleteHalfDiffDT <- athleteHalfDiff %>% 
  ungroup() %>% 
  mutate(
    metric = factor(metric, levels = metricNameFull),
    metric_label = metricLabelShortPerMinute[metric] %>% unname(), 
    metric_unit = metricLabelUnits[metric] %>% unname(),
    msd_pct = glue("{round(m_pct*100,0)}% ± {round(sd*100,0)}%"),
    rank2 = ifelse(is.na(rank),NA,glue("{rank}/{n}")),
    dr =  ifelse(is.na(d), NA, paste0(round(d, 1)," (", r,")"))
  ) %>% 
  select(position, athlete, metric, metric_label, metric_unit, v_pct, msd_pct, rank2, dr, d) %>%  
  nest(data = -c(athlete, position)) %>% 
  mutate(dt = map(.x = data, .f = ~visReactable(.x))) 

## Plotly ----------------------------------------------------------------------------
#Full Match Comparison -- 
athleteMatchGGData <- bind_rows(
  x = matchAnalysisData %>% 
    mutate(match_name = paste0(opponent," : ",match_date)) %>% 
    group_by(type, position, name_display, match_name) %>%
    summarise(.groups = 'keep', 
              dur = sum(dur),
              across(one_of(metricNameFull), .fns = ~ sum(.x) / sum(dur))) %>% 
    pivot_longer(cols = one_of(metricNameFull), names_to = 'metric') %>% 
    mutate(match_half = 'FULL MATCH'), #%>% 
  # filter(dur > 75), 
  y = matchAnalysisData %>% 
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
    type = ifelse(type == 'comparison', 'Comparative Matches', currentMatchData %>% pull(match_name) %>% unique()) %>% factor(., levels = c('Comparative Matches', currentMatchData %>% pull(match_name) %>% unique())),
    match_half = factor(match_half, levels = c('FULL MATCH', 'FIRST HALF', 'SECOND HALF')),
    metric = factor(metric, levels = c("dist", "hml", "hsr", "exp", "spr"))
  ) 

athleteMatchPP <- athleteMatchGGData %>% 
  filter(type != 'Comparative Matches') %>% 
  nest(data = -c(name_display)) %>% 
  mutate(data = map(.x = data, .f = function(.x){
    bind_rows(
      athleteMatchGGData %>% 
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
  # filter(name_display == 'Walker Zimmerman') %>% 
  mutate(pp = map(.x = data, .f = function(.x){
    
    .x1 <- .x %>% 
      filter(if (match_half == 'FULL MATCH') dur >= 75 else T) %>%
      filter(if (match_half != 'FULL MATCH') dur >= 35 else T)

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
    
    namedLimits <- c(
      "Normal Range", 
      "Comparative Matches", 
      .x %>% filter(type != 'Comparative Matches') %>% pull(type) %>% unique() %>% as.character()
    )
    
    if (length(unique(.x1$type)) == 1){
      
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
          text = paste0(match_name, "<br>", name_display, "<br>", round(value,1)),
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

# athleteMatchPP$pp


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





