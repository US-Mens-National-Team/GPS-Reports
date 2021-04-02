#Session Total Time
x <- ssDrill %>% 
  filter(position != 'GK') %>% 
  filter(!is.na(drill_type)) %>% 
  select(
    session_title,
    drill_date, 
    drill_title,
    drill_start_time, 
    drill_end_time
  ) %>% 
  mutate(
    drillTimeRange = map2(.x = drill_start_time, .y = drill_end_time, .f = ~{.x:.y})
  ) %>%  
  group_by(session_title, drill_date) %>% 
  mutate(
    session_start_time = drill_start_time %>% sort(decreasing = F) %>% .[1],
    session_end_time = drill_end_time %>% sort(decreasing = T) %>% .[1]
  ) %>% 
  select(-drill_start_time, -drill_end_time) %>% 
  ungroup() %>% 
  group_by(session_title, session_start_time, session_end_time, drill_date) %>% 
  summarise(
    drillTimeRange = drillTimeRange %>% unlist() %>% unique() %>% c() %>% list,
  ) %>% 
  ungroup() %>% 
  mutate(
    sessionTimeRange = map2(.x = session_start_time, .y = session_end_time, .f = ~{.x:.y}),
    sessionTimeCategory =  map2(.x = drillTimeRange, .y = sessionTimeRange, .f = ~{.y %in% .x}),
    inactiveTimeMin = map2_dbl(.x = drillTimeRange, .y = sessionTimeRange, .f = ~{.y[.y %!in% .x] %>% length()/60}),
    totalTimeMin = as.numeric(session_end_time - session_start_time)/60 %>% round(.,0),
    activeTimePct = ((totalTimeMin-inactiveTimeMin) / totalTimeMin) %>% round(.,2) * 100
  )  


x %>% select(session_title, drill_date, totalTimeMin, inactiveTimeMin, activeTimePct) 