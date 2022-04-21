con_mysql <- MNTSportScience::create_amz_mysql_con(dbName = "sandbox_jwebb")

dbGetQuery(con_mysql, "SELECT drill_date, name_display, drill_type FROM ssDrill GROUP BY drill_date, name_display") %>%
  group_by(drill_date) %>% 
  summarise(
    n = n(),
    isMatch = any(drill_type == 'M')
  ) %>% 
  write_csv(., "Training & Match Norms/Training Calendar.csv")


# -------------------------------------------------------------------------

metricNameFull <- c("dist", "hml", "hsr", "exp", "spr")

#Load Data
x1 <- readxl::read_xlsx(path = 'Training & Match Norms/Training Calendar.xlsx') %>% 
  mutate(drill_date = as.character(drill_date))

#All Match Data
x2 <- tbl(con_mysql, "ssDrill") %>% 
  mutate(
    acc_cnt = acc_z4_cnt + acc_z5_cnt + acc_z6_cnt,
    dec_cnt = dec_z4_cnt + dec_z5_cnt + dec_z6_cnt
  ) %>% 
  select(
    drill_date,
    drill_type,
    match_half = drill_int,
    drill_start_time, 
    drill_end_time,
    name_display, name_first, name_last,
    position = position_drill,
    dur,
    dist, 
    hml = hml_dist, 
    hsr = hsr_dist,
    spr = vel_z6_dist,
    exp = explosive_dist,
    acc = acc_cnt, 
    dec = dec_cnt,
    vel_max
  ) %>% 
  collect() %>% 
  left_join(., y = x1, by = c("drill_date")) 

trainingData <- x2 %>% 
  filter(drill_date >= '2020-01-01') %>% 
  mutate(x = !(MDMinus == "MD" & drill_type != 'M')) %>% 
  unite(., col = "MD",  MDPlus, MDMinus, na.rm=TRUE, sep = ", ") %>% 
  group_by(drill_date, position, name_display, CampName, Type, MD, Impulse) %>% 
  summarise(.groups = 'keep', across(one_of(c(metricNameFull, 'dur')), sum)) %>% 
  pivot_longer(., cols = all_of(metricNameFull), names_to = 'metric', values_to = 'value')


#Match Loads By Position ----
trainingData %>% 
  filter(MD == 'MD, MD') %>% 
  filter(dur >= 75) %>% #only matches with values above 75 minutes
  mutate(value = (value/dur)*95) %>% #Normalize all data to 95 minutes
  group_by(position, metric) %>% 
  summarise(.groups = 'keep', 
    mean = mean(value),
    sd = sd(value),
    n = n()
  )

#All Sessions by Camp -----
trainingData %>% 
  group_by(drill_date, position, CampName, MD, metric) %>% 
  summarise(.groups = 'keep', 
      mean = mean(value),
      sd = sd(value)
    ) %>% 
  nest(data = -c(CampName, position)) %>% 
  mutate(title = glue('{CampName} - {position}')) %>% 
  mutate(gg = map2(.x = data, .y = title, .f = function(.x, .y){
    .x %>% 
      ggplot(aes(x = drill_date, y = mean)) +
      geom_col() +
      geom_text(aes(label = round(mean, 0), vjust = -0.5)) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      facet_wrap(~metric, scales = 'free') +
      ggtitle(.y) 
  })) %>% 
  pull(gg) %>% .[1]

#Typical Days
trainingData %>% 
  filter(MD != "", MD != "MD, MD") %>% 
  group_by(position, MD, metric) %>% 
  summarise(.groups = 'keep', 
            mean = mean(value),
            sd = sd(value)
  ) %>% 
  # pivot_wider(names_from = metric, values_from = c(mean, sd), names_glue = "{metric}_{.value}") 
  nest(data = -c(position)) %>% 
  mutate(
    gg = map2(.x = data, .y = position, .f = function(.x, .y){
      .x %>% 
        ggplot(aes(x = MD, y = mean)) +
        geom_col() +
        geom_text(aes(label = round(mean, 0), vjust = -0.5)) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        facet_wrap(~metric, scales = 'free') +
        ggtitle(.y) 
  })) %>% 
  pull(gg) %>% .[1]
trainingData %>% View()  

library(RcppRoll)

trainingData %>% 
  group_by(drill_date, position, CampName, MD, metric) %>% 
  summarise(.groups = 'keep', 
            mean = mean(value),
            sd = sd(value)
  ) %>% 
  arrange(drill_date) %>% 
  group_by(CampName, position, metric) %>% 
  mutate(
    rollMean3 = roll_sum(mean, 3, align = "right", fill = NA)
  ) %>% 
  group_by(position, metric) %>% 
  summarise(
    max = max(rollMean3, na.rm = T)
  ) %>% 
  filter(max > 0) %>% 
  filter(!(position %in% c('N', 'GK')) & !is.na(position)) %>% 
  pivot_wider(., names_from = position, values_from = max)

  


  
  
  





  