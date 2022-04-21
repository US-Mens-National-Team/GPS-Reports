library(tidyverse)
library(ggpubr)
library(tidyquant)

con_mysql <- MNTSportScience::create_amz_mysql_con(dbName = "sandbox_jwebb")

athleteNames <- c(
    "Kellyn Acosta", 
    # "Gianluca Busio",
    # "Yunus Musah", 
    # "Cristian Roldan", 
    "Weston Mckennie", 
    "Luca De La Torre"
  )

#All Match Data
mntMatchData <- tbl(con_mysql, "ssDrill") %>% 
  filter(drill_type == "M") %>% 
  filter(name_display %in% athleteNames) %>% 
  mutate(
    acc_cnt = acc_z4_cnt + acc_z5_cnt + acc_z6_cnt,
    dec_cnt = dec_z4_cnt + dec_z5_cnt + dec_z6_cnt
  ) %>% 
  select(
    match_date = drill_date,
    match_half = drill_int,
    drill_start_time, 
    drill_end_time,
    opponent = drill_variation,
    name_display, name_first, name_last,
    position = position_drill,
    dur,
    dist, 
    hml = hml_dist, 
    hsr = hsr_dist,
    spd = vel_z6_dist,
    exp = explosive_dist,
    acc = acc_cnt, 
    dec = dec_cnt,
    vel_max
  ) %>% 
  collect() %>% 
  select(opponent, match_date, match_half, name_display, position, dur, dist, hml, hsr, spd, exp) %>% 
  mutate(
    last_name = word(name_display, -1),
    match_date = ymd(match_date)
  ) %>% 
  group_by(match_date, last_name, position) %>% 
  filter(position == "AM") %>%  
  summarise(across(where(is.numeric), sum)) %>% 
  filter(dur > 75) %>% 
  mutate(type = 'mnt')

library(lubridate)

con_mysql2 <- MNTSportScience::create_amz_mysql_con(dbName = "club_data")

clubMatchData <- tbl(con_mysql2, 'all_clubs') %>% 
  filter(athlete %in% athleteNames) %>% 
  filter(sess_name == 'Match') %>% 
  collect() %>% 
  mutate(
    date = ymd(date),
    last_name = word(athlete, -1)
  ) %>% 
  select(
    match_date = date, 
    last_name, 
    dur, 
    dist, 
    hml = hml_dist, 
    hsr = hsr_dist,
    spd = spr_dist, 
    exp = exp_dist
  ) %>% 
  mutate(type = 'club') %>% 
  filter(dur > 75, dist >= 7000)  %>% 
  filter(last_name != 'McKennie')

secondSpectrumData <- read_csv("C:/Users/jorda/Downloads/secondspectrum.csv") %>% 
  mutate(
    last_name = word(athlete,-1), 
    match_date = mdy(date)
  ) %>% 
  select(match_date, last_name, dur = Minutes, dist = Distance) 

matchData <- bind_rows(mntMatchData, clubMatchData, secondSpectrumData)

library(ggdist)
library(gridExtra)

x <- matchData %>% 
  pivot_longer(data = ., cols = c(dist, hml, hsr, spd, exp)) %>% 
  mutate(value = (value/dur)*95) %>% 
  select(-dur) %>% 
  mutate(last_name = stringr::str_to_title(last_name)) 

x2 <- x %>%  
  nest(data = -c(name)) %>% 
  mutate(gg = map2(.x = data, .y = name, .f = function(.x, .y){
    .x %>% 
      ggplot(data = ., aes(
        x = value, 
        y = last_name, 
        fill = type
      )) + 
      ggdist::stat_halfeye(
        adjust = .5,
        width = .6, 
        .width = c(.5, .95)
      ) + 
      ggdist::stat_dots(
        side = "left", dotsize = .1
      ) +
      ggtitle(.y) 
  })) %>% 
  pull(gg)




ggarrange(ncol=3, nrow=2,
  x2[1][[1]], 
  x2[2][[1]], 
  x2[3][[1]], 
  x2[4][[1]], 
  x2[5][[1]],
  common.legend = TRUE, legend="bottom"
)
    



  


