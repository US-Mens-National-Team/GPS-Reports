library(tidyverse)
library(effectsize)
library(plotly)
library(reactable)
library(DBI) 
library(DT)
library(readxl)
library(magrittr)
library(glue)

# FUNCTIONS --------------------------------------------------------

`%P>%` <- function(lhs, rhs){ lhs %T>% print() %>% rhs}

# SET CONNECTION -------------------------------------------------------------

con_sqlite <- dbConnect(SQLite(), dbname = "E:/statsports/statsports.sqlite")

con_mysql <- MNTSportScience::create_amz_mysql_con(dbName = "sandbox_jwebb")

# UPLOAD FORMATION TO MYSQL ---------------------------------------------------

formation <- read_excel("Match Report/formation.xlsx", col_types = c("text", "date", "text", "text", "text", "text", "text")) %>%
  mutate(match_date = as.Date(match_date))

dbWriteTable(con_mysql, "match_formation", value = formation, overwrite = T)

# SET VARIABLES -----------------------------------------------------------

comparisonMatchDates <- c('2021-10-10', '2021-09-02', '2021-09-08', '2021-11-12', '2022-03-24')

currentMatchDate <- c('2022-03-27')

metricNameFull <- c("dist", "hml", "hsr", "exp", "spr") %T>% print()

# METRIC LABELERS ---------------------------------------------------------

metricLabelLongPerMinute <- c(
  `dist` = "Total Distance / Minute (m)",
  `hml` = "Total Intensive Distance / Minute (m > 25.5 w/kg)",
  `hsr` = "High Speed Running Distance / Minute (m > 5.5 m/s)",
  `exp` = "Acceleration Deceleration Distance / Minute (m > ±3 m/s²)",
  `spr` = "Sprint Distance / Minute (m > 7.0 m/s)"
)

metricLabelLong <- c(
  `dist` = "Total Distance (m)",
  `hml` = "Total Intensive Distance (m > 25.5 w/kg)",
  `hsr` = "High Speed Running Distance (m > 5.5 m/s)",
  `exp` = "Acceleration Deceleration Distance (m > ±3 m/s²)",
  `spr` = "Sprint Distance (m > 7.0 m/s)"
)

metricLabelShortPerMinute <- c(
  `dist` = "TD / Min",
  `hml` = "TID / Min",
  `hsr` = "HSRD / Min",
  `exp` = "Ac/DcD / Min",
  `spr` = "SprD / Min"
)

metricLabelLong <- c(
  `dist` = "TD",
  `hml` = "TID",
  `hsr` = "HSRD",
  `exp` = "Ac/DcD",
  `spr` = "SprD"
)

metricLabelUnits <- c(
  `dist` = "m",
  `hml` = "m > 25.5 w/kg",
  `hsr` = "m > 5.5 m/s",
  `exp` = "m > ±3.0 m/s²",
  `spr` = "m > 7.0 m/s"
)

# QUERY DATA -------------------------------------------------------

formationData <- tbl(con_mysql, "match_formation") %>% 
  select(match_name, match_date, formation) %>%
  collect() %>% 
  mutate(
    opponent = match_name %>% str_remove_all(., " v ") %>% str_remove_all(., "USA"),
    match_name = paste0(match_name, " : ", match_date)
  )

#All Match Data
matchData <- tbl(con_mysql, "ssDrill") %>% 
  filter(drill_type == "M") %>% 
  mutate(
    acc_cnt = acc_z4_cnt + acc_z5_cnt + acc_z6_cnt,
    dec_cnt = dec_z4_cnt + dec_z5_cnt + dec_z6_cnt
  ) %>% 
  select(
    match_date = drill_date,
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
  left_join(., y = formationData, by = c("match_date")) %>% 
  filter(!is.na(opponent)) #Filters out all non competitive Matches


# DATA SET UP -------------------------------------------------------------

#Match being reported on
currentMatchData <- matchData %>% 
  filter(match_date == currentMatchDate) %>% 
  mutate(type = 'current') %T>% print()

#Commented out as this is not currently in use. 
# allMatchData <- matchData %>% 
#   filter(match_name != currentMatchName)

#Matches for comparison
comparisonMatchData <- matchData %>% 
  filter(match_date %in% comparisonMatchDates)  %>% 
  mutate(type = 'comparison') %T>% print()

#Combine Current and Comparison into one data set. 
matchAnalysisData <- bind_rows(
    currentMatchData %>% select(type, opponent, match_date, match_half, name_display, position, dur, one_of(metricNameFull)), 
    comparisonMatchData %>% select(type, opponent, match_date, match_half, name_display, position, dur, one_of(metricNameFull))
  ) %T>% print()


#Calculate Full Match Duration
fullMatchDurationData <- matchData %>% 
  select(match_half, match_date, dur) %>% 
  group_by(match_half, match_date) %>% 
  summarise(.groups = 'keep', dur = max(dur)) %>% 
  pivot_wider(names_from = match_half, values_from = dur) %>% 
  rename(
    first_half = `FIRST HALF`,
    second_half = `SECOND HALF`,
    first_overtime = `FIRST OVERTIME`, 
    second_overtime = `SECOND OVERTIME`
  ) %>% 
  select(match_date, first_half, second_half, first_overtime, second_overtime) %>% 
  rowwise() %>% 
  mutate(
    full_match = sum(c(first_half, second_half, first_overtime, second_overtime), na.rm = T) 
  ) %>% 
  ungroup() %T>% print()

#Calculate Athlete Match Duration 
athleteMatchDurationData <- currentMatchData %>% 
  select(match_date, name_display, name_last, position, dur) %>% 
  group_by(match_date, name_display, name_last, position) %>% 
  summarize(.groups = 'keep', dur = sum(dur)) %>% 
  left_join(
    ., 
    fullMatchDurationData %>% select(match_date, full_match), 
    by = c("match_date")
  )

saveRDS(athleteMatchDurationData, file = 'Match Report/vis/athleteMatchDur')


# NOTES -------------------------------------------------------------------

## Table Outline --------
  # type - Current or Comparison Data
  # metric - metric Name
  # current_m - mean current match
  # current_pct - value of H1/H2
  # m - mean of comparison
  # m_pct - mean of h1/h2
  # sd - sd of comparison
  # sd_pct - sd of h1/h2 
  # d - cohens d effect size
  # r - common language effect size

  # rank - Rank

# TEAM LEVEL ANALYSIS ------------------------------------------------------

## Match Effect Size / Rank -----------------------------------------------------

#Calculate Effect Sizes between comparison and normal match
teamMatchCohensD <- matchAnalysisData %>%
  group_by(type, opponent) %>%
  summarise(.groups = 'keep', 
            across(one_of(metricNameFull), .fns = ~ sum(.x) / sum(dur))
  ) %>% 
  pivot_longer(cols = one_of(metricNameFull), names_to = 'metric') %>% 
  group_by(type, metric) %>% 
  summarise(.groups = 'keep', 
            mean = mean(value), 
            sd = sd(value)
  ) %>% 
  pivot_wider(names_from = type, values_from = c(mean, sd)) %>%
  mutate(
    match_half = 'Full',
    d = (mean_current - mean_comparison)/sd_comparison,
    r = effectsize::interpret_cohens_d(d, rules = "sawilowsky2009")
  ) %>%  
  select(
    match_half,
    metric, 
    v = mean_current, 
    m = mean_comparison, 
    sd = sd_comparison,
    d, 
    r
  ) 

#Calculate Rank for full team
teamMatchRank <- matchAnalysisData %>% 
  group_by(type, opponent, match_date) %>%
  summarise(.groups = 'keep', across(one_of(metricNameFull), .fns = ~ sum(.x) / sum(dur))) %>% 
  ungroup() %>% 
  mutate(
    n = n(),
    across(one_of(metricNameFull), ~rank(-.x))
  ) %>% 
  filter(type == 'current') %>% 
  select(-type, -opponent, -match_date) %>% 
  pivot_longer(cols = one_of(metricNameFull), values_to = 'rank', names_to = 'metric')

teamMatch <-  left_join(teamMatchCohensD, teamMatchRank, by = "metric")  %>% 
  mutate(dur_match = fullMatchDurationData %>% filter(match_date == currentMatchDate) %>% pull(full_match))

## Half Effect Size / Rank -----------------------------------------------------

#Calculate Cohens D for Full team firsta and second half
teamHalfCohensD <- matchAnalysisData %>% 
  group_by(type, opponent, match_date, match_half) %>%
  summarise(.groups = 'keep', across(one_of(metricNameFull), .fns = ~ sum(.x) / sum(dur))) %>% 
  pivot_longer(cols = one_of(metricNameFull), names_to = 'metric') %>% 
  group_by(type, match_half, metric) %>% 
  summarise(.groups = 'keep', 
            mean = mean(value), 
            sd = sd(value)
  ) %>% 
  pivot_wider(names_from = type, values_from = c(mean, sd)) %>% 
  select(match_half, metric, mean_comparison, mean_current, sd = sd_comparison) %>% 
  mutate(
    match_half = str_to_title(match_half),
    d = (mean_current - mean_comparison)/sd,
    r = effectsize::interpret_cohens_d(d, rules = "sawilowsky2009")
  ) %>% 
  select(
    match_half, 
    metric, 
    v = mean_current,
    m = mean_comparison,
    sd, 
    d, 
    r
  )

teamHalfRank <- matchAnalysisData %>% 
  group_by(type, opponent, match_date, match_half) %>%
  summarise(.groups = 'keep', across(one_of(metricNameFull), .fns = ~ sum(.x) / sum(dur))) %>% 
  group_by(match_half) %>% 
  mutate(
    match_half = str_to_title(match_half),
    n = n(),
    across(one_of(metricNameFull), ~rank(-.x))
  ) %>% 
  filter(type == 'current') %>% 
  select(-type, -opponent, -match_date) %>% 
  pivot_longer(cols = one_of(metricNameFull), values_to = 'rank', names_to = 'metric')

teamHalf <-  left_join(teamHalfCohensD, teamHalfRank, by = c("metric", "match_half")) %>% 
  left_join(
    x = ., 
    y = fullMatchDurationData %>% filter(match_date == currentMatchDate) %>% 
      select(first_half, second_half) %>% 
      rename(`First Half` = first_half, `Second Half` = second_half) %>% 
      pivot_longer(cols = c(`First Half`, `Second Half`), names_to = 'match_half', values_to = 'dur_match'),
    by = c("match_half")
  )


## Half Difference ----------------------------------------------------

#Calculate Match Half Difference 
teamHalfDiffCohensD <- matchAnalysisData %>% 
  group_by(type, match_date, match_half) %>%
  summarise(.groups = 'keep', across(one_of(metricNameFull), .fns = ~ sum(.x) / sum(dur))) %>% 
  ungroup() %>% 
  pivot_longer(cols = one_of(metricNameFull), names_to = 'metric') %>% 
  pivot_wider(names_from = match_half, values_from = value) %>% 
  rename(first_half = `FIRST HALF`, second_half = `SECOND HALF`) %>% 
  mutate(pct = second_half / first_half) %>% 
  select(type, metric, pct) %>% 
  group_by(type, metric) %>% 
  summarise(.groups = 'keep', 
            across(.cols = c("pct"), mean, .names = "mean"), 
            across(.cols = c("pct"), sd, .names = "sd") 
  ) %>% 
  ungroup() %>% 
  pivot_wider(names_from = type, values_from = c(mean, sd)) %>% 
  select(metric, mean_comparison, mean_current, sd = sd_comparison) %>% 
  mutate(
    d = (mean_current - mean_comparison)/sd,
    r = effectsize::interpret_cohens_d(d, rules = "sawilowsky2009")
  ) %>% 
  select(
    metric,
    v_pct = mean_current,
    m_pct = mean_comparison,
    sd,
    d,
    r
  )
  

teamHalfDiffRank <- matchAnalysisData %>% 
  group_by(type, opponent, match_date, match_half) %>%
  summarise(.groups = 'keep', across(one_of(metricNameFull), .fns = ~ sum(.x) / sum(dur))) %>% 
  pivot_longer(cols = one_of(metricNameFull), names_to = 'metric') %>% 
  pivot_wider(names_from = match_half, values_from = value) %>% 
  rename(first_half = `FIRST HALF`, second_half = `SECOND HALF`) %>% 
  mutate(v_pct = second_half / first_half) %>% 
  select(type, match_date, opponent, metric, v_pct) %>% 
  group_by(metric) %>% 
  # pivot_wider(names_from = match_half, values_from = value) %>% 
  mutate(
    n = n(),
    rank = rank(-v_pct)
  ) %>% 
  filter(type == 'current') %>% 
  select(metric, n, rank) 

teamHalfDiff <- left_join(teamHalfDiffCohensD, teamHalfDiffRank, by = c('metric')) 


# POSITION LEVEL ANALYSIS -----------------------------------------------------------

## Match Effect Size / Rank ---------------------------------------------------------

positionMatchCohensD <- matchAnalysisData %>%
  group_by(type, opponent, match_date, position) %>%
  summarise(.groups = 'keep', across(one_of(metricNameFull), .fns = ~ sum(.x) / sum(dur))) %>%
  pivot_longer(cols = one_of(metricNameFull), names_to = 'metric') %>%
  group_by(type, position, metric) %>%
  summarise(.groups = 'keep',
            mean = mean(value),
            sd = sd(value)
  ) %>%
  pivot_wider(names_from = type, values_from = c(mean, sd)) %>% 
  mutate(match_half = 'Full') %>% 
  select(position, metric, mean_comparison, mean_current, sd = sd_comparison) %>%
  mutate(
    d = (mean_current - mean_comparison)/sd,
    r = effectsize::interpret_cohens_d(d, rules = "sawilowsky2009")
  ) %>% 
  select(
    position,
    metric,
    v = mean_current,
    m = mean_comparison,
    sd, 
    d,
    r
  )

#Rank
positionMatchRank <- matchAnalysisData %>%
  group_by(type, opponent, match_date, position) %>%
  summarise(.groups = 'keep', across(one_of(metricNameFull), .fns = ~ sum(.x) / sum(dur))) %>%
  group_by(position) %>%
  mutate(
    n = n(),
    across(one_of(metricNameFull), ~rank(-.x))
  ) %>%
  filter(type == 'current') %>%
  select(-type, -opponent, -match_date) %>%
  pivot_longer(cols = one_of(metricNameFull), values_to = 'rank', names_to = 'metric')

positionMatch <-  left_join(positionMatchCohensD, positionMatchRank, by = c("metric", "position")) %>%
  mutate(dur = fullMatchDurationData %>% filter(match_date == currentMatchDate) %>% pull(full_match))

## Half Effect Size / Rank ---------------------------------------

#Cohens
positionHalfCohensD <- matchAnalysisData %>%
  group_by(type, opponent, match_date, position, match_half) %>%
  summarise(.groups = 'keep', across(one_of(metricNameFull), .fns = ~ sum(.x) / sum(dur))) %>%
  pivot_longer(cols = one_of(metricNameFull), names_to = 'metric') %>%
  group_by(type, match_half, position, metric) %>%
  summarise(.groups = 'keep',
            mean = mean(value),
            sd = sd(value)
  ) %>%
  pivot_wider(names_from = type, values_from = c(mean, sd)) %>%
  select(position, match_half, metric, mean_comparison, mean_current, sd = sd_comparison) %>%
  mutate(
    match_half = str_to_title(match_half),
    d = (mean_current - mean_comparison)/sd,
    r = effectsize::interpret_cohens_d(d, rules = "sawilowsky2009")
  ) %>% 
  select(
    match_half,
    position,
    metric,
    v = mean_current,
    m = mean_comparison,
    sd, 
    d,
    r
  ) %T>% print() 

positionHalfRank <- matchAnalysisData %>%
  group_by(type, opponent, match_date, match_half, position) %>%
  summarise(.groups = 'keep', across(one_of(metricNameFull), .fns = ~ sum(.x) / sum(dur))) %>%
  group_by(match_half, position) %>%
  mutate(
    match_half = str_to_title(match_half),
    n = n(),
    across(one_of(metricNameFull), ~rank(-.x))
  ) %>%
  filter(type == 'current') %>%
  select(-type, -opponent, -match_date) %>%
  pivot_longer(cols = one_of(metricNameFull), values_to = 'rank', names_to = 'metric')

positionHalf <-  left_join(positionHalfCohensD, positionHalfRank, by = c("metric", "match_half", "position")) %>%
  left_join(
    x = .,
    y = fullMatchDurationData %>%
      filter(match_date == currentMatchDate) %>%
      select(`First Half` = first_half, `Second Half` = second_half) %>%
      pivot_longer(cols = c(`First Half`, `Second Half`), names_to = 'match_half', values_to = 'dur_match'),
    by = c('match_half')
  )

## Half Difference ---------------------------------------------------------

positionHalfDiffCohensD <- matchAnalysisData %>%
  group_by(type, match_date, match_half, position) %>%
  summarise(.groups = 'keep', across(one_of(metricNameFull), .fns = ~ sum(.x) / sum(dur))) %>%
  pivot_longer(cols = one_of(metricNameFull), names_to = 'metric') %>%
  pivot_wider(names_from = match_half, values_from = value) %>%
  rename(first_half = `FIRST HALF`, second_half = `SECOND HALF`) %>%
  mutate(pct = second_half / first_half) %>%
  group_by(type, position, metric) %>%
  select(type, position, metric, pct) %>%
  summarise(.groups = 'keep',
            across(.cols = c("pct"), mean, .names = "mean"),
            across(.cols = c("pct"), sd, .names = "sd")
  ) %>%
  pivot_wider(names_from = type, values_from = c(mean, sd)) %>%
  select(position, metric, mean_comparison, mean_current, sd = sd_comparison) %>%
  mutate(
    d = (mean_current - mean_comparison)/sd,
    r = effectsize::interpret_cohens_d(d, rules = "sawilowsky2009")
  ) %>% 
  select(
    position, metric, v = mean_current, m = mean_comparison, sd, d, r
  )
  

positionHalfDiffRank <- matchAnalysisData %>%
  group_by(type, opponent, match_date, match_half, position) %>%
  summarise(.groups = 'keep', across(one_of(metricNameFull), .fns = ~ sum(.x) / sum(dur))) %>%
  pivot_longer(cols = one_of(metricNameFull), names_to = 'metric') %>%
  pivot_wider(names_from = match_half, values_from = value) %>%
  rename(first_half = `FIRST HALF`, second_half = `SECOND HALF`) %>%
  mutate(pct = second_half / first_half) %>%
  select(type,position, match_date, opponent, metric, pct) %>%
  group_by(position, metric) %>%
  mutate(
    n = n(),
    rank = rank(-pct)
  ) %>%
  filter(type == 'current') %>%
  select(position, metric, n, rank)

positionHalfDiff <- left_join(positionHalfDiffCohensD, positionHalfDiffRank, by = c('metric', 'position'))

# ATHLETE LEVEL ANALYSIS --------------------------------------------------

## Match Effect Size / Rank------------------------------------------------------------------------
athleteMatchCohensD <- currentMatchData %>%
  group_by(position, name_display) %>%
  summarise(.groups = 'keep', 
            dur = sum(dur),
            across(one_of(metricNameFull), .fns = ~ sum(.x) / sum(dur))
  ) %>% 
  pivot_longer(cols = one_of(metricNameFull), names_to = 'metric') %>% 
  left_join(x = ., y = positionMatch %>% select(position,metric, m, sd, dur_match = dur), by = c('position', 'metric')) %>% 
  mutate(
    match_half = 'Full',
    d = (value - m)/sd,
    r = effectsize::interpret_cohens_d(d, rules = "sawilowsky2009"),
    d = ifelse(dur < 75, NA, d),
    r = ifelse(dur < 75, NA, r)
  ) %>% 
  select(match_half, position, athlete = name_display, metric, v = value, m, sd, dur, dur_match, d, r)

#Graph to analyze cut of for intensty
# 30 Minutes seems appropriate. 
# teamData %>% 
#   group_by(type, opponent, match_date, position, name_display) %>%
#   summarise(.groups = 'keep', 
#             dur = sum(dur),
#             across(one_of(metricNameFull), .fns = ~ sum(.x) / sum(dur))
#   ) %>% 
#   pivot_longer(cols = c(metricNameFull), names_to = "metric") %>% 
#   ggplot() + geom_point(aes(dur, value)) + facet_wrap(~metric, scales = 'free_y')

athleteMatchRank <- matchAnalysisData %>%
  group_by(type, opponent, match_date, position, name_display) %>%
  summarise(.groups = 'keep', 
            dur = sum(dur),
            across(one_of(metricNameFull), .fns = ~ sum(.x) / sum(dur))
  ) %>% 
  filter(dur >= 75) %>% 
  group_by(position) %>% 
  mutate(
    n = n(),
    across(one_of(metricNameFull), ~rank(-.x))
  ) %>% 
  filter(type == 'current') %>% 
  select(position, name_display, n, one_of(metricNameFull)) %>% 
  pivot_longer(cols = one_of(metricNameFull), values_to = 'rank', names_to = 'metric') %>% 
  select(position, athlete = name_display, n, metric, rank)

athleteMatch <- left_join(athleteMatchCohensD, athleteMatchRank, by = c('metric', 'position', 'athlete')) 

## Half Effect Size / Rank -------------------------------------------------
athleteHalfCohensD <- currentMatchData %>%
  group_by(position, match_half, name_display) %>%
  summarise(.groups = 'keep', 
            dur = sum(dur),
            across(one_of(metricNameFull), .fns = ~ sum(.x) / sum(dur))
  ) %>%  mutate(
    match_half = str_to_title(match_half)
  ) %>% 
  pivot_longer(cols = one_of(metricNameFull), names_to = 'metric') %>% 
  ungroup() %>% 
  left_join(x = ., y = positionHalf %>% select(position, match_half, metric, dur_match, m, sd), by = c('position', 'match_half', 'metric')) %>% 
  mutate(
    d = (value - m)/sd,
    r = effectsize::interpret_cohens_d(d, rules = "sawilowsky2009"),
    d = ifelse(dur < 35, NA, d),
    r = ifelse(dur < 35, NA, r)
  ) %>% 
  select(match_half, position, athlete = name_display, metric, v = value, dur, dur_match, m, sd, d, r)

#Graph to analyze cut of for intensty
# 30 Minutes seems appropriate. 
# teamData %>% 
#   group_by(type, opponent, match_date, match_half, position, name_display) %>%
#   summarise(.groups = 'keep', 
#             dur = sum(dur),
#             across(one_of(metricNameFull), .fns = ~ sum(.x) / sum(dur))
#   ) %>% 
#   pivot_longer(cols = c(metricNameFull), names_to = "metric") %>% 
#   ggplot() + geom_point(aes(dur, value)) + facet_wrap(~metric, scales = 'free_y')
# 

athleteHalfRank <- matchAnalysisData %>%
  group_by(type, opponent, match_date, position, name_display, match_half) %>%
  summarise(.groups = 'keep', 
            dur = sum(dur),
            across(one_of(metricNameFull), .fns = ~ sum(.x) / sum(dur))
  ) %>% 
  filter(dur >= 35) %>% 
  group_by(position, match_half) %>% 
  mutate(
    match_half = str_to_title(match_half),
    n = n(),
    across(one_of(metricNameFull), ~rank(-.x))
  ) %>% 
  filter(type == 'current') %>% 
  select(position, athlete = name_display, match_half, n, one_of(metricNameFull)) %>% 
  pivot_longer(cols = one_of(metricNameFull), values_to = 'rank', names_to = 'metric')

athleteHalf <- left_join(athleteHalfCohensD, athleteHalfRank, by = c('metric', 'position','match_half', 'athlete'))  


## Half Difference ---------------------------------------------------------

athleteHalfDiffCohensD <- matchAnalysisData %>%
  filter(type == 'current') %>% 
  group_by(type, match_date, match_half, position, name_display) %>%
  summarise(.groups = 'keep', 
            dur = sum(dur),
            across(one_of(metricNameFull), .fns = ~ sum(.x) / sum(dur))
  ) %>% 
  pivot_longer(cols = one_of(metricNameFull), names_to = 'metric') %>%
  pivot_wider(names_from = match_half, values_from = c(dur, value)) %>% 
  rename(
    dur_first_half = `dur_FIRST HALF`,
    dur_second_half = `dur_SECOND HALF`,
    first_half = `value_FIRST HALF`, 
    second_half = `value_SECOND HALF`
  ) %>% 
  mutate(
    first_half = ifelse(dur_first_half < 30, NA, first_half),
    second_half = ifelse(dur_second_half < 30, NA, second_half),
    v_pct = second_half / first_half
  ) %>% 
  ungroup() %>% 
  select(type, position, metric, athlete = name_display, v_pct) %>% 
  left_join(
    x = ., 
    y = positionHalfDiffCohensD %>% select(position, metric, m_pct = m, sd),
    by = c("position", "metric")
  ) %>% 
  mutate(
    d = (v_pct - m_pct)/sd,
    r = ifelse(!is.na(d), effectsize::interpret_cohens_d(d, rules = "sawilowsky2009"), NA)
  ) 

athleteHalfDiffRank <- matchAnalysisData %>% 
  group_by(type, opponent, match_date, match_half, position, name_display) %>%
  summarise(.groups = 'keep', 
            dur = sum(dur),
            across(one_of(metricNameFull), .fns = ~ sum(.x) / sum(dur))
  ) %>% 
  filter(dur > 30) %>% 
  select(-dur) %>% 
  pivot_longer(cols = one_of(metricNameFull), names_to = 'metric') %>% 
  pivot_wider(names_from = match_half, values_from = value) %>% 
  rename(first_half = `FIRST HALF`, second_half = `SECOND HALF`) %>% 
  filter(
    !is.na(first_half) & !is.na(second_half)
  ) %>% 
  mutate(pct = second_half / first_half) %>% 
  select(type, position, match_date, opponent, name_display, metric, pct) %>%
  group_by(position, metric) %>% 
  mutate(
    n = n(),
    rank = rank(-pct)
  ) %>% 
  filter(type == 'current') %>% 
  select(athlete = name_display, position, metric, n, rank)

athleteHalfDiff <- left_join(athleteHalfDiffCohensD, athleteHalfDiffRank, by = c('metric', 'position', 'athlete'))



