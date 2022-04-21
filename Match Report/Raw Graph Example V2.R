library(tidyverse)
library(RSQLite)
library(lubridate)
library(plotly)


# Notes / Todo ------------------------------------------------------------

  #How to handle when GPS is in a different TZ not US/Central
  #US Soccer Blue (33,40,68)

# System Variables --------------------------------------------------------

Sys.setenv(TZ="US/Central")
options(tz="US/Central")
options(digits.secs=2)

# Functions Data ----------------------------------------------------------

convert_hmss_gmt_to_cst <- function(x) {
  time_split <- str_split(x, "\\.",simplify = T)
  time_mili <- time_split[,2]
  time_hms <- time_split[,1]
  time_hmss_cst <- as.POSIXlt(x = time_hms, tz = "GMT") %>% 
    with_tz(., tzone = "US/Central") %>% 
    as.character() %>% 
    paste0(., ".", time_mili) %>%
    as.POSIXlt(x = ., tz = "US/Central", format = "%Y-%m-%d %H:%M:%OS")
  return(time_hmss_cst)
}

get_sonra_raw_by_timestamp <- function(con, athleteName, startTimeGMT, endTimeGMT, fields = c("time_hmss", "speed_ms")) {
  tryCatch(expr = {
    startTimeGMT <- startTimeGMT %>% as.character()
    endTimeGMT <- endTimeGMT %>% as.character()
    left_join(
      x = tbl(con, "sonra_raw"),
      y = tbl(con, "sonra_session_athlete"),
      by = c("session_athlete_id")
    ) %>% 
      filter(athlete_name == athleteName, time_hmss >= startTimeGMT, time_hmss <= endTimeGMT) %>% 
      select(fields) %>% 
      collect() %>% 
      mutate(time_hmss = convert_hmss_gmt_to_cst(time_hmss) %>% as.character()) %>% 
      list()
  }, error = function(e) {
    
      print(e, athleteName)
    
      return(NA)
    
  })
  

}

summarise_sonra_raw_by_max <- function(x, n) { 

  x %>% 
    mutate(
      g = rep(1:(n()/n), each = n)[1:n()]
    ) %>% 
    group_by(g) %>% 
    summarise(
      across(.cols = contains("speed_ms"), max),
      match_time = max(match_time)
    )
}


# Graph Functions ---------------------------------------------------------

individual_graphs <- function(x, teamGraph, athleteName, sessionDate){
  
  if (is.null(x)) return(teamGraph)
  
  x <- summarise_sonra_raw_by_max(x = x, n = 50)
  
  p <- teamGraph %>% 
    add_lines(
      x = ~x$match_time, 
      y = ~x$speed_ms,
      name = athleteName,
      line = list(
        color = 'rgba(33, 40, 68, 1)',
        width = 1.5
      ), 
      hovertemplate = "Speed ms <br> %{y:.1f} ms @ %{x:.1f} min",
      legendgroup= "indi"
    ) %>% 
    layout(
      title = 'Peak Speed Over 3 Sec',
      xaxis = list(title = 'Match Minute', x = .5, y = 1),
      yaxis = list(title = 'Speed ms'), 
      legend = list(orientation = 'h', x = .01, y = 1)
    ) %>% 
    config(displaylogo = FALSE)
  
  p
  
  
}

team_graphs <- function(x) {
  x %>% 
    plot_ly() %>% 
      add_ribbons(
        x = ~match_time, 
        ymin = ~speed_ms_min, 
        ymax = ~speed_ms_max,
        name = "Team Average",
        hovertemplate = "Speed ms <br> %{y:.1f} ms @ %{x:.1f} min",
        line = list(color = 'rgba(33, 40, 68, 0)'),
        fillcolor = 'rgba(33, 40, 68, .3)',
        legendgroup= "team"
      ) %>% 
    layout(
      title = 'Peak Speed Over 3 Sec',
      xaxis = list(title = 'Match Minute', x = .5, y = 1),
      yaxis = list(title = 'Speed ms'), 
      legend = list(orientation = 'h', x = .01, y = 1)
    ) %>% 
    config(displaylogo = FALSE)
  }

# App Variables -----------------------------------------------------------

con_sqlite <- dbConnect(SQLite(), dbname = "E:/statsports/statsports.sqlite")

con_mysql <- MNTSportScience::create_amz_mysql_con(dbName = "sandbox_jwebb")

sessionDate <- '2022-03-27'

athleteNames <- tbl(con_mysql, "ssDrill") %>% 
  select(drill_type, drill_date, athlete_name = name_display, drill_title, drill_start_time, drill_end_time) %>% 
  filter(drill_date == sessionDate) %>% 
  filter(drill_type == 'M') %>% 
  collect() %>% 
  pull(athlete_name) %>% 
  unique()


# Individual Data ---------------------------------------------------------

individualData <- tbl(con_mysql, "ssDrill") %>% 
  select(drill_date, athlete_name = name_display, drill_type, drill_title, drill_start_time, drill_end_time) %>% 
  filter(drill_date == sessionDate) %>% 
  filter(drill_type == 'M') %>% 
  collect() %>% 
  mutate(
    start_time_cst = paste0(drill_date, " ", drill_start_time) %>% as.POSIXlt(x = .),
    end_time_cst = paste0(drill_date, " ", drill_end_time) %>% as.POSIXlt(x = .),
    start_time_gmt = with_tz(start_time_cst, tzone = "GMT"),
    end_time_gmt = with_tz(end_time_cst, tzone = "GMT")
  ) %>% 
  select(-drill_start_time, -drill_end_time) %>% 
  rowwise() %>% 
    mutate(data = get_sonra_raw_by_timestamp(
      con = con_sqlite, 
      athleteName = athlete_name, 
      startTimeGMT = start_time_gmt, 
      endTimeGMT = end_time_gmt,
      fields = c("time_hmss", "speed_ms", "latitude", "longitude")
    )
  ) %>% 
  ungroup() %T>% print()

  individualData %>% select(athlete_name, data) %>% print(n = Inf)

# Match Time In .1 s ------------------------------------------------------


matchTime <- individualData %>% 
  select(drill_title, data) %>% 
  unnest(data) %>% 
  select(drill_title, time_hmss) %>% 
  unique() %>% 
  arrange(time_hmss) %>% 
  nest(data = time_hmss) %>% 
  # mutate(minStart = ifelse(str_detect(drill_title,'FIRST'),.1/60, .1/60)) %>% 
  mutate(minStart = ifelse(str_detect(drill_title,'FIRST'),.1/60, 45)) %>%
  mutate(data = map2(.x = data, .y = minStart, .f = function(x, minStart){
    x %>% 
      mutate(match_time = seq(from = minStart, by = .1/60, length.out = n()))
  })) %>% 
  select(-minStart, -drill_title) %>% 
  unnest(data)

# matchTime %>% ggplot() + geom_line(aes(x = match_time, y = time_hmss))


individualData <- individualData %>% 
  mutate(data = map(.x = data, .f = function(x){
    left_join(x, matchTime , by = "time_hmss")
  }))


# Team Data --------------------------------------------------------------

  team_data <- individualData %>% 
    select(drill_title, data) %>% 
    unnest(data) %>% 
    group_by(drill_title, time_hmss, match_time) %>% 
    summarise(.groups = 'keep', 
      speed_ms_mean = mean(speed_ms),
      speed_ms_max = max(speed_ms),
      speed_ms_min = min(speed_ms)
    )  %>% 
    ungroup() 


# Graphs ------------------------------------------------------------------

teamGraph <- team_data %>% 
  select(-time_hmss) %>% 
  nest(data = c(match_time, speed_ms_mean, speed_ms_max, speed_ms_min)) %>% 
  mutate(data = map(.x = data, .f = ~summarise_sonra_raw_by_max(.x, 50))) %>% 
  mutate(teamGraph = map(.x = data, .f = ~team_graphs(.x))) %>% 
  select(-data)

  x1 <- full_join(
    tibble(drill_title = individualData$drill_title %>% unique()), 
    tibble(athlete_name = athleteNames),
    by = character()
  ) 
  
  x2 <- individualData %>% select(-contains("_time"), -drill_date) 
  
  x3 <- left_join(x1, x2, by = c("athlete_name", "drill_title")) %>% 
   bind_rows(., tibble(athlete_name = 'Team', drill_title = unique(.$drill_title), data = NULL))

  x4 <- left_join(x3, teamGraph, by = 'drill_title')
  
  p <- x4 %>% mutate(
      maxTime = map_dbl(.x = data, .f =  function(.x){
        if (is.null(.x)) return(NA)
        max(.x$match_time, na.rm =T)}
      ),
      p = pmap(.l = list(data, teamGraph, athlete_name, sessionDate), .f = individual_graphs)
    ) %>% 
    group_by(drill_title) %>% 
    mutate(maxTime = max(maxTime, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(graphWidth = {
      t <- ifelse(maxTime==max(maxTime),maxTime-45,maxTime)
      m <- max(t)
      t*100/m
    }) %>% 
    select(-data, -teamGraph, -maxTime, -drill_type) %>% 
    mutate(
      drill_title = ifelse(str_detect(drill_title, "FIRST HALF"), yes = "First", no = "Second")
    ) %>% 
    pivot_wider(., names_from = 'drill_title', values_from = c(p, graphWidth)) %>% 
    mutate(
      p = pmap(.l = list(p_First, p_Second, graphWidth_First, graphWidth_Second), .f = function(p_First, p_Second, graphWidth_First, graphWidth_Second){
        f <- function(x){
          x <- x / 100
          a <- .95
          b <- x * a
          c(.025, b, 1-b-.025) 
        }
        tryCatch(expr = {
          subplot(nrows = 2, shareY = F, shareX = F, titleX = T, titleY = T, which_layout = 1,
                  subplot(
                    nrows = 1, widths = f(graphWidth_First), shareY = F, shareX = F, titleX = F, titleY = T, which_layout = 2,
                    plotly_empty(type = "scatter", mode = "markers"),
                    layout(p_First, xaxis = list(title = "")),
                    plotly_empty(type = "scatter", mode = "markers")
                  ),
                  subplot(
                    nrows = 1, widths = f(graphWidth_Second), shareY = F, shareX = F, titleX = T, titleY = T, which_layout = 2,
                    plotly_empty(type = "scatter", mode = "markers") ,
                    style(p_Second, showlegend = F), 
                    plotly_empty(type = "scatter", mode = "markers") 
                  )
          ) %>% 
            config(displaylogo = FALSE) %>% 
            layout(legend = list(x = .04, y = 1.1))
        }, error = function(e) {
          browser()
        })
        
      })
    ) %>% 
    select(athlete_name, p)
  
  saveRDS(p, "Match Report/vis/match10hz.rds")
