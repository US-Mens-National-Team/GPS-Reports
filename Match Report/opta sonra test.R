library(ggpubr)
library(tidyverse)

shape <- tribble(
  ~lon, ~lat,
  -81.38937930745146, 28.54058159913237, #BL
  -81.38868132963186, 28.54058996360919, #BR
  -81.38869582115048, 28.54157988108667, #TR
  -81.38939591475506, 28.54157173302617  #TL
)

p1 <- ggplot(shape, aes(x = lon, y = lat)) + geom_polygon(fill = 'white')

dgr <- atan((shape$lat[2] - shape$lat[1])/(shape$lon[2] - shape$lon[1]))*180/pi

b <- rotate_coord(x = shape$lon, y = shape$lat, center = coordCenter, angle = dgr, type = 'degrees')
shape2 <- tibble(lon = b[,1], lat = b[,2])
p2 <- ggplot(shape2, aes(x = lon, y = lat)) + geom_polygon(fill = 'white')

ggarrange(p1, p2)


# phase Layover ------------------------------------------------------


sonra10hz <- individualData %>% unnest(data)

sonraTime <- sonra10hz %>% select(drill_title, time_hmss) %>% unique() %>% 
  group_by(drill_title) %>% 
  mutate(elapsed_time = seq(from = 0, length.out = length(time_hmss), by = .1))

sonra10hz <- left_join(sonra10hz, sonraTime, by = c("drill_title", "time_hmss"))

timeStart <- Sys.time()
x <- sonra10hz %>% 
  slice(1:1000) %>%
  mutate(period_id = ifelse(str_detect(drill_title, "FIRST HALF"), 1,2)) %>% 
  mutate(phase = map2_chr(.x = elapsed_time, .y = period_id, .f = function(elapsed_time, period_id){
    x <- opta %>% 
      mutate(phaseName = glue('{team} - {phase}')) %>% 
      filter(period_id == period_id) %>%
      filter(sequence_start_time <= elapsed_time & sequence_end_time >= elapsed_time) %>% 
      pull(phaseName) %>% unique() 
    
    ifelse(identical(x, character(0)), NA, x)  # returns TRUE

  }))
difftime(timeStart, Sys.time())

library(data.table)

s <- sonra10hz %>% 
  mutate(period_id = ifelse(str_detect(drill_title, "FIRST HALF"), 1,2)) %>% 
  select(elapsed_time, period_id) %>% 
  unique()

o <- opta %>% 
  ungroup() %>%  
  filter(game_date == '2022-03-28') %>% 
  mutate(phase_name = glue('{phase} - {team}')) %>% 
  select(period_id, team, phase, phase_name, sequence_id, sequence_start_time, sequence_end_time) 

so <- left_join(s,o, by = 'period_id') %>% 
  filter(between(elapsed_time, sequence_start_time, sequence_end_time)) %>% 
  select(elapsed_time, period_id, sequence_id) %>% 
  group_by(elapsed_time, period_id) %>% 
  summarise(.groups = 'keep', sequence_id = min(sequence_id)) %>% 
  ungroup()

x <- sonra10hz %>% 
  mutate(period_id = ifelse(str_detect(drill_title, "FIRST HALF"), 1,2)) %>% 
  mutate(hml = metabolic_power(speed_ms)) %>% 
  left_join(., so, by = c("elapsed_time", "period_id")) %>% 
  select(period_id, athlete_name, sequence_id, elapsed_time, longitude, latitude, speed_ms, hml) %>% 
  filter(athlete_name == 'Christian Pulisic') %>% 
  filter(period_id == 1) 


 gg <- ggplot() +
    geom_rect(
      data = o %>% 
        filter(period_id == 1), # %>% 
        # filter(team == 'USA'), 
      aes(
        xmin = sequence_start_time, 
        xmax = sequence_end_time, 
        ymin = 0, 
        ymax = 100, 
        fill = phase_name, 
        group = sequence_id, 
        text = phase_name
      )
    ) +
    geom_line(
      data = x %>% 
        mutate(elapsed_time = as.integer(elapsed_time)) %>% 
        group_by(elapsed_time) %>% 
        summarise(hml = max(hml)), 
      aes(x = elapsed_time, y = hml)
    ) 
 
 ggplotly(gg, tooltip="text")
 
 
    





  


 # dir.create('~/.redshiftTools')
# download.file('http://s3.amazonaws.com/redshift-downloads/drivers/RedshiftJDBC41-1.1.9.1009.jar','~/.redshiftTools/redshift-driver.jar')
# install.packages('RJDBC')
# library(RJDBC)

get_opta_match_phases <- function(){
  
  dbname="opta"
  host='pr-ussf-analytics.ctqzjmsblhtk.us-east-2.redshift.amazonaws.com'
  port='5439'
  user='jwebb'
  password='analytics=USSF-1801'
  driver <- JDBC("com.amazon.redshift.jdbc42.Driver", "~/.redshiftTools/redshift-jdbc42-2.1.0.5.jar", identifier.quote="`")
  url <- sprintf("jdbc:redshift://%s:%s/%s?tcpKeepAlive=true&ssl=true&sslfactory=com.amazon.redshift.ssl.NonValidatingFactory", host, port, dbname)
  con <- dbConnect(driver, url, user, password)
  
  sql <- "with game_id_db as (
	select game_id from modeled.team_metrics tm where game_date > '2022-03-27' and team = 'USA'
)
select game_id, team_id, team, opponent, game_date, period_id, sequence_id, sequence_start_time, sequence_end_time, phase, pass_count, finishing_attacks, into_box, shot, goal  
from (
select game_id, team_id, team, opponent, game_date, sequence_id, period_id, 
	MIN(minsecs_adj) as sequence_start_time,
    MAX(minsecs_adj) as sequence_end_time,
    --get the end time of the previous seq and previous team
    LAG( sequence_end_time,1 ) OVER ( PARTITION BY game_id  ORDER BY sequence_id ) AS previous_sequence_end_time,
    LAG( team_id,1 ) OVER ( PARTITION BY game_id ORDER BY sequence_id ) AS previous_team_id,
    (sequence_start_time - previous_sequence_end_time) as time_between_sequences,
     --to get pass count, we can use this nifty trick (only passes and shots count toward the sequence_length_count)
    MAX(sequence_length_count) - max(sequence_goal_attempt) as pass_count,
    --transition if its 7 or less passes, less than 5 seconds between seq, and a different team
    (case when sum(final_third) > 2 then 1 else 0 end) as finishing_attacks, 
    (case when sum(into_box) > 0 then 1 else 0 end) as into_box,
    (case when sum(xg) > 0 then 1 else 0 end) as shot,
    (case when sum(goal) > 0 then 1 else 0 end) as goal,
	max(sequence_length_seconds) as sequence_length_sec, 
	sum(bumf_pass) as bumf,
	(case when previous_team_id != team_id AND time_between_sequences <= 5 AND  pass_count < 8 and (pass_count > 0 or shot > 0) then 1 else 0 end) as transition,
	(case when (sequence_length_sec > 4 and bumf > 2) then 'bumf' 
		  when transition = 1 then 'transition' 
		  when max(attacking_free_kick) = 1 then 'attacking_fk' 
		  when max(corner_taken) = 1 then 'corner'end ) as phase
from (
  		--add flags when necessary to denote qualifiers of seq
  		select game_id, team_id, team, opponent, game_date, minsecs_adj, period_id, sequence_id, sequence_length_count, sequence_goal_attempt, sequence_length_seconds,
  		sequence_inv_length_count, x_start, event_type, x_end, y_end, xg, goal_kick,corner_taken,
  		(case when free_kick_taken = 1 and \"cross\" = 1 then 1 else 0 end ) as attacking_free_kick,
	  	(CASE WHEN event_type = 'Goal' THEN 1 ELSE 0 end) AS goal,
	  	(case when event_type = 'Pass' and x_start > 20 and x_start < 66.7 and exp_pass >= .95 then 1 else 0 end) as bumf_pass,
  		(CASE WHEN x_end >= 66.6 THEN 1 ELSE 0 end) AS final_third,
	  	(CASE WHEN x_end >= 83 AND y_end >= 21.1 AND y_end <=78.9 THEN 1 ELSE 0 end) AS into_box
  		from modeled.event_metrics 
  		WHERE game_id in (select game_id from game_id_db) AND sequence_id > 0	
  		)
  	group by game_id, team_id, period_id, sequence_id, team, opponent, game_date	
  	);"



# sql <- "select game_id from modeled.team_metrics tm where competition_id = 339 and game_date > '2021-09-01' and team = 'USA'"
rslt <- dbGetQuery(con, sql) %>% 
  tibble() %>% 
  group_by(game_date, period_id) %>% 
  mutate(
    halfStartTime = min(sequence_start_time),
    sequence_start_time = sequence_start_time - halfStartTime,
    sequence_end_time = sequence_end_time - halfStartTime
  ) %>% 
  filter(!is.na(phase))
  

dbDisconnect(con)


return(rslt)

}

opta <- get_opta_match_phases()

sonraTime
