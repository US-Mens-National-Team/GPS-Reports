
x1 <- tbl(con_mysql, "ssDrill") %>% 
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
    endTimeGMT = end_time_gmt
  )
  ) %>% 
  ungroup() %T>% print()

matchTime <- x1 %>% 
  select(drill_title, data) %>% 
  unnest(data) %>% 
  select(drill_title, time_hmss) %>% 
  unique() %>% 
  arrange(time_hmss) %>% 
  nest(data = time_hmss) %>% 
  # mutate(minStart = ifelse(str_detect(drill_title,'FIRST'),.1/60, .1/60)) %>% 
  mutate(minStart = ifelse(str_detect(drill_title,'FIRST'),.1/60, 45)) %>%
  mutate(data = map2(.x = data, .y = minStart, .f = function(x, minStart){
    x %>% mutate(match_time = seq(from = minStart, by = .1/60, length.out = n()))
  })) %>% 
  select(-minStart, -drill_title) %>% 
  unnest(data)

x2 <- x1 %>% 
  mutate(data = map(.x = data, .f = function(x){
    left_join(x, matchTime , by = "time_hmss")
  }))

x2 %>% select(athlete_name, drill_title, data)

x3 <- x2 %>% select(athlete_name, drill_title, data) %>% 
  unnest(data) %>% 
  group_by(drill_title, time_hmss, match_time) %>% 
  mutate(
    # speed_ms_mean = mean(speed_ms),
    speed_ms_max = max(speed_ms),
    speed_ms_min = min(speed_ms)
  )  %>% 
  ungroup() 

x4 <- x3 %>% 
  mutate(
    x = (speed_ms-speed_ms_min)/(speed_ms_max - speed_ms_min)
  )

gg <- x4 %>% 
  # filter(speed_ms_max > 3) %>% 
  nest(data = -athlete_name) %>% 
  slice(1) %>% 
  mutate(g = map(.x = data, .f = function(x){
    ggplot(data = x) +
      geom_area(aes(x = match_time, y = x)) 
  }))

gg$g
  

x4 %>% 
  filter(speed_ms_max > 2) %>% 
  group_by(athlete_name) %>% 
  summarise(
    dur = n(),
    m = mean(x, na.rm = T),
    sd = sd(x, na.rm = T)
  ) %>% 
  arrange(m) %>% 
  print(n = Inf)


x4 %>% 
  filter(speed_ms_max > 3) %>% 
  select(athlete_name, time_hmss, match_time, x) %>% 
  nest(data = -athlete_name) %>% 
  mutate(dur = map_dbl(.x = data, .f = nrow) / 600) %>% 
  mutate(cor = map_dbl(.x = data, .f = function(x){
    cor(x$x, y = 1:length(x$x))
  })) %>% 
  arrange(-dur, cor)


# Heat Map ----------------------------------------------------------------

heatMapData <- tbl(con_mysql, "ssDrill") %>% 
  select(drill_date, position = position_drill, athlete_name = name_display, drill_type, drill_title, drill_start_time, drill_end_time) %>% 
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
    fields = all_of(c("time_hmss", "speed_ms", "latitude", "longitude"))
  )) %>% 
  ungroup() %T>% print()

#Athlete
x <- heatMapData %>% 
  # filter(athlete_name == 'Jesus Ferreira') %>% 
  select(athlete_name, drill_title, data) %>% 
  unnest(data) %>% 
  nest(data = -c(athlete_name)) %>% 
  mutate(gg = map2(.x = data, .y = athlete_name, .f = function(.x, .y){
    .x %>% ggplot(aes(x = longitude, y = latitude)) +
      ggtitle(.y) +
      geom_density_2d_filled() +
      scale_y_continuous(limits = c(28.54057, 28.54161)) +
      scale_x_continuous(limits = c(-81.38948, -81.38867)) +
      facet_wrap(vars(drill_title))
  })) 

#Athlete
heatMapData %>% 
  select(position, drill_title, data) %>% 
  unnest(data) %>% 
  nest(data = -c(position)) %>% 
  mutate(gg = map2(.x = data, .y = position, .f = function(.x, .y){
    .x %>% ggplot(aes(x = longitude, y = latitude)) +
      ggtitle(.y) +
      geom_density_2d_filled() +
      scale_y_continuous(limits = c(28.54057, 28.54161)) +
      scale_x_continuous(limits = c(-81.38948, -81.38867)) +
      facet_wrap(vars(drill_title))
  })) %>% 
  pull(gg)

#Team
heatMapData %>% 
  unnest(data) %>% 
      filter(speed_ms >= 5.5) %>% 
      ggplot(aes(x = longitude, y = latitude)) +
      geom_density_2d_filled() +
      scale_y_continuous(limits = c(28.54057, 28.54161)) +
      scale_x_continuous(limits = c(-81.38948, -81.38867)) +
      facet_wrap(vars(drill_title))

  
  


# heatMapData %>% 
#   select(athlete_name, drill_title, data) %>% 
#   unnest(data) %>% 
#   summarise(
#     latMax = max(latitude),
#     longMax = max(longitude),
#     latMin = min(latitude),
#     longMin = min(longitude)
#   ) %>% 
#   as.data.frame()
# 
# 
# 28.54158, -81.38938
# 28.54158, -81.38869
# 28.54058, -81.38937
# 28.54058, -81.38865

library(gganimate)
library(av)


dd1 <- individualData %>% 
  filter(drill_title == "{T:M} {V:PANAMA} {I:FIRST HALF}") %>% 
  # filter(athlete_name == "Jesus Ferreira") %>%
  unnest(data) %>% 
  select(athlete_name, time_hmss, latitude, longitude, speed_ms) %>% 
  mutate(athlete_name = word(string = athlete_name, -1)) %>% 
  arrange(time_hmss)  


dd <- dd1 %>% filter(
    time_hmss >= min(dd1$time_hmss) + minutes(10), 
    time_hmss <= min(dd1$time_hmss) + minutes(12), 
  )
  
inFrames <- dd$time_hmss %>% unique() %>% length()

p <- dd %>% 
  ggplot(aes(x = longitude, y = latitude)) +
    geom_point(aes(color = athlete_name, size = speed_ms)) + 
    geom_text(hjust=0, vjust=0, aes(label = athlete_name, size = 3)) +
    scale_y_continuous(limits = c(28.54057, 28.54161)) +
    scale_x_continuous(limits = c(-81.38948, -81.38867)) +
    transition_time(time_hmss) + 
    shadow_wake(wake_length = 10/inFrames, alpha = FALSE)

  animate(
    plot = p, 
    fps = 10,
    nframes = inFrames,
    renderer = av_renderer()
  )
  
  anim_save("Match Animation.mpeg", animation = last_animation())
  

# Sprint Test -------------------------------------------------------------
  
  find_peaks <- function (x, m = 3){
    shape <- diff(sign(diff(x, na.pad = FALSE)))
    pks <- sapply(which(shape < 0), FUN = function(i){
      z <- i - m + 1
      z <- ifelse(z > 0, z, 1)
      w <- i + m + 1
      w <- ifelse(w < length(x), w, length(x))
      if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) else return(numeric(0))
    })
    pks <- unlist(pks)
    pks
  }
  
  is_peak <- function(x, m){
    x[is.na(x)] <- 0
    pks <- find_peaks(x, m = m)
    y <- 1:length(x)
    y %in% pks
  }
  
  is_run <- function(x, m, v) {
    
    y <- NA
    m <- m-1
    
    for (i in 1:length(x)) {
      
      if (i <= m) {
        if (all(x[i:(i+m)]>=v)) {
        # if (mean(x[i:(i+m)])>=v) {
          y[i] <- TRUE
        } else {
          y[i] <- FALSE
        }
      } else if (i >= (length(x)-m)){
        if (all(x[i:(i+m)]>=v)) {
        # if (mean(x[i:(i-m)])>=v) {
          y[i] <- TRUE
        } else {
          y[i] <- FALSE
        }
    } else {
        if (all(x[i:(i+m)]>= v) || all(x[i:(i-m)]>=v) || all(x[(i-round(m/2,0)):(i+round(m/2,0))]>=v)) {
        # if (mean(x[i:(i+m)])>=v | mean(x[i:(i-m)])>=v) {
          y[i] <- TRUE
        } else {
          y[i] <- FALSE
        }
      }
      
    }
    
    return(y)
      
  }
  
  id_run <- function(x) {
    y <- 1
    id <- 1
    isOn <- FALSE
    
    for (i in 1:length(x)) {
      if (x[i] == TRUE) {
        isOn <- TRUE
        y[i] <- id
      } else {
        if (isOn) {
          id <- id+1
        }
        isOn <- FALSE
        y[i] <- FALSE
      }
    }
    
    return(y)
  }
  
  createPitchList <- function(grass_colour = "#ffffff", line_colour = "#000000",
                              background_colour = "#ffffff",
                              goal_colour = "#000000", BasicFeatures = FALSE, arrow = FALSE, 
                              thirds = FALSE, arrow2 = FALSE, xmin_offset=0, xmax_offset=0, ymin_offset=0, ymax_offset=0){
    
    theme_blankPitch <- function(){
      theme(
        #axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        # axis.ticks.y = element_text(size = size),
        # axis.ticks = element_blank(),
        axis.ticks.length = unit(0, "lines"),
        #axis.ticks.margin = unit(0, "lines"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.background = element_rect(fill = background_colour, colour = NA),
        #legend.key = element_rect(colour = background_colour, fill = background_colour),
        #legend.key.size = unit(1.2, "lines"),
        #legend.text = element_text(size = size),
        #legend.title = element_text(size = size, face = "bold", hjust = 0),
        strip.background = element_rect(colour = background_colour,
                                        fill = background_colour, size = .5),
        panel.background = element_rect(fill = background_colour, colour = background_colour),
        # panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(0, "pt"),
        plot.background = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "lines"))}#,
    #plot.title = element_text(size = size*1.2),
    # strip.text.y = element_text(colour = background_colour,size = size, angle = 270),
    #strip.text.x = element_text(size = size*1))}
    
    ymin <- 0 # minimum width
    ymax <- 100 # maximum width
    xmin <- 0 # minimum length
    xmax <- 100 # maximum length
    
    # Defining features along the length
    boxEdgeDef <- 17
    boxEdgeOff <- 83
    halfwayline <- 50
    sixYardDef <- 5.8
    sixYardOff <- 94.2
    penSpotDef <- 11.5
    penSpotOff <- 88.5
    
    # Defining features along the width
    boxEdgeLeft <- 78.9
    boxEdgeRight <- 21.1
    sixYardLeft <- 63.2
    sixYardRight <- 36.8
    goalPostLeft <- 56.45
    goalPostRight <- 43.55
    CentreSpot <- 50
    
    # other dimensions
    centreCirle_d <- 20
    
    
    circleFun <- function(center  =  c(0,0), diameter  =  1, npoints  =  100){
      r  =  diameter / 2
      tt <- seq(0, 2*pi, length.out  =  npoints)
      xx <- center[1] + r * cos(tt)
      yy <- center[2] + r * sin(tt)
      return(data.frame(x  =  xx, y  =  yy))
    }
    
    #### create center circle ####
    center_circle <- circleFun(c(halfwayline, CentreSpot),
                               centreCirle_d, npoints  =  1000)
    if(BasicFeatures  ==  TRUE){
      ## initiate the plot, set some boundries to the plot
      p <- list( xlim(c(xmin - xmin_offset, xmax + xmax_offset)), ylim(c(ymin - ymin_offset, ymax + ymax_offset)),
                 # add the theme
                 theme_blankPitch(),
                 # add the base rectangle of the pitch
                 geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                           fill  =  grass_colour, colour = line_colour),
                 # add the 18 yard box defensive
                 geom_rect(aes(xmin = xmin, xmax = boxEdgeDef, ymin = boxEdgeLeft,
                               ymax = boxEdgeRight),
                           fill  =  grass_colour, colour  =  line_colour) ,
                 # add the 18 yard box offensive
                 geom_rect(aes(xmin = boxEdgeOff, xmax = xmax, ymin = boxEdgeLeft,
                               ymax = boxEdgeRight),
                           fill = grass_colour, colour = line_colour) ,
                 # add halway line
                 geom_segment(aes(x = halfwayline, y = ymin, xend = halfwayline,
                                  yend = ymax), colour = line_colour) ,
                 # add the goal Defensive
                 geom_segment(aes(x = xmin, y = goalPostLeft, xend = xmin,
                                  yend = goalPostRight), colour =  goal_colour, size  =  1) ,
                 # add the goal offensive
                 geom_segment(aes(x = xmax, y = goalPostLeft, xend = xmax, yend = goalPostRight),
                              colour = goal_colour, size = 1)
      )
      
    }else{
      ## initiate the plot, set some boundries to the plot
      p <- list( xlim(c(xmin - xmin_offset, xmax + xmax_offset)) , ylim(c(ymin - ymin_offset, ymax + ymax_offset)),
                 # add the theme
                 theme_blankPitch() ,
                 # add the base rectangle of the pitch
                 geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                           fill =  grass_colour, colour = line_colour) ,
                 # add the 18 yard box defensive
                 geom_rect(aes(xmin = xmin, xmax = boxEdgeDef, ymin = boxEdgeLeft,
                               ymax = boxEdgeRight),
                           fill = grass_colour, colour = line_colour) ,
                 # add the 18 yard box offensive
                 geom_rect(aes(xmin = boxEdgeOff, xmax = xmax, ymin = boxEdgeLeft,
                               ymax = boxEdgeRight), fill = grass_colour, colour = line_colour) ,
                 # add halway line
                 geom_segment(aes(x  =  halfwayline, y = ymin, xend = halfwayline,
                                  yend  =  ymax),colour = line_colour) ,
                 # add the six yard box Defensive
                 geom_rect(aes(xmin = xmin, xmax = sixYardDef, ymin = sixYardLeft,
                               ymax = sixYardRight), fill = grass_colour, colour = line_colour)  ,
                 # add the six yard box offensive
                 geom_rect(aes(xmin = sixYardOff, xmax = xmax, ymin = sixYardLeft,
                               ymax = sixYardRight), fill = grass_colour, colour = line_colour) ,
                 # add centre circle
                 # geom_path(data = center_circle, aes(x = x,y = y), colour  =  line_colour) +
                 # add penalty spot left
                 #geom_point(aes(x = penSpotDef, y = CentreSpot), colour = line_colour) +
                 # add penalty spot right
                 #geom_point(aes(x = penSpotOff, y = CentreSpot), colour = line_colour) +
                 # add centre spot
                 geom_point(aes(x = halfwayline, y = CentreSpot), colour = line_colour) ,
                 # add the goal Defensive
                 geom_segment(aes(x = xmin, y = goalPostLeft, xend = xmin,
                                  yend = goalPostRight), colour = goal_colour, size = 1) ,
                 # add the goal offensive
                 geom_segment(aes(x = xmax, y = goalPostLeft, xend = xmax,
                                  yend = goalPostRight), colour = goal_colour, size = 1)
      )
    }
    
    if (arrow == TRUE){
      p <- list( p, geom_segment(aes(x=45,xend=55,
                                     y=10,yend=10), color=line_colour, 
                                 arrow = arrow(length = unit(0.25, "cm")), size=.5) )
      p <- list(p,  geom_segment(aes(x=45,xend=55,
                                     y=90,yend=90), color=line_colour, 
                                 arrow = arrow(length = unit(0.25, "cm")), size=.5) )
    }
    if (arrow2 == TRUE){
      p <- list(p,  geom_segment(aes(x=3,xend=8,
                                     y=50,yend=50), color=line_colour, 
                                 arrow = arrow(length = unit(0.25, "cm")), size=.5) )
      
    }
    
    if (thirds == TRUE){
      p <- list( p , geom_segment(aes(x=33.3,xend=33.3, y=0,yend=100), size=.5, color='grey', linetype='dashed', size=0.5) +
                   geom_segment(aes(x=66.7,xend=66.7, y=0,yend=100), size=.5, color='grey', linetype='dashed', size=0.5) )
      
    }
    
    return(p)
    
  }
  
  x <- heatMapData %>% 
    filter(drill_title == "{T:M} {V:PANAMA} {I:FIRST HALF}") %>% 
    filter(athlete_name == 'Christian Pulisic') %>% 
    select(data) %>% 
    unnest(data) %>% 
    mutate(
      t = 1:n(),
      # isPeak = is_peak(speed_ms, 10) & speed_ms > 5.5,
      isRun = is_run(speed_ms, 5, 5.5),
      runId = id_run(isRun) %>% as.factor()
    ) %>% 
    # select(t, speed_ms, isRun, runId) %>% 
    group_by(runId) %>%
    mutate(runId = ifelse(runId == 0, 0, ifelse(length(runId)<5, 0, runId)) %>%  as.factor())
    
    gg <- ggplot() +
      geom_point(data = x, aes(x = t, y = speed_ms, color = runId, group = 1)) +
      geom_line(data = x, aes(x = t, y = speed_ms, color = runId, group = 1)) +
      geom_hline(yintercept = 5.5) 
    
   x <- x %>% 
      filter(runId != 0) %>% 
      arrange(time_hmss)
   
    ggplot() + 
      # createPitchList() + 
      # theme(aspect.ratio = 68/105) +
        geom_path(
          data = x,
          aes(x = longitude, y = latitude, group = runId),
          arrow = arrow(type = "open", length = unit(0.1, "inches"))
        ) 
    
    #Same thing but with HML ----
    
    calc_acc <- function(v, t) {
      x <- tibble(v = v)
      x <- x %>% mutate(
        a = (v-lag(v,1))/t
      ) 
      return(x$a)
    }
    
    # data %>%
    #   mutate(ES=(0.0037*(Speed_ms^2)/9.81)+Acceleration/9.81) %>%
    #   mutate(EM=sqrt(Acceleration^2+9.81^2)/9.81) %>%
    #   mutate(EC = ifelse(Acceleration>0,((155.4*ES^5)-(30.4*ES^4)-(43.3*ES^3)+(46.3*ES^2)+(19.5*ES)+(3.6*1.29))*EM+(0.01*(Speed_ms)^2), ((-30.4*ES^4)-(5.0975*ES^3)+(46.3*ES^2)+(17.696*ES)+(3.6*1.29))*EM+(0.01*(Speed_ms)^2))) %>%
    #   mutate(MetabolicPower=EC*Speed_ms)
    
    metabolic_power2 <- function(v){
      # v - Running Speed in meters per second
      
      #Filter using a butterworth filter. 
      v <- signal::filter(butter(n = 4, W = .1, type = 'low'), v) %>% as.numeric()
      
      #Constant of gravity
      g <- 9.81
      
      #Calculate Acceleration
      # a <- abs((v-lag(v,1))/.1)
      a <- (v-lag(v,1))/.1
      
      es <- (0.0037*(v^2)/g)+a/g

      em <- sqrt(a^2+g^2)/g
      
      ec <- ifelse(a>0,((155.4*es^5)-(30.4*es^4)-(43.3*es^3)+(46.3*es^2)+(19.5*es)+(3.6*1.29))*em+(0.01*(v)^2), ((-30.4*es^4)-(5.0975*es^3)+(46.3*es^2)+(17.696*es)+(3.6*1.29))*em+(0.01*(v)^2))
      
      mp <- ec*v
      
    }
    
    metabolic_power <- function(v){
      # v - Running Speed in meters per second
      
      #Filter using a butterworth filter. 
      v <- signal::filter(butter(n = 4, W = .1, type = 'low'), v) %>% as.numeric()
      
      #Constant of gravity
      g <- 9.81
      
      #Calculate Acceleration
      # a <- abs((v-lag(v,1))/.1)
      a <- (v-lag(v,1))/.1

      #Effective Gravity
      g1 <- sqrt(a^2+9.81^2)
     
      # Equivalent slope
      es <- tan(0.5*pi-atan(g/a))
      
      #Equivalent Mass
      em <- g1/g
      
      # a fixed terrain constant to account for extra energy required for the grass
      kt <- 1.29
      
      #calculate Energy Cost
      ec <- (155.4*es^5 - 30.4*es^4 - 43.3*es^3 + 46.3*es^2 + 19.5*es + 3.6) * em * kt

      #Metabolic Power 
      mp <- (ec*v)
      
      return(mp)
      
    }
    
    detach("package:dplyr")
    detach("package:stats")
    library(signal)
    library(stats)
    library(dplyr)

    x <- heatMapData %>% 
      filter(drill_title == "{T:M} {V:PANAMA} {I:FIRST HALF}") %>% 
      filter(athlete_name == 'Christian Pulisic') %>% 
      select(data) %>% 
      unnest(data) %>% 
      mutate(
        # speed_ms = signal::filter(butter(n = 4, W = .1, type = 'low'), speed_ms) %>% as.numeric(),
        t = 1:n() / 10,
        dist = speed_ms/10,
        # acc = calc_acc(speed_ms, .1) %>% abs(),
        hml = metabolic_power2(speed_ms),
        hmld = ifelse(hml >= 25.5, dist, NA),
        isRun = is_run(hml, 5, 25.5),
        runId = id_run(isRun) %>% as.factor()
      ) %>%  
      group_by(runId) %>%
      mutate(runId = ifelse(runId == 0, 0, ifelse(length(runId)<5, 0, runId)) %>%  as.factor())
    
    x %>% 
      ungroup() %>% 
      summarise(hmld = sum(hmld, na.rm = T))
    
    tbl(con_mysql, 'ssDrill') %>% 
      filter(drill_date == sessionDate) %>% 
      filter(drill_title == "{T:M} {V:PANAMA} {I:FIRST HALF}") %>% 
      filter(name_display == 'Christian Pulisic') %>% 
      pull(hml_dist)
      
    
    ggplot(data = x) + geom_line(aes(x = t, y = hml))

    gg <- ggplot() +
      geom_point(data = x, aes(x = t, y = hmld, color = runId, group = 1)) +
      geom_line(data = x, aes(x = t, y = hmld, color = runId, group = 1)) +
      geom_hline(yintercept = 25.5) 
    
    gg
    
    
    gg %>% ggplotly()
    
    x <- x %>% 
      filter(runId != 0) %>% 
      arrange(time_hmss)
    
    gg <- ggplot() + 
      # createPitchList() + 
      # theme(aspect.ratio = 68/105) +
      scale_y_continuous(limits = c(28.54057, 28.54161)) +
      scale_x_continuous(limits = c(-81.38948, -81.38867)) +
      geom_path(
        data = x,
        aes(x = longitude, y = latitude, group = runId),
        arrow = arrow(type = "open", length = unit(0.1, "inches"))
      ) 
   
    gg %>% ggplotly()
    

# R background try --------------------------------------------------------

  
    library(ggsoccer)

    scale_0_to_1 <- function(x, coordMin, coordMax){(x-coordMin)/(coordMax-coordMin)}
    
    x <- x %>% 
      mutate(
        xCoord = scale_0_to_1(longitude, coordMin = -81.38948, coordMax = -81.38867)*100,
        yCoord = scale_0_to_1(latitude,  coordMin = 28.54057, coordMax = 28.54161)*100
      )
    
    ggplot() + 
      geom_rect(aes(xmin = -81.38948, xmax = -81.38867, ymin = 28.54057, ymax = 28.54161))
    


    rotate_coord <- function(x,y,angle, type=c("degrees","radial"), method=c("transform","polar","polar_extended"), center=c(0,0), translate=NULL, stretch=NULL, flip=FALSE){
      
      type <- match.arg(type)
      method <- match.arg(method)
      if(!(length(translate)==2 || is.null(translate))){stop("translation coordinates should be a vector of length 2")}
      if(!(is.logical(flip))){stop("Flip should be TRUE or FALSE")}
      
      if(flip){
        x <- -x
      }
      
      
      if(!is.null(stretch)){
        x <- x*stretch
        y <- y*stretch
        center <- center*stretch
        if(!is.null(translate)){translate<- translate*stretch}
      }
      
      
      x <- x-center[1]
      y <- y-center[2]
      
      
      if(type=="degrees"){angle <- angle*pi/180}
      if(type=="radial" && angle>(2*pi)){warning("Angle is bigger than 2pi are you sure it's in rads", call. = F)}
      
      if(method=="polar" || method=="polar_extended"){
        r <-sqrt(x^2+y^2)
        phi <- atan2(x,y)
        new_x <- r*sin(phi+angle)
        new_y <- r*cos(phi+angle)
        xy <- cbind(new_x,new_y)
      }
      
      if(method=="polar_extended"){
        switch(type,
               degrees={phi <- (phi+angle)*180/pi},
               radial={phi <- phi+angle}
        )
        ext_list <- list(Coordinates=xy, Angles=phi, Distance_from_center=r)
        return(invisible(ext_list))
        
      }
      
      
      if(method=="transform"){
        conversionmatrix <- matrix(c(cos(angle),sin(angle),-sin(angle),cos(angle)), ncol=2, nrow=2)
        xy <- cbind(x,y)%*%conversionmatrix
      }
      
      xy[,1] <- xy[,1]+center[1]
      xy[,2] <- xy[,2]+center[2]
      
      if(!is.null(translate)){
        xy[,1] <- xy[,1]+translate[1]
        xy[,2] <- xy[,2]+translate[2]
      }
      
      
      
      return(xy)
    }

    

    #120yds by 75  yds
    shape <- tribble(
      ~lon, ~lat,
      -81.38937930745146, 28.54058159913237, #BL
      -81.38868132963186, 28.54058996360919, #TL
      -81.38869582115048, 28.54157988108667, #TR
      -81.38939591475506, 28.54157173302617  #BR
    )
    
    dgr <- atan((( shape$lat[4] - 0)/(shape$lon[4] - 0)))*180/pi
    
    coordCenter <- c((min(shape$lon)+max(shape$lon))/2,(min(shape$lat)+max(shape$lat))/2) 
    
    b <- rotate_coord(x = shape$lon, y = shape$lat, center = coordCenter, angle = dgr, type = 'degrees') 
    d1 <- rotate_coord(x = x$longitude, y = x$latitude, center = coordCenter, angle = dgr, type = 'degrees') %>% 
      as.data.frame()
    
      
    shape2 <- tibble(lon = b[,1], lat = b[,2])
    x2 <- x %>% 
      ungroup() %>% 
      mutate(
        lon = d1$V1, 
        lat = d1$V2
      ) 
    
    # Expand and round
    ggplot(shape, aes(x = lon, y = lat)) +
      geom_polygon(fill = 'white') #+
      # geom_path(
      #   data = x2,
      #   aes(x = lon, y = lat, group = runId),
      #   arrow = arrow(type = "open", length = unit(0.1, "inches"))
      # ) 

    ggplot() +
      annotate_pitch() +
      coord_flip() +
      geom_path(
        data = x,
        aes(x = xCoord, y = yCoord, group = runId),
        arrow = arrow(type = "open", length = unit(0.1, "inches"))
      ) +
      scale_y_reverse() +
      scale_x_reverse() +
      theme_void()
  
  