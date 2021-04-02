



  #SUMMARY DATA MANAGEMENT -------------------------------------------------------------
  #Functions for the processing and upload to database and reporting infrastructure

  ## Convert Functions ----

  convert_apex_export_to_excel_dtr <- function(sonra) {

    #This function takes the .csv export from statsports apex and converts the fields

    sonra %>% mutate(
      `Drill Title`  = create_clean_drill_name(
        title = `Drill Title`,
        type = dpart_type(`Drill Title`),
        num = dpart_number(`Drill Title`),
        var = dpart_variation(`Drill Title`),
        field = dpart_field(`Drill Title`),
        rep = dpart_repTime(`Drill Title`),
        int = dpart_interval(`Drill Title`)
      ),
      drill_date = mdy_hms(paste0(`Drill Date`," ","00:00:00")),
      time_in_heart_rate_zone6 = `Time In Heart Rate Zone6`,
      `time_in_red_zone (min)` = `Time In Red Zone`,
      `Impacts z3-z6` = `Impacts Zone3` + `Impacts Zone4` + `Impacts Zone5` + `Impacts Zone6`,
      `Impacts z4-z6` = `Impacts Zone4` + `Impacts Zone5` + `Impacts Zone6`,
      `Impacts z5-z6` = `Impacts Zone5` + `Impacts Zone6`,
      `HRE Per Min` = `Heart Rate Exertion` / `Total Time`,
      `Accelerations Z3 to Z6` = `Accelerations Zone3` + `Accelerations Zone4` + `Accelerations Zone5` + `Accelerations Zone6`,
      `Accelerations Z4 to Z6` = `Accelerations Zone4` + `Accelerations Zone5` + `Accelerations Zone6`,
      `Decelerations Z3 to Z6` = `Decelerations Zone3` + `Decelerations Zone4` + `Decelerations Zone5` + `Decelerations Zone6`,
      `Decelerations Z4 to Z6` = `Decelerations Zone4` + `Decelerations Zone5` + `Decelerations Zone6`,
      `SPI Per Min` = `Speed Intensity` / `Total Time`,
      `DSL Per Min` = `Dynamic Stress Load` / `Total Time`,
      `HRE Per Min` = `Heart Rate Exertion` / `Total Time`
    )  %>% select(
      drill_date,
      `last_name` = `Player Last Name`,
      `total_time (min)` = `Total Time`,
      `total_distance` = `Distance Total`,
      `distance_zone6` = `Distance Zone 6 (Absolute)`,
      `hml` = `HML Distance`,
      `drill_type` = `Drill Title`,
      `display_name` = `Player Display Name`,
      `hml_distance_per_minute` = `HML Distance Per Minute`,
      `max_speed` = `Max Speed`,
      `position` = `Player Position`,
      `distance_per_minute` = `Distance Per Min`,
      `high_speed_running_ab` = `High Speed Running (Absolute)`,
      `session_type` = `Session Type`,
      `speed_exertion` = `Speed Intensity`,
      `sprints` = Sprints,
      `dynamic_stress_load` = `Dynamic Stress Load`,
      `average_metabolic_power` = `Average Metabolic Power`,
      `HSR Per Minute` = `HSR Per Minute (Absolute)`,
      `heart_rate_exertion` = `Heart Rate Exertion`,
      `time_in_red_zone (min)`,
      `time_in_heart_rate_zone6 (min)` = time_in_heart_rate_zone6,
      `power_acceleration` = `Explosive Distance`,
      `impacts_zone6` = `Impacts Zone6`,
      `Impacts z3-z6`,
      `Impacts z4-z6`,
      `Impacts z5-z6`,
      `Impacts_zone6` = `Impacts Zone6`,
      `HRE Per Min`,
      `Accelerations Z3 to Z6`,
      `Accelerations Z4 to Z6`,
      `Decelerations Z3 to Z6`,
      `Decelerations Z4 to Z6`,
      `SPI Per Min`,
      `DSL Per Min`,
      `HRE Per Min`,
      session_start_time = `Session Start Time`
    )

  }

  convert_sonra_export_to_excel_dtr <- function(apex) {

    #This function takes the .csv export from statsports sonra and converts the fields

    apex %>% mutate(
      `Drill Title`  = create_clean_drill_name(
        title = `Drill Title`,
        type = dpart_type(`Drill Title`),
        num = dpart_number(`Drill Title`),
        var = dpart_variation(`Drill Title`),
        field = dpart_field(`Drill Title`),
        rep = dpart_repTime(`Drill Title`),
        int = dpart_interval(`Drill Title`)
      ),
      drill_date = mdy_hms(paste0(`Drill Date`," ","00:00:00")),
      time_in_heart_rate_zone6 = `Time In Heart Rate Zone 6`,
      `time_in_red_zone (min)` = `Time In Red Zone`,
      `Impacts z3-z6` = `Impacts Zone 3` + `Impacts Zone 4` + `Impacts Zone 5` + `Impacts Zone 6`,
      `Impacts z4-z6` = `Impacts Zone 4` + `Impacts Zone 5` + `Impacts Zone 6`,
      `Impacts z5-z6` = `Impacts Zone 5` + `Impacts Zone 6`,
      `HRE Per Min` = `Heart Rate Exertion` / `Total Time`,
      `Accelerations Z3 to Z6` = `Accelerations Zone 3` + `Accelerations Zone 4` + `Accelerations Zone 5` + `Accelerations Zone 6`,
      `Accelerations Z4 to Z6` = `Accelerations Zone 4` + `Accelerations Zone 5` + `Accelerations Zone 6`,
      `Decelerations Z3 to Z6` = `Decelerations Zone 3` + `Decelerations Zone 4` + `Decelerations Zone 5` + `Decelerations Zone 6`,
      `Decelerations Z4 to Z6` = `Decelerations Zone 4` + `Decelerations Zone 5` + `Decelerations Zone 6`,
      `SPI Per Min` = `Speed Intensity` / `Total Time`,
      `DSL Per Min` = `Dynamic Stress Load` / `Total Time`,
      `HRE Per Min` = `Heart Rate Exertion` / `Total Time`
    )  %>% select(
      drill_date,
      `last_name` = `Player Last Name`,
      `total_time (min)` = `Total Time`,
      `total_distance` = `Total Distance`,
      `distance_zone6` = `Distance Zone 6 (Absolute)`,
      `hml` = `HML Distance`,
      `drill_type` = `Drill Title`,
      `display_name` = `Player Display Name`,
      `hml_distance_per_minute` = `HMLD Per Minute`,
      `max_speed` = `Max Speed`,
      `position` = `Player Position`,
      `distance_per_minute` = `Distance Per Min`,
      `high_speed_running_ab` = `High Speed Running (Absolute)`,
      `session_type` = `Session Type`,
      `speed_exertion` = `Speed Intensity`,
      `sprints` = Sprints,
      `dynamic_stress_load` = `Dynamic Stress Load`,
      `average_metabolic_power` = `Average Metabolic Power`,
      `HSR Per Minute` = `HSR Per Minute (Absolute)`,
      `heart_rate_exertion` = `Heart Rate Exertion`,
      `time_in_red_zone (min)`,
      `time_in_heart_rate_zone6 (min)` = time_in_heart_rate_zone6,
      `power_acceleration` = `Explosive Distance (Absolute)`,
      `impacts_zone6` = `Impacts Zone 6`,
      `Impacts z3-z6`,
      `Impacts z4-z6`,
      `Impacts z5-z6`,
      `Impacts_zone6` = `Impacts Zone 6`,
      `HRE Per Min`,
      `Accelerations Z3 to Z6`,
      `Accelerations Z4 to Z6`,
      `Decelerations Z3 to Z6`,
      `Decelerations Z4 to Z6`,
      `SPI Per Min`,
      `DSL Per Min`,
      `HRE Per Min`,
      session_start_time = `Session Start Time`
    )

  }

  convert_apex_export_to_ss <- function(apex){

    apex %>% select(
      `session_id` = `Session Id`,
      `sess_type` = `Session Type`,
      `session_title` = `Session Title`,
      `session_date` = `Session Date`,

      `session_start_time` = `Session Start Time`,
      `session_end_time` = `Session End Time`,

      `drill_id` = `Drill Id`,
      `drill_title` = `Drill Title`,
      `drill_date` = `Drill Date`,

      `drill_start_time` = `Drill Start Time`,
      `drill_end_time` = `Drill End Time`,

      `name_display` = `Player Display Name`,
      `name_first` = `Player First Name`,
      `name_last` = `Player Last Name`,
      `position` = `Player Position`,
      `name_id` = `Custom Player Id`,
      #`player_id` = `Player id`,

      `dur` = `Total Time`,

      `dist` = `Distance Total`,

      `vel_z1_dist` = `Distance Zone 1 (Absolute)`,
      `vel_z2_dist` = `Distance Zone 2 (Absolute)`,
      `vel_z3_dist` = `Distance Zone 3 (Absolute)`,
      `vel_z4_dist` = `Distance Zone 4 (Absolute)`,
      `vel_z5_dist` = `Distance Zone 5 (Absolute)`,
      `vel_z6_dist` = `Distance Zone 6 (Absolute)`,

      `hsr_dist` = `High Speed Running (Absolute)`,

      `vel_z1_dur` = `Time Zone 1 (Absolute)`,
      `vel_z2_dur` = `Time Zone 2 (Absolute)`,
      `vel_z3_dur` = `Time Zone 3 (Absolute)`,
      `vel_z4_dur` = `Time Zone 4 (Absolute)`,
      `vel_z5_dur` = `Time Zone 5 (Absolute)`,
      `vel_z6_dur` = `Time Zone 6 (Absolute)`,

      `vel_z5_cnt` = `Entries Zone 5 (Absolute)`,
      `vel_z6_cnt` = `Entries Zone 6 (Absolute)`,

      `vel_avg` = `Average Speed`,
      `vel_max` = `Max Speed`,

      # `vel_z1_rel_dist` = `Distance Zone 1 (Relative)`,
      # `vel_z2_rel_dist` = `Distance Zone 2 (Relative)`,
      # `vel_z3_rel_dist` = `Distance Zone 3 (Relative)`,
      # `vel_z4_rel_dist` = `Distance Zone 4 (Relative)`,
      # `vel_z5_rel_dist` = `Distance Zone 5 (Relative)`,
      # `vel_z6_rel_dist` = `Distance Zone 6 (Relative)`,

      # `hsr_rel` = `High Speed Running (Relative)`,

      # `vel_z1_rel_dur` = `Time Zone 1 (Relative)`,
      # `vel_z2_rel_dur` = `Time Zone 2 (Relative)`,
      # `vel_z3_rel_dur` = `Time Zone 3 (Relative)`,
      # `vel_z4_rel_dur` = `Time Zone 4 (Relative)`,
      # `vel_z5_rel_dur` = `Time Zone 5 (Relative)`,
      # `vel_z6_rel_dur` = `Time Zone 6 (Relative)`,

      # `vel_z5_rel_cnt` = `Entries Zone 5 (Relative)`,
      # `vel_z6_rel_cnt` = `Entries Zone 6 (Relative)`,

      `sprint_cnt` = `Sprints`,   # Defaults 5.5 hold at least 1 second at sprint speed
      `sprint_dist` = `Sprint Distance`,

      `acc_z1_dist` = `Accelertions Total Distance Z1`,
      `acc_z2_dist` = `Accelertions Total Distance Z2`,
      `acc_z3_dist` = `Accelertions Total Distance Z3`,
      `acc_z4_dist` = `Accelertions Total Distance Z4`,
      `acc_z5_dist` = `Accelertions Total Distance Z5`,
      `acc_z6_dist` = `Accelertions Total Distance Z6`,

      `dec_z1_dist` = `Decelertions Total Distance Z1`,
      `dec_z2_dist` = `Decelertions Total Distance Z2`,
      `dec_z3_dist` = `Decelertions Total Distance Z3`,
      `dec_z4_dist` = `Decelertions Total Distance Z4`,
      `dec_z5_dist` = `Decelertions Total Distance Z5`,
      `dec_z6_dist` = `Decelertions Total Distance Z6`,

      # `acc_cnt` = `Accelerations`, #Zone 4-6 count
      # `dec_cnt` = `Decelerations`, #Zone 4-6 count

      `explosive_dist` = `Explosive Distance`, # acc & dec z5-6 dist

      `acc_z1_cnt` = `Accelerations Zone1`,
      `acc_z2_cnt` = `Accelerations Zone2`,
      `acc_z3_cnt` = `Accelerations Zone3`,
      `acc_z4_cnt` = `Accelerations Zone4`,
      `acc_z5_cnt` = `Accelerations Zone5`,
      `acc_z6_cnt` = `Accelerations Zone6`,

      `high_int_burst_cnt` = `Number of High Intensity Bursts`, #What is the high intensity burst?
      `high_int_burst_dur` = `Duration of High Intensity Bursts`, #What is the duration of high intenisty bursts

      `dec_z1_cnt` = `Decelerations Zone1`,
      `dec_z2_cnt` = `Decelerations Zone2`,
      `dec_z3_cnt` = `Decelerations Zone3`,
      `dec_z4_cnt` = `Decelerations Zone4`,
      `dec_z5_cnt` = `Decelerations Zone5`,
      `dec_z6_cnt` = `Decelerations Zone6`,

      `si` = `Speed Intensity`,
      `si_z1` = `Speed Intensity Zone1`,
      `si_z2` = `Speed Intensity Zone2`,
      `si_z3` = `Speed Intensity Zone3`,
      `si_z4` = `Speed Intensity Zone4`,
      `si_z5` = `Speed Intensity Zone5`,
      `si_z6` = `Speed Intensity Zone6`,

      `dsl` = `Dynamic Stress Load`,
      `dsl_z1` = `Dynamic Stress Load Zone1`,
      `dsl_z2` = `Dynamic Stress Load Zone2`,
      `dsl_z3` = `Dynamic Stress Load Zone3`,
      `dsl_z4` = `Dynamic Stress Load Zone4`,
      `dsl_z5` = `Dynamic Stress Load Zone5`,
      `dsl_z6` = `Dynamic Stress Load Zone6`,

      `mp_z1_dur` = `Metabolic Time Zone 1`,
      `mp_z2_dur` = `Metabolic Time Zone 2`,
      `mp_z3_dur` = `Metabolic Time Zone 3`,
      `mp_z4_dur` = `Metabolic Time Zone 4`,
      `mp_z5_dur` = `Metabolic Time Zone 5`,
      `mp_z6_dur` = `Metabolic Time Zone 6`,

      `mp_z1_dist` = `Metabolic Distance Zone 1`,
      `mp_z2_dist` = `Metabolic Distance Zone 2`,
      `mp_z3_dist` = `Metabolic Distance Zone 3`,
      `mp_z4_dist` = `Metabolic Distance Zone 4`,
      `mp_z5_dist` = `Metabolic Distance Zone 5`,
      `mp_z6_dist` = `Metabolic Distance Zone 6`,

      `mp_avg` = `Average Metabolic Power`,
      # `` = `Equivalent Metabolic Distance`,

      `hre` = `Heart Rate Exertion`,

      `hr_z1_dur` = `Time In Heart Rate Zone1`,
      `hr_z2_dur` = `Time In Heart Rate Zone2`,
      `hr_z3_dur` = `Time In Heart Rate Zone3`,
      `hr_z4_dur` = `Time In Heart Rate Zone4`,
      `hr_z5_dur` = `Time In Heart Rate Zone5`,
      `hr_z6_dur` = `Time In Heart Rate Zone6`,

      `hr_max` = `Max Heart Rate`,
      `hr_avg` = `Average Heart Rate`,

      `impact` = `Impacts`,
      `impact_z1` = `Impacts Zone1`,
      `impact_z2` = `Impacts Zone2`,
      `impact_z3` = `Impacts Zone3`,
      `impact_z4` = `Impacts Zone4`,
      `impact_z5` = `Impacts Zone5`,
      `impact_z6` = `Impacts Zone6`,

      `impact_l_vert` = `Left Vertical Impact`,
      `impact_l_vert_avg` = `Left Average Vert Impact`, #The different is?
      `impact_l_lat` = `Left Lateral Impact`,
      `impact_l_antpost` = `Left Ant Post Impact`,
      `impact_l_mag` = `Left Mag Impact`,

      `impact_r_vert` = `Right Vertical Impact`,
      `impact_r_lat` = `Right Lateral Impact`,
      `impact_r_antpost` = `Right Ant Post Impact`,
      `impact_r_mag` = `Right Mag Impact`,
      `impact_r_vert_avg` = `Right Average Vert Impact`,

      `step_l_ttl` = `Total Left Steps`,
      `step_r_ttl` = `Total Right Steps`,
      `step_balance` = `Step Balance`,

      `fatigue_index` = `Fatigue Index`,

      `hml_dist` = `HML Distance`,  #How is it calculated
      `hml_dur` = `HML Time`,
      `hml_cnt` = `HML Efforts`,

      ####

      # `` = `Total Loading`,
      # `` = `Acceleration Impulse`, #
      # `` = `Total Deceleration Loading`, # Send me a paper on this one.
      # `` = `Heart Rate Recovery Beats`, # What are you
      # `` = `Heart Rate Load`, # What are you?
      # `` = `Energy Expenditure (KCal)`,

      # `` = `Distance Zonal`,
      # `` = `Metabolic Time Zonal`,
      # `` = `Metabolic Distance Zonal`,

      # `` = `Lower Speed Loading`,

      # `` = `Distance Per Min`,
      # `` = `HML Distance Per Minute`,
      # `` = `Time In Red Zone`,
      # `` = `HSR Per Minute (Absolute)`,

      # `` = `Max Acceleration (Disabled)`,
      # `` = `Max Deceleration (Disabled)`,
      # `` = `Entries Zone 3 (Relative) (Disabled)`,
      # `` = `Entries Zone 4 (Relative) (Disabled)`,

      # `` = `Acute Vs Chronic Ratio`,

    ) %>%
      mutate(
        session_date = mdy(session_date),
        drill_date = mdy(drill_date)
      )

  }

  convert_sonra_export_to_ss <- function(sonra){

    sonra %>% select(
      # `session_id` = `Session Id`, #Doesn't Exist
      `sess_type` = `Session Type`,
      `session_title` = `Session Title`,
      `session_date` = `Session Date`,

      `session_start_time` = `Session Start Time`,
      `session_end_time` = `Session End Time`,

      # `drill_id` = `Drill Id`, #No Drill
      `drill_title` = `Drill Title`,
      `drill_date` = `Drill Date`,

      `drill_start_time` = `Drill Start Time`,
      `drill_end_time` = `Drill End Time`,

      `name_display` = `Player Display Name`,
      `name_first` = `Player First Name`,
      `name_last` = `Player Last Name`,
      `position` = `Player Position`,
      # `name_id` = `Custom Player Id`,
      #`player_id` = `Player id`,

      `dur` = `Total Time`,

      `dist` = `Total Distance`,

      `vel_z1_dist` = `Distance Zone 1 (Absolute)`,
      `vel_z2_dist` = `Distance Zone 2 (Absolute)`,
      `vel_z3_dist` = `Distance Zone 3 (Absolute)`,
      `vel_z4_dist` = `Distance Zone 4 (Absolute)`,
      `vel_z5_dist` = `Distance Zone 5 (Absolute)`,
      `vel_z6_dist` = `Distance Zone 6 (Absolute)`,

      `hsr_dist` = `High Speed Running (Absolute)`,

      `vel_z1_dur` = `Time Zone 1 (Absolute)`,
      `vel_z2_dur` = `Time Zone 2 (Absolute)`,
      `vel_z3_dur` = `Time Zone 3 (Absolute)`,
      `vel_z4_dur` = `Time Zone 4 (Absolute)`,
      `vel_z5_dur` = `Time Zone 5 (Absolute)`,
      `vel_z6_dur` = `Time Zone 6 (Absolute)`,

      `vel_z5_cnt` = `Entries Zone 5 (Absolute)`,
      `vel_z6_cnt` = `Entries Zone 6 (Absolute)`,

      `vel_avg` = `Average Speed`,
      `vel_max` = `Max Speed`,

      # `vel_z1_rel_dist` = `Distance Zone 1 (Relative)`,
      # `vel_z2_rel_dist` = `Distance Zone 2 (Relative)`,
      # `vel_z3_rel_dist` = `Distance Zone 3 (Relative)`,
      # `vel_z4_rel_dist` = `Distance Zone 4 (Relative)`,
      # `vel_z5_rel_dist` = `Distance Zone 5 (Relative)`,
      # `vel_z6_rel_dist` = `Distance Zone 6 (Relative)`,

      # `hsr_rel` = `High Speed Running (Relative)`,

      # `vel_z1_rel_dur` = `Time Zone 1 (Relative)`,
      # `vel_z2_rel_dur` = `Time Zone 2 (Relative)`,
      # `vel_z3_rel_dur` = `Time Zone 3 (Relative)`,
      # `vel_z4_rel_dur` = `Time Zone 4 (Relative)`,
      # `vel_z5_rel_dur` = `Time Zone 5 (Relative)`,
      # `vel_z6_rel_dur` = `Time Zone 6 (Relative)`,

      # `vel_z5_rel_cnt` = `Entries Zone 5 (Relative)`,
      # `vel_z6_rel_cnt` = `Entries Zone 6 (Relative)`,

      `sprint_cnt` = `Sprints`,   # Defaults 5.5 hold at least 1 second at sprint speed
      `sprint_dist` = `Sprint Distance`,

      `acc_z1_dist` = `Accelerations Total Distance Zone 1`,
      `acc_z2_dist` = `Accelerations Total Distance Zone 2`,
      `acc_z3_dist` = `Accelerations Total Distance Zone 3`,
      `acc_z4_dist` = `Accelerations Total Distance Zone 4`,
      `acc_z5_dist` = `Accelerations Total Distance Zone 5`,
      `acc_z6_dist` = `Accelerations Total Distance Zone 6`,

      `dec_z1_dist` = `Decelerations Total Distance Zone 1`,
      `dec_z2_dist` = `Decelerations Total Distance Zone 2`,
      `dec_z3_dist` = `Decelerations Total Distance Zone 3`,
      `dec_z4_dist` = `Decelerations Total Distance Zone 4`,
      `dec_z5_dist` = `Decelerations Total Distance Zone 5`,
      `dec_z6_dist` = `Decelerations Total Distance Zone 6`,

      # `acc_cnt` = `Accelerations`, #Zone 4-6 count
      # `dec_cnt` = `Decelerations`, #Zone 4-6 count

      `explosive_dist` = `Explosive Distance (Absolute)`, # acc & dec z5-6 dist

      `acc_z1_cnt` = `Accelerations Zone 1`,
      `acc_z2_cnt` = `Accelerations Zone 2`,
      `acc_z3_cnt` = `Accelerations Zone 3`,
      `acc_z4_cnt` = `Accelerations Zone 4`,
      `acc_z5_cnt` = `Accelerations Zone 5`,
      `acc_z6_cnt` = `Accelerations Zone 6`,

      `high_int_burst_cnt` = `Number Of High Intensity Bursts`, #What is the high intensity burst?
      `high_int_burst_dur` = `Duration Of High Intensity Bursts`, #What is the duration of high intenisty bursts

      `dec_z1_cnt` = `Decelerations Zone 1`,
      `dec_z2_cnt` = `Decelerations Zone 2`,
      `dec_z3_cnt` = `Decelerations Zone 3`,
      `dec_z4_cnt` = `Decelerations Zone 4`,
      `dec_z5_cnt` = `Decelerations Zone 5`,
      `dec_z6_cnt` = `Decelerations Zone 6`,

      `si` = `Speed Intensity`,
      `si_z1` = `Speed Intensity Zone 1 (Absolute)`,
      `si_z2` = `Speed Intensity Zone 2 (Absolute)`,
      `si_z3` = `Speed Intensity Zone 3 (Absolute)`,
      `si_z4` = `Speed Intensity Zone 4 (Absolute)`,
      `si_z5` = `Speed Intensity Zone 5 (Absolute)`,
      `si_z6` = `Speed Intensity Zone 6 (Absolute)`,

      `dsl` = `Dynamic Stress Load`,
      `dsl_z1` = `Dynamic Stress Load Zone 1`,
      `dsl_z2` = `Dynamic Stress Load Zone 2`,
      `dsl_z3` = `Dynamic Stress Load Zone 3`,
      `dsl_z4` = `Dynamic Stress Load Zone 4`,
      `dsl_z5` = `Dynamic Stress Load Zone 5`,
      `dsl_z6` = `Dynamic Stress Load Zone 6`,

      `mp_z1_dur` = `Metabolic Time Zone 1`,
      `mp_z2_dur` = `Metabolic Time Zone 2`,
      `mp_z3_dur` = `Metabolic Time Zone 3`,
      `mp_z4_dur` = `Metabolic Time Zone 4`,
      `mp_z5_dur` = `Metabolic Time Zone 5`,
      `mp_z6_dur` = `Metabolic Time Zone 6`,

      `mp_z1_dist` = `Metabolic Distance Zone 1`,
      `mp_z2_dist` = `Metabolic Distance Zone 2`,
      `mp_z3_dist` = `Metabolic Distance Zone 3`,
      `mp_z4_dist` = `Metabolic Distance Zone 4`,
      `mp_z5_dist` = `Metabolic Distance Zone 5`,
      `mp_z6_dist` = `Metabolic Distance Zone 6`,

      `mp_avg` = `Average Metabolic Power`,
      # `` = `Equivalent Metabolic Distance`,

      `hre` = `Heart Rate Exertion`,

      `hr_z1_dur` = `Time In Heart Rate Zone 1`,
      `hr_z2_dur` = `Time In Heart Rate Zone 2`,
      `hr_z3_dur` = `Time In Heart Rate Zone 3`,
      `hr_z4_dur` = `Time In Heart Rate Zone 4`,
      `hr_z5_dur` = `Time In Heart Rate Zone 5`,
      `hr_z6_dur` = `Time In Heart Rate Zone 6`,

      `hr_max` = `Max Heart Rate`,
      `hr_avg` = `Average Heart Rate`,

      `impact` = `Impacts`,
      `impact_z1` = `Impacts Zone 1`,
      `impact_z2` = `Impacts Zone 2`,
      `impact_z3` = `Impacts Zone 3`,
      `impact_z4` = `Impacts Zone 4`,
      `impact_z5` = `Impacts Zone 5`,
      `impact_z6` = `Impacts Zone 6`,

      `impact_l_vert` = `Left Vertical Impact`,
      `impact_l_vert_avg` = `Left Average Vertical Impact`, #The different is?
      `impact_l_lat` = `Left Lateral Impact`,
      `impact_l_antpost` = `Left Anterior Post Impact`,
      `impact_l_mag` = `Left Mag Impact`,

      `impact_r_vert` = `Right Vertical Impact`,
      `impact_r_lat` = `Right Lateral Impact`,
      `impact_r_antpost` = `Right Anterior Post Impact`,
      `impact_r_mag` = `Right Mag Impact`,
      `impact_r_vert_avg` = `Right Average Vertical Impact`,

      `step_l_ttl` = `Total Left Steps`,
      `step_r_ttl` = `Total Right Steps`,
      `step_balance` = `Step Balance`,

      `fatigue_index` = `Fatigue Index`,

      `hml_dist` = `HML Distance`,  #How is it calculated
      `hml_dur` = `HML Time`,
      `hml_cnt` = `HML Efforts`,

      ####

      # `` = `Total Loading`,
      # `` = `Acceleration Impulse`, #
      # `` = `Total Deceleration Loading`, # Send me a paper on this one.
      # `` = `Heart Rate Recovery Beats`, # What are you
      # `` = `Heart Rate Load`, # What are you?
      # `` = `Energy Expenditure (KCal)`,

      # `` = `Distance Zonal`,
      # `` = `Metabolic Time Zonal`,
      # `` = `Metabolic Distance Zonal`,

      # `` = `Lower Speed Loading`,

      # `` = `Distance Per Min`,
      # `` = `HML Distance Per Minute`,
      # `` = `Time In Red Zone`,
      # `` = `HSR Per Minute (Absolute)`,

      # `` = `Max Acceleration (Disabled)`,
      # `` = `Max Deceleration (Disabled)`,
      # `` = `Entries Zone 3 (Relative) (Disabled)`,
      # `` = `Entries Zone 4 (Relative) (Disabled)`,

      # `` = `Acute Vs Chronic Ratio`,

    ) %>%
      mutate(
        session_date = mdy(session_date),
        drill_date = mdy(drill_date),
        drill_id = paste0(drill_date," ",drill_start_time," ", drill_title)
      )

  }

  convert_ss_to_ssSess <- function(ss){

    con <- db.con()

    ssScoreInput <- dbReadTable(con, "ssScoreInput")

    ssSess <- ss %>%
      filter(drill_title %in% c("Entire Session - Live", "ENTIRE SESSION-LIVE")) %>%
      select(-contains("drill"))  %>% #Drop Drill Related Fields
      left_join(.,ssScoreInput,by = c("name_display" = "name_display")) %>%
      mutate( #Create Steve's Training Load Calculations
        extensive_score = calc_extensive_score(dist = dist, dist_game = dist_game, dur = dur, dist_min_game = dist_min_game, mp_avg = mp_avg, mp_avg_game = mp_avg_game ,hml_dist = hml_dist,hml_dist_game = hml_dist_game),
        speed_score = calc_speed_score(dur = dur, vel_z5_dist = vel_z5_dist, vel_z6_dist = vel_z6_dist, hml_dist = hml_dist, si = si, vel_z6_dist_game = vel_z6_dist_game, hml_dist_game = hml_dist_game, vel_z56_dist_game = vel_z56_dist_game, si_game = si_game, hml_dist_min_game = hml_dist_min_game, vel_z56_dist_min_game = vel_z56_dist_min_game, si_min_game = si_min_game),
        muscle_score = calc_muscle_score(dur = dur, explosive_dist = explosive_dist, hml_dist = hml_dist, dsl = dsl, acc_z3_cnt = acc_z3_cnt, acc_z4_cnt = acc_z4_cnt, acc_z5_cnt = acc_z5_cnt, acc_z6_cnt = acc_z6_cnt, dec_z3_cnt = dec_z3_cnt, dec_z4_cnt = dec_z4_cnt, dec_z5_cnt = dec_z5_cnt, dec_z6_cnt = dec_z6_cnt, explosive_dist_game = explosive_dist_game, hml_dist_min_game = hml_dist_min_game, dsl_game = dsl_game, dsl_min_game = dsl_min_game, acc_z3456_cnt_game = acc_z3456_cnt_game, acc_z456_cnt_game = acc_z456_cnt_game, dec_z3456_cnt_game = dec_z3456_cnt_game, dec_z456_cnt_game = dec_z456_cnt_game),
        cv_score = calc_cv_score(dur = dur, hre = hre, hr_z5_dur = hr_z5_dur, hr_z6_dur = hr_z6_dur, hre_game = hre_game, hre_min_game = hre_min_game, hr_z56_dur_game = hr_z56_dur_game, hr_z6_dur_game = hr_z6_dur_game)
      ) %>%
      mutate(
        extensive_load = calc_load_scale(extensive_score),
        speed_load = calc_load_scale(speed_score),
        muscle_load = calc_load_scale(muscle_score),
        cv_load = calc_load_scale(cv_score)
      )

    dbDisconnect(con)

    return(ssSess)

  }

  convert_ss_to_ssDrill <- function(ss, ssDrillRename = NA){

    # ssDrillRename is off currently until we finish the renaming process.

    ss %>%
      filter(drill_title %!in% c("Entire Session", "Entire Session - Live", "ENTIRE SESSION-LIVE")) %>%
      # left_join(., ssDrillRename, by = c("drill_id" = "drill_id")) %>%
      # rowwise %>%
      # mutate(
      #   drill_title = if (is.na(drill_renamed) == T) {drill_title} else {drill_renamed}
      # ) %>%
      # select(-drill_renamed) %>%
      # ungroup() %>%
      mutate(drill_title = toupper(drill_title)) %>%
      mutate(
        drill_type = dpart_type(drill_title),
        drill_num = dpart_number(drill_title),
        drill_variation = dpart_variation(drill_title),
        drill_field = dpart_field(drill_title),
        drill_field_length = dpart_field_length(drill_field),
        drill_field_width = dpart_field_width(drill_field),
        drill_reptime = dpart_repTime(drill_title),
        drill_rep = dpart_repTime_rep(drill_reptime),
        drill_time = dpart_repTime_time(drill_reptime),
        drill_int = dpart_interval(drill_title),
        drill_note = dpart_note(drill_title),
        drill_tag = dpart_tag(drill_title),
        drill_name_short  = create_clean_drill_name(title = drill_title, type = drill_type, num = drill_num,var = drill_variation, field = drill_field),
        drill_name_full = create_clean_drill_name(title = drill_title, type = drill_type, num = drill_num,var = drill_variation, field = drill_field, rep = drill_reptime,int = drill_int,note = drill_note, tag = drill_tag)
      ) %>%
      select(contains("drill"), everything())


  }

  convert_ssSess_to_sbSess <- function(ssSess){

    sbNameProfile <- get_sb_ss_user_profile()

    #Test Whats Matching
    # ssSess %>% select(name_display) %>% unique() %>%
    #   full_join(., sbNameProfile, by = c("name_display" = "ss_name")) %>%
    #   filter(is.na(sb_name)) %>% .$name_display

    message("WARNING Are exlcuded ", "J.Y 14, ", "Jesse Gonzalez, ", "Brad Guzan, ")

    ssSess %>%
      filter(name_display %!in% c("J.Y 14", "Jesse Gonzalez", "Brad Guzan")) %>%
      left_join(., sbNameProfile, by = c("name_display" = "ss_name")) %>%
      mutate(
        session_start_time = as.character(session_start_time),
        session_end_time = as.character(session_end_time),
        start_date = session_date,
        end_date = session_date,
        start_time = paste0(Sys.Date()," ",session_start_time) %>% ymd_hms() %>% format("%I:%M %p"),
        end_time = paste0(Sys.Date()," ",session_end_time) %>% ymd_hms() %>% format("%I:%M %p")
      )

  }

  convert_ssDrill_to_sbDrill <- function(ssDrill){
    sbNameProfile <- get_sb_ss_user_profile()

    ssDrill %>%
      left_join(., sbNameProfile, by = c("name_display" = "ss_name")) %>%
      mutate(
        start_date = drill_date,
        end_date = drill_date,
        start_time = paste0(Sys.Date()," ",drill_start_time) %>% ymd_hms() %>% format("%I:%M %p"),
        end_time = paste0(Sys.Date()," ",drill_end_time) %>% ymd_hms() %>% format("%I:%M %p")
      )
  }

  ## Upload SQL and SB API Functions ----

  append_ssSess_to_sql <- function(ssSess){
    con <- db.con()

    sessIdInDb <- dbGetQuery(con, "SELECT session_id FROM ssSess GROUP BY session_id") %>% pull(session_id)

    ssSess2 <- ssSess %>% filter(session_id %!in% sessIdInDb)

    if (nrow(ssSess2) > 0) {
      dbWriteTable(con, name = 'ssSess', value = ssSess2, append = T, overwrite = F)
    }
    dbDisconnect(con)
    return(nrow(ssSess2))
  }
  delete_sess_from_sql <- function(dateStart = NA, dateEnd = NA, ids = NA){

    con <- db.con()

    if (!is.na(dateStart) & !is.na(dateEnd)){
      r <- dbExecute(con, paste0("DELETE FROM ssSess WHERE session_date >= '",dateStart,"' AND session_date <= '",dateEnd,"'"))
    }

    if (!is.na(ids)){
      r <-  dbExecute(con, paste0("DELETE FROM ssSess WHERE session_id IN (",ids,")"))
    }

    dbDisconnect(con)
    return(r)

  }
  overwrite_ssSess_to_sql <- function(ssSess){

    con <- db.con()

    ids <- ssSess$session_id %>% unique() %>% paste0(., collapse = ", ")

    delete_sess_from_sql(ids = ids)

    r <- append_ssSess_to_sql(ssSess)

    dbDisconnect(con)

    return(r)

  }

  append_ssDrill_to_sql <- function(ssDrill){

    con <- db.con()

    drillIdInDb <- dbGetQuery(con, "SELECT drill_id FROM ssDrill GROUP BY drill_id") %>% pull(drill_id)

    ssDrill2 <- ssDrill %>% filter(drill_id %!in% drillIdInDb)

    if (nrow(ssDrill2) > 0) {
      dbWriteTable(con, name = 'ssDrill', value = ssDrill2, append = T, overwrite = F)
    }

    dbDisconnect(con)
    return(nrow(ssDrill2))

  }
  delete_drill_from_sql <- function(dateStart = NA, dateEnd = NA, ids = NA){

    con <- db.con()

    if (!is.na(dateStart) & !is.na(dateEnd)){
      r <- dbExecute(con, paste0("DELETE FROM ssDrill WHERE drill_date >= '",dateStart,"' AND drill_date <= '",dateEnd,"'"))
    }

    if (!is.na(ids)){
      r <- dbExecute(con, paste0("DELETE FROM ssDrill WHERE drill_id IN (",ids,")"))
    }

    dbDisconnect(con)

    return(r)

  }
  overwrite_ssDrill_to_sql <- function(ssDrill){

    con <- db.con()

    # ids <- ssDrill$drill_id %>% unique() %>% paste0(., collapse = ", ")

    dateStart <- ssDrill$drill_date %>% unique() %>% min()
    dateEnd <- ssDrill$drill_date %>% unique() %>% max()

    delete_drill_from_sql(dateStart = dateStart, dateEnd = dateEnd)
    r <- append_ssDrill_to_sql(ssDrill)

    dbDisconnect(con)
    return(r)

  }

  append_sbSess_to_sb <- function(sbSess){

    if (any(is.na(sbSess$user_id))) {
      message("The following users are missing matching smartabase statsports profile names: ", paste(sbSess %>% filter(is.na(user_id)) %>% pull(name_display) %>% unique(), collapse = ", "))
      return()
    }



    neon::push_smartabase(
      df = sbSess,
      form = 'MNT - Statsports Session',
      current_date_format = "ymd",
      entered_by_user_id = 39456
    )

  }
  delete_sess_from_sb <- function(dateStart, dateEnd){

    dateStart <- dateStart %>% dmy() %>% format("%d/%m/%Y")
    dateEnd   <- dateEnd %>% dmy() %>% format("%d/%m/%Y")

    con <- db.con()

    #Get event_ids from SB in Date Range
    event_ids <- pull_smartabase(
      form = "MNT - Statsports Session",
      start_date = dateStart,
      end_date = dateEnd
    ) %>% pull(event_id) %>% unique()

    #Delete event_ids from SB
    lapply(X = event_ids, FUN = function(x) {
      delete_smartabase(event_id = x)
    })

    dbDisconnect(con)

  }
  overwrite_sbSess_to_sb <- function(sbSess){

    #Get Dates and Session Ids From sbSess
    startDate <- sbSess %>% pull(start_date) %>% unique() %>% min()  %>% format(., "%d/%m/%Y")
    endDate   <- sbSess %>% pull(start_date) %>% unique() %>% max()  %>% format(., "%d/%m/%Y")

    delete_sess_from_sb(startDate, endDate)

    append_sbSess_to_sb(sbSess)

  }

  append_sbDrill_to_sb <- function(sbDrill){

    if (any(is.na(sbDrill$user_id))) {
      message("The following users are missing matching smartabase statsports profile names: ", paste(ssDrill2 %>% filter(is.na(user_id)) %>% pull(name_display) %>% unique(), collapse = ", "))
      return()
    }

    neon::push_smartabase(
      df = sbDrill,
      form = 'MNT - Statsports Drill',
      current_date_format = "ymd",
      entered_by_user_id = 39456
    )
  }
  delete_drill_from_sb <- function(dateStart, dateEnd){

    con <- db.con()

    dateStart <- dateStart %>% dmy() %>% format("%d/%m/%Y")
    dateEnd   <- dateEnd %>% dmy() %>% format("%d/%m/%Y")

    #Get event_ids from SB in Date Range
    event_ids <- pull_smartabase(
      form = "MNT - Statsports Drill",
      start_date = dateStart,
      end_date = dateEnd
    ) %>% pull(event_id) %>% unique()

    if (length(event_ids) > 0) {
      #Delete event_ids from SB
      lapply(X = event_ids, FUN = function(x) {
        delete_smartabase(event_id = x)
      })
    }

    dbDisconnect(con)

  }
  overwrite_sbDrill_to_sb <- function(sbDrill){
    #Get Dates and Session Ids From sbSess

    startDate <- sbDrill %>% pull(start_date) %>% unique() %>% ymd() %>% min()  %>% format(., "%d/%m/%Y")
    endDate   <- sbDrill %>% pull(start_date) %>% unique() %>% ymd() %>% max()  %>% format(., "%d/%m/%Y")

    delete_drill_from_sb(startDate, endDate)

    append_sbDrill_to_sb(sbDrill)

  }

  ## Misc Functions ----

  get_sb_ss_user_profile <- function(){

    neon::pull_smartabase(
      form              = "STATSports Profile",
      type              = "profile",
      filter_user_key   = "group",
      filter_user_value = "USMNT"
    ) %>%
      select(user_id, `sb_name` = about, `ss_name` = `Display Name`)

  }

  ## Drill Editing Functions ----

  dpart <- function(drillName, part) {

    if (part %!in% c("T", "N", "V", "F", "R", "I", "NT", "TG")) {stop()}

    str_extract(drillName, paste0("\\{",part,":(.*?)\\}")) %>%
      str_remove(., "\\}") %>%
      str_remove(., paste0("\\{",part,":"))

  }

  dpart_type <- function(drillName){
    dpart(drillName, "T")
  }

  dpart_variation <- function(drillName){
    dpart(drillName, "V")
  }

  dpart_number <- function(drillName) {
    dpart(drillName, "N")
  }

  dpart_field <- function(drillName){

    dpart(drillName, "F")

  }

  dpart_field_length <- function(field) {

    field %>%
      str_extract_all(.,"[:digit:]+X") %>%
      paste0() %>%
      str_extract_all(.,"[:digit:]+",simplify = F) %>%
      sapply(., as.numeric) %>%
      sapply(., sum)

  }

  dpart_field_width <- function(field) {

    field %>%
      str_extract_all(.,"X[:digit:]+") %>%
      paste0() %>%
      str_extract_all(.,"[:digit:]+",simplify = F) %>%
      sapply(., as.numeric) %>%
      sapply(., sum)

  }

  dpart_repTime <- function(drillName){
    dpart(drillName, "R")
  }

  dpart_repTime_rep <- function(repTime) {
    str_extract(repTime, "[:digit:]+X") %>% str_extract(.,"[:digit:]+" ) %>% strtoi(.,0)
  }

  dpart_repTime_time <- function(repTime) {
    str_extract(repTime, "X[:digit:]+") %>% str_remove(., "X")
  }

  dpart_interval <- function(drillName){
    dpart(drillName, "I")
  }

  dpart_note <- function(drillName){
    dpart(drillName, "NT")
  }

  dpart_tag <- function(drillName) {

    dpart(drillName, "TG")
    # touch <- str_extract(x, "TCH:[:digit:]+") %>% str_extract(., "[:digit:]+") %>% strtoi(.,0)
    # pass  <- str_extract(x, "PSS:[:digit:]+") %>% str_extract(., "[:digit:]+") %>% strtoi(.,0)
    # team  <- str_extract(x, "TM:[:digit:]+") %>% str_extract(., "[:digit:]+") %>% strtoi(.,0)

  }

  create_clean_drill_name <- function(title, type = NA, num = NA, var = NA, field = NA, rep = NA, int = NA, note = NA, tag = NA) {

    a <- paste0(
      paste0(type," - "),
      paste0(num," - "),
      paste0(var," - "),
      paste0(field," - "),
      paste0(rep," - "),
      paste0(int," - "),
      paste0(note," - "),
      paste0(tag," - ")
    ) %>%
      str_remove_all(., "NA - ") %>%
      str_remove(., " - $")

    b <- c()
    for (i in 1:length(a)){
      x <- if (a[i] == '') {title[i]} else a[i]
      b <- c(b, x)
    }

    b

  }

  ## Training Load Functions --------------------------------------------------

  perMin <- function(metric,time){metric/time}

  calc_extensive_score <- function(dist, dist_game, dur, dist_min_game, mp_avg, mp_avg_game, hml_dist, hml_dist_game){

    dist_min <- dist / dur

    x <-
      ((dist/dist_game) * 50) +
      ((dist_min/dist_min_game) * 25) +
      ((mp_avg / mp_avg_game) * 12.5)  +
      ((hml_dist / hml_dist_game) * 12.5)

    x

  }

  calc_speed_score <- function(dur, vel_z5_dist, vel_z6_dist, hml_dist, si, vel_z6_dist_game, hml_dist_game, vel_z56_dist_game, si_game, hml_dist_min_game, vel_z56_dist_min_game, si_min_game) {

    vel_z56_dist <- vel_z5_dist + vel_z6_dist
    hml_dist_min <- hml_dist / dur
    vel_z56_dist_min <- vel_z56_dist / dur
    si_min <- si / dur

    x <-
      ((vel_z6_dist / vel_z6_dist_game) * 37.5) +
      ((hml_dist / hml_dist_game) * 20) +
      ((vel_z56_dist / vel_z56_dist_game) + 10) +
      ((si / si_game) + 10) +
      ((hml_dist_min / hml_dist_min_game) + 7.5) +
      ((vel_z56_dist_min / vel_z56_dist_min_game) * 7.5) +
      ((si_min / si_min_game) * 7.5)

    x


  }

  calc_muscle_score <- function(dur, explosive_dist, hml_dist, dsl, acc_z3_cnt, acc_z4_cnt, acc_z5_cnt, acc_z6_cnt, dec_z3_cnt, dec_z4_cnt, dec_z5_cnt, dec_z6_cnt, explosive_dist_game, hml_dist_min_game, dsl_game, dsl_min_game, acc_z3456_cnt_game, acc_z456_cnt_game, dec_z3456_cnt_game, dec_z456_cnt_game) {

    hml_dist_min <- hml_dist / dur
    dsl_min <- dsl / dur
    acc_z456_cnt <- acc_z4_cnt + acc_z5_cnt + acc_z6_cnt
    acc_z3456_cnt <- acc_z3_cnt + acc_z4_cnt + acc_z5_cnt + acc_z6_cnt
    dec_z456_cnt <- dec_z4_cnt + dec_z5_cnt + dec_z6_cnt
    dec_z3456_cnt <- dec_z3_cnt + dec_z4_cnt + dec_z5_cnt + dec_z6_cnt

    x <-
      ((explosive_dist/explosive_dist_game) * 20) +
      ((hml_dist_min / hml_dist_min_game) * 5) +
      ((dsl / dsl_game) * 35) +
      ((dsl_min / dsl_min_game) * 5) +
      ((acc_z3456_cnt/ acc_z3456_cnt_game) * 7.5) +
      ((acc_z456_cnt/ acc_z456_cnt_game) * 10) +
      ((dec_z3456_cnt/ dec_z3456_cnt_game) * 7.5) +
      ((dec_z456_cnt / dec_z456_cnt_game) * 10)

    x

  }

  calc_cv_score <- function(dur, hre, hr_z5_dur, hr_z6_dur, hre_game, hre_min_game, hr_z56_dur_game, hr_z6_dur_game){

    #! Create system to check if values actually exsist? Need to evaluate HR qualities.
    hre_min <- hre / dur
    hr_z56_dur <- hr_z5_dur + hr_z6_dur

    x <-
      ((hr_z6_dur/hr_z6_dur_game)*25)+
      ((hr_z56_dur/hr_z56_dur_game)*20)+
      ((hre/hre_game)*40)+
      ((hre_min/hre_min_game)*15)

    x

  }

  calc_load_scale <- function(score) {

    score[score < 45] <- 1
    score[score >= 45 & score < 58] <- 2
    score[score >= 58 & score < 70] <- 3
    score[score >= 70 & score < 94] <- 4
    score[score >= 94] <- 5

    score

  }

  convert_pos_num_to_name <- function(pos){

    pos[pos == 1] <- "GK"
    pos[pos == 2 | pos == 3] <- "FB"
    pos[pos == 4 | pos == 5] <- "CB"
    pos[pos == 6 ] <- "CM"
    pos[pos == 7 | pos == 11] <- "WAM"
    pos[pos == 8 | pos == 10] <- "AM"
    pos[pos == 9] <- "FWD"

    pos

  }

  # RAW DATA MANAGEMENT -----------

  '%!in%' <- function(x,y)!('%in%'(x,y))

  append_statsports_raw_to_sqlite <- function(x, pos) {

    x <- x %>%
      as_tibble() %>%
      rename(
        `session_id` = "Session Id",
        `player_id` = "Player Id",
        `name_display` = "Player Display Name",
        `time` = "Time",
        `elapsed_time` = "Elapsed Time",
        `speed_ms` = "Speed (m/s)",
        `acc_impulse_mss` = "Instantaneous Acceleration Impulse",
        `heart_rate_interval` = "Heart Rate Interval",
        `latitude` = "Latitude",
        `longitude` = "Longitude",
        `accl_x` = "Accl X",
        `accl_y` = "Accl Y",
        `accl_z` = "Accl Z",
        `gyro_x` = "Gyro X",
        `gyro_y` = "Gyro Y",
        `gyro_z` = "Gyro Z" ,
        `hacc` = "Hacc",
        `hdop` = "Hdop",
        `signal_quality` = "Quality of Signal",
        `satellite_num` = "No. of Satellites"
      )

    xSize <- object.size(x) %>% as.numeric()
    sFileSizeCum <<- sFileSizeCum + xSize

    dbWriteTable(con, 'ss100hz', value = x, append = T, overwrite = F)

    x <- x %>%
      select(
        session_id,
        player_id,
        name_display,
        time,
        speed_ms,
        latitude,
        longitude,
        hacc,
        hdop,
        signal_quality,
        satellite_num
      ) %>%
      mutate(time = substr(time,1,nchar(time)-1)) %>%
      group_by(session_id, player_id, name_display, time) %>%
      summarise(.groups = 'keep',
                speed_ms = mean(speed_ms),
                latitude = mean(latitude),
                longitude = mean(longitude),
                hacc = mean(hacc),
                hdop = mean(hdop),
                signal_quality = mean(signal_quality),
                satellite_num = mean(satellite_num)
      ) %>% ungroup()

    dbWriteTable(con, 'ss10hz', value = x, append = T, overwrite = F)

    x <- x %>% select(session_id, player_id, name_display) %>% unique()

    dbWriteTable(con, 'ssId', value = x, append = T, overwrite = F)

    setTxtProgressBar(pb, sFileSizeCum)

  }

  append_statsports_10hz_sqlite <- function(x, pos) {

    # xSize <- object.size(x) %>% as.numeric()
    # sFileSizeCum <<- sFileSizeCum + xSize

    x <- x %>%
      as_tibble() %>%
      rename(
        `name_display` = "Player Display Name",
        `time_stamp` = "Time",
        `speed_ms` = "Speed m/s"
      )  %>%
      mutate(
        session_date = basename(sFile) %>% str_remove(., '.csv')
      ) %>%
      select(
        session_date,
        name_display,
        time_stamp,
        speed_ms
      )

    dbWriteTable(con, 'ssMatch10hz', value = x, append = T, overwrite = F)

    # setTxtProgressBar(pb, sFileSizeCum)

  }

