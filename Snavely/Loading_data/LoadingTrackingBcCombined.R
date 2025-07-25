# This file is to load all the data
library(tidyverse)

# Original data frames
tracking <- arrow::read_parquet("data/tracking.parquet")
games <- read_csv("data/games.csv")
players <- read_csv("data/players.csv")
player_play <- read_csv("data/player_play.csv")
plays <- read_csv("data/plays.csv")

# Putting plays in the correct direction
tracking <- tracking |>
  mutate(
    # Plays will always go from left to right
    x = ifelse(playDirection == "left", 120 - x, x),
    y = ifelse(playDirection == "left", 160 / 3 - y, y),
    # flip player direction and orientation
    dir = ifelse(playDirection == "left", dir + 180, dir),
    dir = ifelse(dir > 360, dir - 360, dir),
    o = ifelse(playDirection == "left", o + 180, o),
    o = ifelse(o > 360, o - 360, o)
  )

tracking <- tracking |> 
  filter(frameType != "BEFORE_SNAP")

# Getting velocity in the x (endzone) and y (sideline) direction
tracking <- tracking |>
  mutate(
    # Converting degrees into radians
    dir_rad = pi * (dir / 180),
    # get angle of motion in x (endzone) and y (sideline) direction
    # NA checks are for the ball
    dir_x = ifelse(is.na(dir), NA_real_, sin(dir_rad)),
    dir_y = ifelse(is.na(dir), NA_real_, cos(dir_rad)),
    # directional speed (also known as velocity)
    s_x = dir_x * s,
    s_y = dir_y * s,
    # directional acceleration
    a_x = dir_x * a,
    a_y = dir_y * a,
    a_x2 = ((s_x - lag(s_x)) / 0.1),
    a_y2 = ((s_y - lag(s_y)) / 0.1)
  )  

# Filtering for all running back plays
# bc_id = ball carrier id, bc_club = team id
plays_rb_runs <- player_play |> 
  # Make sure it's a running play
  filter(hadRushAttempt == 1) |> 
  left_join(select(players, nflId, position)) |> 
  filter(position == "RB") |> 
  select(gameId, playId, bc_id = nflId, bc_club = teamAbbr)

# Tracking data will now show only plays with running backs
tracking_rb_runs <- tracking |> 
  inner_join(plays_rb_runs)

## TRACKING FOR ALL RUNNING BACK PLAYS
# Keeping only frames between handoff and end of play event (out of bounds, tackle, TD)
tracking_rb_runs <- tracking_rb_runs |> 
  group_by(gameId, playId) |> 
  mutate(
    frame_handoff = frameId[which(event == "handoff")][1],
    frame_end = frameId[which(event %in% c("out_of_bounds", "tackle", "touchdown"))][1]
  ) |> 
  ungroup() |> 
  filter(!is.na(frame_handoff), !is.na(frame_end)) |> 
  filter(frameId >= frame_handoff & frameId <= frame_end)

## TRACKING FOR RUNNING BACKS ONLY FOR RUNNING BACK PLAYS
# Tracking data for ball carriers only
tracking_bc <- tracking_rb_runs |> 
  filter(nflId == bc_id) |> 
  left_join(select(players, nflId, weight)) |> 
  mutate(ke = 0.5 * weight * s^2,
         m_x = weight * s_x, # momentum in the x
         dis_x = ifelse(gameId == lag(gameId) & playId == lag(playId), x - lag(x), NA),
         work = ifelse(gameId==lag(gameId) & playId == lag(playId), ke-lag(ke), NA),
         positive_work = ifelse(gameId==lag(gameId) & playId == lag(playId), pmax(ke-lag(ke),0), NA),
         COD = ifelse(gameId==lag(gameId) & playId == lag(playId), abs(dir - lag(dir)), NA),
         jerk = ifelse(gameId==lag(gameId) & playId ==lag(playId), (a-lag(a))/.1, NA),
         s_mph = s * (3600 / 1760),
         a_mpsh = a * (3600 / 1760),
         dir_a = a*cos(dir_rad),
         dir_a_mpsh = dir_a*(3600/1760),
         dir_a_right = (s_x * a_x2 + s_y * a_y2) / sqrt(s_x ^ 2 + s_y^2),
         dir_a_right_mpsh = dir_a_right * (3600/1760))

tracking_bc_after_contact <- tracking_bc |> 
  group_by(gameId, playId) |> 
  mutate(
    frame_contact = frameId[which(event == "first_contact")][1],
    frame_end = frameId[which(event %in% c("out_of_bounds", "tackle", "touchdown"))][1]
  ) |> 
  ungroup() |> 
  filter(!is.na(frame_contact), !is.na(frame_end)) |> 
  filter(frameId >= frame_contact & frameId <= frame_end)

# Running back stats per play
rb_stats_per_play <- tracking_bc |> 
  group_by(playId, gameId, bc_id, displayName) |> 
  summarize(dis_gained = sum(dis),
            mean_ke = mean(ke),
            mean_m_x = mean(m_x),
            sd_ke=sd(ke),
            dis_gained_x = sum(dis_x, na.rm = TRUE),
            mean_pos_work = mean(positive_work, na.rm=TRUE),
            sd_pos_work = sd(positive_work, na.rm=TRUE),
            effort_consistency = mean_ke/sd_ke,
            total_pos_work=sum(positive_work, na.rm=TRUE),
            avg_accel = mean(a),
            avg_COD = mean(COD, na.rm = TRUE) / n(),
            avg_jerk = mean(jerk, na.rm=TRUE)) |> 
  ungroup() |> 
  left_join(select(plays, playId, gameId, yardsGained, expectedPointsAdded)) |> 
  left_join(select(player_play, playId, bc_id = nflId, gameId, rushingYards))

# Adding after contact stats
after_contact <- tracking_bc_after_contact |> 
  group_by(playId, gameId, bc_id, displayName) |> 
  summarize(dis_gained_x_ac = sum(dis_x),
            avg_accel_ac = mean(a),
            time_ac = n() / 10,
            mean_ke_ac = mean(ke)) |> 
  ungroup() |> 
  mutate(weighted_ke_ac = (1 + sqrt(time_ac)) * mean_ke_ac)

rb_stats_per_play <- rb_stats_per_play |> 
  left_join(after_contact) |> 
  mutate(dis_gained_x_ac = ifelse(is.na(dis_gained_x_ac), 0, dis_gained_x_ac),
         avg_accel_ac = ifelse(is.na(avg_accel_ac), 0, avg_accel_ac)) 

# Overall rb stats
rb_stats_total <- rb_stats_per_play |> 
  group_by(bc_id, displayName) |> 
  summarize(
    total_dis_gained = sum(dis_gained),
    total_dis_gained_x = sum(dis_gained_x, na.rm = TRUE),
    avg_dis_gained_x = mean(dis_gained_x, na.rm = TRUE) / n(),
    mean_ke = mean(mean_ke),
    avg_sd_ke=mean(sd_ke, na.rm=TRUE),
    avg_sd_work= mean(sd_pos_work, na.rm=TRUE),
    avg_effort_consistency =mean(effort_consistency, na.rm=TRUE),
    mean_m_x = mean(mean_m_x),
    mean_pos_work = mean(mean_pos_work, na.rm=TRUE),
    total_pos_work = sum(total_pos_work, na.rm=TRUE),
    total_yards_gained = sum(yardsGained),
    avg_yards_gained = mean(yardsGained),
    avg_EPA = mean(expectedPointsAdded),
    num_of_rushes = n(),
    avg_accel = mean(avg_accel),
    avg_COD = mean(avg_COD),
    avg_jerk = mean(avg_jerk),
    avg_dis_gained_ac = mean(dis_gained_x_ac),
    avg_acc_ac = mean(avg_accel_ac)
  ) |> 
  ungroup()

rb_stats_total_filtered <- rb_stats_total |> 
  filter(num_of_rushes >= 20) 

rbs_names <- rb_stats_total_filtered$displayName

# Effort Function ---------------------------------------------------------
# Effort function
eff_function <- function(name, graph = FALSE, player_table = FALSE) {
  # Filtering the data set to only include the inputted player
  player_runs <- tracking_bc |> 
    filter(displayName == name)
  
  # Making the bins
  bins <- seq(3, round(max(player_runs$s), 2) + .2, .2)
  
  # Picking out the top two accelerations for each speed in each bins
  max_acc <- map_dfr(1:(length(bins) - 1), function(i) {
    player_runs |> 
      filter(s > bins[i], s <= bins[i + 1]) |> 
      slice_max(a, n = 2, with_ties = FALSE) |> 
      select(speed = s, acceleration = a)
  })
  
  # Fitting the first regression line
  player_lm <- lm(acceleration ~ speed, data = max_acc)
  
  # Adding confidence intervals
  confs <- predict(player_lm, interval = "confidence")
  max_acc <- max_acc |> 
    bind_cols(confs)
  
  # Filtering out outliers (any point that does not fall within a 95% conf interval)
  max_acc_clean <- max_acc |> 
    filter(acceleration >= lwr, acceleration <= upr)
  
  # Fitting the new regression line (without outliers)
  player_lm_clean <- lm(acceleration ~ speed, data = max_acc_clean)
  
  # Finding out how many points are near the line
  test_player_a <- data.frame(speed = player_runs$s, acceleration = player_runs$a)
  test_preds <- predict(player_lm_clean, newdata = test_player_a)
  
  # Final calculation for distance away from the fitted line
  player_final <- player_runs |> 
    select(a, s) |> 
    mutate(pred = test_preds) |> 
    mutate(diff = pred - a) |> 
    mutate(eff = ifelse(diff <= .25, TRUE, FALSE),
           eff_50 = ifelse(diff <= .5, TRUE, FALSE),
           eff_75 = ifelse(diff <= .75, TRUE, FALSE)) |> 
    mutate(gameId = player_runs$gameId, 
           playId = player_runs$playId, 
           bc_id = player_runs$bc_id, 
           displayName = player_runs$displayName,
           frameId = player_runs$frameId)
  
  # Final effort metric
  eff <- tibble(
    eff_metric = sum(player_final$eff == TRUE),
    eff_metric_perc = sum(player_final$eff == TRUE) / nrow(player_final) * 100,
    eff_metric_50 = sum(player_final$eff_50 == TRUE),
    eff_metric_perc_50 = sum(player_final$eff_50 == TRUE) / nrow(player_final) * 100,
    eff_metric_75 = sum(player_final$eff_75 == TRUE),
    eff_metric_perc_75 = sum(player_final$eff_75 == TRUE) / nrow(player_final) * 100,
  )
  
  # Building the graph if specified
  if (graph == TRUE) {
    player_graph <- player_runs |> 
      ggplot(aes(x = s, y = a)) +
      geom_smooth(method = lm, aes(x = speed, y = acceleration), data = max_acc_clean, lwd = 1.5, se = FALSE) +
      geom_abline(aes(color = "Regression line",
                      intercept = player_lm_clean$coefficients[1], slope = player_lm_clean$coefficients[2]),
                  lwd = 1.5,) +
      geom_abline(aes(color = "Minimum line (.25)",
                      intercept = player_lm_clean$coefficients[1] - .25, slope = player_lm_clean$coefficients[2]), 
                  lty = 2, lwd = 1.5) +
      geom_abline(aes(color = "Minimum line (.5)",
                      intercept = player_lm_clean$coefficients[1] - .5, slope = player_lm_clean$coefficients[2]),
                  lty = 2, lwd = 1.5) +
      geom_abline(aes(color = "Minimum line (.75)",
                      intercept = player_lm_clean$coefficients[1] - .75, slope = player_lm_clean$coefficients[2]), 
                  lty = 2, lwd = 1.5) +
      scale_color_manual("Line", values = c("#4B92DB", "darkgreen", "turquoise", "#FFB612")) +
      geom_point(size = 2, alpha = .5, col = "grey2") +
      xlim(0, 13) +
      ylim(0, 10) +
      labs(x = "Speed",
           y = "Acceleration",
           title = paste0(name, "'s effort is defined as the percentage of points above the minimum line"),
           caption = "Data from Weeks 1-9 of the 2022 NFL Season") +
      theme(plot.title = element_text(face = "bold",
                                      size = 20, 
                                      hjust = .5),
            legend.title = element_text(face = "bold",
                                        size = 15),
            axis.title = element_text(face = "bold",
                                      size = 15),
            axis.text = element_text(size = 13),
            plot.caption = element_text(face = "italic",
                                        size = 8))
    
    return(player_graph)
  }
  
  if (player_table == TRUE) {
    return(player_final)
  }
  
  return(eff)
}


rbs_names <- unique(rb_stats_total_filtered$displayName)

eff_movements <- purrr::map(rbs_names, eff_function) |> 
  bind_rows() |> 
  mutate(displayName = rbs_names)
# Fatigue -----------------------------------------------------------------
fatigue <- tracking_bc |> 
  group_by(gameId, bc_id, displayName) |> 
  mutate(fatigue = cumsum(dis)) |> 
  select(gameId, bc_id, playId, frameId, displayName, fatigue)

# Adding starter names ----------------------------------------------------

primary_teams <- plays_rb_runs |> 
  semi_join(rb_stats_total_filtered, by="bc_id") |> #only look at the 69 RBs
  group_by(bc_id, bc_club) |> 
  summarise(rushes=n(), .groups="drop") |> #count how many rushes each player had for each team 
  group_by(bc_id) |> 
  slice_max(order_by=rushes,n=1,with_ties=FALSE) #top team per player 


#append players' primary team names to rb_stats_total_filtered
rb_stats_teams <- rb_stats_total_filtered |> 
  left_join(primary_teams, by="bc_id")

#get top rushers per team
top_rushers_labeled <- rb_stats_teams |> 
  group_by(bc_club) |> 
  slice_max(order_by=rushes, n=1, with_ties=FALSE) |> #pick single top rusher for each team
  mutate(starter=TRUE) #mark the top rushers with starter=TRUE

#join starter labels to rb_stats_teams
rb_stats_labeled <- rb_stats_teams |> 
  left_join(top_rushers_labeled |> #join starter label from top rusher df, matching on player name and team
              select(bc_id, bc_club, starter),
            by=c("bc_id", "bc_club")) |> 
  mutate(starter=ifelse(is.na(starter), FALSE, starter)) #if NA after join, then label FALSE for starter column. if not, then label original value from starter column (TRUE)

rb_names <- rb_stats_labeled |> 
  select(bc_id, starter)
# Acceleration before contact ---------------------------------------------
#get change in acc 10 and 5 frames before first contact
acc_before_contact <- tracking_bc |> 
  #semi_join(rb_stats_teams, by="displayName") |>  #only include plays from the 69 RBs
  group_by(gameId, playId) |> 
  mutate(frame_contact = frameId[which(event=="first_contact")][1]) |>  #find first frame ID where event is first_contact and store in new frame_contact column |> 
  filter(!is.na(frame_contact)) |> 
  #look up acc at 10 and 5 frames before contact, take [1] to avoid multiple matches
  #reframe() can return multiple rows per group (unlike summarise), we extract the first one
  #basically collapse each group (play) into one row
  reframe(
    displayName=displayName[1],
    acc_10_before=a[which(frameId == frame_contact-10)][1],
    acc_5_before=a[which(frameId==frame_contact-5)][1]) |> 
  filter(!is.na(acc_10_before), !is.na(acc_5_before)) |> 
  mutate(acc_change=acc_5_before - acc_10_before,
         label=case_when(
           acc_change > 0.5 ~ "acc",
           acc_change < -0.5~ "dec",
           TRUE ~ "maintain")) 
# Final Data Frame --------------------------------------------------------

tracking_bc_filtered <- tracking_bc |> 
  filter(displayName %in% rbs_names)

tracking_bc_effort <- purrr::map(rbs_names, eff_function, player_table = TRUE) |> 
  bind_rows()

tracking_bc_combined <- left_join(tracking_bc_filtered, tracking_bc_effort, 
                                  by = c("gameId", "bc_id", "playId", "frameId", "displayName"))

tracking_bc_combined <- left_join(tracking_bc_combined, fatigue, 
                                  by = c("gameId", "bc_id", "playId", "frameId", "displayName"))

tracking_bc_play_stats <- tracking_bc_combined |> 
  group_by(gameId, playId, bc_id, displayName) |> 
  summarize(mean_ke = mean(ke),
            mean_acc = mean(a_mpsh),
            mean_speed = mean(s_mph),
            mean_jerk = mean(jerk, na.rm = TRUE),
            num_of_effort_move = sum(eff == TRUE),
            eff_move_prop = sum(eff == TRUE) / n(),
            total_dist_covered_of_game = max(fatigue)) |> 
  ungroup() |> 
  left_join(rb_stats_per_play) |> 
  left_join(rb_names) |> 
  left_join(acc_before_contact)
