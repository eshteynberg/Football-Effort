library(tidyverse)

# Data cleaning -----------------------------------------------------------

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

# Don't care about pre-snap plays
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
    a_y = dir_y * a
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
         jerk = ifelse(gameId==lag(gameId) & playId ==lag(playId), (a-lag(a))/.1, NA))

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
            avg_accel_ac = mean(a)) |> 
  ungroup()

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

summary(rb_stats_total$num_of_rushes) # Do a minimum of 20 rushes to eliminate players with low rushes

rb_stats_total_filtered <- rb_stats_total |> 
  filter(num_of_rushes >= 20)


# Saquon's Acc and Speed ----------------------------------------------------
library(broom)
# Plot of acceleration and speed for all players
tracking_bc |> 
  ggplot(aes(x = s, y = a)) +
  geom_point(alpha = .3)

saquon_runs <- tracking_bc |> 
  filter(displayName == "Saquon Barkley")

# Saquon's speed and acceleration
saquon_runs |> 
  ggplot(aes(x = s, y = a)) +
  geom_point()

max(saquon_runs$s) # 10.4 is Barkley's max speed

# Making speed bins
bins <- seq(3, 10.4, .2)

# finding the maximum acceleration for each speed bin
max_acc <- map_dfr(1:(length(bins) - 1), function(i) {
  saquon_runs |> 
    filter(s > bins[i], s <= bins[i + 1]) |> 
    slice_max(a, n = 2, with_ties = FALSE) |> 
    select(speed = s, acceleration = a)
})

# Test to see if code worked
saquon_runs |> 
  filter(s > 10.0 & s <= 10.2) |> 
  slice_max(a, n = 2, with_ties = FALSE) |> 
  select(s, a)
  
# plotting the maximum accelerations per bin
max_acc |> 
  ggplot(aes(x = speed, y = acceleration)) +
  geom_point()

# fitting a regression line 
saquon_lm <- lm(acceleration ~ speed, data = max_acc)

# Looking for outliers
saquon_tidy <- saquon_lm |> 
  tidy(conf.int = TRUE)
summary(saquon_lm)

saquon_tidy

# Plotting speed and acceleartion with first regression line
max_acc |> 
  ggplot(aes(x = speed, y = acceleration)) +
  geom_point() +
  geom_smooth(method = lm, se = TRUE, conf.int = TRUE)

# Adding predicted values to the original df
confs <- predict(saquon_lm, interval = "confidence")
max_acc <- max_acc |> 
  bind_cols(confs)

# Filtering out outliers
max_acc_clean <- max_acc |> 
  filter(acceleration >= lwr, acceleration <= upr)

# New regression line
saquon_lm_clean <- lm(acceleration ~ speed, data = max_acc_clean)
tidy(saquon_lm_clean)
summary(saquon_lm_clean)

# Maximal acceleration (y-intercept)
A_0 <- saquon_lm_clean$coefficients[1]

# Plotting new regression line
max_acc_clean |> 
  ggplot(aes(x = speed, y = acceleration)) +
  geom_point() +
  geom_smooth(method = lm, se = TRUE, conf.int = TRUE)

# Finally relaying the line onto the original data points
saquon_runs |> 
  ggplot(aes(x = s, y = a)) +
  geom_point() +
  geom_smooth(method = lm, aes(x = speed, y = acceleration), data = max_acc_clean) +
  geom_abline(intercept = saquon_lm_clean$coefficients[1], slope = saquon_lm_clean$coefficients[2]) +
  xlim(0, 17)

# Finding out how many points are near the line
test_saquon_a <- data.frame(speed = saquon_runs$s, acceleration = saquon_runs$a)
test_preds <- predict(saquon_lm_clean, newdata = test_saquon_a)

# Adding the test predictions to the df
# Definition of effort: within 1 of the predicted fitted values
saquon_final <- saquon_runs |> 
  select(a, s) |> 
  mutate(pred = test_preds) |> 
  mutate(diff = pred - a) |> 
  mutate(eff = ifelse(diff <= 1, TRUE, FALSE))

eff_metric <- (sum(saquon_final$eff == TRUE) / nrow(saquon_final)) * 100


# Creating a function -----------------------------------------------------
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
    mutate(eff = ifelse(diff <= .25, TRUE, FALSE)) |> 
    mutate(gameId = player_runs$gameId, 
           playId = player_runs$playId, 
           bc_id = player_runs$bc_id, 
           displayName = player_runs$displayName,
           frameId = player_runs$frameId)
  
  # Final effort metric
  eff <- tibble(
    eff_metric = sum(player_final$eff == TRUE),
    eff_metric_perc = sum(player_final$eff == TRUE) / nrow(player_final) * 100
  )
  
  # Building the graph if specified
  if (graph == TRUE) {
    player_graph <- player_runs |> 
      ggplot(aes(x = s, y = a)) +
      geom_smooth(method = lm, aes(x = speed, y = acceleration), data = max_acc_clean, lwd = 1.5, se = FALSE) +
      geom_abline(aes(color = "Regression line",
                      intercept = player_lm_clean$coefficients[1], slope = player_lm_clean$coefficients[2]),
                      lwd = 1.5,) +
      geom_abline(aes(color = "Minimum line",
                  intercept = player_lm_clean$coefficients[1] - .25, slope = player_lm_clean$coefficients[2]), 
                  lty = 2, lwd = 1.5) +
      scale_color_manual("Line", values = c("#4B92DB", "#FFB612")) +
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

# Test
eff_function("Rex Burkhead", graph = TRUE)
eff_function("Saquon Barkley", graph = TRUE)
eff_function("Saquon Barkley", player_table = TRUE)
eff_function("Saquon Barkley")


# Eff metric for all players ----------------------------------------------

rbs <- unique(rb_stats_total_filtered$displayName)

eff_movements <- purrr::map(rbs, eff_function) |> 
  bind_rows() |> 
  mutate(displayName = rbs)

eff_movements_top <- eff_movements |> 
  slice_max(eff_metric, n = 5) |> 
  mutate(type = "high")

eff_movements_bottom <- eff_movements |> 
  slice_min(eff_metric, n = 5) |> 
  mutate(type = "low")

eff_together <- rbind(eff_movements_top, eff_movements_bottom)

# top 5 vs. bottom 5 eff movements
eff_together |> 
  ggplot(aes(x = eff_metric, y = fct_reorder(displayName, eff_metric), fill = type)) +
  geom_col() +
  labs(x = "Number of effort movements", 
       y = "",
       title = "Top and bottom 5 players for effort movements") +
  geom_text(aes(label = eff_metric), hjust = 1, nudge_x = -.5, fontface = "bold", size = 5) +
  scale_fill_manual(values = c("#FFB612", "#4B92DB")) +
  theme(plot.title = element_text(face = "bold",
                                  size = 20, 
                                  hjust = .5),
        legend.position = "none",
        axis.title = element_text(face = "bold",
                                  size = 15),
        axis.text.y = element_text(face = "italic"),
        axis.text = element_text(size = 13))

eff_movements_perc_top <- eff_movements |> 
  slice_max(eff_metric_perc, n = 5) |> 
  mutate(type = "high")

eff_movements_perc_bottom <- eff_movements |> 
  slice_min(eff_metric_perc, n = 5) |> 
  mutate(type = "low")

eff_movements_perc <- rbind(eff_movements_perc_top, eff_movements_perc_bottom) |> 
  mutate(eff_metric_perc = round(eff_metric_perc, 2)) |> 
  mutate(perc = paste0(eff_metric_perc, "%"))


# Top 5 vs. bottom 5 eff perc
eff_movements_perc |> 
  ggplot(aes(x = eff_metric_perc, y = fct_reorder(displayName, eff_metric_perc), fill = type)) +
  geom_col() +
  labs(x = "Percentage of movements that are effortful", 
       y = "",
       title = "Top and bottom 5 players for effort movement percentages") +
  geom_text(aes(label = perc), hjust = 1, fontface = "bold", size = 5) +
  scale_fill_manual(values = c("#FFB612", "#4B92DB")) +
  theme(plot.title = element_text(face = "bold",
                                  size = 20, 
                                  hjust = .5),
        legend.position = "none",
        axis.title = element_text(face = "bold",
                                  size = 15),
        axis.text.y = element_text(face = "italic"),
        axis.text = element_text(size = 13))

# Adding fatigue ----------------------------------------------------------

fatigue <- tracking_bc |> 
  group_by(gameId, bc_id, displayName) |> 
  mutate(fatigue = cumsum(dis)) |> 
  select(gameId, bc_id, playId, frameId, displayName, fatigue)

# Effort by play ----------------------------------------------------------
tracking_bc_filtered <- tracking_bc |> 
  filter(displayName %in% rbs)

tracking_bc_effort <- purrr::map(rbs, eff_function, player_table = TRUE) |> 
  bind_rows()

tracking_bc_combined <- left_join(tracking_bc_filtered, tracking_bc_effort, 
                                  by = c("gameId", "bc_id", "playId", "frameId", "displayName"))

tracking_bc_combined <- left_join(tracking_bc_combined, fatigue, 
                                  by = c("gameId", "bc_id", "playId", "frameId", "displayName"))

tracking_bc_play_stats <- tracking_bc_combined |> 
  group_by(gameId, playId, bc_id, displayName) |> 
  summarize(mean_ke = mean(ke),
            mean_jerk = mean(jerk, na.rm = TRUE),
            num_of_effort_move = sum(eff == TRUE),
            eff_move_prop = sum(eff == TRUE) / n(),
            total_dist_covered_of_game = max(fatigue)) |> 
  left_join(rb_stats_per_play)


# Viz ---------------------------------------------------------------------
# Relation between effortful movements and fatigue
tracking_bc_play_stats |> 
  ggplot(aes(x = total_dist_covered_of_game, y = num_of_effort_move)) +
  geom_point()

# Relationship between distance after contact and effort
tracking_bc_play_stats |> 
  ggplot(aes(x = dis_gained_x_ac, y = num_of_effort_move)) +
  geom_point()

# Relationship between EPA and effort
# Seems like some effort indicates an increase in points
tracking_bc_play_stats |> 
  ggplot(aes(x = expectedPointsAdded, y = num_of_effort_move)) +
  geom_point()
tracking_bc_play_stats |> 
  ggplot(aes(x = expectedPointsAdded, y = eff_move_prop)) +
  geom_point()

# Relationship between KE and distance after contact
tracking_bc_play_stats |> 
  ggplot(aes(x = dis_gained_x_ac, y = mean_ke)) +
  geom_point()

# KE and EPA
tracking_bc_play_stats |> 
  ggplot(aes(x = mean_ke, y = expectedPointsAdded)) +
  geom_point()

# Distance after contact and EPA
tracking_bc_play_stats |> 
  ggplot(aes(x = expectedPointsAdded, y = dis_gained_x_ac)) +
  geom_point()

# rushing yards and EPA
tracking_bc_play_stats |> 
  ggplot(aes(x = rushingYards, y = expectedPointsAdded)) +
  geom_point()
