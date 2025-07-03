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


#  Saquon's Acc and Speed ----------------------------------------------------
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
preds <- predict(saquon_lm, interval = "confidence")
max_acc <- max_acc |> 
  bind_cols(preds)

# Filtering out outliers
max_acc_clean <- max_acc |> 
  filter(acceleration >= lwr, acceleration <= upr)

# New regression line
saquon_lm_clean <- lm(acceleration ~ speed, data = max_acc_clean)
tidy(saquon_lm_clean)
summary(saquon_lm_clean)

# Maximal acceleration (y-intercept)
A_0 <- saquon_lm_clean$coefficients[1]

# Maximum speed
S_0 <- s

# Plotting new regression line
max_acc_clean |> 
  ggplot(aes(x = speed, y = acceleration)) +
  geom_point() +
  geom_smooth(method = lm, se = TRUE, conf.int = TRUE)

# Finally relaying the line onto the orginal data points
saquon_runs |> 
  ggplot(aes(x = s, y = a)) +
  geom_point() +
  geom_smooth(method = lm, aes(x = speed, y = acceleration), data = max_acc_clean) +
  geom_abline(intercept = saquon_lm_clean$coefficients[1], slope = saquon_lm_clean$coefficients[2])
