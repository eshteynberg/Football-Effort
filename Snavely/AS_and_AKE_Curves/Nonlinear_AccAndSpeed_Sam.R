library(qgam)
library(tidyverse)
library(quantreg)

# Loading in Required Data ------------------------------------------------
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
         jerk = ifelse(gameId==lag(gameId) & playId ==lag(playId), (a-lag(a))/.1, NA),
         s_mph = s * (3600 / 1760),
         a_mpsh = a * (3600 / 1760))

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

rbs <- rb_stats_total_filtered |> 
  group_by(displayName) |> 
  distinct(gameId) |> 
  summarize(games=n()) |>
  ungroup() |> 
  filter(games >= 5) |> 
  select(displayName)

rbs_names <- unique(rbs$displayName)


# Player test -------------------------------------------------------------
set.seed(1)
# Choosing player name
player_runs <- tracking_bc |> 
  filter(displayName == "Saquon Barkley")

# 5 folds
N_FOLDS <- 5

# # Making sure plays are in the same fold
plays_folds <- player_runs |>
  distinct(gameId) |> 
  mutate(fold = sample(rep(1:N_FOLDS, length.out = n())))

# Making the modeling data frame
player_runs_modeling <- player_runs |> 
  select(s_mph, a_mpsh, gameId) |> 
  left_join(plays_folds) |>
  select(-gameId)

player_runs_cv <- function(x){
  test_data <- player_runs_modeling |> 
    filter(fold == 1)
  train_data <- player_runs_modeling |> 
    filter(fold != 1)
  
  # Modeling
  qgam_fit <- qgam(a_mpsh ~ s(s_mph, k = 35, bs = "ad"),
                   data = train_data,
                   qu = .99)

  out <- tibble(
    displayName = "Saquon Barkley",
    qgam_pred = predict(qgam_fit, newdata = test_data),
    actual_acc = test_data$a_mpsh,
    res = abs(actual_acc - qgam_pred),
    test_fold = x
  )
  return(out)
}

# Saquon
player_runs_test_preds <- map(1:N_FOLDS, player_runs_cv) |> 
  bind_rows()


# Function (qgam) ----------------------------------------------------------------
eff_function_qgam <- function(name, graph = FALSE) {
  # Choosing player name
  player_runs <- tracking_bc |> 
    filter(displayName == name)
  
  # 5 folds
  N_FOLDS <- 5
  
  # # Making sure plays are in the same fold
  plays_folds <- player_runs |>
    distinct(gameId) |> 
    mutate(fold = sample(rep(1:N_FOLDS, length.out = n())))
  
  # Making the modeling data frame
  player_runs_modeling <- player_runs |> 
    select(s_mph, a_mpsh, gameId) |> 
    left_join(plays_folds) |>
    select(-gameId)
  
  player_runs_cv <- function(x){
    test_data <- player_runs_modeling |> 
      filter(fold == x)
    train_data <- player_runs_modeling |> 
      filter(fold != x)
    
    # Modeling
    n_rows <- nrow(train_data)
    k_val <- max(20, min(100, round(n_rows * 0.08))) # Optimizes k (k should roughly be 8% of the nrows)
    qgam_fit <- qgam(a_mpsh ~ s(s_mph, k = k_val, bs = "ad"),
                     data = train_data,
                     qu = .99)
    
    out <- tibble(
      displayName = name,
      qgam_pred = predict(qgam_fit, newdata = test_data),
      actual_acc = test_data$a_mpsh,
      actual_speed = test_data$s_mph,
      res = abs(actual_acc - qgam_pred),
      qgam_pred_minus_3 = qgam_pred - 3,
      test_fold = x
    )
    return(out)
  }
  
  # Doing cross fold validation
  player_runs_test_preds <- map(1:N_FOLDS, player_runs_cv) |> 
    bind_rows()
  
  
  if (graph == TRUE) {
    player_graph <- player_runs_test_preds |> 
      ggplot(aes(x = actual_speed, y = qgam_pred)) +
      geom_point(aes(y = actual_acc), alpha=.3, color="grey2")+
      stat_smooth(method="gam", formula=y~s(x),
                  method.args=list(se=FALSE), aes(color="99th percentile line"), size=1.2) + 
      stat_smooth(method="gam", formula=y~s(x),
                  method.args=list(se=FALSE), aes(y=qgam_pred_minus_3, color="Relaxed 99th percentile line"), size=1.2) + 
      labs(x = "Speed",
           y = "Acceleration",
           title = paste0(name, "'s effort is defined as the mean distance of points \npast the 95th quartile line to the expected acceleration line"),
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
  return(player_runs_test_preds)
}

# Test
eff_function_qgam("Saquon Barkley", graph = TRUE)
eff_function_qgam("Craig Reynolds", graph = TRUE)
eff_function_qgam("Boston Scott", graph = TRUE)

# Checking percentage above 99th percentile line
Craig <- eff_function_qgam("Craig Reynolds")
Craig |> 
  mutate(res_adj = ifelse(actual_acc >= qgam_pred, 0, res),
         indiv_dis_score = 1 / (1 + res_adj)) |> 
  group_by(displayName) |> 
  summarize(dis_score = sum(indiv_dis_score / n()),
            prop_of_points_above_99 = sum(res_adj == 0) / n()) |> 
  ungroup()

Austin <- eff_function_qgam("Austin Ekeler")
Austin |> 
  mutate(res_adj = ifelse(actual_acc >= qgam_pred, 0, res),
         indiv_dis_score = 1 / (1 + res_adj)) |> 
  group_by(displayName) |> 
  summarize(dis_score = sum(indiv_dis_score / n()),
            prop_of_points_above_99 = sum(res_adj == 0) / n()) |> 
  ungroup()

# Running the function for every player
rbs_names <- unique(rbs$displayName)

# Note: This will take A LONG TIME to run!!!!!!
percentile_dists <- purrr::map(rbs, eff_function_qgam) |> 
  bind_rows()

# Finding the effort component of each frame
percentile_dists_adj <- percentile_dists |> 
  mutate(res_adj = ifelse(actual_acc >= qgam_pred, 0, res),
         indiv_dis_score = 1 / (1 + res_adj))

# Finding the effort score of each player
dis_scores_players <- percentile_dists_adj |> 
  group_by(displayName) |> 
  summarize(dis_score = sum(indiv_dis_score / n()),
            prop_of_points_above_99 = sum(res_adj == 0) / n()) |> 
  ungroup()

# Function (rqss) ----------------------------------------------------------------
eff_function_rqss_new <- function(name, graph = FALSE) {
  set.seed(1)
  # Choosing player name
  player_runs <- tracking_bc |> 
    filter(displayName == name)
  
  # 5 folds
  N_FOLDS <- 5
  
  # # Making sure plays are in the same fold
  plays_folds <- player_runs |>
    distinct(gameId) |> 
    mutate(fold = sample(rep(1:N_FOLDS, length.out = n())))
  
  # Making the modeling data frame
  player_runs_modeling <- player_runs |> 
    select(s_mph, a_mpsh, gameId) |> 
    left_join(plays_folds) |>
    select(-gameId)
  
  player_runs_cv <- function(x){
    test_data <- player_runs_modeling |> 
      filter(fold == x)
    train_data <- player_runs_modeling |> 
      filter(fold != x)
    
    s_range <- range(train_data$s_mph)
    test_data <- test_data |>
      filter(s_mph >= max(s_range[1]), 
             s_mph <= min(s_range[2])) 
    #nonparametric quantile reg using smooth spline
    #fit a smooth spline of acc as a function of speed
    #lambda controls smoothness (higher =smoother/less wiggly, lower=more flexible)
    rqss_fit_99 <- rqss(a_mpsh~qss(s_mph, lambda=20), tau=.99, data = train_data)
    
    out <- tibble(
      displayName = name,
      rqss_99_pred = predict(rqss_fit_99, newdata = test_data),
      actual_acc = test_data$a_mpsh,
      actual_speed = test_data$s_mph,
      res = abs(actual_acc - rqss_99_pred),
      rqss_99_pred_minus_3 = rqss_99_pred - 3,
      test_fold = x
    )
    return(out)
  }
  
  # Doing cross fold validation
  player_runs_test_preds <- map(1:N_FOLDS, player_runs_cv) |> 
    bind_rows()
  
  if (graph == TRUE) {
    player_graph <- player_runs_test_preds |> 
      ggplot(aes(x = actual_speed, y = actual_acc)) +
      geom_point(aes(y = actual_acc), alpha=.3, color="grey2")+
      stat_smooth(method="rqss", formula=y~qss(x, lambda = 20),
                  method.args=list(tau = .99), se=FALSE, aes(color="99th percentile line"), size=1.2) + 
      stat_smooth(method="rqss", formula=(y - 3)~qss(x, lambda = 20),
                  method.args=list(tau=.99), se=FALSE, aes(color="Relaxed 99th percentile line"), size=1.2) + 
      labs(x = "Speed (mph)",
           y = "Acceleration (mph/s)",
           title = paste0(name, "'s effort is defined as the mean distance of points \npast the 95th quartile line to the expected acceleration line"),
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
  return(player_runs_test_preds)
}

# Test
eff_function_rqss_new("Saquon Barkley", graph = TRUE)
eff_function_rqss_new("Chase Edmonds", graph = TRUE)

# Running the function for every player
rbs_names <- unique(rbs$displayName)

percentile_dists_rqss <- purrr::map(rbs_names, eff_function_rqss_new) |> 
  bind_rows()

dists_rqss_adj <- percentile_dists_rqss |> 
  mutate(res_adj = ifelse(actual_acc >= rqss_99_pred, 0, res),
         indiv_dis_score = 1 / (1 + res_adj))

dis_scores_players_rqss <- dists_rqss_adj |> 
  group_by(displayName) |> 
  summarize(dis_score = sum(indiv_dis_score / n()),
            prop_of_points_above_99 = sum(res_adj == 0) / n()) |> 
  ungroup()

