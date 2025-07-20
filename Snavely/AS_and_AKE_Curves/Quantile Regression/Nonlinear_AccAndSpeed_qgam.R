library(qgam)
library(tidyverse)
library(quantreg)
library(gt)
library(gtExtras)

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

rbs_names <- unique(rb_stats_total_filtered$displayName)


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
    filter(fold == x)
  train_data <- player_runs_modeling |> 
    filter(fold != x)
  
  # Modeling
  qgam_fit <- qgam(a_mpsh ~ s(s_mph, k = 10, bs = "ad"),
                   data = train_data,
                   qu = .99)
  qgam_fit_s <- qgam(s_mph ~ s(a_mpsh, k = 10, bs = "ad"),
                   data = train_data,
                   qu = .99)

  out <- tibble(
    displayName = "Saquon Barkley",
    qgam_pred = predict(qgam_fit_s, newdata = test_data),
    actual_acc = test_data$a_mpsh,
    actual_speed = test_data$s_mph,
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
    qgam_fit <- qgam(a_mpsh ~ s(s_mph, k = 15, bs = "ad"),
                     data = train_data,
                     qu = .99,
                     multicore = TRUE,
                     ncores = 7)
    
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
      stat_smooth(method="gam", formula=y~s(x),se=FALSE, lwd = 1.5, aes(color="99th percentile line"), size=1.2) + 
      stat_smooth(method="gam", formula=y~s(x),se=FALSE, lty = 2, lwd = 1.5, aes(y=qgam_pred_minus_3, color="99th percentile line - 3"), size=1.2) + 
      scale_color_manual("Line", values = c("#0072B2", "#D55E00")) +
      labs(x = "Speed",
           y = "Acceleration",
           title = paste0(name),
           caption = "Data from Weeks 1-9 of the 2022 NFL Season") +
      theme_minimal(base_size=16) +
      theme(plot.title = element_text(face = "bold.italic",
                                      size = 18, 
                                      hjust = .5),
            legend.title = element_text(face = "bold"),
            axis.title = element_text(face = "bold"),
            legend.text=element_text(size=15),
            plot.caption = element_text(face = "italic", size = 8))
    return(player_graph)
  }
  return(player_runs_test_preds)
}

# Test
eff_function_qgam("Saquon Barkley", graph = TRUE)
eff_function_qgam("Rex Burkhead", graph = TRUE)
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
rbs_names <- unique(rb_stats_total_filtered$displayName)

# Note: This will take A LONG TIME to run!!!!!!
# percentile_dists <- purrr::map(rbs_names, eff_function_qgam) |> 
#   bind_rows()

percentile_dists_k15 <- read_csv("created_data/percentile_dists.csv")

# Finding the effort component of each frame
points_below_k15 <- percentile_dists_k15 |>
  filter(actual_acc < qgam_pred) |> 
  mutate(frame_dis_score = 1 / (1 + res),
         between_lines = ifelse(actual_acc > qgam_pred_minus_3,
                                TRUE, FALSE))

# Finding the effort score of each player
dis_scores_players_k15 <- points_below_k15 |> 
  group_by(displayName) |> 
  summarize(dis_score_below = sum(frame_dis_score) / n(),
            prop_between = mean(between_lines)) |> 
  ungroup()

# Looking at points above the line
points_above <- percentile_dists_k15 |> 
  filter(actual_acc >= qgam_pred) |> 
  group_by(displayName) |> 
  summarize(dis_score_above = mean(res)) |> 
  ungroup()
  

# Looking at all points
points_all <- percentile_dists_k15 |> 
  group_by(displayName) |> 
  summarize(prop_of_points_above = sum(actual_acc >= qgam_pred) / n()) |> 
  ungroup()

# Joining
dis_scores <- dis_scores_players_k15 |> 
  left_join(points_above) |> 
  left_join(points_all)

# Looking at relationship between eff metrics
dis_scores |> 
  ggplot(aes(x = dis_score_below, y = prop_between)) +
  geom_point()

dis_scores |> 
  ggplot(aes(x = dis_score_above, y = prop_between)) +
  geom_point()

# Checking to see if the proportion of points above the line correlate with eff metrics
dis_scores |> 
  ggplot(aes(x = dis_score_below, y = prop_of_points_above)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color="#0072B2")+
  labs(title = "No relationship suggests \nqgam problem is negligible",
       x = "Distance score of points \nbelow 99th percentile",
       y = "Proportion of points \nabove 99th percentile") +
  theme_minimal(base_size = 16)+
  theme(plot.title = element_text(face = "bold",
                                  hjust = .5),
        axis.title = element_text(face = "bold", size=14),
        legend.position = "none")
        
dis_scores |> 
  ggplot(aes(x = dis_score_above, y = prop_of_points_above)) +
  geom_point()


## TOP PLAYERS
top_dis_scores <- dis_scores |> 
  slice_max(dis_score_below, n = 10)

bottom_dis_scores <- dis_scores |> 
  slice_min(dis_score_below, n = 10) |> 
  arrange(desc(dis_score_below))

mix_dis_scores <- rbind(top_dis_scores, bottom_dis_scores) |> 
  select(displayName, dis_score_below) |> 
  mutate(dis_score_below = round(dis_score_below, 3))

top_prop_scores <- dis_scores |> 
  slice_max(prop_between, n = 10)

bottom_prop_scores <- dis_scores |> 
  slice_min(prop_between, n = 10) |> 
  arrange(desc(prop_between))

mix_prop_scores <- rbind(top_prop_scores, bottom_prop_scores) |> 
  select(displayName, prop_between) |> 
  mutate(prop_between = round(prop_between, 3) * 100)

mix_dis_scores |>
  gt() |>
  tab_header(title = md("**Top and bottom players for distance score**")) |>
  cols_label(displayName = "Player Name", dis_score_below = "Distance score of points below 99th percentile") |>
  data_color(columns = c(dis_score_below),
             fn = scales::col_numeric(palette = c("#0072B2","white", "#D55E00"), domain = NULL)) |>
  gtExtras::gt_theme_espn() # |>
# gtsave(file = "dis_scores_below.png")

mix_prop_scores |>
  gt() |>
  tab_header(title = md("**Top and bottom players for proportion score**")) |>
  cols_label(displayName = "Player Name", prop_between = "Proportion between lines (%)") |>
  data_color(columns = c(prop_between),
             fn = scales::col_numeric(palette = c("#0072B2","white", "#D55E00"), domain = NULL)) |>
  gtExtras::gt_theme_espn() # |>
# gtsave(file = "prop_scores.png")


# Adjusted qgam score -----------------------------------------------------

# This is the same function as the one up top, only difference is adding a vline to graph
eff_function_adjusted <- function(name, graph = FALSE) {
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
    qgam_fit <- qgam(a_mpsh ~ s(s_mph, k = 15, bs = "ad"),
                     data = train_data,
                     qu = .99,
                     multicore = TRUE,
                     ncores = 7)
    
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
      stat_smooth(method="gam", formula=y~s(x),se=FALSE, lwd = 1.5, aes(color="99th percentile line"), size=1.2) + 
      stat_smooth(method="gam", formula=y~s(x),se=FALSE, lty = 2, lwd = 1.5, aes(y=qgam_pred_minus_3, color="99th percentile line - 3"), size=1.2) +
      geom_vline(aes(xintercept = quantile(actual_speed, probs = c(.85))), linetype="dashed", color = "grey50", size = 1.2) +
      scale_color_manual("Line", values = c("#0072B2", "#D55E00")) +
      labs(x = "Speed",
           y = "Acceleration",
           title = paste0(name),
           caption = "Data from Weeks 1-9 of the 2022 NFL Season") +
      theme_minimal(base_size=16) +
      theme(plot.title = element_text(face = "bold.italic",
                                      size = 18, 
                                      hjust = .5),
            legend.title = element_text(face = "bold"),
            axis.title = element_text(face = "bold"),
            legend.text=element_text(size=15),
            plot.caption = element_text(face = "italic", size = 8))
    return(player_graph)
  }
  return(player_runs_test_preds)
}

# test for graph
eff_function_adjusted("Saquon Barkley", graph = TRUE)
eff_function_adjusted("Rex Burkhead", graph = TRUE)

# Loading in data
percentile_dists_k15 <- read_csv("created_data/percentile_dists.csv")

# Adjusting for higher velocity
# Finding the effort component of each frame
adj_points_below_k15 <- percentile_dists_k15 |>
  filter(actual_acc < qgam_pred) |> 
  mutate(between_lines = ifelse(actual_acc > qgam_pred_minus_3,
                                TRUE, FALSE),
         adj_res = ifelse(actual_speed >= quantile(actual_speed, probs = c(.85)),
                                      res/2, res),
         adj_frame_dis_score = 1 / (1 + adj_res))

# New adjusted dis scores for speed
adj_dis_scores_players_k15 <- adj_points_below_k15 |> 
  group_by(displayName) |> 
  summarize(adj_dis_score_below = sum(adj_frame_dis_score) / n(),
            prop_between = mean(between_lines)) |> 
  ungroup()



# Incorporating 99th percentile speed -------------------------------------

eff_function_qgam_mix <- function(name, graph = FALSE) {
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
    qgam_fit_a <- qgam(a_mpsh ~ s(s_mph, k = 10, bs = "ad"),
                     data = train_data,
                     qu = .95,
                     multicore = TRUE,
                     ncores = 7)
    qgam_fit_s <- qgam(s_mph ~ s(a_mpsh, k = 10, bs = "ad"),
                     data = train_data,
                     qu = .99,
                     multicore = TRUE,
                     ncores = 7)
    
    out <- tibble(
      displayName = name,
      qgam_pred_a = predict(qgam_fit_a, newdata = test_data),
      qgam_pred_s = predict(qgam_fit_s, newdata = test_data),
      actual_acc = test_data$a_mpsh,
      actual_speed = test_data$s_mph,
      res_a = abs(actual_acc - qgam_pred_a),
      res_s = abs(actual_speed - qgam_pred_s),
      qgam_pred_minus_3 = qgam_pred_a - 3,
      test_fold = x
    )
    return(out)
  }
  
  # Doing cross fold validation
  player_runs_test_preds <- map(1:N_FOLDS, player_runs_cv) |> 
    bind_rows()
  
  
  if (graph == TRUE) {
    player_graph <- player_runs_test_preds |> 
      ggplot(aes(x = actual_speed, y = qgam_pred_a)) +
      geom_point(aes(y = actual_acc), alpha=.3, color="grey2")+
      stat_smooth(method="gam", formula=y~s(x),se=FALSE, lwd = 1.5, aes(color="99th acc percentile line"), size=1.2) + 
      stat_smooth(method="gam", formula=y~s(x),se=FALSE, lty = 2, lwd = 1.5, aes(y=qgam_pred_minus_3, 
                                                                                 color="99th percentile line - 3"), size=1.2) +
      stat_smooth(method="gam", formula=y~s(x),se=FALSE, lwd = 1.5, aes(x = qgam_pred_s, y = actual_acc,
                                                                        color="99th speed percentile line"), size=1.2) +
      scale_color_manual("Line", values = c("#0072B2", "#D55E00", "purple")) +
      labs(x = "Speed",
           y = "Acceleration",
           title = paste0(name),
           caption = "Data from Weeks 1-9 of the 2022 NFL Season") +
      theme_minimal(base_size=16) +
      theme(plot.title = element_text(face = "bold.italic",
                                      size = 18, 
                                      hjust = .5),
            legend.title = element_text(face = "bold"),
            axis.title = element_text(face = "bold"),
            legend.text=element_text(size=15),
            plot.caption = element_text(face = "italic", size = 8))
    return(player_graph)
  }
  return(player_runs_test_preds)
}

eff_function_qgam_mix("Saquon Barkley", graph = TRUE)
