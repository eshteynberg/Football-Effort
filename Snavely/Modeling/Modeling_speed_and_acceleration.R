# Goal: Find expected speed and acceleration for running backs
library(tidyverse)
library(broom)
library(glmnet)
library(mgcv)
library(caret)
library(rpart)
library(rpart.plot)
library(ranger)

# Data cleaning -----------------------------------------------------------
plays <- plays |> 
  mutate(yards_from_endzone = ifelse((possessionTeam != yardlineSide) | (yardlineNumber == 50), 
                                     yardlineNumber, 100 - yardlineNumber),
         adj_x_first_down = yards_from_endzone - yardsToGo)

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

# This data frame shows tracking only for running backs
tracking_bc_quang <- tracking_rb_runs |> 
  filter(nflId == bc_id) |> 
  select(gameId, playId, frameId, 
         bc_id, bc_club,
         bc_x = x, bc_y = y, bc_s = s, bc_a = a,
         bc_dis = dis, bc_o = o, bc_dir = dir) |> 
  mutate(adj_bc_x = 110 - bc_x, # Adjusting for the endzone
         adj_bc_y = bc_y - (160 / 6)) |> 
  left_join(select(plays, gameId, playId, adj_x_first_down)) |> 
  mutate(adj_bc_x_from_first_down = adj_bc_x - adj_x_first_down,
         bc_position = "RB",
         bc_type = "rusher")

# Finding the nearest defender
tracking_def <- tracking_rb_runs |> 
  filter(club != bc_club, displayName != "football") |> 
  left_join(select(tracking_bc_quang, gameId, playId, frameId,
                   bc_x, bc_y, adj_bc_x, adj_bc_y, bc_s, bc_a),
            by = c("gameId", "playId", "frameId")) |> 
  mutate(dist_to_bc = sqrt((x - bc_x) ^ 2 + (y - bc_y) ^ 2)) |> 
  group_by(gameId, playId, frameId) |>
  arrange(dist_to_bc) |> 
  mutate(player_dist_bc_rank = row_number()) |> 
  ungroup() |> 
  filter(player_dist_bc_rank == 1) |> 
  select(gameId, playId, frameId, playDirection,
         nflId, displayName,
         dist_to_bc, def_x = x, def_y = y, def_s = s, def_a = a,
         bc_x, bc_y, adj_bc_x, adj_bc_y) |> 
  mutate(adj_x = 110 - def_x,
         adj_y = def_y - (160 / 6),
         adj_x_change = adj_bc_x - adj_x, adj_y_change = adj_bc_y - adj_y,
         angle_with_bc = atan2(adj_y_change, -adj_x_change)) |> 
  select(-bc_x, -bc_y, -adj_bc_x, -adj_bc_y) |> 
  left_join(select(tracking_bc_quang, gameId, playId, frameId, bc_id, bc_club,
                   bc_x, bc_y, adj_bc_x, adj_bc_y, bc_s, bc_a),
            by = c("gameId", "playId", "frameId")) |> 
  arrange(gameId, playId, frameId)


# Joining cols from plays with the tracking_def data frame
plays_filtered <- plays |> 
  select(gameId, playId, preSnapVisitorScore, preSnapHomeScore, quarter, down, yardsToGo, yards_from_endzone)

tracking_def <- tracking_def |> 
  left_join(plays_filtered, by=c("gameId", "playId")) |> 
  left_join(select(games, gameId, homeTeamAbbr, visitorTeamAbbr)) |> 
  left_join(select(players, bc_id = nflId, weight)) |> 
  mutate(score_diff = ifelse(bc_club == visitorTeamAbbr, preSnapVisitorScore - preSnapHomeScore, 
                             preSnapHomeScore - preSnapVisitorScore),
         bc_s_mph = bc_s * (3600 / 1760),
         def_s_mph = def_s * (3600 / 1760),
         bc_a_mpsh = bc_a * (3600 / 1760),
         def_a_mpsh = def_a * (3600 / 1760),
         down = as.factor(down),
         quarter = as.factor(quarter))


# Modeling speed ----------------------------------------------------------
set.seed(1)
N_FOLDS <- 5

# Making sure plays are in the same fold
plays_folds <- tracking_def |> 
  distinct(playId) |> 
  mutate(fold = sample(rep(1:N_FOLDS, length.out = n())))
  
# Data to be modeled
rb_model_before <- tracking_def |> 
  select(adj_bc_x, adj_bc_y, dist_to_bc,
         down, quarter, yardsToGo,
         yards_from_endzone, weight,
         score_diff, bc_s_mph, bc_a_mpsh, def_s_mph, def_a_mpsh,
         angle_with_bc, playId, bc_id, gameId, frameId) |> 
  left_join(plays_folds)

rb_model_speed <- rb_model_before |> 
  select(-c(playId, bc_id, gameId, frameId, bc_a_mpsh, def_a_mpsh))
  

# Function to estimate speed
speed_cv <- function(x) {
  test_data <- rb_model_speed |> 
    filter(fold == x)
  train_data <- rb_model_speed |> 
    filter(fold != x)
  
  # For lasso and ridge
  test_x <- as.matrix(select(test_data, -bc_s_mph))
  train_x <- as.matrix(select(train_data, -bc_s_mph))
  
  # Models
  reg_fit <- lm(bc_s_mph ~ ., data = train_data)
  ridge_fit <- cv.glmnet(train_x, train_data$bc_s_mph, alpha = 0)
  lasso_fit <- cv.glmnet(train_x, train_data$bc_s_mph, alpha = 1)
  # gam_fit <- gam(bc_s ~ s(adj_bc_x) + s(adj_bc_y) + s(dist_to_bc) +
  #                down + quarter + s(yardsToGo) + s(yards_from_endzone) +
  #                weight + s(score_diff),
  #                data = train_data,
  #                family = gaussian(),
  #                method = "REML")
  speed_rf <- ranger(bc_s_mph ~ ., 
                     num.trees = 500, importance = "impurity", 
                     data = train_data)
  
  
  # Predictions
  out <- tibble(
    reg_pred = predict(reg_fit, newdata = test_data),
    ridge_pred = as.numeric(predict(ridge_fit, newx = test_x)),
    lasso_pred = as.numeric(predict(lasso_fit, newx = test_x)),
    # gam_pred = predict(gam_fit, newdata = test_data, type = "response"),
    rf_pred = (predict(speed_rf, data = test_data))$predictions,
    speed_actual = test_data$bc_s_mph,
    test_fold = x
  )
  return(out)
}

# Binding predictions for folds together
speed_test_preds <- map(1:N_FOLDS, speed_cv) |> 
  bind_rows()

# Comparing RMSE of models
speed_results <- speed_test_preds |> 
  pivot_longer(reg_pred:rf_pred,
               names_to = "method",
               values_to = "test_pred") |> 
  group_by(method, test_fold) |> 
  summarize(rmse = sqrt(mean((speed_actual - test_pred) ^ 2))) |> 
  group_by(method) |> 
  summarize(cv_rmse = mean(rmse),
            se_rse = sd(rmse) / sqrt(N_FOLDS))

## Looking at predictions for speed
# Random Forest
speed_test_preds |> 
  ggplot(aes(x = rf_pred, y = speed_actual)) +
  geom_point(alpha = .2) +
  geom_abline(intercept = 0, slope = 1, col = "blue")

# GAM
# speed_results |> 
#   ggplot(aes(x = gam_pred, y = speed_actual)) +
#   geom_point(alpha = .2) +
#   geom_abline(intercept = 0, slope = 1, col = "blue")

# Linear Regression
speed_test_preds |> 
  ggplot(aes(x = reg_pred, y = speed_actual)) +
  geom_point(alpha = .2) +
  geom_abline(intercept = 0, slope = 1, col = "blue")

# Lasso Regression
speed_test_preds |> 
  ggplot(aes(x = lasso_pred, y = speed_actual)) +
  geom_point(alpha = .2) +
  geom_abline(intercept = 0, slope = 1, col = "blue")


# Modeling acceleration ----------------------------------------------------------
set.seed(1)
N_FOLDS <- 5

rb_model_acceleration <- rb_model_before |> 
  select(-c(playId, bc_id, gameId, frameId, bc_s_mph, def_s_mph))


# Function to estimate speed
acceleration_cv <- function(x) {
  test_data <- rb_model_acceleration |> 
    filter(fold == x)
  train_data <- rb_model_acceleration |> 
    filter(fold != x)
  
  # For lasso and ridge
  test_x <- as.matrix(select(test_data, -bc_s_mph))
  train_x <- as.matrix(select(train_data, -bc_s_mph))
  
  # Models
  reg_fit <- lm(bc_a_mpsh ~ ., data = train_data)
  ridge_fit <- cv.glmnet(train_x, train_data$bc_a_mpsh, alpha = 0)
  lasso_fit <- cv.glmnet(train_x, train_data$bc_a_mpsh, alpha = 1)
  # gam_fit <- gam(bc_s ~ s(adj_bc_x) + s(adj_bc_y) + s(dist_to_bc) +
  #                down + quarter + s(yardsToGo) + s(yards_from_endzone) +
  #                weight + s(score_diff),
  #                data = train_data,
  #                family = gaussian(),
  #                method = "REML")
  speed_rf <- ranger(bc_a_mpsh ~ ., 
                     num.trees = 500, importance = "impurity", 
                     data = train_data)
  
  
  # Predictions
  out <- tibble(
    reg_pred = predict(reg_fit, newdata = test_data),
    ridge_pred = as.numeric(predict(ridge_fit, newx = test_x)),
    lasso_pred = as.numeric(predict(lasso_fit, newx = test_x)),
    # gam_pred = predict(gam_fit, newdata = test_data, type = "response"),
    rf_pred = (predict(speed_rf, data = test_data))$predictions,
    acceleration_actual = test_data$bbc_a_mpsh,
    test_fold = x
  )
  return(out)
}

# Binding predictions for folds together
acceleration_test_preds <- map(1:N_FOLDS, speed_cv) |> 
  bind_rows()

# Adding expected velocity back into the df -------------------------------
# Random Forest is best model, so take predictions from that
expected_speed <- speed_test_preds |> 
  select(rf_pred)

# Joining back to data set
ids_modeling_speed <- rb_model_before |> 
  mutate(expected_speed = expected_speed$rf_pred) |> 
  select(bc_id, gameId, playId, frameId, expected_speed)

tracking_bc_expected <- tracking_bc |> 
  left_join(ids_modeling_speed, by = c("bc_id", "gameId", "playId", "frameId")) |> 
  mutate(expected_acceleration = ifelse(gameId==lag(gameId) & playId ==lag(playId), 
                                        abs((expected_speed - lag(expected_speed)) / .1), NA))
