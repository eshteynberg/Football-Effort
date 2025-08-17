# Goal: Find expected speed and acceleration for running backs
library(tidyverse)
library(broom)
library(glmnet)
library(mgcv)
library(caret)
library(rpart)
library(rpart.plot)
library(ranger)

# Modeling acceleration ----------------------------------------------------------
set.seed(1)
N_FOLDS <- 5

# Making sure plays are in the same fold
game_folds <- tracking_def |> 
  distinct(gameId) |> 
  mutate(fold = sample(rep(1:N_FOLDS, length.out = n())))

# Data to be modeled
rb_model_before <- tracking_def |> 
  select(adj_bc_x, adj_bc_y, dist_to_bc,
         down, quarter, yardsToGo,
         yards_from_endzone, weight,
         score_diff, bc_s_mph, bc_dir_a_mpsh, def_s_mph, def_dir_a_mpsh,
         angle_with_bc, playId, bc_id, gameId, frameId) |> 
  left_join(game_folds) |> 
  na.omit()

rb_model_acceleration <- rb_model_before |> 
  select(-c(playId, bc_id, gameId, frameId))

# Function to estimate dir acceleration
acceleration_cv <- function(x) {
  test_data <- rb_model_acceleration |> 
    filter(fold == x)
  train_data <- rb_model_acceleration |> 
    filter(fold != x)
  
  
  # Models
  reg_fit <- lm(bc_dir_a_mpsh ~ ., data = train_data)
  # gam_fit <- gam(bc_s ~ s(adj_bc_x) + s(adj_bc_y) + s(dist_to_bc) +
  #                down + quarter + s(yardsToGo) + s(yards_from_endzone) +
  #                weight + s(score_diff),
  #                data = train_data,
  #                family = gaussian(),
  #                method = "REML")
  acceleration_rf <- ranger(bc_dir_a_mpsh ~ ., 
                     num.trees = 500, importance = "impurity", 
                     data = train_data)
  
  
  # Predictions
  out <- tibble(
    reg_pred = predict(reg_fit, newdata = test_data),
    # ridge_pred = as.numeric(predict(ridge_fit, newx = test_x)),
    # lasso_pred = as.numeric(predict(lasso_fit, newx = test_x)),
    # gam_pred = predict(gam_fit, newdata = test_data, type = "response"),
    rf_pred = (predict(acceleration_rf, data = test_data))$predictions,
    acceleration_actual = test_data$bc_dir_a_mpsh,
    test_fold = x
  )
  return(out)
}

# Binding predictions for folds together
acceleration_test_preds <- map(1:N_FOLDS, acceleration_cv) |> 
  bind_rows()

# Comparing RMSE of models
acceleration_results <- acceleration_test_preds |> 
  pivot_longer(reg_pred:rf_pred,
               names_to = "method",
               values_to = "test_pred") |> 
  group_by(method, test_fold) |> 
  summarize(rmse = sqrt(mean((acceleration_actual - test_pred) ^ 2))) |> 
  group_by(method) |> 
  summarize(cv_rmse = mean(rmse),
            se_rse = sd(rmse) / sqrt(N_FOLDS))

## Looking at predictions for acceleration
# Random Forest
acceleration_test_preds |> 
  ggplot(aes(x = rf_pred, y = acceleration_actual)) +
  geom_point(alpha = .2) +
  geom_abline(intercept = 0, slope = 1, col = "blue")

# Linear regression
acceleration_test_preds |> 
  ggplot(aes(x = rf_pred, y = acceleration_actual)) +
  geom_point(alpha = .2) +
  geom_abline(intercept = 0, slope = 1, col = "blue")

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
                                        abs((expected_speed - lag(expected_speed)) / .1), NA),
         speed_residual = s - expected_speed)

# Plotting residuals
tracking_bc_expected |> 
  ggplot(aes(x = speed_residual)) +
  geom_histogram()

# Finding the players with the most points of speed above expected
speed_above_expected <- tracking_bc_expected |> 
  group_by(displayName) |> 
  summarize(points_above_expected = sum(speed_residual > 0),
            percentage_above_expected = sum(speed_residual > 0) / n()) |> 
  ungroup()
