library(tidyverse)
library(broom)
library(glmnet)
library(mgcv)
library(caret)
library(rpart)
library(rpart.plot)
library(ranger)
library(vip)


# Modeling EPA (new) ----------------------------------------------------------

#play-level data 
tracking_def_plays <- tracking_def |> 
  group_by(gameId, playId, bc_id) |> 
  mutate(
         avg_bc_s_mph = mean(bc_s_mph),
         avg_bc_dir_a_mpsh = mean(bc_dir_a_mpsh),
         avg_dist_to_bc=mean(dist_to_bc)) |> 
  ungroup() |> 
  distinct(gameId, playId, .keep_all=TRUE) |> 
  left_join(select(plays, gameId, playId, expectedPointsAdded)) |> 
  mutate(home = bc_club==homeTeamAbbr) |> 
  filter(bc_id %in% rb_stats_total_filtered$bc_id) 

set.seed(1)
N_FOLDS <- 5

# Making sure plays are in the same fold
plays_folds <- tracking_def_plays |> 
  distinct(gameId) |> 
  mutate(fold = sample(rep(1:N_FOLDS, length.out = n())))

rb_modeling <- tracking_def_plays |> 
  na.omit() |>
  select(score_diff, home, quarter, down, yardsToGo, yards_from_endzone,
         avg_bc_s_mph, avg_bc_dir_a_mpsh, preSnapVisitorScore,
         preSnapHomeScore, weight, avg_dist_to_bc, expectedPointsAdded, gameId) |> 
  mutate(fold = sample(rep(1:N_FOLDS, length.out = n()))) |> 
  left_join(plays_folds) |> 
  select(-gameId)




# Function to estimate EPA
epa_cv <- function(x) {
  test_data <- rb_modeling |> 
    filter(fold == x)
  train_data <- rb_modeling |> 
    filter(fold != x)
  
  # For lasso and ridge
  test_x <- as.matrix(select(test_data, -expectedPointsAdded))
  train_x <- as.matrix(select(train_data, -expectedPointsAdded))
  
  # Models
  reg_fit <- lm(expectedPointsAdded ~ ., data = train_data)
  ridge_fit <- cv.glmnet(train_x, train_data$expectedPointsAdded, alpha = 0)
  lasso_fit <- cv.glmnet(train_x, train_data$expectedPointsAdded, alpha = 1)
  gam_fit <- gam(expectedPointsAdded ~ s(score_diff) + s(avg_bc_s_mph) + s(avg_bc_dir_a_mpsh) +
                 as.factor(down) + as.factor(quarter) + as.factor(home)+ s(yardsToGo) + s(yards_from_endzone) +
                 weight + s(score_diff)+ s(preSnapVisitorScore) +s(preSnapHomeScore) +s(avg_dist_to_bc),
                 data = train_data,
                 family = gaussian(),
                 method = "REML")
  epa_rf <- ranger(expectedPointsAdded ~ ., 
                     num.trees = 500, importance = "impurity", 
                     data = train_data)
  
  
  # Predictions
  out <- tibble(
    reg_pred = predict(reg_fit, newdata = test_data),
    ridge_pred = as.numeric(predict(ridge_fit, newx = test_x)),
    lasso_pred = as.numeric(predict(lasso_fit, newx = test_x)),
    gam_pred = predict(gam_fit, newdata = test_data, type = "response"),
    rf_pred = (predict(epa_rf, data = test_data))$predictions,
    epa_actual = test_data$expectedPointsAdded,
    test_fold = x
  )
  return(out)
}

# bind predictions for folds together
epa_test_preds <- map(1:N_FOLDS, epa_cv) |> 
  bind_rows()

# Comparing RMSE and SE_RSE
epa_test_preds |> 
  pivot_longer(reg_pred:rf_pred,
               names_to = "method",
               values_to = "test_pred") |> 
  group_by(method, test_fold) |> 
  summarize(rmse = sqrt(mean((epa_actual - test_pred) ^ 2))) |> 
  group_by(method) |> 
  summarize(cv_rmse = mean(rmse),
            se_rse = sd(rmse) / sqrt(N_FOLDS))

epa_test_preds |> 
  ggplot(aes(x=rf_pred, y=epa_actual))+
  geom_point()









# Modeling rushing yards --------------------------------------------------
tracking_def_player_play <- tracking_def |> 
  group_by(gameId, playId, bc_id) |> 
  mutate(
    avg_bc_s_mph = mean(bc_s_mph),
    avg_bc_dir_a_mpsh = mean(bc_dir_a_mpsh),
    avg_dist_to_bc=mean(dist_to_bc)) |> 
  ungroup() |> 
  left_join(select(rb_stats_per_play, gameId, playId, bc_id, rushingYards)) |> 
  distinct(gameId, playId, .keep_all=TRUE) |> 
  mutate(home = bc_club==homeTeamAbbr) |> 
  filter(bc_id %in% rb_stats_total_filtered$bc_id) 



set.seed(1)
N_FOLDS <- 5

# Making sure plays are in the same fold
plays_folds <- tracking_def_player_play |> 
  distinct(gameId) |> 
  mutate(fold = sample(rep(1:N_FOLDS, length.out = n())))

rb_modeling <- tracking_def_player_play |> 
  na.omit() |>
  select(score_diff, home, quarter, down, yardsToGo, yards_from_endzone,
         avg_bc_s_mph, avg_bc_dir_a_mpsh, preSnapVisitorScore,
         preSnapHomeScore, weight, avg_dist_to_bc, rushingYards, gameId) |> 
  mutate(fold = sample(rep(1:N_FOLDS, length.out = n()))) |> 
  left_join(plays_folds) |> 
  select(-gameId)



# Function to estimate EPA
rushingYards_cv <- function(x) {
  test_data <- rb_modeling |> 
    filter(fold == x)
  train_data <- rb_modeling |> 
    filter(fold != x)
  
  # For lasso and ridge
  test_x <- as.matrix(select(test_data, -rushingYards))
  train_x <- as.matrix(select(train_data, -rushingYards))
  
  # Models
  reg_fit <- lm(rushingYards ~ ., data = train_data)
  ridge_fit <- cv.glmnet(train_x, train_data$rushingYards, alpha = 0)
  lasso_fit <- cv.glmnet(train_x, train_data$rushingYards, alpha = 1)
  gam_fit <- gam(rushingYards ~ s(score_diff) + s(avg_bc_s_mph) + s(avg_bc_dir_a_mpsh) +
                   as.factor(down) + as.factor(quarter) + as.factor(home)+ s(yardsToGo) + s(yards_from_endzone) +
                   weight + s(score_diff)+ s(preSnapVisitorScore) +s(preSnapHomeScore) +s(avg_dist_to_bc),
                 data = train_data,
                 family = gaussian(),
                 method = "REML")
  rushingYards_rf <- ranger(rushingYards ~ .,
                   num.trees = 500, importance = "impurity",
                   data = train_data)

  
  # Predictions
  out <- tibble(
    reg_pred = predict(reg_fit, newdata = test_data),
    ridge_pred = as.numeric(predict(ridge_fit, newx = test_x)),
    lasso_pred = as.numeric(predict(lasso_fit, newx = test_x)),
    gam_pred = predict(gam_fit, newdata = test_data, type = "response"),
    rf_pred = (predict(rushingYards_rf, data = test_data))$predictions,
    rushingYards_actual = test_data$rushingYards,
    test_fold = x
  )
  return(out)
}

# bind predictions for folds together
rushingYards_test_preds <- map(1:N_FOLDS, rushingYards_cv) |> 
  bind_rows()

# Comparing RMSE and SE_RSE
rushingYards_test_preds |> 
  pivot_longer(reg_pred:rf_pred,
               names_to = "method",
               values_to = "test_pred") |> 
  group_by(method, test_fold) |> 
  summarize(rmse = sqrt(mean((rushingYards_actual - test_pred) ^ 2))) |> 
  group_by(method) |> 
  summarize(cv_rmse = mean(rmse),
            se_rse = sd(rmse) / sqrt(N_FOLDS))


rushingYards_test_preds |> 
  ggplot(aes(x=rf_pred, y=rushingYards_actual))+
  geom_point()

