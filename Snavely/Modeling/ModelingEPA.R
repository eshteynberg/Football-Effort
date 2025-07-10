# Goal: predict EPA
library(tidyverse)
library(broom)
library(glmnet)
library(mgcv)
library(caret)
library(rpart)
library(rpart.plot)
library(ranger)


# Continuous EPA ----------------------------------------------------------
set.seed(1)
N_FOLDS <- 5
rb_modeling <- tracking_bc_play_stats |> 
  na.omit() |> 
  select(mean_ke, mean_jerk, eff_move_prop, 
         total_dist_covered_of_game, avg_COD, acc_change, 
         expectedPointsAdded) |> 
  mutate(fold = sample(rep(1:N_FOLDS, length.out = n())))

EPA_cv <- function(x) {
  test_data <- rb_modeling |> 
    filter(fold == x)
  train_data <- rb_modeling |> 
    filter(fold != x)
  
  # For lasso and ridge
  test_x <- as.matrix(select(test_data, -expectedPointsAdded))
  train_x <- as.matrix(select(train_data, -expectedPointsAdded))
  
  # Models
  reg_fit <- lm(expectedPointsAdded ~ mean_ke + mean_jerk + eff_move_prop +
                total_dist_covered_of_game + avg_COD + acc_change, 
                data = train_data)
  ridge_fit <- cv.glmnet(train_x, train_data$expectedPointsAdded, alpha = 0)
  lasso_fit <- cv.glmnet(train_x, train_data$expectedPointsAdded, alpha = 1)
  gam_fit <- gam(expectedPointsAdded ~ s(mean_ke) + s(mean_jerk) + s(eff_move_prop) +
                 s(total_dist_covered_of_game) + s(avg_COD) + s(acc_change),
                 data = train_data,
                 family = gaussian(),
                 method = "REML")
  
  out <- tibble(
    reg_pred = predict(reg_fit, newdata = test_data),
    ridge_pred = as.numeric(predict(ridge_fit, newx = test_x)),
    lasso_pred = as.numeric(predict(lasso_fit, newx = test_x)),
    gam_pred = predict(gam_fit, newdata = test_data, type = "response"),
    epa_actual = test_data$expectedPointsAdded,
    test_fold = x
  )
  return(out)
}

EPA_test_preds <- map(1:N_FOLDS, EPA_cv) |> 
  bind_rows()

# Comparing RMSE and SE_RSE
EPA_test_preds |> 
  pivot_longer(reg_pred:gam_pred,
               names_to = "method",
               values_to = "test_pred") |> 
  group_by(method, test_fold) |> 
  summarize(rmse = sqrt(mean((epa_actual - test_pred) ^ 2))) |> 
  group_by(method) |> 
  summarize(cv_rmse = mean(rmse),
            se_rse = sd(rmse) / sqrt(N_FOLDS))

# GAM predictions
EPA_test_preds |> 
  ggplot(aes(x = gam_pred, y = epa_actual)) +
  geom_point(alpha = .4) +
  geom_abline(intercept = 0, slope = 1)

# Simple linear predictions
EPA_test_preds |> 
  ggplot(aes(x = reg_pred, y = epa_actual)) +
  geom_point(alpha = .4) +
  geom_abline(intercept = 0, slope = 1)

# Lasso predictions
EPA_test_preds |> 
  ggplot(aes(x = lasso_pred, y = epa_actual)) +
  geom_point(alpha = .4) +
  geom_abline(intercept = 0, slope = 1)

# Ridge predictions
EPA_test_preds |> 
  ggplot(aes(x = ridge_pred, y = epa_actual)) +
  geom_point(alpha = .4) +
  geom_abline(intercept = 0, slope = 1)


# Binary EPA --------------------------------------------------------------

# Converting EPA into a binary variable (either positive or negative)
N_FOLDS <- 5
rb_modeling_binary <- tracking_bc_play_stats |> 
  na.omit() |> 
  mutate(pos_EPA = ifelse(expectedPointsAdded > 0, TRUE, FALSE)) |> 
  select(mean_ke, mean_jerk, eff_move_prop, 
         total_dist_covered_of_game, avg_COD, acc_change, 
         pos_EPA) |> 
  mutate(fold = sample(rep(1:N_FOLDS, length.out = n())))

EPA_binary_cv <- function(x) {
  test_data <- rb_modeling_binary |> 
    filter(fold == x)
  train_data <- rb_modeling_binary |> 
    filter(fold != x)
  
  # Models
  logit_fit <- glm(pos_EPA ~ mean_ke + mean_jerk + eff_move_prop +
                   avg_COD + acc_change,
                   family = binomial,
                   data = train_data)
  gam_fit <- gam(pos_EPA ~ s(mean_ke) + s(mean_jerk) + s(eff_move_prop) +
                 s(avg_COD) + s(acc_change),
                 data = train_data,
                 family = binomial(),
                 method = "REML")
  pos_epa_rf <- ranger(pos_EPA ~ mean_ke + mean_jerk + eff_move_prop +
                       total_dist_covered_of_game + avg_COD + acc_change, 
                       num.trees = 500, importance = "impurity", data = test_data)
  
  # Predictions
  out <- tibble(
    logit_pred = predict(logit_fit, newdata = test_data, type = "response"),
    gam_pred = predict(gam_fit, newdata = test_data, type = "response"),
    rf_pred = pos_epa_rf$predictions,
    test_actual = test_data$pos_EPA,
    test_fold = x
  )
  return(out)
}

pos_EPA_preds <- map(1:N_FOLDS, EPA_binary_cv) |> 
  bind_rows()

# Determining accuracy
pos_EPA_preds |> 
  pivot_longer(logit_pred:rf_pred,
               names_to = "method",
               values_to = "test_pred") |> 
  mutate(test_pred_class = round(test_pred)) |> 
  group_by(method, test_fold) |> 
  summarize(accuracy = mean(test_actual == test_pred_class)) |> 
  group_by(method) |> 
  summarize(cv_accuracy = mean(accuracy),
            se_accuracy = sd(accuracy) / sqrt(N_FOLDS))