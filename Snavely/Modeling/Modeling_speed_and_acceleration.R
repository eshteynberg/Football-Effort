# Goal: Find expected speed and acceleration for running backs
library(tidyverse)
library(broom)
library(glmnet)
library(mgcv)
library(caret)
library(rpart)
library(rpart.plot)
library(ranger)

# Modeling speed ----------------------------------------------------------
set.seed(1)
N_FOLDS <- 5
rb_model_as <- tracking_def |> 
  select(adj_bc_x, adj_bc_y, dist_to_bc,
         down, quarter, yardsToGo,
         yards_from_endzone, weight,
         score_diff, bc_s) |> 
  mutate(down = as.factor(down),
         quarter = as.factor(quarter)) |> 
  mutate(fold = sample(rep(1:N_FOLDS, length.out = n())))


speed_cv <- function(x) {
  test_data <- rb_model_as |> 
    filter(fold == x)
  train_data <- rb_model_as |> 
    filter(fold != x)
  
  # For lasso and ridge
  test_x <- as.matrix(select(test_data, -bc_s))
  train_x <- as.matrix(select(train_data, -bc_s))
  
  # Models
  reg_fit <- lm(bc_s ~ ., data = train_data)
  ridge_fit <- cv.glmnet(train_x, train_data$bc_s, alpha = 0)
  lasso_fit <- cv.glmnet(train_x, train_data$bc_s, alpha = 1)
  gam_fit <- gam(bc_s ~ s(adj_bc_x) + s(adj_bc_y) + s(dist_to_bc) +
                 down + quarter + s(yardsToGo) + s(yards_from_endzone) +
                 weight + s(score_diff),
                 data = train_data,
                 family = gaussian(),
                 method = "REML")
  
  out <- tibble(
    reg_pred = predict(reg_fit, newdata = test_data),
    ridge_pred = as.numeric(predict(ridge_fit, newx = test_x)),
    lasso_pred = as.numeric(predict(lasso_fit, newx = test_x)),
    gam_pred = predict(gam_fit, newdata = test_data, type = "response"),
    speed_actual = test_data$bc_s,
    test_fold = x
  )
  return(out)
}

# Random Forrest
speed_rf <- ranger(bc_s ~ ., 
                   num.trees = 500, importance = "impurity", 
                   data = rb_model_as)

speed_test_preds <- map(1:N_FOLDS, speed_cv) |> 
  bind_rows()

speed_test_preds <- speed_test_preds |> 
  mutate(rf_pred = speed_rf$predictions) |> 
  select(reg_pred:gam_pred, rf_pred, speed_actual, test_fold)

speed_test_preds |> 
  pivot_longer(reg_pred:rf_pred,
               names_to = "method",
               values_to = "test_pred") |> 
  group_by(method, test_fold) |> 
  summarize(rmse = sqrt(mean((speed_actual - test_pred) ^ 2))) |> 
  group_by(method) |> 
  summarize(cv_rmse = mean(rmse),
            se_rse = sd(rmse) / sqrt(N_FOLDS))

## Looking at predictions for speed
# GAM
speed_test_preds |> 
  ggplot(aes(x = gam_pred, y = speed_actual)) +
  geom_point(alpha = .2) +
  geom_abline(intercept = 0, slope = 1, col = "blue")

# Random Forrest
speed_test_preds |> 
  ggplot(aes(x = rf_pred, y = speed_actual)) +
  geom_point(alpha = .2) +
  geom_abline(intercept = 0, slope = 1, col = "blue")

# Linear Regression
speed_test_preds |> 
  ggplot(aes(x = reg_pred, y = speed_actual)) +
  geom_point(alpha = .2) +
  geom_abline(intercept = 0, slope = 1, col = "blue")
