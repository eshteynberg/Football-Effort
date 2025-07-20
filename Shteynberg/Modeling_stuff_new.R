library(tidyverse)
library(broom)
library(glmnet)
library(mgcv)
library(caret)
library(rpart)
library(rpart.plot)
library(ranger)
library(vip)



# Play-level data ---------------------------------------------------------
tracking_modeling <- tracking_def |>
  left_join(select(tracking_bc_filtered, event, gameId, playId, frameId))

#play-level data 
tracking_def_plays <- tracking_modeling |> 
  filter(event == "handoff") |> 
  left_join(select(plays, gameId, playId, expectedPointsAdded)) |> 
  left_join(select(rb_stats_per_play, gameId, playId, bc_id, rushingYards)) |> 
  mutate(home = bc_club==homeTeamAbbr) |> 
  filter(bc_id %in% rb_stats_total_filtered$bc_id) 

# Modeling EPA (new) ----------------------------------------------------------
set.seed(1)
N_FOLDS <- 5

# Making sure plays are in the same fold
plays_folds <- tracking_def_plays |> 
  distinct(gameId) |> 
  mutate(fold = sample(rep(1:N_FOLDS, length.out = n())))

rb_modeling <- tracking_def_plays |> 
  na.omit() |>
  select(score_diff, home, quarter, down, yardsToGo, yards_from_endzone,
         bc_s_mph, bc_dir_a_mpsh, preSnapVisitorScore, angle_with_bc,
         preSnapHomeScore, weight, dist_to_bc, 
         expectedPointsAdded, rushingYards, gameId) |> 
  mutate(fold = sample(rep(1:N_FOLDS, length.out = n()))) |> 
  left_join(plays_folds) |> 
  select(-gameId)




# Function to estimate EPA and rushingYards
epa_rushingYards_cv <- function(x) {
  test_data <- rb_modeling |> 
    filter(fold == x)
  train_data <- rb_modeling |> 
    filter(fold != x)
  
  # For lasso and ridge
  test_x <- as.matrix(select(test_data, -c(expectedPointsAdded, rushingYards, fold)))
  train_x <- as.matrix(select(train_data, -c(expectedPointsAdded, rushingYards, fold)))
  
  # Models for epa
  epa_reg_fit <- lm(expectedPointsAdded ~ . - fold - rushingYards, data = train_data)
  epa_ridge_fit <- cv.glmnet(train_x, train_data$expectedPointsAdded, alpha = 0)
  epa_lasso_fit <- cv.glmnet(train_x, train_data$expectedPointsAdded, alpha = 1)
  epa_gam_fit <- gam(expectedPointsAdded ~ s(score_diff) + s(bc_s_mph) + s(bc_dir_a_mpsh) +
                 as.factor(down) + as.factor(quarter) + as.factor(home)+ s(yardsToGo) + s(yards_from_endzone) +
                 weight + s(score_diff)+ s(preSnapVisitorScore) +s(preSnapHomeScore) +s(dist_to_bc)
                 +s(angle_with_bc),
                 data = train_data,
                 family = gaussian(),
                 method = "REML")
  epa_rf <- ranger(expectedPointsAdded ~ ., 
                     num.trees = 500, importance = "impurity", 
                     data = train_data)
  
  
  # Models for rushing yards
  rushingYards_reg_fit <- lm(rushingYards ~ . - fold - expectedPointsAdded, data = train_data)
  rushingYards_ridge_fit <- cv.glmnet(train_x, train_data$rushingYards, alpha = 0)
  rushingYards_lasso_fit <- cv.glmnet(train_x, train_data$rushingYards, alpha = 1)
  rushingYards_gam_fit <- gam(rushingYards ~ s(score_diff) + s(bc_s_mph) + s(bc_dir_a_mpsh) +
                   as.factor(down) + as.factor(quarter) + as.factor(home)+ s(yardsToGo) + s(yards_from_endzone) +
                   weight + s(score_diff)+ s(preSnapVisitorScore) +s(preSnapHomeScore) +s(dist_to_bc) +s(angle_with_bc),
                 data = train_data,
                 family = gaussian(),
                 method = "REML")
  rushingYards_rf <- ranger(rushingYards ~ .,
                            num.trees = 500, importance = "impurity",
                            data = train_data)
  
  # Predictions
  out <- tibble(
    epa_reg_pred = predict(epa_reg_fit, newdata = test_data),
    epa_ridge_pred = as.numeric(predict(epa_ridge_fit, newx = test_x)),
    epa_lasso_pred = as.numeric(predict(epa_lasso_fit, newx = test_x)),
    epa_gam_pred = predict(epa_gam_fit, newdata = test_data, type = "response"),
    epa_rf_pred = (predict(epa_rf, data = test_data))$predictions,
    epa_actual = test_data$expectedPointsAdded,
    epa_rf_res = epa_actual - epa_rf_pred,
    rushingYards_reg_pred = predict(rushingYards_reg_fit, newdata = test_data),
    rushingYards_ridge_pred = as.numeric(predict(rushingYards_ridge_fit, newx = test_x)),
    rushingYards_lasso_pred = as.numeric(predict(rushingYards_lasso_fit, newx = test_x)),
    rushingYards_gam_pred = predict(rushingYards_gam_fit, newdata = test_data, type = "response"),
    rushingYards_rf_pred = (predict(rushingYards_rf, data = test_data))$predictions,
    rushingYards_actual = test_data$rushingYards,
    rushingYards_rf_res = rushingYards_actual - rushingYards_rf_pred,
    test_fold = x
  )
  return(out)
}

# bind predictions for folds together
epa_rushingYards_test_preds <- map(1:N_FOLDS, epa_rushingYards_cv) |> 
  bind_rows()

# Comparing RMSE and SE_RSE for EPA
epa_rushingYards_test_preds |> 
  pivot_longer(c(epa_reg_pred:epa_rf_pred, 
               rushingYards_reg_pred:rushingYards_rf_pred),
               names_to = "method",
               values_to = "test_pred") |> 
  group_by(method, test_fold) |> 
  summarize(rmse = ifelse(method %in% c("epa_reg_pred", "epa_ridge_pred", "epa_lasso_pred",
                                 "epa_gam_pred", "epa_rf_pred"), 
                   sqrt(mean((epa_actual - test_pred) ^ 2)), 
                   sqrt(mean((rushingYards_actual - test_pred) ^ 2)))) |> 
  group_by(method) |> 
  summarize(cv_rmse = mean(rmse),
            se_rse = sd(rmse) / sqrt(N_FOLDS))

# Comparing RMSE and SE_RSE for rushing Yards
rushingYards_test_preds |> 
  pivot_longer(reg_pred:rf_pred,
               names_to = "method",
               values_to = "test_pred") |> 
  group_by(method, test_fold) |> 
  summarize(rmse = sqrt(mean((rushingYards_actual - test_pred) ^ 2))) |> 
  group_by(method) |> 
  summarize(cv_rmse = mean(rmse),
            se_rse = sd(rmse) / sqrt(N_FOLDS))


epa_test_preds |> 
  ggplot(aes(x=epa_rf_pred, y=epa_actual))+
  geom_point()


rushingYards_test_preds |> 
  ggplot(aes(x=rushingYards_rf_pred, y=rushingYards_actual))+
  geom_point()


