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

tracking_def_plays <- tracking_modeling |>
  left_join(tracking_num_defs) |> 
  drop_na(num_of_def_5) |> 
  left_join(select(tracking_blockers, gameId, playId, num_blockers_ahead)) |> 
  left_join(select(plays, gameId, playId, expectedPointsAdded, offenseFormation, pff_runConceptPrimary)) |> 
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
         bc_s_mph, bc_dir_a_mpsh, angle_with_bc, num_blockers_ahead,
         weight, dist_to_bc, num_of_def_5, adj_bc_x, adj_bc_y,
         def_s_mph, def_dir_a_mpsh, offenseFormation, pff_runConceptPrimary,
         expectedPointsAdded, gameId, playId, displayName, frameId) |> 
  left_join(plays_folds) |> 
  mutate(quarter = as.factor(quarter),
         down = as.factor(down),
         offenseFormation = as.factor(offenseFormation),
         pff_runConceptPrimary = as.factor(pff_runConceptPrimary))

# Function to estimate EPA and rushingYards
epa_cv <- function(x) {
  test_data <- rb_modeling |> 
    filter(fold == x)
  train_data <- rb_modeling |> 
    filter(fold != x)
  
  # Models for epa
  epa_rf <- ranger(expectedPointsAdded ~ . - fold - gameId - playId - displayName - frameId, 
                   num.trees = 1000, importance = "permutation", 
                   data = train_data)
  gam_fit <- gam(expectedPointsAdded ~ s(score_diff) + home + quarter + down + s(yardsToGo)+ s(yards_from_endzone)+
                 s(bc_s_mph)+ s(bc_dir_a_mpsh)+ s(angle_with_bc)+ num_blockers_ahead+
                 s(weight)+ s(dist_to_bc)+ num_of_def_5+ s(adj_bc_x)+ s(adj_bc_y)+
                 s(def_s_mph)+ s(def_dir_a_mpsh)+ offenseFormation+ pff_runConceptPrimary,
                 data = train_data)
  
  
  # Predictions
  out <- tibble(
    gameId = test_data$gameId,
    playId = test_data$playId,
    frameId = test_data$frameId,
    displayName = test_data$displayName,
    epa_rf_pred = (predict(epa_rf, data = test_data))$predictions,
    gam_pred = predict(gam_fit, newdata = test_data),
    epa_actual = test_data$expectedPointsAdded,
    epa_rf_res = epa_actual - epa_rf_pred,
    test_fold = x
  )
  return(out)
}

# bind predictions for folds together
epa_test_preds <- map(1:N_FOLDS, epa_cv) |> 
  bind_rows()

# Comparing RMSE and SE_RSE for EPA
RMSE <- epa_test_preds |> 
  group_by(test_fold) |> 
  summarize(rmse = sqrt(mean((epa_actual - epa_rf_pred) ^ 2))) |> 
  ungroup() 

# Final RMSE
mean(RMSE$rmse)

epa_test_preds |> 
  ggplot(aes(x=gam_pred, y=epa_actual))+
  geom_point(alpha = .7, col = "grey2") +
  geom_abline(intercept = 0, slope = 1, lwd = 1.3, col = "#D50A0A", lty = 2) +
  xlim(-10, 6)

# Random forest graph
epa_test_preds |> 
  ggplot(aes(x=epa_rf_pred, y=epa_actual))+
  geom_point(alpha = .7, col = "grey2") +
  geom_abline(intercept = 0, slope = 1, lwd = 1.3, col = "#D50A0A", lty = 2) +
  labs(title = "Random forest model poorly predicts extreme values of EPA",
       subtitle = "Points closer to the red line indicate more accurate predictions",
       x = "Random Forest Prediction (Expected EPA)",
       y = "Actual EPA")+
  theme_minimal(base_size=16) +
  theme(plot.title = element_text(face = "bold.italic",
                                  size = 18, 
                                  hjust = .5),
        axis.title = element_text(face = "bold"),
        plot.subtitle = element_text(face = "italic",
                                     hjust = .5))

# Combining effort metrics and preds
res_combined <- epa_test_preds |> 
  left_join(select(qgam_dis_play_epa, gameId, playId, qgam_dis_score)) |> 
  left_join(select(nlrq_dis_play_epa, gameId, playId, nlrq_dis_score))

# Residuals dis_scatter# Residuals graph
epa_plot1 <- res_combined |> 
  ggplot(aes(x=qgam_dis_score, y=epa_rf_res))+
  geom_point(alpha = .7, col = "grey2") +
  labs(title = "",
       x = "Play-level effort metric #1 (Quadratic quantile regression)",
       y = "Random forest EPA residuals")+
  theme_minimal(base_size=16) +
  theme(plot.title = element_text(face = "bold.italic",
                                  size = 18, 
                                  hjust = .5),
        axis.title = element_text(face = "bold"),
        plot.subtitle = element_text(face = "italic",
                                     hjust = .5))

epa_plot2 <- res_combined |> 
  ggplot(aes(x=nlrq_dis_score, y=epa_rf_res))+
  geom_point(alpha = .7, col = "grey2") +
  labs(title = "",
       x = "Play-level effort metric #2 (QGAM)",
       y = "")+
  theme_minimal(base_size=16) +
  theme(plot.title = element_text(face = "bold.italic",
                                  size = 18, 
                                  hjust = .5),
        axis.title = element_text(face = "bold"),
        plot.subtitle = element_text(face = "italic",
                                     hjust = .5))

library(patchwork)
(epa_plot1 | epa_plot2) + 
  plot_annotation(title = "No strong correlation between residuals of EPA model and each effort metric",
                  theme = theme(plot.title = element_text(size = 18, 
                                                          face = "bold",
                                                          hjust = .5),
                                plot.subtitle = element_text(size = 16,
                                                             face = "italic",
                                                             hjust = .5)))


# Trying XGBoost ----------------------------------------------------------

# XG BOOST attempt
epa <- x_train <- rb_modeling |> 
  filter(fold != 1)

x_train <- rb_modeling |> 
  filter(fold != 1) |> 
  select(-c(fold, expectedPointsAdded)) |>
  mutate(quarter = as.factor(quarter),
         down = as.factor(down),
         offenseFormation = as.factor(offenseFormation),
         pff_runConceptPrimary = as.factor(pff_runConceptPrimary))

x_test <- rb_modeling |> 
  filter(fold == 1) |> 
  select(-c(fold, expectedPointsAdded)) |>
  mutate(quarter = as.factor(quarter),
         down = as.factor(down),
         offenseFormation = as.factor(offenseFormation),
         pff_runConceptPrimary = as.factor(pff_runConceptPrimary))

# Hot one encoding
x_train_matrix <- model.matrix(~ . - 1, data = x_train)
x_test_matrix <- model.matrix(~ . - 1, data = x_test)

xg_grid <- crossing(nrounds = seq(20, 150, 10), # number of rounds
                    eta = c(0.01, 0.05, 0.1), gamma = 0, # eta is the learning rate
                    max_depth = c(2, 3, 4), colsample_bytree = 1,
                    min_child_weight = 1, subsample = 1)

# tuning
set.seed(1234)
xg_tune <- train(x = x_train_matrix,
                 y = epa$expectedPointsAdded, 
                 tuneGrid = xg_grid,
                 trControl = trainControl(method = "cv", number = 5),
                 objective = "reg:squarederror",
                 method = "xgbTree")

# Final model fit
# fit final model  data
xg_fit <- xgboost(
  data = x_train_matrix,
  label = epa$expectedPointsAdded,
  objective = "reg:squarederror",
  nrounds = xg_tune$bestTune$nrounds,
  params = as.list(select(xg_tune$bestTune, -nrounds)),
  verbose = 0
)

library(vip)
xg_fit |> 
  vip()

preds <- predict(xg_fit, newdata = x_test_matrix)

epa_test <- x_train <- rb_modeling |> 
  filter(fold == 1) |> 
  mutate(epa_pred = preds) |> 
  mutate(res = expectedPointsAdded - epa_pred)

epa_test |> 
  ggplot(aes(x = epa_pred, y = expectedPointsAdded)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)

epa_test |> 
  ggplot(aes(x = epa_pred, y = res)) +
  geom_point()

epa_test |> 
  summarize(rmse = sqrt(mean((expectedPointsAdded - epa_pred) ^ 2)))
  
  
