#use luke's AccAndSpeed.R script dfs
# Goal: predict rushing yards
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
  select(mean_ke, mean_jerk, eff_move_prop, 
         total_dist_covered_of_game, avg_COD, acc_change, 
         dis_gained) |> 
  drop_na() |> 
  mutate(fold = sample(rep(1:N_FOLDS, length.out = n())))

dis_gained_cv <- function(x) {
  test_data <- rb_modeling |> 
    filter(fold == x)
  train_data <- rb_modeling |> 
    filter(fold != x)

  # For lasso and ridge
  test_x <- as.matrix(select(test_data, -dis_gained))
  train_x <- as.matrix(select(train_data, -dis_gained))
  
  # Models
  reg_fit <- lm(dis_gained ~ mean_ke + mean_jerk + eff_move_prop +
                  total_dist_covered_of_game + avg_COD + acc_change, 
                data = train_data)
  ridge_fit <- cv.glmnet(train_x, train_data$dis_gained, alpha = 0)
  lasso_fit <- cv.glmnet(train_x, train_data$dis_gained, alpha = 1)
  gam_fit <- gam(dis_gained ~ s(mean_ke) + s(mean_jerk) + s(eff_move_prop) +
                   s(total_dist_covered_of_game) + s(avg_COD) + s(acc_change),
                 data = train_data,
                 family = gaussian(),
                 method = "REML")
  
  out <- tibble(
    reg_pred = predict(reg_fit, newdata = test_data),
    ridge_pred = as.numeric(predict(ridge_fit, newx = test_x)),
    lasso_pred = as.numeric(predict(lasso_fit, newx = test_x)),
    gam_pred = predict(gam_fit, newdata = test_data, type = "response"),
    dis_gained_actual = test_data$dis_gained,
    test_fold = x
  )
  return(out)
}

dis_gained_test_preds <- map(1:N_FOLDS, dis_gained_cv) |> 
  bind_rows()

# Comparing RMSE and SE_RSE
dis_gained_test_preds |> 
  pivot_longer(reg_pred:gam_pred,
               names_to = "method",
               values_to = "test_pred") |> 
  group_by(method, test_fold) |> 
  summarize(rmse = sqrt(mean((dis_gained_actual - test_pred) ^ 2))) |> 
  group_by(method) |> 
  summarize(cv_rmse = mean(rmse),
            se_rse = sd(rmse) / sqrt(N_FOLDS))

# GAM predictions
dis_gained_test_preds |> 
  ggplot(aes(x = gam_pred, y = dis_gained_actual)) +
  geom_point(alpha = .4) +
  geom_abline(intercept = 0, slope = 1)

# Simple linear predictions
dis_gained_test_preds |> 
  ggplot(aes(x = reg_pred, y = dis_gained_actual)) +
  geom_point(alpha = .4) +
  geom_abline(intercept = 0, slope = 1)

# Lasso predictions
dis_gained_test_preds |> 
  ggplot(aes(x = lasso_pred, y = dis_gained_actual)) +
  geom_point(alpha = .4) +
  geom_abline(intercept = 0, slope = 1)

# Ridge predictions
dis_gained_test_preds |> 
  ggplot(aes(x = ridge_pred, y = dis_gained_actual)) +
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
  
  # Predictions
  out <- tibble(
    logit_pred = predict(logit_fit, newdata = hr_test, type = "response"),
    gam_pred = predict(gam_fit, newdata = hr_test, type = "response"),
    test_actual = test_data$pos_EPA,
    test_fold = x
  )
  return(out)
}



#MODELING EXPECTED SPEED AND ACCEL STUFF

# Calculating yards from the endzone and yards til a first down
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
  arrange(gameId, playId, frameId)


# Joining cols from plays with the tracking_def data frame
plays_filtered <- plays |> 
  select(gameId, playId, preSnapVisitorScore, preSnapHomeScore, quarter, down, yardsToGo, yards_from_endzone)

tracking_def <- tracking_def |> 
  left_join(plays_filtered, by=c("gameId", "playId")) |> 
  left_join(select(games, gameId, homeTeamAbbr, visitorTeamAbbr)) |> 
  left_join(select(players, bc_id = nflId, weight)) |> 
  mutate(score_diff = ifelse(bc_club == visitorTeamAbbr, preSnapVisitorScore - preSnapHomeScore, 
                             preSnapHomeScore - preSnapVisitorScore)) 

tracking_def <- tracking_def |> 
  mutate(down = as.factor(down))

tracking_def <- tracking_def |> 
  mutate(quarter = as.factor(quarter))

class(tracking_def$down)


#Fitting Random forest model
set.seed(1)
library(ranger)
speed_rf <- ranger(bc_s ~ adj_bc_x + 
                     adj_bc_y + 
                     dist_to_bc +
                     down +
                     quarter +
                     yardsToGo +
                     yards_from_endzone +
                     score_diff +
                     weight,
                   num.trees =500,
                   importance="impurity",
                   data=tracking_def)

speed_rf

library(vip)
vip(speed_rf)

tracking_def |> 
  mutate(pred=speed_rf$predictions) |> 
  summarise(rmse=sqrt(mean((bc_s-pred)^2)))

tracking_def |> 
  mutate(pred=speed_rf$predictions) |> 
  ggplot(aes(bc_s, pred))+
  geom_point(alpha=0.5)+
  geom_abline(slope=1, intercept=0, linetype="dashed", linewidth= 2, color="red")






# IMPROVING EXPECTED SPEED MODEL -------------------------------------------

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
         dist_to_bc, def_x = x, def_y = y, def_s = s,
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
rb_model_as <- tracking_def |> 
  select(adj_bc_x, adj_bc_y, dist_to_bc,
         down, quarter, yardsToGo,
         yards_from_endzone, weight,
         score_diff, bc_s_mph, playId) |> 
  left_join(plays_folds) |> 
  select(-playId)


speed_cv <- function(x) {
  test_data <- rb_model_as |> 
    filter(fold == x)
  train_data <- rb_model_as |> 
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

speed_test_preds <- map(1:N_FOLDS, speed_cv) |> 
  bind_rows()

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
# Random Forrest
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



#get acceleration from speed
#change in speed over 0.1
tracking_def <- tracking_def |>
  mutate(row = row_number()) |> 
  left_join(
    speed_test_preds |> 
      select(rf_pred, speed_actual) |> 
      mutate(row=row_number()),
    by="row"
  ) |> 
  arrange(gameId, playId, frameId) |> 
  group_by(gameId, playId) |> 
  mutate(pred_accel = (lead(rf_pred)-rf_pred)/0.1) |> 
  ungroup()



#another option: model accel
rb_model_accel <- tracking_def |> 
  select(adj_bc_x, adj_bc_y, dist_to_bc,
         down, quarter, yardsToGo,
         yards_from_endzone, weight,
         score_diff, bc_a, playId) |> 
  left_join(plays_folds) |>   
  select(-playId)


accel_cv <- function(x){
  test_data <- rb_model_accel |> filter(fold == x)
  train_data <- rb_model_accel |> filter(fold != x)
  
  accel_rf <- ranger(bc_a~.,
                     num.trees=500,
                     importance="impurity",
                     data=train_data)
  
  out <- tibble(
    rf_pred_accel =predict(accel_rf, data=test_data)$predictions,
    accel_actual=test_data$bc_a,
    test_fold=x
  )
  return(out)
}

accel_test_preds <- map(1:N_FOLDS, accel_cv) |> 
  bind_rows()

accel_results <- accel_test_preds |> 
  group_by(test_fold) |> 
  summarize(rmse = sqrt(mean((accel_actual - rf_pred_accel)^2))) |> 
  summarize(cv_rmse = mean(rmse),
            se_rse = sd(rmse) / sqrt(N_FOLDS))


tracking_def <- tracking_def |> 
  mutate(row=row_number()) |> 
  left_join(accel_test_preds |> 
              mutate(row=row_number()),
            by="row")


#maybe do 3-frame rolling avg?
#to make it smoother
#avg of current frame's accel pred, frame before and frame after
library(zoo)
tracking_def <- tracking_def |> 
  arrange(gameId, playId, frameId) |> 
  group_by(gameId, playId) |> 
  mutate(expected_accel_new = rollmean(rf_pred_accel, k=3, fill=NA, align="center"))



# Sam Suggestion ----------------------------------------------------------

#FIX TO BE FROM NONLINEAR EXPECTED ACC AND SPEED
#NEED FRAMES FROM SAME PLAY TO BE IN SAME FOLD
#NEED mph 
#this one is old

eff_function_rqss_new <- function(name, graph=FALSE){
  player_runs <- tracking_bc |> 
    filter(displayName == name)
  
  N_FOLDS <- 5
  player_runs_modeling <- player_runs |> 
    select(s, a) |> 
    mutate(fold = sample(rep(1:N_FOLDS, length.out = n())))
  
  player_runs_cv <- function(x) {
    test_data <- player_runs_modeling |> filter(fold == x)
    train_data <- player_runs_modeling |> filter(fold != x)
    
    s_range <- range(train_data$s)
    test_data <- test_data |>
      filter(s >= max(s_range[1], 1),
             s <= min(s_range[2], 9))
    
    rq_fit_99 <- rqss(a ~ qss(s, lambda = 8), tau = 0.99, data = train_data)
    pred_99 <- predict(rq_fit_99, newdata = test_data)
    
    out <- tibble(
      displayName = name,
      s = test_data$s,
      a = test_data$a,
      pred_99 = pred_99,
      res_99 = test_data$a - pred_99
    )
    
    return(out)
  }
  player_runs_test_preds <- map(1:N_FOLDS, player_runs_cv) |> 
    list_rbind()
  
  #cap points above curve
  player_runs_test_preds <- player_runs_test_preds |> 
    mutate(adj_res = ifelse(res_99 > 0, 0, abs(res_99)),
           dist_score = 1 / (1 + adj_res))
  
  #mean 
  final_score <- mean(player_runs_test_preds$dist_score)

  #percent above relaxed line metric
  relaxed_line <- player_runs_test_preds$pred_99 - 0.25
  perc_above_relaxed_99 <- mean(player_runs_test_preds$a > relaxed_line) 
  
  
  if (graph == TRUE) {
    player_graph <- player_runs |> 
      ggplot(aes(x = s, y = a)) +
      geom_point(alpha = 0.3, color = "grey2") +
      stat_smooth(method = "rqss", formula = y ~ qss(x, lambda = 8),
                  method.args = list(tau = 0.99), se = FALSE,
                  aes(color = "99th Percentile Line"), size = 1.2) +
      geom_line(data = player_runs_test_preds |> 
                  mutate(relaxed_99 = pred_99 - 0.25),
                aes(x = s, y = relaxed_99, color = "Relaxed 99th Percentile Line"),
                linetype = "dashed", size = 1.2) +
      scale_color_manual("Line", values = c("99th Percentile Line" = "darkblue", "Relaxed 99th Percentile Line" = "orange")) +
      labs(x = "Speed",
           y = "Acceleration",
           title = paste0(name, "'s 99th percentile effort relaxed"),
           caption = "Relaxed line = 99th percentile - 0.25") +
      theme(plot.title = element_text(face = "bold", size = 20, hjust = .5),
            legend.title = element_text(face = "bold", size = 15),
            axis.title = element_text(face = "bold", size = 15),
            axis.text = element_text(size = 13),
            plot.caption = element_text(face = "italic", size = 8))
    return(player_graph)
    
  }
  return(tibble(
    displayName = name,
    eff_99_metric = eff_99_metric,
    perc_above_relaxed_99 = perc_above_relaxed_99
  ))
}


eff_function_rqss_new("Saquon Barkley", graph=TRUE)

#for all players
rbs <- unique(rb_stats_total_filtered$displayName)

eff_scores <- purrr::map(rbs, eff_function_rqss_new) |> 
  bind_rows()

View(eff_scores)


