library(tidyverse)
library(arrow)
# Quang Stuff --------------------------------------------------------------------
games <- read_csv("data/games.csv")
plays <- read_csv("data/plays.csv")
players <- read_csv("data/players.csv")
player_play <- read_csv("data/player_play.csv")
tracking <- arrow::read_parquet("data/tracking.parquet")

## TRACKING FOR RUNNING BACKS
theme_set(theme_bw())

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
         jerk = ifelse(gameId==lag(gameId) & playId ==lag(playId), (a-lag(a))/.1, NA),
         COD = ifelse(gameId==lag(gameId) & playId == lag(playId), abs(dir - lag(dir)), NA))

View(tracking_bc)

# Running back metrics per play and for weeks 1-9------------------------------------------------------
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
            avg_jerk = mean(jerk, na.rm=TRUE),
            avg_COD = mean(COD, na.rm = TRUE) / n()) |> 
  ungroup() |> 
  left_join(select(plays, playId, gameId, yardsGained, expectedPointsAdded))

# Overall rb stats
rb_stats_total <- rb_stats_per_play |> 
  group_by(bc_id, displayName) |> 
  summarize(
    total_dis_gained = sum(dis_gained),
    total_dis_gained_x = sum(dis_gained_x, na.rm = TRUE),
    avg_dis_gained_x = mean(dis_gained_x, na.rm=TRUE) / n(),
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
    avg_jerk = mean(avg_jerk),
    avg_COD = mean(avg_COD)
  ) |> 
  ungroup()


rb_stats_total_filtered <- rb_stats_total |> 
  filter(num_of_rushes >= 20)


#LABELING "starters" vs "backups"-------------------------
#get each RB's primary team based on where they had the most rushes
#basically most-played-for team (bc_club) based on rushes
primary_teams <- plays_rb_runs |> 
  semi_join(rb_stats_total_filtered, by="bc_id") |> #only look at the 69 RBs
  group_by(bc_id, bc_club) |> 
  summarise(rushes=n(), .groups="drop") |> #count how many rushes each player had for each team 
  group_by(bc_id) |> 
  slice_max(order_by=rushes,n=1,with_ties=FALSE) #top team per player 


#append players' primary team names to rb_stats_total_filtered
rb_stats_teams <- rb_stats_total_filtered |> 
  left_join(primary_teams, by="bc_id")

#get top rushers per team
top_rushers_labeled <- rb_stats_teams |> 
  group_by(bc_club) |> 
  slice_max(order_by=rushes, n=1, with_ties=FALSE) |> #pick single top rusher for each team
  mutate(starter=TRUE) #mark the top rushers with starter=TRUE

#join starter labels to rb_stats_teams
rb_stats_labeled <- rb_stats_teams |> 
  left_join(top_rushers_labeled |> #join starter label from top rusher df, matching on player id and team
              select(bc_id, bc_club, starter),
            by=c("bc_id", "bc_club")) |> 
  mutate(starter=ifelse(is.na(starter), FALSE, starter)) #if NA after join, then label FALSE for starter column. if not, then label original value from starter column (TRUE)


# Saquon's Acc and Speed ----------------------------------------------------
library(broom)
# Plot of acceleration and speed for all players
tracking_bc |> 
  ggplot(aes(x = s, y = a)) +
  geom_point(alpha = .3)

saquon_runs <- tracking_bc |> 
  filter(displayName == "Saquon Barkley")

# Saquon's speed and acceleration
saquon_runs |> 
  ggplot(aes(x = s, y = a)) +
  geom_point()

max(saquon_runs$s) # 10.4 is Barkley's max speed

# Making speed bins
bins <- seq(3, 10.4, .2)

# finding the maximum acceleration for each speed bin
max_acc <- map_dfr(1:(length(bins) - 1), function(i) {
  saquon_runs |> 
    filter(s > bins[i], s <= bins[i + 1]) |> 
    slice_max(a, n = 2, with_ties = FALSE) |> 
    select(speed = s, acceleration = a)
})

# Test to see if code worked
saquon_runs |> 
  filter(s > 10.0 & s <= 10.2) |> 
  slice_max(a, n = 2, with_ties = FALSE) |> 
  select(s, a)

# plotting the maximum accelerations per bin
max_acc |> 
  ggplot(aes(x = speed, y = acceleration)) +
  geom_point()

# fitting a regression line 
saquon_lm <- lm(acceleration ~ speed, data = max_acc)

# Looking for outliers
saquon_tidy <- saquon_lm |> 
  tidy(conf.int = TRUE)
summary(saquon_lm)

saquon_tidy

# Plotting speed and acceleartion with first regression line
max_acc |> 
  ggplot(aes(x = speed, y = acceleration)) +
  geom_point() +
  geom_smooth(method = lm, se = TRUE, conf.int = TRUE)

# Adding predicted values to the original df
confs <- predict(saquon_lm, interval = "confidence")
max_acc <- max_acc |> 
  bind_cols(confs)

# Filtering out outliers
max_acc_clean <- max_acc |> 
  filter(acceleration >= lwr, acceleration <= upr)

# New regression line
saquon_lm_clean <- lm(acceleration ~ speed, data = max_acc_clean)
tidy(saquon_lm_clean)
summary(saquon_lm_clean)

# Maximal acceleration (y-intercept)
A_0 <- saquon_lm_clean$coefficients[1]

# Plotting new regression line
max_acc_clean |> 
  ggplot(aes(x = speed, y = acceleration)) +
  geom_point() +
  geom_smooth(method = lm, se = TRUE, conf.int = TRUE)

# Finally relaying the line onto the original data points
saquon_runs |> 
  ggplot(aes(x = s, y = a)) +
  geom_point() +
  geom_smooth(method = lm, aes(x = speed, y = acceleration), data = max_acc_clean) +
  geom_abline(intercept = saquon_lm_clean$coefficients[1], slope = saquon_lm_clean$coefficients[2]) +
  xlim(0, 17)

# Finding out how many points are near the line
test_saquon_a <- data.frame(speed = saquon_runs$s, acceleration = saquon_runs$a)
test_preds <- predict(saquon_lm_clean, newdata = test_saquon_a)

# Adding the test predictions to the df
# Definition of effort: within 1 of the predicted fitted values
saquon_final <- saquon_runs |> 
  select(a, s) |> 
  mutate(pred = test_preds) |> 
  mutate(diff = pred - a) |> 
  mutate(eff = ifelse(diff <= 1, TRUE, FALSE))

eff_metric <- (sum(saquon_final$eff == TRUE) / nrow(saquon_final)) * 100









# Creating a function -----------------------------------------------------
eff_function <- function(name, graph = FALSE) {
  # Filtering the data set to only include the inputted player
  player_runs <- tracking_bc |> 
    filter(displayName == name)
  
  # Making the bins
  bins <- seq(3, round(max(player_runs$s), 2) + .2, .2)
  
  # Picking out the top two accelerations for each speed in each bins
  max_acc <- map_dfr(1:(length(bins) - 1), function(i) {
    player_runs |> 
      filter(s > bins[i], s <= bins[i + 1]) |> 
      slice_max(a, n = 2, with_ties = FALSE) |> 
      select(speed = s, acceleration = a)
  })
  
  # Fitting the first regression line
  player_lm <- lm(acceleration ~ speed, data = max_acc)
  
  # Adding confidence intervals
  confs <- predict(player_lm, interval = "confidence")
  max_acc <- max_acc |> 
    bind_cols(confs)
  
  # Filtering out outliers (any point that does not fall within a 95% conf interval)
  max_acc_clean <- max_acc |> 
    filter(acceleration >= lwr, acceleration <= upr)
  
  # Fitting the new regression line (without outliers)
  player_lm_clean <- lm(acceleration ~ speed, data = max_acc_clean)
  
  # Finding out how many points are near the line
  test_player_a <- data.frame(speed = player_runs$s, acceleration = player_runs$a)
  test_preds <- predict(player_lm_clean, newdata = test_player_a)
  
  # Final calculation for distance away from the fitted line
  player_final <- player_runs |> 
    select(a, s) |> 
    mutate(pred = test_preds) |> 
    mutate(diff = pred - a) |> 
    mutate(eff = ifelse(diff <= .25, TRUE, FALSE))

  # Alternative: effort ratio ------------------------------------------------
  
  player_runs_effort_ratio <- player_runs |> 
    select(a, s) |> 
    mutate(pred = test_preds) |> 
    mutate(effort_ratio = a / pred)
  
  # Final effort metric
  eff <- tibble(
    eff_metric = sum(player_final$eff == TRUE),
    eff_metric_perc = sum(player_final$eff == TRUE) / nrow(player_final) * 100,
    eff_ratio = mean(player_runs_effort_ratio$effort_ratio >= 0.75) * 100)


  # Building the graph if specified
  if (graph == TRUE) {
    player_graph <- player_runs |> 
      ggplot(aes(x = s, y = a)) +
      geom_smooth(method = lm, aes(x = speed, y = acceleration), data = max_acc_clean, lwd = 1.5, se = FALSE) +
      geom_abline(aes(color = "Regression line",
                      intercept = player_lm_clean$coefficients[1], slope = player_lm_clean$coefficients[2]),
                  lwd = 1.5,) +
      geom_abline(aes(color = "Minimum line",
                      intercept = player_lm_clean$coefficients[1] - .25, slope = player_lm_clean$coefficients[2]), 
                  lty = 2, lwd = 1.5) +
      scale_color_manual("Line", values = c("#4B92DB", "#FFB612")) +
      geom_point(size = 2, alpha = .5, col = "grey2") +
      xlim(0, 13) +
      ylim(0, 10) +
      labs(x = "Speed",
           y = "Acceleration",
           title = paste0(name, "'s effort is defined as the percentage of points above the minimum line"),
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
  
  return(eff)
}

# Test
eff_function("Saquon Barkley", graph = TRUE)
eff_function("Jaylen Warren")


# Eff metric for all players ----------------------------------------------

rbs <- unique(rb_stats_total_filtered$displayName)

eff_scores <- purrr::map(rbs, eff_function) |> 
  bind_rows() |> 
  mutate(displayName = rbs)

eff_scores



# Alternative: effort ratio ------------------------------------------------

saquon_runs_effort_ratio <- saquon_runs |> 
  select(a, s) |> 
  mutate(pred = predict(saquon_lm_clean, newdata = data.frame(speed = s))) |> 
  mutate(effort_ratio = a / pred)

mean(saquon_runs_effort_ratio$effort_ratio >= 0.75) * 100


#Alternative: residuals --------------------------------------------------
saquon_runs_residuals <- saquon_runs |> 
  select(a, s) |> 
  mutate(pred=predict(saquon_lm_clean, newdata = test_saquon_a)) |> 
  mutate(resid = abs(pred - a))

threshold <- quantile(saquon_runs_residuals$resid, 0.20)  
saquon_runs_residuals <- saquon_runs_residuals |> 
  mutate(high_effort = resid <= threshold)

mean(saquon_runs_residuals$high_effort) * 100

#----------------------------------------------------------------




# Test
eff_function("Rex Burkhead", graph = TRUE)
eff_function("Saquon Barkley", player_table = TRUE)
eff_function("Saquon Barkley")


# Eff metric for all players ----------------------------------------------

rbs <- unique(rb_stats_total_filtered$displayName)

eff_movements <- purrr::map(rbs, eff_function) |> 
  bind_rows() |> 
  mutate(displayName = rbs)

eff_movements_top <- eff_movements |> 
  slice_max(eff_metric, n = 5) |> 
  mutate(type = "high")

eff_movements_bottom <- eff_movements |> 
  slice_min(eff_metric, n = 5) |> 
  mutate(type = "low")

eff_together <- rbind(eff_movements_top, eff_movements_bottom)

# top 5 vs. bottom 5 eff movements
eff_together |> 
  ggplot(aes(x = eff_metric, y = fct_reorder(displayName, eff_metric), fill = type)) +
  geom_col() +
  labs(x = "Number of effort movements", 
       y = "",
       title = "Top and bottom 5 players for effort movements") +
  geom_text(aes(label = eff_metric), hjust = 1, nudge_x = -.5, fontface = "bold", size = 5) +
  scale_fill_manual(values = c("#FFB612", "#4B92DB")) +
  theme(plot.title = element_text(face = "bold",
                                  size = 20, 
                                  hjust = .5),
        legend.position = "none",
        axis.title = element_text(face = "bold",
                                  size = 15),
        axis.text.y = element_text(face = "italic"),
        axis.text = element_text(size = 13))

eff_movements_perc_top <- eff_movements |> 
  slice_max(eff_metric_perc, n = 5) |> 
  mutate(type = "high")

eff_movements_perc_bottom <- eff_movements |> 
  slice_min(eff_metric_perc, n = 5) |> 
  mutate(type = "low")

eff_movements_perc <- rbind(eff_movements_perc_top, eff_movements_perc_bottom) |> 
  mutate(eff_metric_perc = round(eff_metric_perc, 2)) |> 
  mutate(perc = paste0(eff_metric_perc, "%"))


# Top 5 vs. bottom 5 eff perc
eff_movements_perc |> 
  ggplot(aes(x = eff_metric_perc, y = fct_reorder(displayName, eff_metric_perc), fill = type)) +
  geom_col() +
  labs(x = "Percentage of movements that are effortful", 
       y = "",
       title = "Top and bottom 5 players for effort movement percentages") +
  geom_text(aes(label = perc), hjust = 1, fontface = "bold", size = 5) +
  scale_fill_manual(values = c("#FFB612", "#4B92DB")) +
  theme(plot.title = element_text(face = "bold",
                                  size = 20, 
                                  hjust = .5),
        legend.position = "none",
        axis.title = element_text(face = "bold",
                                  size = 15),
        axis.text.y = element_text(face = "italic"),
        axis.text = element_text(size = 13))


# Effort by play ----------------------------------------------------------
tracking_bc_filtered <- tracking_bc |> 
  filter(displayName %in% rbs)

tracking_bc_effort <- purrr::map(rbs, eff_function, player_table = TRUE) |> 
  bind_rows()

tracking_bc_combined <- left_join(tracking_bc_filtered, tracking_bc_effort, 
                                  by = c("gameId", "bc_id", "playId", "frameId", "displayName"))










eff_movements <- purrr::map(rbs, eff_function) |> 
  bind_rows() |> 
  mutate(displayName = rbs)

eff_movements_top <- eff_movements |> 
  slice_max(eff_ratio, n = 5) |> 
  mutate(type = "high")

eff_movements_bottom <- eff_movements |> 
  slice_min(eff_ratio, n = 5) |> 
  mutate(type = "low")

eff_together <- rbind(eff_movements_top, eff_movements_bottom)

# top 5 vs. bottom 5 eff movements
eff_together |> 
  ggplot(aes(x = eff_ratio, y = fct_reorder(displayName, eff_ratio), fill = type)) +
  geom_col() +
  labs(x = "Effort ratio", 
       y = "",
       title = "Top and bottom 5 players for effort movement ratios") +
  geom_text(aes(label = eff_metric), hjust = 1, nudge_x = -.5, fontface = "bold", size = 5) +
  scale_fill_manual(values = c("#FFB612", "#4B92DB")) +
  theme(plot.title = element_text(face = "bold",
                                  size = 20, 
                                  hjust = .5),
        legend.position = "none",
        axis.title = element_text(face = "bold",
                                  size = 15),
        axis.text.y = element_text(face = "italic"),
        axis.text = element_text(size = 13))





# Quantile regression -----------------------------------------------------
library(quantreg)

set.seed(1)
# Creating a function -----------------------------------------------------
eff_function <- function(name, graph = FALSE) {
  # Filtering the data set to only include the inputted player
  player_runs <- tracking_bc |> 
    filter(displayName == name)

  N_FOLDS <- 5
  player_runs_modeling <- player_runs |> 
    select(s, a) |> 
    mutate(fold = sample(rep(1:N_FOLDS, length.out = n())))
  
  player_runs_cv <- function(x){
    test_data <- player_runs_modeling |> 
      filter(fold == x)
    train_data <- player_runs_modeling |> 
      filter(fold != x)
    
  #quantile regression
  player_runs_rq <- rq(a ~ s, tau=.9, data=train_data)
  
  
  # Predictions
  out <- tibble(
    rq_pred = predict.rq(player_runs_rq, newdata = test_data),
    test_actual = test_data$a,
    res_rq = test_actual-rq_pred,
    test_fold = x
  )
  return(out)
  }
  player_runs_test_preds <- map(1:N_FOLDS, player_runs_cv) |> 
    bind_rows()
  return(player_runs_test_preds)
}

player_runs_rq_output <- eff_function("Saquon Barkley")
#View(player_runs_rq_output)

sum(player_runs_rq_output$res_rq<0)/nrow(player_runs_rq_output)










# Nonparametric quantile regression -------------------------------------------
set.seed(1)
eff_function_rqss <- function(name, graph=FALSE){
  player_runs <- tracking_bc |> 
    filter(displayName == name)

  N_FOLDS <- 5
  player_runs_modeling <- player_runs |> 
    select(s, a) |> 
    mutate(fold = sample(rep(1:N_FOLDS, length.out = n())))
  
  player_runs_cv <- function(x){
    test_data <- player_runs_modeling |> 
      filter(fold==x)
    train_data <- player_runs_modeling |> 
      filter(fold != x)
    
    s_range <- range(train_data$s)
    test_data <- test_data |>
      filter(s >= max(s_range[1], 1),  #>= max of training lower bound and 1
             s <= min(s_range[2], 9))  #<= min of training upper bound and 9
    #nonparametric quantile reg using smooth spline
    #fit a smooth spline of acc as a function of speed
    #lambda controls smoothness (higher =smoother/less wiggly, lower=more flexible)
    rq_fit <- rqss(a~qss(s, lambda=3), tau=.9, data=train_data)
    
    out <- tibble(
      rq_pred = predict(rq_fit, newdata=test_data),
      test_actual = test_data$a,
      res_rq = test_actual - rq_pred,
      test_fold=x
    )
    return(out)
  }
  player_runs_test_preds <- map(1:N_FOLDS, player_runs_cv) |> 
    list_rbind()
  
  #percentage of points below quantile line (not top 10%)
  eff_score <- mean(player_runs_test_preds$res_rq <0)
  
  if (graph == TRUE) {
    player_graph <- player_runs |> 
      ggplot(aes(x = s, y = a)) +
      geom_point(alpha=.5, color="grey2")+
      stat_smooth(method="rqss", formula=y~qss(x,lambda=3),
                  method.args=list(tau=0.9), se=FALSE, color="blue", size=1.2)+
      labs(x = "Speed",
           y = "Acceleration",
           title = paste0(name, "'s effort quantile curve (90%)"),
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
  
  #percentage of points above 90th percentile curve
  #eff_score is perc of points below, 1-eff_score is above
  return(tibble(displayName=name, effort_ratio_90 = 1-eff_score))
}


# Test
eff_function_rqss("Latavius Murray", graph = TRUE)
eff_function_rqss("Saquon Barkley", graph = TRUE)
eff_function_rqss("Saquon Barkley")


# Eff metric for all players ----------------------------------------------

rbs <- unique(rb_stats_total_filtered$displayName)

eff_scores <- purrr::map(rbs, eff_function_rqss) |> 
  bind_rows() |> 
  mutate(displayName = rbs)

eff_scores


# # Gamma function ----------------------------------------------------------
# set.seed(1)
# eff_gamma <- function(name, graph= FALSE){
#   player_runs <- tracking_bc |> 
#     filter(displayName==name) |> 
#     select(s,a) |> 
#     filter(s>0)
#   
#   N_FOLDS <- 5
#   player_runs_modeling <- player_runs |> 
#     mutate(fold=sample(rep(1:N_FOLDS, length.out=n())))
#   
#   player_runs_cv <- function(x){
#     test_data <- player_runs_modeling |> filter(fold == x)
#     train_data <- player_runs_modeling |> filter(fold != x)
#     
#     
#     #fit model with shape of gamma function
#     # a= b_0 * s^b_1 * e^{-b_2*s}
#     #b0 is scale 
#     #b1 is rise
#     #b2 is decay
#     gamma_fit <- tryCatch(
#       nls(a~b0*s^b1*exp(-b2 * s),
#           data=train_data,
#           start=list(b0 = 3, b1 = 2, b2 = 0.15),
#           control=list(maxiter=500)),
#       error=function(e) return(NULL))
#     if(is.null(gamma_fit)) return(NULL)
#     
#     rq_pred <- predict(gamma_fit, newdata = test_data)
#     
#     out <- tibble(
#       rq_pred = rq_pred,
#       test_actual = test_data$a,
#       res_rq = test_actual - rq_pred,
#       test_fold = x
#     )
#     return(out)
#   }
#   player_runs_test_preds <- map(1:N_FOLDS, player_runs_cv) |> 
#     compact() |> 
#     list_rbind()
# 
#   #proportion of points that fall below predicted curve
#   eff_below <- mean(player_runs_test_preds$res_rq < 0)
#   
#   if (graph == TRUE) {
#     
#     #refit for viz
#     gamma_fit_viz <- tryCatch(
#       nls(a ~ b0 * s^b1 * exp(-b2 * s),
#           data = player_runs,
#           start = list(b0 = 3, b1 = 2, b2 = 0.15),
#           control = list(maxiter = 500)),
#       error = function(e) return(NULL)
#     )
#     
#     if (is.null(gamma_fit_viz)) return(NULL)
#     
#     b0 <- coef(gamma_fit_viz)[1]
#     b1 <- coef(gamma_fit_viz)[2]
#     b2 <- coef(gamma_fit_viz)[3]
#     
#     player_graph <- player_runs |> 
#       ggplot(aes(x = s, y = a)) +
#       geom_point(alpha = 0.5, color = "grey2") +
#       stat_function(fun = function(x) b0 * x^b1 * exp(-b2 * x),
#                     color = "blue", size = 1.2) +
#       labs(x = "Speed",
#            y = "Acceleration",
#            title = paste0(name, "'s effort gamma curve"),
#            caption = "Data from Weeks 1-9 of the 2022 NFL Season") +
#       theme(plot.title = element_text(face = "bold", size = 20, hjust = .5),
#             legend.title = element_text(face = "bold", size = 15),
#             axis.title = element_text(face = "bold", size = 15),
#             axis.text = element_text(size = 13),
#             plot.caption = element_text(face = "italic", size = 8))
#     
#     return(player_graph)
#   }
#   
#   return(tibble(displayName = name, effort_ratio = 1 - eff_below))
#   
# }
# 
# 
# 
# # Test
# eff_gamma("Rex Burkhead", graph = TRUE)
# eff_gamma("Saquon Barkley", graph = TRUE)
# eff_gamma("Saquon Barkley")
# 
# 
# # Eff metric for all players ----------------------------------------------
# 
# rbs <- unique(rb_stats_total_filtered$displayName)
# 
# eff_scores <- purrr::map(rbs, eff_gamma) |> 
#   bind_rows() |> 
#   mutate(displayName = rbs)
# 
# eff_scores
# View(eff_scores)

library(magick)
Saquon <- image_read("Shteynberg/Checkpoint_2_Images/Saquon_AS.png")
Rex <- image_read("Shteynberg/Checkpoint_2_Images/Rex_AS.png")
Saquon <- image_scale(Saquon, "x500") 
Rex <- image_scale(Rex, "x500") 

Rex_Saquon_AS <- image_append(c(Saquon, Rex)) 
image_write(Rex_Saquon_AS, "Rex_Saquon_AS.png")




# Nonparam regression 2 splines  ------------------------------------------

# Nonparametric quantile regression -------------------------------------------
set.seed(1)
eff_function_rqss <- function(name, graph=FALSE){
  player_runs <- tracking_bc |> 
    filter(displayName == name)
  
  N_FOLDS <- 5
  player_runs_modeling <- player_runs |> 
    select(s, a) |> 
    mutate(fold = sample(rep(1:N_FOLDS, length.out = n())))
  
  player_runs_cv <- function(x){
    test_data <- player_runs_modeling |> 
      filter(fold==x)
    train_data <- player_runs_modeling |> 
      filter(fold != x)
    
    s_range <- range(train_data$s)
    test_data <- test_data |>
      filter(s >= max(s_range[1], 1),  #>= max of training lower bound and 1
             s <= min(s_range[2], 9))  #<= min of training upper bound and 9
    #nonparametric quantile reg using smooth spline
    #fit a smooth spline of acc as a function of speed
    #lambda controls smoothness (higher =smoother/less wiggly, lower=more flexible)
    rq_fit_95 <- rqss(a~qss(s, lambda=3), tau=.95, data=train_data)
    rq_fit_50 <- rqss(a~qss(s, lambda=3), tau=.5, data=train_data)
    
    out <- tibble(
      rq_pred_95 = predict(rq_fit_95, newdata=test_data),
      rq_pred_50 = predict(rq_fit_50, newdata=test_data),
      test_actual = test_data$a,
      x_values = test_data$s,
      res_rq_95 = test_actual - rq_pred_95,
      res_rq_50 = test_actual - rq_pred_50,
      test_fold=x
    )
    return(out)
  }
  
  spline_fn_95 <- splinefun(out$x_values,out$rq_pred_95)
  spline_fn_50 <- splinefun(out$x_values,out$rq_pred_50)
  
  auc_95 <- integrate(spline_fn_95, lower=min(out$x_values), upper=max(out$x_values))$value
  auc_50 <- integrate(spline_fn_50, lower=min(out$x_values), upper=max(out$x_values))$value
  
  player_runs_test_preds <- map(1:N_FOLDS, player_runs_cv) |> 
    list_rbind()
  
  #percentage of points below quantile line (not top 10%)
  eff_score <- mean(player_runs_test_preds$res_rq <0)
  
  if (graph == TRUE) {
    player_graph <- player_runs |> 
      ggplot(aes(x = s, y = a)) +
      geom_point(alpha=.5, color="grey2")+
      stat_smooth(method="rqss", formula=y~qss(x,lambda=3),
                  method.args=list(tau=0.95), se=FALSE, color="blue", size=1.2)+
      stat_smooth(method="rqss", formula=y~qss(x,lambda=3),
                  method.args=list(tau=0.5), se=FALSE, color="red", size=1.2)+
      labs(x = "Speed",
           y = "Acceleration",
           title = paste0(name, "'s effort quantile curves (95 and 50)"),
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
  
  #percentage of points above 90th percentile curve
  #eff_score is perc of points below, 1-eff_score is above
  return(tibble(displayName=name, effort_ratio_90 = 1-eff_score, auc_diff = auc_95-auc_90))
}


# Test
eff_function_rqss("Latavius Murray", graph = TRUE)
eff_function_rqss("Saquon Barkley", graph = TRUE)
eff_function_rqss("Saquon Barkley")


# Eff metric for all players ----------------------------------------------

rbs <- unique(rb_stats_total_filtered$displayName)

eff_scores <- purrr::map(rbs, eff_function_rqss) |> 
  bind_rows() |> 
  mutate(displayName = rbs)

eff_scores