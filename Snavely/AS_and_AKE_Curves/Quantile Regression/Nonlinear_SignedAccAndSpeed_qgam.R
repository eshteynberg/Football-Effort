library(tidyverse)
library(qgam)

# Player Test -------------------------------------------------------------
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
  select(s_mph, dir_a_mpsh, gameId) |> 
  left_join(plays_folds) |>
  select(-gameId)

player_runs_cv <- function(x){
  test_data <- player_runs_modeling |> 
    filter(fold == x)
  train_data <- player_runs_modeling |> 
    filter(fold != x)
  
  # Modeling
  qgam_fit <- qgam(dir_a_mpsh ~ s(s_mph, k = 10, bs = "ad"),
                   data = train_data,
                   qu = .99)
  qgam_fit_s <- qgam(s_mph ~ s(dir_a_mpsh, k = 10, bs = "ad"),
                     data = train_data,
                     qu = .99)
  
  out <- tibble(
    displayName = "Saquon Barkley",
    qgam_pred = predict(qgam_fit_s, newdata = test_data),
    actual_acc = test_data$dir_a_mpsh,
    actual_speed = test_data$s_mph,
    res = abs(actual_acc - qgam_pred),
    test_fold = x
  )
  return(out)
}

# Saquon
player_runs_test_preds <- map(1:N_FOLDS, player_runs_cv) |> 
  bind_rows()



# Function ----------------------------------------------------------------

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
    select(s_mph, dir_a_mpsh, gameId, bc_id, playId, frameId) |> 
    left_join(plays_folds)

  
  player_runs_cv_pos <- function(x){
    # Finding 99th percentile of speed
    speed_99 <- quantile(player_runs_modeling$s_mph, probs = c(.99))
    
    test_data_pos <- player_runs_modeling |> 
      filter(fold == x) |> 
      filter(dir_a_mpsh >= 0)
    train_data_pos <- player_runs_modeling |> 
      filter(fold != x) |> 
      filter(dir_a_mpsh >= 0)
    
    
    # Modeling
    qgam_fit_a_top <- qgam(dir_a_mpsh ~ s(s_mph, k = 10, bs = "ad"),
                       data = train_data_pos,
                       qu = .98,
                       multicore = TRUE,
                       ncores = 7)
    
    pos <- tibble(
      displayName = name,
      bc_id = test_data_pos$bc_id,
      gameId = test_data_pos$gameId,
      playId = test_data_pos$playId,
      frameId = test_data_pos$frameId,
      qgam_pred = predict(qgam_fit_a_top, newdata = test_data_pos),
      actual_acc = test_data_pos$dir_a_mpsh,
      actual_speed = test_data_pos$s_mph,
      diff_a = qgam_pred - actual_acc, # negative indicates above line
      diff_speed = speed_99 - actual_speed, # negative indicates right of line
      test_fold = x
    )

    return(pos)
  }
  
  player_runs_cv_neg <- function(x){
    # Finding 99th percentile of speed
    speed_99 <- quantile(player_runs_modeling$s_mph, probs = c(.99))
    
    test_data_neg <- player_runs_modeling |> 
      filter(fold == x) |> 
      filter(dir_a_mpsh < 0)
    train_data_neg <- player_runs_modeling |> 
      filter(fold != x) |> 
      filter(dir_a_mpsh < 0)
    
    
    # Modeling
    qgam_fit_a_bottom <- qgam(dir_a_mpsh ~ s(s_mph, k = 10, bs = "ad"),
                              data = train_data_neg,
                              qu = .02,
                              multicore = TRUE,
                              ncores = 7)
    
    neg <- tibble(
      displayName = name,
      bc_id = test_data_neg$bc_id,
      gameId = test_data_neg$gameId,
      playId = test_data_neg$playId,
      frameId = test_data_neg$frameId,
      qgam_pred = predict(qgam_fit_a_bottom, newdata = test_data_neg),
      actual_acc = test_data_neg$dir_a_mpsh,
      actual_speed = test_data_neg$s_mph,
      diff_a = actual_acc - qgam_pred, # negative indicates below line
      diff_speed = speed_99 - actual_speed, # negative indicates right of line
      test_fold = x
    )

    return(neg)
  }
  
  
  # Doing cross fold validation
    player_runs_test_preds_pos <- map(1:N_FOLDS, player_runs_cv_pos) |> 
      bind_rows()

    player_runs_test_preds_neg <- map(1:N_FOLDS, player_runs_cv_neg) |> 
      bind_rows()
    
    player_runs_test_preds <- rbind(player_runs_test_preds_pos, player_runs_test_preds_neg)
  
  if (graph == TRUE) {
    player_graph <- player_runs_test_preds |> 
      ggplot(aes(x = actual_speed, y = actual_acc)) +
      geom_point(alpha=.3, color="grey2")+
      stat_smooth(data = player_runs_test_preds_pos,
                  method="gam", 
                  formula=y~s(x),
                  se=FALSE, 
                  lwd = 1.5, 
                  aes(y = qgam_pred, color="0.98 quantile regression line"), size=1.2) + 
      stat_smooth(data = player_runs_test_preds_neg, 
                  method="gam", 
                  formula=y~s(x),
                  se=FALSE, 
                  lwd = 1.5, 
                  aes(y = qgam_pred, color="0.02 quantile regression line"), size=1.2) +
      geom_vline(aes(color = "Speed 0.99 quantile line", 
                     xintercept = quantile(actual_speed, probs = c(.99))), 
                 lty = 2, lwd = 1.5) +
      geom_hline(aes(yintercept = 0), color = "black", lwd = 1.2, lty = 2) +
      scale_color_manual("Line", values = c("#D55E00", "#0072B2", "goldenrod")) +
      labs(x = "Speed (mph)",
           y = "Acceleration (mph/s)",
           title = paste0(name)) +
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

# Player test
eff_function_qgam_mix("Craig Reynolds", graph = TRUE)
eff_function_qgam_mix("Joshua Kelley", graph = TRUE)
eff_function_qgam_mix("Saquon Barkley", graph = TRUE)

# qgam_mixed <- purrr::map(rbs_names, eff_function_qgam_mix) |>
#   bind_rows()
# write.csv(qgam_mixed, "SignedAccPercentiles.csv")

# Loading in the data
signedPercentiles <- read.csv("created_data/SignedAccPercentiles.csv") |> 
  select(-X) |> 
  rowwise() |> 
  mutate(minimum_diff = min(diff_a, diff_speed), # Finding minimum distance
         adj_minimum_diff = ifelse(minimum_diff <= 0, 0, minimum_diff), # Giving value of 0 for points past lines
         dis_stat = (1 / (1 + adj_minimum_diff)),
         dis_stat_adj = ifelse(actual_acc < 0, dis_stat / 2, dis_stat)) # Penalty for deccelerating

# Final effort score for players
dis_scores_players <- signedPercentiles |> 
  group_by(displayName) |> 
  summarize(dis_score_mix = mean(dis_stat_adj)) |> 
  ungroup()

# For plays
dis_scores_plays <- signedPercentiles |> 
  group_by(gameId, playId, displayName) |> 
  summarize(dis_score_mix = mean(dis_stat_adj)) |> 
  ungroup()

# Factoring in rushing yards
scores_and_rushingYards <- tracking_bc_play_stats |> 
  left_join(dis_scores_plays) |> 
  mutate(dis_score_rush = dis_score_mix * rushingYards)

players_scoresRushed <- scores_and_rushingYards |> 
  group_by(bc_id, displayName) |> 
  summarize(dis_score_rush = mean(dis_score_rush))

cor(scores_and_rushingYards$expectedPointsAdded, scores_and_rushingYards$dis_score_rush)
