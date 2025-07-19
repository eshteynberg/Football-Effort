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
  
  player_runs_cv <- function(x){
    test_data <- player_runs_modeling |> 
      filter(fold == x)
    train_data <- player_runs_modeling |> 
      filter(fold != x)
    
    
    # Modeling
    qgam_fit_a_top <- qgam(dir_a_mpsh ~ s(s_mph, k = 10, bs = "ad"),
                       data = train_data,
                       qu = .98,
                       multicore = TRUE,
                       ncores = 7)
    qgam_fit_a_bottom <- qgam(dir_a_mpsh ~ s(s_mph, k = 10, bs = "ad"),
                              data = train_data,
                              qu = .02,
                              multicore = TRUE,
                              ncores = 7)
    
    out <- tibble(
      displayName = name,
      bc_id = test_data$bc_id,
      gameId = test_data$gameId,
      playId = test_data$playId,
      frameId = test_data$frameId,
      qgam_pred_a_top = predict(qgam_fit_a_top, newdata = test_data),
      qgam_pred_a_bottom = predict(qgam_fit_a_bottom, newdata = test_data),
      actual_acc = test_data$dir_a_mpsh,
      actual_speed = test_data$s_mph,
      diff_a_top = qgam_pred_a_top - actual_acc, # negative indicates above line
      diff_a_bottom = actual_acc - qgam_pred_a_bottom, # negative indicates below line
      diff_speed = actual_speed - quantile(actual_speed, probs = c(.99)), # positive indicates right of line
      test_fold = x
    )
    return(out)
  }
  
  # Doing cross fold validation
  player_runs_test_preds <- map(1:N_FOLDS, player_runs_cv) |> 
    bind_rows()
  
  if (graph == TRUE) {
    player_graph <- player_runs_test_preds |> 
      ggplot(aes(x = actual_speed, y = actual_acc)) +
      geom_point(alpha=.3, color="grey2")+
      stat_smooth(method="gam", formula=y~s(x),se=FALSE, lwd = 1.5, aes(y = qgam_pred_a_top, 
                                                                        color="98th acc percentile line"), size=1.2) + 
      stat_smooth(method="gam", formula=y~s(x),se=FALSE, lwd = 1.5, aes(y = qgam_pred_a_bottom,
                                                                        color="2nd acc percentile line"), size=1.2) +
      geom_vline(aes(color = "99th speed percentile line", 
                     xintercept = quantile(actual_speed, probs = c(.99))), 
                 lty = 2, lwd = 1.5) +
      scale_color_manual("Line", values = c("#D55E00", "#0072B2", "goldenrod")) +
      labs(x = "Speed (mph)",
           y = "Acceleration (mph/s)",
           title = paste0(name),
           caption = "Data from Weeks 1-9 of the 2022 NFL Season") +
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
eff_function_qgam_mix("Derrick Henry", graph = TRUE)
eff_function_qgam_mix("Saquon Barkley")

qgam_mixed <- purrr::map(rbs_names, eff_function_qgam_mix) |>
  bind_rows()
write.csv(qgam_mixed, "SignedAccPercentiles.csv")

# Loading in the data
signedPercentiles <- read.csv("created_data/SignedAccPercentiles.csv") |> 
  select(-X) |> 
  mutate(diff_speed = -1 * diff_speed) |> # Accidentally subtracted the two wrong in the data frame
  rowwise() |> 
  mutate(minimum_diff = ifelse(actual_acc >= 0, min(diff_a_top, diff_speed), min(diff_a_bottom, diff_speed)), # Finding minimum distance
         adj_minimum_diff = ifelse(minimum_diff <= 0, 0, minimum_diff), # Giving value of 0 for points past lines
         adj_negative_acc_diff = ifelse(actual_acc < 0, adj_minimum_diff * 1.25, adj_minimum_diff)) |> # Penalty for deccelerating
  mutate(s_mph = actual_speed, dir_a_mpsh = round(actual_acc, 5))

# Final effort score for players
dis_scores_players <- signedPercentiles |> 
  group_by(displayName) |> 
  summarize(dis_score_mix = mean(1 / (1 + adj_negative_acc_diff)))|> 
  ungroup()
