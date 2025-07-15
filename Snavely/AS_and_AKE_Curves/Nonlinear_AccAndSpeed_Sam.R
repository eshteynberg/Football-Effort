library(qgam)
library(tidyverse)
?qgam


# Player test -------------------------------------------------------------
set.seed(1)
# Choosing player name
player_runs <- tracking_bc |> 
  filter(displayName == "Saquon Barkley")

# 5 folds
N_FOLDS <- 5

# Making sure plays are in the same fold
plays_folds <- player_runs |> 
  distinct(playId) |> 
  mutate(fold = sample(rep(1:N_FOLDS, length.out = n())))

# Making the modeling data frame
player_runs_modeling <- player_runs |> 
  select(s_mph, a_mpsh, playId) |> 
  left_join(plays_folds) |> 
  select(-playId)

player_runs_cv <- function(x){
  test_data <- player_runs_modeling |> 
    filter(fold == x)
  train_data <- player_runs_modeling |> 
    filter(fold != x)
  
  # Modeling
  qgam_fit <- qgam(a_mpsh ~ s(s_mph),
                   data = train_data,
                   qu = .99)
  
  out <- tibble(
    displayName = "Saquon Barkley",
    qgam_pred = predict(qgam_fit, newdata = test_data),
    actual_acc = test_data$a_mpsh,
    res = abs(actual_acc - qgam_pred),
    test_fold = x
  )
  return(out)
}

# Saquon
player_runs_test_preds <- map(1:N_FOLDS, player_runs_cv) |> 
  bind_rows()


# Function ----------------------------------------------------------------
eff_function_qgam <- function(name, graph = FALSE) {
  # Choosing player name
  player_runs <- tracking_bc |> 
    filter(displayName == name)
  
  # 5 folds
  N_FOLDS <- 5
  
  # Making sure plays are in the same fold
  plays_folds <- player_runs |> 
    distinct(playId) |> 
    mutate(fold = sample(rep(1:N_FOLDS, length.out = n())))
  
  # Making the modeling data frame
  player_runs_modeling <- player_runs |> 
    select(s_mph, a_mpsh, playId) |> 
    left_join(plays_folds) |> 
    select(-playId)
  
  player_runs_cv <- function(x){
    test_data <- player_runs_modeling |> 
      filter(fold == x)
    train_data <- player_runs_modeling |> 
      filter(fold != x)
    
    # Modeling
    qgam_fit <- qgam(a_mpsh ~ s(s_mph, k=20),
                     data = train_data,
                     qu = .99)
    
    out <- tibble(
      displayName = name,
      qgam_pred = predict(qgam_fit, newdata = test_data),
      actual_acc = test_data$a_mpsh,
      actual_speed = test_data$s_mph,
      res = abs(actual_acc - qgam_pred),
      qgam_pred_minus_3 = qgam_pred - 3,
      test_fold = x
    )
    return(out)
  }
  
  # Doing cross fold validation
  player_runs_test_preds <- map(1:N_FOLDS, player_runs_cv) |> 
    bind_rows()
  
  
  
  if (graph == TRUE) {
    player_graph <- player_runs_test_preds |> 
      ggplot(aes(x = actual_speed, y = qgam_pred)) +
      geom_point(aes(y = actual_acc), alpha=.3, color="grey2")+
      stat_smooth(method="gam", formula=y~s(x),
                  method.args=list(se=FALSE), aes(color="99th percentile line"), size=1.2) + 
      stat_smooth(method="gam", formula=y~s(x),
                  method.args=list(se=FALSE), aes(y=qgam_pred_minus_3, color="Relaxed 99th percentile line"), size=1.2) + 
      labs(x = "Adjusted Speed",
           y = "Acceleration",
           title = paste0(name, "'s effort is defined as the mean distance of points \npast the 95th quartile line to the expected acceleration line"),
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
  return(player_runs_test_preds)
}

# Test
eff_function_qgam("Saquon Barkley", graph = TRUE)

# Running the function for every player
rbs <- unique(rb_stats_total_filtered$displayName)

percentile_dists <- purrr::map(rbs, eff_function_qgam) |> 
  bind_rows()
